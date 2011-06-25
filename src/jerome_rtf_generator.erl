-module(jerome_rtf_generator).

-export([generate/1]).

generate(Ast) ->
    ["{\\rtf1\\ansi\\ansicpg1252\n",
        "{\\fonttbl\\f0\\fswiss\\fcharset0 Helvetica;}\n",
        "{\\colortbl;\\red255\\green255\\blue255;}\n",
        "\\pard\\pardirnatural\\uc0\n", generate(Ast, []), "}"].

generate([], Acc) ->
    lists:reverse(Acc);
generate([{text, Text, Properties}|Rest], Acc) ->
    generate(Rest, [write_attributed_text(Text, Properties)|Acc]);
generate([{table, Rows}|Rest], Acc) ->
    generate(Rest, [generate(Rows, [])|Acc]);
generate([{table_row, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["\\trowd ", generate(Ast, []), "\\row\n"], Acc));
generate([{table_cell, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["\\pard\\intbl ", generate(Ast, []), "\\cell\n"], Acc));
generate([{list, ListItems}|Rest], Acc) ->
    generate(Rest, [generate(ListItems, [])|Acc]);
generate([{list_item, ListItem}|Rest], Acc) ->
    generate(Rest, lists:reverse(["{\\listtext\\uc0\\u9642}", generate(ListItem, [])], Acc));
generate([{paragraph, _}|Rest], Acc) ->
    generate(Rest, ["\\\n"|Acc]);
generate([{image, ImageBinary}|Rest], Acc) when is_binary(ImageBinary) ->
    {PictType, Width, Height} = image_info(ImageBinary),
    generate(Rest, lists:reverse(["{\\*\\shppict {\\pict ",
                "\\picw", integer_to_list(Width), "\\pich", integer_to_list(Height), 
                PictType, "\\bin", integer_to_list(byte_size(ImageBinary)), " ",
                ImageBinary, "}}"], Acc)).

image_info(<<137, $P, $N, $G, $\r, $\n, 26, $\n, _Length:32, $I, $H, $D, $R, Width:32, Height:32, _/binary>>) ->
    {"\\pngblip", Width, Height}.

write_attributed_text(Text, [bold|Rest]) ->
    ["\\b ", write_attributed_text(Text, Rest), "\\b0 "];
write_attributed_text(Text, [italic|Rest]) ->
    ["\\i ", write_attributed_text(Text, Rest), "\\i0 "];
write_attributed_text(Text, [superscript|Rest]) ->
    ["\\super ", write_attributed_text(Text, Rest), "\\nosupersub "];
write_attributed_text(Text, [subscript|Rest]) ->
    ["\\sub ", write_attributed_text(Text, Rest), "\\nosupersub "];
write_attributed_text(Text, [{hyperlink, Destination}|Rest]) ->
    ["{\\field{\\*\\fldinst{HYPERLINK \"", Destination, "\"}}{\\fldrslt ", 
        write_attributed_text(Text, Rest), "}}"];
write_attributed_text(Text, []) ->
    write_text(Text).

write_text(Text) ->
    write_text(Text, []).

write_text([], Acc) ->
    lists:reverse(Acc);
write_text([H|T], Acc) when H > 127 ->
    write_text(T, lists:reverse("\\u"++integer_to_list(H), Acc));
write_text([H|T], Acc) ->
    write_text(T, [H|Acc]).
