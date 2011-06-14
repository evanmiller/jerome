-module(jerome_textile_generator).

-export([generate/1]).

generate(Ast) ->
    generate(Ast, []).

generate([], Acc) ->
    lists:reverse(Acc);
generate([{text, Text, Properties}|Rest], Acc) ->
    generate(Rest, [write_attributed_text(Text, Properties)|Acc]);
generate([{table, Rows}|Rest], Acc) ->
    generate(Rest, [generate(Rows, [])|Acc]);
generate([{table_row, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse([generate(Ast, []), "|"], Acc));
generate([{table_cell, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["|", generate(Ast, [])], Acc));
generate([{paragraph, _}|Rest], Acc) ->
    generate(Rest, ["\n"|Acc]);
generate([{list, ListItems}|Rest], Acc) ->
    generate(Rest, [generate(ListItems, [])|Acc]);
generate([{list_item, ListItem}|Rest], Acc) ->
    generate(Rest, lists:reverse(["* ", generate(ListItem, [])], Acc));
generate([{blockquote, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["bq. ", generate(Ast, [])], Acc));
generate([{blockcode, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["bc. ", generate(Ast, [])], Acc));
generate([{preformatted, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["pre. ", generate(Ast, [])], Acc));
generate([{heading, Level, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["h"++integer_to_list(Level)++". ", generate(Ast, [])], Acc));
generate([{image, ImageURL}|Rest], Acc) when is_list(ImageURL) ->
    generate(Rest, lists:reverse(["!", ImageURL, "!"], Acc)).

write_attributed_text(Text, [bold|Rest]) ->
    ["*", write_attributed_text(Text, Rest), "*"];
write_attributed_text(Text, [italic|Rest]) ->
    ["_", write_attributed_text(Text, Rest), "_"];
write_attributed_text(Text, [{hyperlink, Destination}|Rest]) ->
    ["\"", write_attributed_text(Text, Rest), "\":", Destination];
write_attributed_text(Text, [superscript|Rest]) ->
    ["^", write_attributed_text(Text, Rest), "^"];
write_attributed_text(Text, [subscript|Rest]) ->
    ["~", write_attributed_text(Text, Rest), "~"];
write_attributed_text(Text, []) ->
    write_text(Text).

write_text(Text) ->
    Text.
