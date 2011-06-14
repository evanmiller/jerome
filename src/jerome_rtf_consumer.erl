-module(jerome_rtf_consumer).

-include("jerome.hrl").

-export([consume/2]).

recognize_word("fldinst") -> true;
recognize_word(_) -> false.

consume(Binary, ImageFun) when is_binary(Binary) ->
    {ok, Tokens} = jerome_rtf_scanner:scan(unicode:characters_to_list(Binary)),
    {ok, ParseTree} = jerome_rtf_parser:parse(Tokens),
    PrunedTree = prune(ParseTree),
    process_tree(PrunedTree, ImageFun).

process_tree(PrunedTree, ImageFun) ->
    {Ast, _Context} = process_tree(PrunedTree, [], #jerome_ctx{ image_fun = ImageFun }),
    Ast.

process_tree([], Acc, Context) ->
    {jerome:consolidate(lists:reverse(Acc)), Context};
process_tree([{control_word, _, "pc"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ ansi_code_page = 437 });
process_tree([{control_word, _, "pca"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ ansi_code_page = 850 });
process_tree([{control_word, _, "ansicpg", CodePage}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ ansi_code_page = CodePage });
process_tree([{control_word, _, "i"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ italic = true });
process_tree([{control_word, _, "i", 0}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ italic = false });
process_tree([{control_word, _, "b"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ bold = true });
process_tree([{control_word, _, "b", 0}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ bold = false });
process_tree([{control_word, _, "plain"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ italic = false, bold = false, underline = false });
process_tree([{control_word, _, "sub"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ subscript = true, superscript = false });
process_tree([{control_word, _, "super"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ subscript = false, superscript = true });
process_tree([{control_word, _, "nosupersub"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ subscript = false, superscript = false });
process_tree([{control_word, _, "uc", ByteSize}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ unicode_size = ByteSize });
process_tree([{control_word, _, "u", UnicodePoint}|Rest], Acc, #jerome_ctx{ unicode_size = 0 } = Context) ->
    process_tree(Rest, [{text, [UnicodePoint], jerome:text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "ql"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ paragraph_alignment = left });
process_tree([{control_word, _, "qc"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ paragraph_alignment = center });
process_tree([{control_word, _, "qr"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ paragraph_alignment = right });
process_tree([{control_word, _, "qj"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ paragraph_alignment = justified });
process_tree([{control_word, _, "pard"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ paragraph_alignment = left, table = false });
process_tree([{control_word, _, "intbl"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#jerome_ctx{ table = true });
process_tree([{group, [{control_word, _, "NeXTGraphic"}, 
                {text, _, Graphic}|_]}|Rest], Acc, Context) ->
    {ok, Image} = (Context#jerome_ctx.image_fun)(Graphic),
    process_tree(Rest, [{image, Image}|Acc], Context);
process_tree([{table_row, Tree}|Rest], [{table, Rows}|Acc], Context) ->
    {Ast, Context1} = process_tree(Tree, [], Context),
    process_tree(Rest, [{table, Rows ++ [{table_row, Ast}]}|Acc], Context1);
process_tree([{table_row, Tree}|Rest], Acc, Context) ->
    {Ast, Context1} = process_tree(Tree, [], Context),
    process_tree(Rest, [{table, [{table_row, Ast}]}|Acc], Context1);
process_tree([{table_cell, Tree}|Rest], Acc, Context) ->
    {Ast, Context1} = process_tree(Tree, [], Context),
    process_tree(Rest, [{table_cell, Ast}|Acc], Context1);
process_tree([{group, [{control_word, _, "info"}|_]}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context);
process_tree([{group, [{control_word, _, "fonttbl"}|_]}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context);
process_tree([{group, [{control_word, _, "colortbl"}|_]}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context);
process_tree([{group, [{control_word, _, "field"}, 
                {group, [{control_word, _, "fldinst"}, 
                        {group, [{text, _, "HYPERLINK "++Hyperlink}]}]}, 
                {group, [{control_word, _, "fldrslt"}|Text]}]}|Rest], Acc, Context) ->
    {Ast, _} = process_tree(Text, [], Context#jerome_ctx{ hyperlink = lists:sublist(Hyperlink, 2, length(Hyperlink)-2)}),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{list_item, _Bullet, Contents}|Rest], [{list, ListItems}|Acc], Context) ->
    {Ast, Context1} = process_tree(Contents, [], Context),
    process_tree(Rest, [{list, [{list_item, Ast}|ListItems]}|Acc], Context1);
process_tree([{list_item, _Bullet, Contents}|Rest], Acc, Context) ->
    {Ast, Context1} = process_tree(Contents, [], Context),
    process_tree(Rest, [{list, [{list_item, Ast}]}|Acc], Context1);
process_tree([{group, Tree}|Rest], Acc, Context) ->
    {Ast, _} = process_tree(Tree, [], Context),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{text, _, Text}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, Text, jerome:text_properties(Context)}|Acc], Context);
process_tree([{new_paragraph, _}|Rest], Acc, Context) ->
    process_tree(Rest, [{paragraph, Context#jerome_ctx.paragraph_alignment}|Acc], Context);
process_tree([{control_word, _, "line"}|Rest], Acc, Context) ->
    process_tree(Rest, [break|Acc], Context);
% UNICODE ALERT
process_tree([{control_word, _, "bullet"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, "•", jerome:text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "lquote"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, "‘", jerome:text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "rquote"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, "’", jerome:text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "ldblquote"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, "“", jerome:text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "rdblquote"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, "”", jerome:text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "emdash"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, "—", jerome:text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "endash"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, "–", jerome:text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "emspace"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, " ", jerome:text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "enspace"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, " ", jerome:text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "qmspace"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, " ", jerome:text_properties(Context)}|Acc], Context);
% END UNICODE ALERT
process_tree([{control_char, _, '~'}|Rest], Acc, Context) ->
    process_tree(Rest, [{nonbreaking_space, jerome:text_properties(Context)}|Acc], Context);
process_tree([{control_char, _, '-'}|Rest], Acc, Context) ->
    process_tree(Rest, [{optional_hyphen, jerome:text_properties(Context)}|Acc], Context);
process_tree([{control_char, _, '_'}|Rest], Acc, Context) ->
    process_tree(Rest, [{nonbreaking_hyphen, jerome:text_properties(Context)}|Acc], Context);
process_tree([{control_hex, _, Char}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, [to_unicode(Char)], jerome:text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, _}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context);
process_tree([{control_word, _, _, _}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context).

prune(ParseTree) ->
    prune(ParseTree, []).

prune([], Acc) ->
    lists:reverse(Acc);

prune([{group, Tree}|Rest], Acc) ->
    case prune(Tree, []) of
        [] -> prune(Rest, Acc);
        PrunedTree -> prune(Rest, [{group, PrunedTree}|Acc])
    end;

prune([{control_char, _, '*'}, {control_word, _, Word} = ControlToken |Rest], Acc) ->
    case recognize_word(Word) of
        true -> prune(Rest, [ControlToken|Acc]);
        false -> []
    end;

prune([{control_char, _, '*'}, {control_word, _, Word, _} = ControlToken |Rest], Acc) ->
    case recognize_word(Word) of
        true -> prune(Rest, [ControlToken|Acc]);
        false -> []
    end;

prune([Other|Rest], Acc) ->
    prune(Rest, [Other|Acc]).


to_unicode(C) when C < 16#80; C >= 16#A0 ->
    C;
to_unicode(C) ->
    case C of
        16#80 -> 16#20AC;
        16#82 -> 16#201A;
        16#83 -> 16#0192;
        16#84 -> 16#201E;
        16#85 -> 16#2026;
        16#86 -> 16#2020;
        16#87 -> 16#2021;
        16#88 -> 16#02C6;
        16#89 -> 16#2030;
        16#8A -> 16#0160;
        16#8B -> 16#2039;
        16#8C -> 16#0152;
        % 0x8D UNDEFINED
        16#8E -> 16#017D;
        % 0x8F UNDEFINED
        % 0x90 UNDEFINED
        16#91 -> 16#2018;
        16#92 -> 16#2019;
        16#93 -> 16#201C;
        16#94 -> 16#201D;
        16#95 -> 16#2022;
        16#96 -> 16#2013;
        16#97 -> 16#2014;
        16#98 -> 16#02DC;
        16#99 -> 16#2122;
        16#9A -> 16#0161;
        16#9B -> 16#203A;
        16#9C -> 16#0153;
        % 0x9D UNDEFINED
        16#9E -> 16#017E;
        16#9F -> 16#0178
    end.
