-module(jerome_rtf).

-compile(export_all).

-record(rtf_context, {
        italic = false,
        bold = false,
        underline = false,
        paragraph_alignment = left,
        ansi_code_page = undefined,
        hyperlink = undefined,
        table = false,
        unicode_size = 1
    }).

recognize_word("fldinst") -> true;
recognize_word(_) -> false.

text_properties(#rtf_context{ italic = true } = Ctx) ->
    [italic] ++ text_properties(Ctx#rtf_context{ italic = false });
text_properties(#rtf_context{ bold = true } = Ctx) ->
    [bold] ++ text_properties(Ctx#rtf_context{ bold = false });
text_properties(#rtf_context{ underline = true } = Ctx) ->
    [underline] ++ text_properties(Ctx#rtf_context{ underline = false });
text_properties(#rtf_context{ hyperlink = Link } = Ctx) when Link =/= undefined ->
    [{hyperlink, lists:sublist(Link, 2, length(Link)-2)}] ++ 
        text_properties(Ctx#rtf_context{ hyperlink = undefined });
text_properties(_) -> [].

read(Binary) when is_binary(Binary) ->
    {ok, Tokens} = jerome_rtf_scanner:scan(binary_to_list(Binary)),
    {ok, ParseTree} = jerome_rtf_parser:parse(Tokens),
    PrunedTree = prune(ParseTree),
    process_tree(PrunedTree).

process_tree(PrunedTree) ->
    {Ast, _Context} = process_tree(PrunedTree, [], #rtf_context{}),
    lists:reverse(Ast).

process_tree([], Acc, Context) ->
    {Acc, Context};
process_tree([{control_word, _, "pc"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ ansi_code_page = 437 });
process_tree([{control_word, _, "pca"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ ansi_code_page = 850 });
process_tree([{control_word, _, "ansicpg", CodePage}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ ansi_code_page = CodePage });
process_tree([{control_word, _, "i"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ italic = true });
process_tree([{control_word, _, "i", 0}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ italic = false });
process_tree([{control_word, _, "b"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ bold = true });
process_tree([{control_word, _, "b", 0}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ bold = false });
process_tree([{control_word, _, "plain"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ italic = false, bold = false, underline = false });
process_tree([{control_word, _, "uc", ByteSize}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ unicode_size = ByteSize });
process_tree([{control_word, _, "u", UnicodePoint}|Rest], Acc, #rtf_context{ unicode_size = 0 } = Context) ->
    process_tree(Rest, [{text, unicode:characters_to_binary([UnicodePoint], unicode, utf8), text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "ql"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ paragraph_alignment = left });
process_tree([{control_word, _, "qc"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ paragraph_alignment = center });
process_tree([{control_word, _, "qr"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ paragraph_alignment = right });
process_tree([{control_word, _, "qj"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ paragraph_alignment = justified });
process_tree([{control_word, _, "pard"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ paragraph_alignment = left, table = false });
process_tree([{control_word, _, "intbl"}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context#rtf_context{ table = true });
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
    {Ast, _} = process_tree(Text, [], Context#rtf_context{ hyperlink = Hyperlink }),
    process_tree(Rest, Ast ++ Acc, Context);
process_tree([{group, Tree}|Rest], Acc, Context) ->
    {Ast, _} = process_tree(Tree, [], Context),
    process_tree(Rest, Ast ++ Acc, Context);
process_tree([{text, _, Text}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, Text, text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "par"}|Rest], Acc, Context) ->
    process_tree(Rest, [{paragraph, Context#rtf_context.paragraph_alignment}|Acc], Context);
process_tree([{control_word, _, "line"}|Rest], Acc, Context) ->
    process_tree(Rest, [break|Acc], Context);
% UNICODE ALERT
process_tree([{control_word, _, "bullet"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, "•", text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "lquote"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, "‘", text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "rquote"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, "’", text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "ldblquote"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, "“", text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "rdblquote"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, "”", text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "emdash"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, "—", text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "endash"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, "–", text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "emspace"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, " ", text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "enspace"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, " ", text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, "qmspace"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, " ", text_properties(Context)}|Acc], Context);
% END UNICODE ALERT
process_tree([{control_char, _, '~'}|Rest], Acc, Context) ->
    process_tree(Rest, [{nonbreaking_space, text_properties(Context)}|Acc], Context);
process_tree([{control_char, _, '-'}|Rest], Acc, Context) ->
    process_tree(Rest, [{optional_hyphen, text_properties(Context)}|Acc], Context);
process_tree([{control_char, _, '_'}|Rest], Acc, Context) ->
    process_tree(Rest, [{nonbreaking_hyphen, text_properties(Context)}|Acc], Context);
process_tree([{control_hex, _, Char}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, [to_unicode(Char, Context#rtf_context.ansi_code_page)], text_properties(Context)}|Acc], Context);
process_tree([{control_word, _, _}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context);
process_tree([{control_word, _, _, _}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context).


to_unicode(_, _) -> % TODO
    $..

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
