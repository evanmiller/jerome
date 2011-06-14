-module(jerome_bbcode_consumer).

-export([consume/2]).

-include("jerome.hrl").

consume(Binary, ImageFun) when is_binary(Binary), is_function(ImageFun) ->
    {ok, Tokens} = jerome_bbcode_scanner:scan(unicode:characters_to_list(Binary)),
    {ok, ParseTree} = jerome_bbcode_parser:parse(Tokens),
    process_tree(ParseTree, ImageFun).

process_tree(Tree, ImageFun) ->
    process_tree(Tree, [], #jerome_ctx{ image_fun = ImageFun }).

process_tree([], Acc, _) ->
    jerome:consolidate(lists:reverse(Acc));
process_tree([{bold, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context#jerome_ctx{ bold = true }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{italic, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context#jerome_ctx{ italic = true }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{underline, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context#jerome_ctx{ underline = true }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{superscript, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context#jerome_ctx{ superscript = true }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{subscript, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context#jerome_ctx{ subscript = true }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{hyperlink, {text, _, Value} = Token}|Rest], Acc, Context) ->
    Ast = process_tree([Token], [], Context#jerome_ctx{ hyperlink = Value }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{hyperlink, {text, _, Value}, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context#jerome_ctx{ hyperlink = Value }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{image, {text, _, Value}}|Rest], Acc, Context) ->
    {ok, Image} = (Context#jerome_ctx.image_fun)(Value),
    process_tree(Rest, [{image, Image}|Acc], Context);
process_tree([{quote, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context),
    process_tree(Rest, [{blockquote, Ast}|Acc], Context);
process_tree([{code, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context),
    process_tree(Rest, [{blockcode, Ast}|Acc], Context);
process_tree([{list, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context),
    process_tree(Rest, [{list, Ast}|Acc], Context);
process_tree([{list_item, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context),
    process_tree(Rest, [{list_item, Ast}|Acc], Context);
process_tree([{table, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context),
    process_tree(Rest, [{table, Ast}|Acc], Context);
process_tree([{table_row, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context),
    process_tree(Rest, [{table_row, Ast}|Acc], Context);
process_tree([{table_cell, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context),
    process_tree(Rest, [{table_cell, Ast}|Acc], Context);
process_tree([{text, _, Text}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, Text, jerome:text_properties(Context)}|Acc], Context);
process_tree([{newline, _}, {newline, _}|Rest], Acc, Context) ->
    process_tree(Rest, [{paragraph, left}, {paragraph, left}|Acc], Context);
process_tree([{newline, _}|Rest], Acc, Context) ->
    process_tree(Rest, Acc, Context).
