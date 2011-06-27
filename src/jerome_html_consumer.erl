-module(jerome_html_consumer).

-export([consume/2]).

-include("jerome.hrl").

consume(Binary, ImageFun) when is_binary(Binary), is_function(ImageFun) ->
    {ok, Tokens} = jerome_html_scanner:scan(unicode:characters_to_list(Binary)),
    {ok, ParseTree} = jerome_html_parser:parse(Tokens),
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
process_tree([{hyperlink, {open_url, _, Value}, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context#jerome_ctx{ hyperlink = Value }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{text, _, Text}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, Text, jerome:text_properties(Context)}|Acc], Context);
process_tree([{newline, _}|Rest], Acc, Context) ->
    process_tree(Rest, [{paragraph, left}|Acc], Context).
