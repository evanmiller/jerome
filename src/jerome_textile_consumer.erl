-module(jerome_textile_consumer).

-include("jerome.hrl").

-export([consume/2]).

consume(Binary, ImageFun) when is_binary(Binary), is_function(ImageFun)  ->
    {ok, Tokens} = jerome_textile_scanner:scan(binary_to_list(Binary)),
    {ok, ParseTree} = jerome_textile_parser:parse(Tokens),
    process_tree(ParseTree, ImageFun).

process_tree(Tree, ImageFun) ->
    process_tree(Tree, [], #jerome_ctx{ image_fun = ImageFun }).

process_tree([], Acc, _) ->
    jerome:consolidate(lists:reverse(Acc));
process_tree([{strong, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context#jerome_ctx{ bold = true }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{bold, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context#jerome_ctx{ bold = true }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{em, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context#jerome_ctx{ italic = true }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{italic, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context#jerome_ctx{ italic = true }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{superscript, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context#jerome_ctx{ superscript = true }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{subscript, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context#jerome_ctx{ subscript = true }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{block, {block_tag, _, [C]}, Elements}|Rest], [{list, ListItems}|Acc], Context) when C=:=$*; C=:=$#->
    Ast = process_tree(Elements, [], Context),
    process_tree(Rest, [{list, [{list_item, Ast}|ListItems]}|Acc], Context);
process_tree([{block, {block_tag, _, [C]}, Elements}|Rest], Acc, Context) when C=:=$*; C=:=$#->
    Ast = process_tree(Elements, [], Context),
    process_tree(Rest, [{list, [{list_item, Ast}]}|Acc], Context);
process_tree([{block, {block_tag, _, "bq"}, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context),
    process_tree(Rest, [{blockquote, Ast}|Acc], Context);
process_tree([{block, {block_tag, _, "bc"}, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context),
    process_tree(Rest, [{blockcode, Ast}|Acc], Context);
process_tree([{block, {block_tag, _, "pre"}, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context),
    process_tree(Rest, [{preformatted, Ast}|Acc], Context);
process_tree([{block, {block_tag, _, [$h, C]}, Elements}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context),
    process_tree(Rest, [{heading, C - $0, Ast}|Acc], Context);
process_tree([{punctuation, _, " - "}|Rest], Acc, Context) ->
    Props = jerome:text_properties(Context),
    process_tree(Rest, [{text, [$\ , 8211, $\ ], Props}|Acc], Context);
process_tree([{punctuation, _, " -- "}|Rest], Acc, Context) ->
    Props = jerome:text_properties(Context),
    process_tree(Rest, [{text, [$\ , 8212, $\ ], Props}|Acc], Context);
process_tree([{punctuation, _, "(r)"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, [16#00AE], jerome:text_properties(Context)}|Acc], Context);
process_tree([{punctuation, _, "(c)"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, [16#00A9], jerome:text_properties(Context)}|Acc], Context);
process_tree([{punctuation, _, "(tm)"}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, [16#2122], jerome:text_properties(Context)}|Acc], Context);
process_tree([{punctuation, _, " x "}|Rest], Acc, Context) ->
    Props = jerome:text_properties(Context),
    process_tree(Rest, [{text, [$\ , 16#00D7, $\ ], Props}|Acc], Context);
process_tree([{newline, _}|Rest], Acc, _Context) ->
    process_tree(Rest, [{paragraph, left}|Acc], #jerome_ctx{});
process_tree([{hyperlink, _, Elements, {url, _, Link}}|Rest], Acc, Context) ->
    Ast = process_tree(Elements, [], Context#jerome_ctx{ hyperlink = Link }),
    process_tree(Rest, lists:reverse(Ast, Acc), Context);
process_tree([{double_quote, _, _}|Rest], Acc, #jerome_ctx{ double_quote_count = Count } = Context) ->
    process_tree(Rest, [{text, [16#201C + (Count rem 2)], jerome:text_properties(Context)}|Acc], 
        Context#jerome_ctx{ double_quote_count = Count + 1 });
process_tree([{image, _, ImageURL}|Rest], Acc, #jerome_ctx{ image_fun = ImageFun } = Context) ->
    {ok, ImageResult} = ImageFun(ImageURL),
    process_tree(Rest, [{image, ImageResult}|Acc], Context);
process_tree([{text, _, Text}|Rest], Acc, Context) ->
    process_tree(Rest, [{text, Text, jerome:text_properties(Context)}|Acc], Context).
