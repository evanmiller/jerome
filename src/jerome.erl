-module(jerome).

-include("jerome.hrl").

-compile(export_all).

% Jerome - a rich-text reader/writer

parse(Path, Format) when is_list(Path) ->
    {ok, Binary} = file:read_file(Path),
    parse(Binary, Format);

parse(Binary, Format) when is_binary(Binary) ->
    case Format of
        rtf -> jerome_rtf_consumer:consume(Binary)
    end.

generate(Ast, Format) ->
    case Format of
        html ->
            jerome_html_generator:generate(Ast);
        rtf ->
            jerome_rtf_generator:generate(Ast)
    end.

consolidate(List) ->
    consolidate(List, []).

consolidate([], Acc) ->
    lists:reverse(Acc);
consolidate([{text, Text2, Props1}|T], [{text, Text1, Props1}|Acc]) ->
    consolidate(T, [{text, Text1 ++ Text2, Props1}|Acc]);
consolidate([H|T], Acc) ->
    consolidate(T, [H|Acc]).

text_properties(#jerome_ctx{ italic = true } = Ctx) ->
    [italic] ++ text_properties(Ctx#jerome_ctx{ italic = false });
text_properties(#jerome_ctx{ bold = true } = Ctx) ->
    [bold] ++ text_properties(Ctx#jerome_ctx{ bold = false });
text_properties(#jerome_ctx{ underline = true } = Ctx) ->
    [underline] ++ text_properties(Ctx#jerome_ctx{ underline = false });
text_properties(#jerome_ctx{ hyperlink = Link } = Ctx) when Link =/= undefined ->
    [{hyperlink, Link}] ++ text_properties(Ctx#jerome_ctx{ hyperlink = undefined });
text_properties(#jerome_ctx{ superscript = true } = Ctx) ->
    [superscript] ++ text_properties(Ctx#jerome_ctx{ superscript = false });
text_properties(#jerome_ctx{ subscript = true } = Ctx) ->
    [subscript] ++ text_properties(Ctx#jerome_ctx{ subscript = false });
text_properties(_) -> [].

