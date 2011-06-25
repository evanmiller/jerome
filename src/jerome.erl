-module(jerome).

-include("jerome.hrl").

-compile(export_all).

% Jerome - a rich-text reader/writer

parse(PathOrBinary, Format) ->
    parse(PathOrBinary, Format, fun(Img) -> {ok, Img} end).

parse(Path, Format, ImageFun) when is_list(Path) ->
    {ok, Binary} = file:read_file(Path),
    parse(Binary, Format, ImageFun);

parse(Binary, bbcode, ImageFun) when is_binary(Binary) ->
    jerome_bbcode_consumer:consume(Binary, ImageFun);
parse(Binary, rtf, ImageFun) when is_binary(Binary) ->
    jerome_rtf_consumer:consume(Binary, ImageFun);
parse(Binary, textile, ImageFun) when is_binary(Binary) ->
    jerome_textile_consumer:consume(Binary, ImageFun).

generate(Ast, Format) ->
    case Format of
        bbcode -> jerome_bbcode_generator:generate(Ast);
        html -> jerome_html_generator:generate(Ast);
        rtf -> jerome_rtf_generator:generate(Ast);
        textile -> jerome_textile_generator:generate(Ast)
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

image_mime_type(<<137, $P, $N, $G,$\r, $\n, 26, $\n, _/binary>>) -> "image/png";
image_mime_type(<<16#FF, 16#D8, 16#FF, 16#E0, _/binary>>) -> "image/jpeg";
image_mime_type(<<16#FF, 16#D8, 16#FF, 16#E1, _/binary>>) -> "image/jpeg";
image_mime_type(<<$G, $I, $F, $8, $7, $a>>) -> "image/gif";
image_mime_type(<<$G, $I, $F, $8, $9, $a>>) -> "image/gif";
image_mime_type(<<(16#4949):16, (42):16/little, _/binary>>) -> "image/tiff";
image_mime_type(<<(16#4D4D):16, (42):16/big, _/binary>>) -> "image/tiff".

