-module(jerome).

-compile(export_all).

% Jerome - a rich-text reader/writer

read(Path, Format) ->
    {ok, Binary} = file:read_file(Path),
    case Format of
        rtf -> jerome_rtf:read(Binary)
    end.

write(Ast, Path, Format) ->
    IOList = case Format of
        html ->
            jerome_html:write(Ast)
    end,
    file:write_file(Path, IOList).
