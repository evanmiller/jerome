-module(jerome_html).

-compile(export_all).


write_text(Text, [bold|Rest]) ->
    ["<strong>", write_text(Text, Rest), "</strong>"];
write_text(Text, [italic|Rest]) ->
    ["<em>", write_text(Text, Rest), "</em>"];
write_text(Text, [{hyperlink, Destination}|Rest]) ->
    ["<a href=\"", Destination, "\">", write_text(Text, Rest), "</a>"];
write_text(Text, []) ->
    Text.

write(Ast) ->
    write(Ast, []).

write([], Acc) ->
    lists:reverse(Acc);
write([{text, Text, Properties}|Rest], Acc) ->
    write(Rest, [write_text(Text, Properties)|Acc]);
write([{table, Rows}|Rest], Acc) ->
    write(Rest, lists:reverse(["<table>", write(Rows, []), "</table>"], Acc));
write([{table_row, Ast}|Rest], Acc) ->
    write(Rest, lists:reverse(["<tr>", write(Ast, []), "</tr>"], Acc));
write([{table_cell, Ast}|Rest], Acc) ->
    write(Rest, lists:reverse(["<td>", write(Ast, []), "</td>"], Acc));
write([{paragraph, _}|Rest], Acc) ->
    write(Rest, ["<br>"|Acc]).
