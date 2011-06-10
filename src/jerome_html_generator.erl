-module(jerome_html_generator).

-export([generate/1]).


generate(Ast) ->
    generate(Ast, []).

generate([], Acc) ->
    lists:reverse(Acc);
generate([{text, Text, Properties}|Rest], Acc) ->
    generate(Rest, [write_attributed_text(Text, Properties)|Acc]);
generate([{table, Rows}|Rest], Acc) ->
    generate(Rest, lists:reverse(["<table>", generate(Rows, []), "</table>"], Acc));
generate([{table_row, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["<tr>", generate(Ast, []), "</tr>"], Acc));
generate([{table_cell, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["<td>", generate(Ast, []), "</td>"], Acc));
generate([{paragraph, _}|Rest], Acc) ->
    generate(Rest, ["<br>"|Acc]);
generate([{list, ListItems}|Rest], Acc) ->
    generate(Rest, lists:reverse(["<ul>", generate(ListItems, []), "</ul>"], Acc));
generate([{list_item, ListItem}|Rest], Acc) ->
    generate(Rest, lists:reverse(["<li>", generate(ListItem, []), "</li>"], Acc));
generate([{blockquote, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["<blockquote>", generate(Ast, []), "</blockquote>"], Acc));
generate([{blockcode, Ast}|Rest], Acc) -> % TODO
    generate(Rest, lists:reverse(["<code><pre>", generate(Ast, []), "</pre></code>"], Acc));
generate([{preformatted, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["<pre>", generate(Ast, []), "</pre>"], Acc));
generate([{heading, Level, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["<h"++integer_to_list(Level)++">", generate(Ast, []), "</h"++integer_to_list(Level)++">"], Acc)).

write_attributed_text(Text, [bold|Rest]) ->
    ["<strong>", write_attributed_text(Text, Rest), "</strong>"];
write_attributed_text(Text, [italic|Rest]) ->
    ["<em>", write_attributed_text(Text, Rest), "</em>"];
write_attributed_text(Text, [{hyperlink, Destination}|Rest]) ->
    ["<a href=\"", Destination, "\">", write_attributed_text(Text, Rest), "</a>"];
write_attributed_text(Text, []) ->
    write_text(Text).

write_text(Text) ->
    write_text(Text, []).

write_text([], Acc) ->
    lists:reverse(Acc);
write_text([H|T], Acc) when H > 127 ->
    write_text(T, lists:reverse("&#"++integer_to_list(H)++";", Acc));
write_text([H|T], Acc) ->
    write_text(T, [H|Acc]).
