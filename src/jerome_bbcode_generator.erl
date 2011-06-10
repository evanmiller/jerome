-module(jerome_bbcode_generator).

-compile(export_all).


generate(Ast) ->
    generate(Ast, []).

generate([], Acc) ->
    lists:reverse(Acc);
generate([{text, Text, Properties}|Rest], Acc) ->
    generate(Rest, [write_attributed_text(Text, Properties)|Acc]);
generate([{table, Rows}|Rest], Acc) ->
    generate(Rest, lists:reverse(["[table]", generate(Rows, []), "[/table]"], Acc));
generate([{table_row, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["[tr]", generate(Ast, []), "[/tr]"], Acc));
generate([{table_cell, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["[td]", generate(Ast, []), "[/td]"], Acc));
generate([{paragraph, _}|Rest], Acc) ->
    generate(Rest, ["\r\n"|Acc]);
generate([{list, ListItems}|Rest], Acc) ->
    generate(Rest, lists:reverse(["[list]", generate(ListItems, []), "[/list]"], Acc));
generate([{list_item, ListItem}|Rest], Acc) ->
    generate(Rest, lists:reverse(["[*]", generate(ListItem, [])], Acc));
generate([{blockquote, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["[quote]", generate(Ast, []), "[/quote]"], Acc));
generate([{blockcode, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["[code]", generate(Ast, []), "[/code]"], Acc));
generate([{preformatted, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["[code]", generate(Ast, []), "[/pre]"], Acc));
generate([{heading, Level, Ast}|Rest], Acc) ->
    generate(Rest, lists:reverse(["[h"++integer_to_list(Level)++"]", generate(Ast, []), "[/h"++integer_to_list(Level)++"]"], Acc)).

write_attributed_text(Text, [bold|Rest]) ->
    ["[b]", write_attributed_text(Text, Rest), "[/b]"];
write_attributed_text(Text, [italic|Rest]) ->
    ["[i]", write_attributed_text(Text, Rest), "[/i]"];
write_attributed_text(Text, [{hyperlink, Destination}|Rest]) ->
    ["[url=", Destination, "]", write_attributed_text(Text, Rest), "[/url]"];
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
