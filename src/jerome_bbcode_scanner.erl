-module(jerome_bbcode_scanner).

-compile(export_all).

scan(BBCode) ->
    scan(BBCode, [], {1, 1}, text).

scan([], Scanned, _, _) ->
    {ok, lists:reverse(
            lists:map(fun
                    ({text, Pos, Text}) ->
                        {text, Pos, lists:reverse(Text)};
                    ({url_value, Pos, Value}) ->
                        {url_value, Pos, lists:reverse(Value)};
                    (Token) ->
                        Token
                end, Scanned))};

scan("[b]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_bold, Pos}|Scanned], {Row, Column + length("[b]")}, text);
scan("[/b]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_bold, Pos}|Scanned], {Row, Column + length("[/b]")}, text);
scan("[i]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_italic, Pos}|Scanned], {Row, Column + length("[i]")}, text);
scan("[/i]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_italic, Pos}|Scanned], {Row, Column + length("[/i]")}, text);
scan("[u]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_underline, Pos}|Scanned], {Row, Column + length("[u]")}, text);
scan("[/u]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_underline, Pos}|Scanned], {Row, Column + length("[/u]")}, text);
scan("[sub]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_subscript, Pos}|Scanned], {Row, Column + length("[sub]")}, text);
scan("[/sub]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_subscript, Pos}|Scanned], {Row, Column + length("[/sub]")}, text);
scan("[sup]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_superscript, Pos}|Scanned], {Row, Column + length("[sup]")}, text);
scan("[/sup]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_superscript, Pos}|Scanned], {Row, Column + length("[/sup]")}, text);
scan("[url]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_url, Pos}|Scanned], {Row, Column + length("[url]")}, text);
scan("[/url]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_url, Pos}|Scanned], {Row, Column + length("[/url]")}, text);
scan("[url="++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_url_equals, Pos}|Scanned], {Row, Column + length("[url=")}, in_url);
scan("]"++T, Scanned, {Row, Column}, in_url) ->
    scan(T, Scanned, {Row, Column + 1}, text);
scan([H|T], [{url_value, UPos, Value}|Scanned], {Row, Column}, in_url) ->
    scan(T, [{url_value, UPos, [H|Value]}|Scanned], {Row, Column + 1}, in_url);
scan([H|T], Scanned, {Row, Column} = Pos, in_url) ->
    scan(T, [{url_value, Pos, [H]}|Scanned], {Row, Column + 1}, in_url);
scan("[quote]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_quote, Pos}|Scanned], {Row, Column + length("[quote]")}, text);
scan("[/quote]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_quote, Pos}|Scanned], {Row, Column + length("[/quote]")}, text);
scan("[code]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_code, Pos}|Scanned], {Row, Column + length("[code]")}, text);
scan("[/code]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_code, Pos}|Scanned], {Row, Column + length("[/code]")}, text);
scan("[list]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_list, Pos}|Scanned], {Row, Column + length("[list]")}, text);
scan("[/list]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_list, Pos}|Scanned], {Row, Column + length("[/list]")}, text);
scan("[*]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{list_item, Pos}|Scanned], {Row, Column + length("[*]")}, text);
scan("[table]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_table, Pos}|Scanned], {Row, Column + length("[table]")}, text);
scan("[/table]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_table, Pos}|Scanned], {Row, Column + length("[/table]")}, text);
scan("[tr]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_table_row, Pos}|Scanned], {Row, Column + length("[tr]")}, text);
scan("[/tr]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_table_row, Pos}|Scanned], {Row, Column + length("[/tr]")}, text);
scan("[td]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_table_cell, Pos}|Scanned], {Row, Column + length("[td]")}, text);
scan("[/td]"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_table_row, Pos}|Scanned], {Row, Column + length("[/td]")}, text);
scan("\r\n"++T, Scanned, {Row, _Column} = Pos, _) ->
    scan(T, [{newline, Pos}|Scanned], {Row + 1, 0}, text);
scan("\n"++T, Scanned, {Row, _Column} = Pos, _) ->
    scan(T, [{newline, Pos}|Scanned], {Row + 1, 0}, text);
scan([$\ |T], [{_NotText, _Pos} = PrevToken|Scanned], {Row, Column}, text) ->
    scan(T, [PrevToken|Scanned], {Row, Column + 1}, text);
scan([$\ |T], [{NotText, _Pos, _Val} = PrevToken|Scanned], {Row, Column}, text) when NotText =/= text ->
    scan(T, [PrevToken|Scanned], {Row, Column + 1}, text);
scan([H|T], [{text, TPos, Text}|Scanned], {Row, Column}, text) ->
    scan(T, [{text, TPos, [H|Text]}|Scanned], {Row, Column + 1}, text);
scan([H|T], Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{text, Pos, [H]}|Scanned], {Row, Column + 1}, text).
