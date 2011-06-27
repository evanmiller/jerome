-module(jerome_html_scanner).

-export([scan/1]).

scan(HTML) ->
    scan(HTML, [], {1, 1}, text).

scan([], Scanned, _, _) ->
    {ok, lists:reverse(
            lists:map(fun
                    ({text, Pos, Text}) ->
                        {text, Pos, lists:reverse(Text)};
                    ({open_url, Pos, Value}) ->
                        {open_url, Pos, lists:reverse(Value)};
                    (Token) ->
                        Token
                end, Scanned))};

scan([$<, B, $> |T], Scanned, {Row, Column} = Pos, text) when B =:= $B; B =:= $b ->
    scan(T, [{open_bold, Pos}|Scanned], {Row, Column + length("<b>")}, text);
scan([$<, $/, B, $> |T], Scanned, {Row, Column} = Pos, text) when B =:= $B; B =:= $b ->
    scan(T, [{close_bold, Pos}|Scanned], {Row, Column + length("</b>")}, text);
scan([$<, I, $> |T], Scanned, {Row, Column} = Pos, text) when I =:= $I; I =:= $i ->
    scan(T, [{open_italic, Pos}|Scanned], {Row, Column + length("<i>")}, text);
scan([$<, $/, I, $> |T], Scanned, {Row, Column} = Pos, text) when I =:= $I; I =:= $i ->
    scan(T, [{close_italic, Pos}|Scanned], {Row, Column + length("</i>")}, text);
scan([$<, U, $> |T], Scanned, {Row, Column} = Pos, text) when U =:= $U; U =:= $u ->
    scan(T, [{open_underline, Pos}|Scanned], {Row, Column + length("<u>")}, text);
scan([$<, $/, U, $> |T], Scanned, {Row, Column} = Pos, text) when U =:= $U; U =:= $u ->
    scan(T, [{close_underline, Pos}|Scanned], {Row, Column + length("</u>")}, text);
scan("<strong>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_bold, Pos}|Scanned], {Row, Column + length("<strong>")}, text);
scan("<STRONG>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_bold, Pos}|Scanned], {Row, Column + length("<strong>")}, text);
scan("</strong>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_bold, Pos}|Scanned], {Row, Column + length("</strong>")}, text);
scan("</STRONG>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_bold, Pos}|Scanned], {Row, Column + length("</strong>")}, text);
scan("<em>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_italic, Pos}|Scanned], {Row, Column + length("<em>")}, text);
scan("<EM>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_italic, Pos}|Scanned], {Row, Column + length("<em>")}, text);
scan("</em>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_italic, Pos}|Scanned], {Row, Column + length("</em>")}, text);
scan("</EM>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_italic, Pos}|Scanned], {Row, Column + length("</em>")}, text);
scan("<sup>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_superscript, Pos}|Scanned], {Row, Column + length("<sup>")}, text);
scan("<SUP>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_superscript, Pos}|Scanned], {Row, Column + length("<sup>")}, text);
scan("</sup>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_superscript, Pos}|Scanned], {Row, Column + length("</sup>")}, text);
scan("</SUP>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_superscript, Pos}|Scanned], {Row, Column + length("</sup>")}, text);
scan("<sub>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_subscript, Pos}|Scanned], {Row, Column + length("<sub>")}, text);
scan("<SUB>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_subscript, Pos}|Scanned], {Row, Column + length("<sub>")}, text);
scan("</sub>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_subscript, Pos}|Scanned], {Row, Column + length("</sub>")}, text);
scan("</SUB>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{close_subscript, Pos}|Scanned], {Row, Column + length("</sub>")}, text);
scan("<br>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{newline, Pos}|Scanned], {Row, Column + length("<br>")}, text);
scan("<BR>"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{newline, Pos}|Scanned], {Row, Column + length("<br>")}, text);
scan("<a href=\""++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_url, Pos, ""}|Scanned], {Row, Column + length("<a href=\"")}, in_url);
scan("<A HREF=\""++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, [{open_url, Pos, ""}|Scanned], {Row, Column + length("<a href=\"")}, in_url);
scan("\">"++T, Scanned, {Row, Column}, in_url) ->
    scan(T, Scanned, {Row, Column + length("\">")}, text);
scan("&amp;"++T, [{open_url, Pos, Value}|Scanned], {Row, Column}, in_url) ->
    scan(T, [{open_url, Pos, [$&|Value]}|Scanned], {Row, Column + length("&amp;")}, in_url);
scan([H|T], [{open_url, Pos, Value}|Scanned], {Row, Column}, in_url) ->
    scan(T, [{open_url, Pos, [H|Value]}|Scanned], {Row, Column + 1}, in_url);
scan([$<, $/, A, $> |T], Scanned, {Row, Column} = Pos, text) when A =:= $A; A =:= $a ->
    scan(T, [{close_url, Pos}|Scanned], {Row, Column} = Pos, text);
scan("\r\n"++T, [{text, TPos, Text}|Scanned], {Row, _Column}, text) ->
    scan(T, [{text, TPos, [$\ |Text]}|Scanned], {Row + 1, 0}, text);
scan("\r\n"++T, [{text, TPos, " "++Text}|Scanned], {Row, _Column}, text) ->
    scan(T, [{text, TPos, [$\ |Text]}|Scanned], {Row + 1, 0}, text);
scan("\r\n"++T, Scanned, {Row, _Column}, text) ->
    scan(T, Scanned, {Row + 1, 0}, text);
scan("\n"++T, [{text, TPos, Text}|Scanned], {Row, _Column}, text) ->
    scan(T, [{text, TPos, [$\ |Text]}|Scanned], {Row + 1, 0}, text);
scan("\n"++T, [{text, TPos, " "++Text}|Scanned], {Row, _Column}, text) ->
    scan(T, [{text, TPos, [$\ |Text]}|Scanned], {Row + 1, 0}, text);
scan("\n"++T, Scanned, {Row, _Column}, text) ->
    scan(T, Scanned, {Row + 1, 0}, text);
scan("<"++T, Scanned, {Row, Column}, text) ->
    scan(T, Scanned, {Row, Column + 1}, in_tag);
scan("\""++T, Scanned, {Row, Column}, in_tag) ->
    scan(T, Scanned, {Row, Column + 1}, in_double_quote);
scan("\""++T, Scanned, {Row, Column}, in_double_quote) ->
    scan(T, Scanned, {Row, Column + 1}, in_tag);
scan("\'"++T, Scanned, {Row, Column}, in_tag) ->
    scan(T, Scanned, {Row, Column + 1}, in_single_quote);
scan("\'"++T, Scanned, {Row, Column}, in_single_quote) ->
    scan(T, Scanned, {Row, Column + 1}, in_tag);
scan(">"++T, Scanned, {Row, Column}, in_tag) ->
    scan(T, Scanned, {Row, Column + 1}, text);
scan([_H|T], Scanned, {Row, Column}, State) when State =:= in_tag; State =:= in_double_quote; State =:= in_single_quote ->
    scan(T, Scanned, {Row, Column + 1}, State);
scan("&amp;"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, append_text(Scanned, Pos, [$&]), {Row, Column + length("&amp;")}, text);
scan("&quot;"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, append_text(Scanned, Pos, [$"]), {Row, Column + length("&quot;")}, text);
scan("&lt;"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, append_text(Scanned, Pos, [$<]), {Row, Column + length("&lt;")}, text);
scan("&gt;"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, append_text(Scanned, Pos, [$>]), {Row, Column + length("&gt;")}, text);
scan("&nbsp;"++T, Scanned, {Row, Column} = Pos, text) ->
    scan(T, append_text(Scanned, Pos, [$\ ]), {Row, Column + length("&nbsp;")}, text);
scan("&#x"++T, Scanned, {Row, Column}, text) ->
    scan(T, Scanned, {Row, Column + length("&#x")}, {in_hex, 0});
scan("&#"++T, Scanned, {Row, Column}, text) ->
    scan(T, Scanned, {Row, Column + length("&#")}, {in_decimal, 0});
scan([H|T], Scanned, {Row, Column}, {in_hex, Value}) when H >= $A andalso H =< $F ->
    scan(T, Scanned, {Row, Column + 1}, {in_hex, Value * 16 + H - $A});
scan([H|T], Scanned, {Row, Column}, {in_hex, Value}) when H >= $a andalso H =< $f ->
    scan(T, Scanned, {Row, Column + 1}, {in_hex, Value * 16 + H - $a});
scan([H|T], Scanned, {Row, Column}, {in_hex, Value}) when H >= $0 andalso H =< $9 ->
    scan(T, Scanned, {Row, Column + 1}, {in_hex, Value * 16 + H - $0});
scan([H|T], Scanned, {Row, Column}, {in_decimal, Value}) when H >= $0 andalso H =< $9 ->
    scan(T, Scanned, {Row, Column + 1}, {in_decimal, Value * 10 + H - $0});
scan(";"++T, Scanned, {Row, Column} = Pos, {in_hex, Value}) ->
    scan(T, append_text(Scanned, Pos, [Value]), {Row, Column + 1}, text);
scan(";"++T, Scanned, {Row, Column} = Pos, {in_decimal, Value}) ->
    scan(T, append_text(Scanned, Pos, [Value]), {Row, Column + 1}, text);
scan([H|T], Scanned, {Row, Column} = Pos, text) ->
    scan(T, append_text(Scanned, Pos, [H]), {Row, Column + 1}, text).

append_text([{text, TPos, Chars}|Scanned], _Pos, Text) ->
    [{text, TPos, lists:reverse(Text, Chars)}|Scanned];
append_text(Scanned, Pos, Text) ->
    [{text, Pos, lists:reverse(Text)}|Scanned].
