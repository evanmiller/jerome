-module(jerome_textile_scanner).

-compile(export_all).


scan(Textile) ->
    scan(Textile, [], {1, 1}, newline).

scan([], Scanned, _, _) ->
    {ok, lists:reverse(
            lists:map(fun
                    ({text, Pos, Text}) ->
                        {text, Pos, lists:reverse(Text)};
                    ({url, Pos, Link}) ->
                        {url, Pos, lists:reverse(Link)};
                    ({image, Pos, Link}) ->
                        {image, Pos, lists:reverse(Link)};
                    (Token) ->
                        Token
                end, Scanned))};

scan("* "++T, Scanned, {Row, Column} = Pos, newline) ->
    scan(T, [{block_tag, Pos, "*"}|Scanned], {Row, Column + length("* ")}, inline);
scan("# "++T, Scanned, {Row, Column} = Pos, newline) ->
    scan(T, [{block_tag, Pos, "#"}|Scanned], {Row, Column + length("# ")}, inline);
scan("**"++T, Scanned, {Row, Column} = Pos, _) ->
    scan(T, [{double_star, Pos}|Scanned], {Row, Column + 2}, inline);
scan("*"++T, Scanned, {Row, Column} = Pos, _) ->
    scan(T, [{single_star, Pos}|Scanned], {Row, Column + 1}, inline);
scan("__"++T, Scanned, {Row, Column} = Pos, _) ->
    scan(T, [{double_underscore, Pos}|Scanned], {Row, Column + 2}, inline);
scan("_"++T, Scanned, {Row, Column} = Pos, _) ->
    scan(T, [{single_underscore, Pos}|Scanned], {Row, Column + 1}, inline);
scan("~"++T, Scanned, {Row, Column} = Pos, _) ->
    scan(T, [{tilde, Pos}|Scanned], {Row, Column + 1}, inline);
scan("^"++T, Scanned, {Row, Column} = Pos, _) ->
    scan(T, [{caret, Pos}|Scanned], {Row, Column + 1}, inline);
scan(" - "++T, Scanned, {Row, Column} = Pos, _) ->
    scan(T, [{punctuation, Pos, " - "}|Scanned], {Row, Column + length(" - ")}, inline);
scan(" -- "++T, Scanned, {Row, Column} = Pos, _) ->
    scan(T, [{punctuation, Pos, " -- "}|Scanned], {Row, Column + length(" -- ")}, inline);
scan("(tm)"++T, Scanned, {Row, Column} = Pos, _) ->
    scan(T, [{punctuation, Pos, "(tm)"}|Scanned], {Row, Column + length("(tm)")}, inline);
scan([$(, C, $)|T], Scanned, {Row, Column} = Pos, _) when C=:=$c; C=:=$r ->
    scan(T, [{punctuation, Pos, [$(, C, $)] }|Scanned], {Row, Column + length("(r)")}, inline);
scan(" x "++T, Scanned, {Row, Column} = Pos, _) ->
    scan(T, [{punctuation, Pos, " x "}|Scanned], {Row, Column + length(" x ")}, inline);
scan("\r\n"++T, Scanned, {Row, _Column} = Pos, _) ->
    scan(T, [{newline, Pos}|Scanned], {Row + 1, 0}, newline);
scan("\n"++T, Scanned, {Row, _Column} = Pos, _) ->
    scan(T, [{newline, Pos}|Scanned], {Row + 1, 0}, newline);
scan([$h, D, $., $\ |T], Scanned, {Row, Column} = Pos, newline) when D>=$0, D=<$9 ->
    scan(T, [{block_tag, Pos, [$h, D]}|Scanned], {Row, Column + length("hX. ")}, inline);
scan([$f, $n, D, $., $\ |T], Scanned, {Row, Column} = Pos, newline) when D>=$0, D=<$9 ->
    scan(T, [{block_tag, Pos, [$f, $n, D]}|Scanned], {Row, Column + length("fnX. ")}, inline);
scan("bq. "++T, Scanned, {Row, Column} = Pos, newline) ->
    scan(T, [{block_tag, Pos, "bq"}|Scanned], {Row, Column + length("bq. ")}, inline);
scan("p. "++T, Scanned, {Row, Column} = Pos, newline) ->
    scan(T, [{block_tag, Pos, "p"}|Scanned], {Row, Column + length("p. ")}, inline);
scan("bc. "++T, Scanned, {Row, Column} = Pos, newline) ->
    scan(T, [{block_tag, Pos, "bc"}|Scanned], {Row, Column + length("bc. ")}, inline);
scan("pre. "++T, Scanned, {Row, Column} = Pos, newline) ->
    scan(T, [{block_tag, Pos, "pre"}|Scanned], {Row, Column + length("pre. ")}, inline);
scan("|_. "++T, Scanned, {Row, Column} = Pos, _) ->
    scan(T, [{header_cell_start, Pos}|Scanned], {Row, Column + length("|_. ")}, inline);
scan("|"++T, Scanned, {Row, Column} = Pos, _) ->
    scan(T, [{cell_delimiter, Pos}|Scanned], {Row, Column + 1}, inline);
scan("\":http://"++T, Scanned, {Row, Column} = Pos, inline) ->
    scan(T, lists:reverse([{double_quote, Pos}, {url, {Row, Column + 2}, lists:reverse("http://")}], Scanned), 
        {Row, Column + length("\":http://")}, inlink);
scan("\""++T, Scanned, {Row, Column} = Pos, inline) ->
    scan(T, [{double_quote, Pos, [$"]}|Scanned], {Row, Column + 1}, inline);
scan("!http://"++T, Scanned, {Row, Column} = Pos, inline) ->
    scan(T, [{image, Pos, lists:reverse("http://")}|Scanned],
        {Row, Column + length("!http://")}, inimage);
scan("!"++T, Scanned, {Row, Column}, inimage) ->
    scan(T, Scanned, {Row, Column + 1}, inline);
scan([H|T], [{url, IPos, Link}|Scanned], {Row, Column}, inimage) ->
    scan(T, [{url, IPos, [H|Link]}|Scanned], {Row, Column + 1}, inimage);
scan(" "++T, Scanned, {Row, Column} = Pos, inlink) ->
    scan(T, [{text, Pos, " "}|Scanned], {Row, Column + 1}, inline);
scan(". "++T, Scanned, {Row, Column} = Pos, inlink) ->
    scan(T, [{text, Pos, ". "}|Scanned], {Row, Column + 2}, inline);
scan([H|T], [{url, HPos, Link}|Scanned], {Row, Column}, inlink) ->
    scan(T, [{url, HPos, [H|Link]}|Scanned], {Row, Column}, inlink);
scan([H|T], [{text, TPos, Text}|Scanned], {Row, Column}, inline) ->
    scan(T, [{text, TPos, [H|Text]}|Scanned], {Row, Column + 1}, inline);
scan([H|T], Scanned, {Row, Column} = Pos, inline) ->
    scan(T, [{text, Pos, [H]}|Scanned], {Row, Column + 1}, inline).
