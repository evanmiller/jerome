-module(jerome_textile_scanner).

-compile(export_all).

-record(ctx, {
        state,
        single_star_count = 0,
        double_star_count = 0,
        single_underscore_count = 0,
        double_underscore_count = 0,
        tilde_count = 0,
        caret_count = 0
    }).

scan(Textile) ->
    scan(Textile, [], {1, 1}, #ctx{ state = newline }).

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

scan("* "++T, Scanned, {Row, Column} = Pos, #ctx{ state = newline } = Ctx) ->
    scan(T, [{block_tag, Pos, "*"}|Scanned], {Row, Column + length("* ")}, Ctx#ctx{ state = inline });
scan("# "++T, Scanned, {Row, Column} = Pos, #ctx{ state = newline } = Ctx) ->
    scan(T, [{block_tag, Pos, "#"}|Scanned], {Row, Column + length("# ")}, Ctx#ctx{ state = inline });
scan("**"++T, Scanned, {Row, Column} = Pos, #ctx{ double_star_count = Count} = Ctx) when Count rem 2 =:= 0 ->
    scan(T, [{double_star_even, Pos}|Scanned], {Row, Column + 2}, Ctx#ctx{ double_star_count = Count + 1, state = inline });
scan("**"++T, Scanned, {Row, Column} = Pos, #ctx{ double_star_count = Count} = Ctx) when Count rem 2 =:= 1 ->
    scan(T, [{double_star_odd, Pos}|Scanned], {Row, Column + 2}, Ctx#ctx{ double_star_count = Count + 1, state = inline });
scan("*"++T, Scanned, {Row, Column} = Pos, #ctx{ single_star_count = Count} = Ctx) when Count rem 2 =:= 0 ->
    scan(T, [{single_star_even, Pos}|Scanned], {Row, Column + 1}, Ctx#ctx{ single_star_count = Count + 1, state = inline});
scan("*"++T, Scanned, {Row, Column} = Pos, #ctx{ single_star_count = Count} = Ctx) when Count rem 2 =:= 1 ->
    scan(T, [{single_star_odd, Pos}|Scanned], {Row, Column + 1}, Ctx#ctx{ single_star_count = Count + 1, state = inline });
scan("__"++T, Scanned, {Row, Column} = Pos, #ctx{ double_underscore_count = Count } = Ctx) when Count rem 2 =:= 0 ->
    scan(T, [{double_underscore_even, Pos}|Scanned], {Row, Column + 2}, Ctx#ctx{ double_underscore_count = Count + 1, state = inline });
scan("__"++T, Scanned, {Row, Column} = Pos, #ctx{ double_underscore_count = Count } = Ctx) when Count rem 2 =:= 1 ->
    scan(T, [{double_underscore_odd, Pos}|Scanned], {Row, Column + 2}, Ctx#ctx{ double_underscore_count = Count + 1, state = inline });
scan("_"++T, Scanned, {Row, Column} = Pos, #ctx{ single_underscore_count = Count } = Ctx) when Count rem 2 =:= 0 ->
    scan(T, [{single_underscore_even, Pos}|Scanned], {Row, Column + 1}, Ctx#ctx{ single_underscore_count = Count + 1, state = inline });
scan("_"++T, Scanned, {Row, Column} = Pos, #ctx{ single_underscore_count = Count } = Ctx) when Count rem 2 =:= 1 ->
    scan(T, [{single_underscore_odd, Pos}|Scanned], {Row, Column + 1}, Ctx#ctx{ single_underscore_count = Count + 1, state = inline });
scan("~"++T, Scanned, {Row, Column} = Pos, #ctx{ tilde_count = Count } = Ctx) when Count rem 2 =:= 0 ->
    scan(T, [{tilde_even, Pos}|Scanned], {Row, Column + 1}, Ctx#ctx{ tilde_count = Count + 1, state = inline });
scan("~"++T, Scanned, {Row, Column} = Pos, #ctx{ tilde_count = Count } = Ctx) when Count rem 2 =:= 1 ->
    scan(T, [{tilde_odd, Pos}|Scanned], {Row, Column + 1}, Ctx#ctx{ tilde_count = Count + 1, state = inline });
scan("^"++T, Scanned, {Row, Column} = Pos, #ctx{ caret_count = Count} = Ctx) when Count rem 2 =:= 0 ->
    scan(T, [{caret_even, Pos}|Scanned], {Row, Column + 1}, Ctx#ctx{ caret_count = Count + 1, state = inline });
scan("^"++T, Scanned, {Row, Column} = Pos, #ctx{ caret_count = Count} = Ctx) when Count rem 2 =:= 1 ->
    scan(T, [{caret_odd, Pos}|Scanned], {Row, Column + 1}, Ctx#ctx{ caret_count = Count + 1, state = inline });
scan(" - "++T, Scanned, {Row, Column} = Pos, Ctx) ->
    scan(T, [{punctuation, Pos, " - "}|Scanned], {Row, Column + length(" - ")}, Ctx#ctx{ state = inline });
scan(" -- "++T, Scanned, {Row, Column} = Pos, Ctx) ->
    scan(T, [{punctuation, Pos, " -- "}|Scanned], {Row, Column + length(" -- ")}, Ctx#ctx{ state = inline });
scan("(tm)"++T, Scanned, {Row, Column} = Pos, Ctx) ->
    scan(T, [{punctuation, Pos, "(tm)"}|Scanned], {Row, Column + length("(tm)")}, Ctx#ctx{ state = inline });
scan([$(, C, $)|T], Scanned, {Row, Column} = Pos, Ctx) when C=:=$c; C=:=$r ->
    scan(T, [{punctuation, Pos, [$(, C, $)] }|Scanned], {Row, Column + length("(r)")}, Ctx#ctx{ state = inline });
scan(" x "++T, Scanned, {Row, Column} = Pos, Ctx) ->
    scan(T, [{punctuation, Pos, " x "}|Scanned], {Row, Column + length(" x ")}, Ctx#ctx{ state = inline });
scan("\r\n"++T, Scanned, {Row, _Column} = Pos, _) ->
    scan(T, [{newline, Pos}|Scanned], {Row + 1, 0}, #ctx{ state = newline });
scan("\n"++T, Scanned, {Row, _Column} = Pos, _) ->
    scan(T, [{newline, Pos}|Scanned], {Row + 1, 0}, #ctx{ state = newline });
scan([$h, D, $., $\ |T], Scanned, {Row, Column} = Pos, #ctx{ state = newline } = Ctx) when D>=$0, D=<$9 ->
    scan(T, [{block_tag, Pos, [$h, D]}|Scanned], {Row, Column + length("hX. ")}, Ctx#ctx{ state = inline });
scan([$f, $n, D, $., $\ |T], Scanned, {Row, Column} = Pos, #ctx{ state = newline } = Ctx) when D>=$0, D=<$9 ->
    scan(T, [{block_tag, Pos, [$f, $n, D]}|Scanned], {Row, Column + length("fnX. ")}, Ctx#ctx{ state = inline });
scan("bq. "++T, Scanned, {Row, Column} = Pos, #ctx{ state = newline } = Ctx) ->
    scan(T, [{block_tag, Pos, "bq"}|Scanned], {Row, Column + length("bq. ")}, Ctx#ctx{ state = inline });
scan("p. "++T, Scanned, {Row, Column} = Pos, #ctx{ state = newline } = Ctx) ->
    scan(T, [{block_tag, Pos, "p"}|Scanned], {Row, Column + length("p. ")}, Ctx#ctx{ state = inline });
scan("bc. "++T, Scanned, {Row, Column} = Pos, #ctx{ state = newline } = Ctx) ->
    scan(T, [{block_tag, Pos, "bc"}|Scanned], {Row, Column + length("bc. ")}, Ctx#ctx{ state = inline });
scan("pre. "++T, Scanned, {Row, Column} = Pos, #ctx{ state = newline } = Ctx) ->
    scan(T, [{block_tag, Pos, "pre"}|Scanned], {Row, Column + length("pre. ")}, Ctx#ctx{ state = inline });
scan("|_. "++T, Scanned, {Row, Column} = Pos, _) ->
    scan(T, [{header_cell_start, Pos}|Scanned], {Row, Column + length("|_. ")}, #ctx{ state = inline });
scan("|"++T, Scanned, {Row, Column} = Pos, _) ->
    scan(T, [{cell_delimiter, Pos}|Scanned], {Row, Column + 1}, #ctx{ state = inline });
scan("\":http://"++T, Scanned, {Row, Column} = Pos, #ctx{ state = inline } = Ctx) ->
    Scanned1 = mark_previous_double_quote(Scanned),
    scan(T, lists:reverse([{double_quote_close, Pos}, {url, {Row, Column + 2}, lists:reverse("http://")}], Scanned1), 
        {Row, Column + length("\":http://")}, Ctx#ctx{ state = inlink });
scan("\""++T, Scanned, {Row, Column} = Pos, Ctx) ->
    scan(T, [{double_quote, Pos, [$"]}|Scanned], {Row, Column + 1}, Ctx#ctx{ state = inline });
scan("!http://"++T, Scanned, {Row, Column} = Pos, #ctx{ state = inline } = Ctx) ->
    scan(T, [{image, Pos, lists:reverse("http://")}|Scanned],
        {Row, Column + length("!http://")}, Ctx#ctx{ state = inimage });
scan("!"++T, Scanned, {Row, Column}, #ctx{ state = inimage } = Ctx) ->
    scan(T, Scanned, {Row, Column + 1}, Ctx#ctx{ state = inline });
scan([H|T], [{url, IPos, Link}|Scanned], {Row, Column}, #ctx{ state = inimage } = Ctx) ->
    scan(T, [{url, IPos, [H|Link]}|Scanned], {Row, Column + 1}, Ctx);
scan(" "++T, Scanned, {Row, Column} = Pos, #ctx{ state = inlink } = Ctx) ->
    scan(T, [{text, Pos, " "}|Scanned], {Row, Column + 1}, Ctx#ctx{ state = inline });
scan(". "++T, Scanned, {Row, Column} = Pos, #ctx{ state = inlink } = Ctx) ->
    scan(T, [{text, Pos, ". "}|Scanned], {Row, Column + 2}, Ctx#ctx{ state = inline });
scan([H|T], [{url, HPos, Link}|Scanned], {Row, Column}, #ctx{ state = inlink } = Ctx) ->
    scan(T, [{url, HPos, [H|Link]}|Scanned], {Row, Column}, Ctx);
scan([H|T], [{text, TPos, Text}|Scanned], {Row, Column}, #ctx{ state = inline } = Ctx) ->
    scan(T, [{text, TPos, [H|Text]}|Scanned], {Row, Column + 1}, Ctx);
scan([H|T], Scanned, {Row, Column} = Pos, #ctx{ state = inline } = Ctx) ->
    scan(T, [{text, Pos, [H]}|Scanned], {Row, Column + 1}, Ctx);
scan([H|T], Scanned, {Row, Column} = Pos, #ctx{ state = newline } = Ctx) ->
    scan(T, [{text, Pos, [H]}|Scanned], {Row, Column + 1}, Ctx#ctx{ state = inline }).

mark_previous_double_quote(Scanned) ->
    mark_previous_double_quote(Scanned, []).

mark_previous_double_quote([], Acc) ->
    lists:reverse(Acc);
mark_previous_double_quote([{double_quote, Pos, Val}|Rest], Acc) ->
    lists:reverse([{double_quote_open, Pos, Val}|Acc], Rest);
mark_previous_double_quote([H|T], Acc) ->
    mark_previous_double_quote(T, [H|Acc]).
