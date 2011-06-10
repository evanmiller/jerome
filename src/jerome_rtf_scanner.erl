-module(jerome_rtf_scanner).

-compile(export_all).


scan(RTF) ->
    scan(RTF, [], {1, 1}, in_text).

scan([], Scanned, _, in_text) ->
    {ok, lists:reverse(
            lists:map(fun
                    ({bin, Pos, Binary}) ->
                        {bin, Pos, lists:reverse(Binary)};
                    ({text, Pos, Text}) ->
                        {text, Pos, lists:reverse(Text)};
                    ({control_word, Pos, RevCode}) ->
                        case lists:reverse(RevCode) of
                            "trowd" -> 
                                {begin_row, Pos};
                            "row" ->
                                {end_row, Pos};
                            "lastrow" ->
                                {last_row, Pos};
                            "cell" ->
                                {end_cell, Pos};
                            "listtext" ->
                                {list_text, Pos};
                            "par" ->
                                {new_paragraph, Pos};
                            Code ->
                                {control_word, Pos, Code}
                        end;
                    ({control_word, Pos, Code, Param}) ->
                        {control_word, Pos, lists:reverse(Code), Param};
                    (Token) ->
                        Token
                end, Scanned))};

scan([$\\, $b, $i, $n, D | T], Scanned, {Row, Column} = Pos, in_text) when D>=$0, D=<$9 ->
    scan(T, [{control_bin, Pos, D-$0}|Scanned], {Row, Column + length("\\binX")}, in_bin_word);

scan([H|T], [{control_bin, CPos, Len}|Scanned], {Row, Column}, in_bin_word) when H>=$0, H=<$9 ->
    scan(T, [{control_bin, CPos, Len * 10 + (H-$0)}|Scanned], {Row, Column + 1}, in_bin_word);

scan(" " ++ T, [{control_bin, CPos, Len}|Scanned], {Row, Column} = Pos, in_bin_word) ->
    scan(T, [{bin, Pos, []}, {control_bin, CPos, Len}|Scanned], {Row, Column + 1}, {in_bin, Len});

scan([H|T], [{bin, BPos, Binary}|Scanned], {Row, Column}, {in_bin, 0}) ->
    scan(T, [{bin, BPos, [H|Binary]}|Scanned], {Row, Column + 1}, in_text);

scan([H|T], [{bin, BPos, Binary}|Scanned], {Row, Column}, {in_bin, BytesLeft}) ->
    scan(T, [{bin, BPos, [H|Binary]}|Scanned], {Row, Column + 1}, {in_bin, BytesLeft - 1});


scan("{" ++ T, Scanned, {Row, Column} = Pos, _State) ->
    scan(T, [{open_brace, Pos, '{'}|Scanned], {Row, Column + 1}, in_text);
scan("}" ++ T, Scanned, {Row, Column} = Pos, _State) ->
    scan(T, [{close_brace, Pos, '}'}|Scanned], {Row, Column + 1}, in_text);

scan("\\*" ++ T, Scanned, {Row, Column} = Pos, _State) ->
    scan(T, [{control_char, Pos, '*'}|Scanned], {Row, Column + 2}, in_text);
scan("\\~" ++ T, Scanned, {Row, Column} = Pos, _State) ->
    scan(T, [{control_char, Pos, '~'}|Scanned], {Row, Column + 2}, in_text);
scan("\\-" ++ T, Scanned, {Row, Column} = Pos, _State) ->
    scan(T, [{control_char, Pos, '-'}|Scanned], {Row, Column + 2}, in_text);
scan("\\_" ++ T, Scanned, {Row, Column} = Pos, _State) ->
    scan(T, [{control_char, Pos, '_'}|Scanned], {Row, Column + 2}, in_text);
scan("\\:" ++ T, Scanned, {Row, Column} = Pos, _State) ->
    scan(T, [{control_char, Pos, ':'}|Scanned], {Row, Column + 2}, in_text);
scan("\\|" ++ T, Scanned, {Row, Column} = Pos, _State) ->
    scan(T, [{control_char, Pos, '|'}|Scanned], {Row, Column + 2}, in_text);
scan([$\\, $', H1, H2 | T], Scanned, {Row, Column} = Pos, _State) when ((H1>=$0 andalso H1=<$9) orelse 
                                                                        (H1>=$A andalso H1 =<$Z)) andalso 
                                                                        ((H2>=$0 andalso H2 =<$9) orelse
                                                                        (H2>=$A andalso H2=<$Z))->
    scan(T, [{control_hex, Pos, hexchar_to_int(H1) * 16 + hexchar_to_int(H2)}|Scanned], {Row, Column + 4}, in_text);


scan([$\t | T], Scanned, {Row, Column}, _State) ->
    scan(T, [{control_word, {Row, Column}, lists:reverse("tab")}|Scanned], {Row, Column + 2}, in_text);
scan([$\\, $\r, $\n |T], Scanned, {Row, Column}, _State) ->
    scan(T, [{control_word, {Row, Column}, lists:reverse("par")}|Scanned], {Row + 1, 0}, in_text);
scan([$\\, $\r |T], Scanned, {Row, Column}, _State) ->
    scan(T, [{control_word, {Row, Column}, lists:reverse("par")}|Scanned], {Row + 1, 0}, in_text);
scan([$\\, $\n |T], Scanned, {Row, Column}, _State) ->
    scan(T, [{control_word, {Row, Column}, lists:reverse("par")}|Scanned], {Row + 1, 0}, in_text);
scan([$\\, H|T], [{text, TPos, Text}|Scanned], {Row, Column}, in_text) when H=:=${; H=:=$}; H=:=$\\ ->
    scan(T, [{text, TPos, [H|Text]}|Scanned], {Row, Column + 2}, in_text);
scan([$\\, H|T], Scanned, {Row, Column}, in_text) when H=:=${; H=:=$}; H=:=$\\ ->
    scan(T, [{text, {Row, Column}, [H]}|Scanned], {Row, Column + 2}, in_text);
scan([$\\|T], Scanned, {Row, Column}, _State) ->
    scan(T, [{control_word, {Row, Column}, ""}|Scanned], {Row, Column + 1}, in_word);

scan([$\r, $\n |T], Scanned, {Row, _Column}, _State) ->
    scan(T, Scanned, {Row + 1, 0}, in_text);
scan([$\r |T], Scanned, {Row, _Column}, _State) ->
    scan(T, Scanned, {Row + 1, 0}, in_text);
scan([$\n |T], Scanned, {Row, _Column}, _State) ->
    scan(T, Scanned, {Row + 1, 0}, in_text);

scan([$-, H|T], [{control_word, Pos, Code}|Scanned], {Row, Column}, in_word) when H>=$0, H=<$9 ->
    scan(T, [{control_word, Pos, Code, -(H-$0)}|Scanned], {Row, Column + 2}, in_word_param);

scan([H|T], [{control_word, Pos, Code}|Scanned], {Row, Column}, in_word) when H>=$0, H=<$9 ->
    scan(T, [{control_word, Pos, Code, H-$0}|Scanned], {Row, Column + 1}, in_word_param);

scan([H|T], [{control_word, Pos, Code}|Scanned], {Row, Column}, in_word) when H>=$a, H=<$z; H>=$A, H=<$Z ->
    scan(T, [{control_word, Pos, [H|Code]}|Scanned], {Row, Column + 1}, in_word);

scan([H|T], [{control_word, Pos, Code, Param}|Scanned], {Row, Column}, in_word_param) when H>=$0, H=<$9, Param > 0 ->
    scan(T, [{control_word, Pos, Code, Param * 10 + (H-$0)}|Scanned], {Row, Column + 1}, in_word_param);
scan([H|T], [{control_word, Pos, Code, Param}|Scanned], {Row, Column}, in_word_param) when H>=$0, H=<$9, Param < 0 ->
    scan(T, [{control_word, Pos, Code, Param * 10 - (H-$0)}|Scanned], {Row, Column + 1}, in_word_param);

scan([$\ |T], Scanned, {Row, Column}, State) when State =:= in_word; State =:= in_word_param ->
    scan(T, Scanned, {Row, Column + 1}, in_text);

scan([_H|T], Scanned, {Row, Column}, State) when State =:= in_word; State =:= in_word_param ->
    scan(T, Scanned, {Row, Column + 1}, in_text);

scan([H|T], [{text, TPos, Text}|Scanned], {Row, Column}, in_text) ->
    scan(T, [{text, TPos, [H|Text]}|Scanned], {Row, Column + 1}, in_text);

scan([H|T], Scanned, {Row, Column} = Pos, in_text) ->
    scan(T, [{text, Pos, [H]}|Scanned], {Row, Column+1}, in_text).



hexchar_to_int(C) when C>=$0, C=<$9 ->
    C-$0;
hexchar_to_int(C) when C>=$A, C=<$Z ->
    C-$A.
