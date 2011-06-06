%% RTF parser. Doesn't do much besides putting control codes into groups.

Nonterminals
        Elements
        Group
        TableRow
        Cells
        Cell.

Terminals
        text
        bin
        control_word
        control_char
        control_bin
        control_hex
        open_brace
        close_brace
        begin_row
        end_row
        last_row
        end_cell.

Rootsymbol
        Elements.

Elements -> '$empty' : [].
Elements -> Elements Group : '$1' ++ ['$2'].
Elements -> Elements TableRow : '$1' ++ ['$2'].
Elements -> Elements text : '$1' ++ ['$2'].
Elements -> Elements bin : '$1' ++ ['$2'].
Elements -> Elements control_word : '$1' ++ ['$2'].
Elements -> Elements control_bin : '$1' ++ ['$2'].
Elements -> Elements control_char : '$1' ++ ['$2'].
Elements -> Elements control_hex : '$1' ++ ['$2'].

Group -> open_brace Elements close_brace : {group, '$2'}.
TableRow -> begin_row Cells end_row : {table_row, '$2'}.
TableRow -> begin_row Cells last_row end_row : {table_row, '$2'}.
Cells -> '$empty' : [].
Cells -> Cells Cell : '$1' ++ ['$2'].
Cell -> Elements end_cell : {table_cell, '$1'}.
