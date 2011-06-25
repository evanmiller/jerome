%% RTF parser. Doesn't do much besides putting control codes into groups.

Nonterminals
        Elements
        ListElements
        ListItem
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
        end_cell
        new_paragraph
        list_text.

Rootsymbol
        Elements.

Elements -> '$empty' : [].
Elements -> Elements Group : '$1' ++ ['$2'].
Elements -> Elements TableRow : '$1' ++ ['$2'].
Elements -> Elements ListItem : '$1' ++ ['$2'].
Elements -> Elements text : '$1' ++ ['$2'].
Elements -> Elements bin : '$1' ++ ['$2'].
Elements -> Elements control_word : '$1' ++ ['$2'].
Elements -> Elements control_bin : '$1' ++ ['$2'].
Elements -> Elements control_char : '$1' ++ ['$2'].
Elements -> Elements control_hex : '$1' ++ ['$2'].
Elements -> Elements new_paragraph : '$1' ++ ['$2'].

ListElements -> '$empty' : [].
ListElements -> ListElements Group : '$1' ++ ['$2'].
ListElements -> ListElements text : '$1' ++ ['$2'].
ListElements -> ListElements bin : '$1' ++ ['$2'].
ListElements -> ListElements control_word : '$1' ++ ['$2'].
ListElements -> ListElements control_bin : '$1' ++ ['$2'].
ListElements -> ListElements control_char : '$1' ++ ['$2'].
ListElements -> ListElements control_hex : '$1' ++ ['$2'].

Group -> open_brace Elements close_brace : {group, '$2'}.
TableRow -> begin_row Cells end_row : {table_row, '$2'}.
TableRow -> begin_row Cells last_row end_row : {table_row, '$2'}.
ListItem -> open_brace list_text Elements close_brace ListElements new_paragraph : {list_item, '$3', '$5'}.
Cells -> '$empty' : [].
Cells -> Cells Cell : '$1' ++ ['$2'].
Cell -> Elements end_cell : {table_cell, '$1'}.
