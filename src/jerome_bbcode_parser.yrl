% BBCode parser

Nonterminals
        Elements
        Table
        TableRows
        TableCells
        ListItems.

Terminals
        open_bold
        close_bold
        open_italic
        close_italic
        open_underline
        close_underline
        open_superscript
        close_superscript
        open_subscript
        close_subscript
        open_url
        open_url_equals
        url_value
        close_url
        open_img
        close_img
        open_quote
        close_quote
        open_code
        close_code
        open_list
        list_item
        close_list
        open_table
        close_table
        open_table_row
        close_table_row
        open_table_cell
        close_table_cell
        newline
        text.

Rootsymbol
        Elements.

Elements -> '$empty' : [].
Elements -> Elements Table : '$1' ++ ['$2'].
Elements -> Elements text : '$1' ++ ['$2'].
Elements -> Elements newline : '$1' ++ ['$2'].
Elements -> open_bold Elements close_bold : {bold, '$2'}.
Elements -> open_italic Elements close_italic : {italic, '$2'}.
Elements -> open_underline Elements close_underline : {underline, '$2'}.
Elements -> open_superscript Elements close_superscript : {superscript, '$2'}.
Elements -> open_subscript Elements close_subscript : {subscript, '$2'}.
Elements -> open_url text close_url : {hyperlink, '$2'}.
Elements -> open_url_equals url_value Elements close_url : {hyperlink, '$2', '$3'}.
Elements -> open_img text close_img : {image, '$2'}.
Elements -> open_quote Elements close_quote : {quote, '$2'}.
Elements -> open_code Elements close_code : {code, '$2'}.
Elements -> open_list ListItems close_list : {list, '$2'}.

ListItems -> '$empty' : [].
ListItems -> ListItems list_item Elements : '$1' ++ [{list_item, '$3'}].

Table -> open_table TableRows close_table : {table, '$2'}.

TableRows -> '$empty' : [].
TableRows -> TableRows open_table_row TableCells close_table_row : '$1' ++ [{table_row, '$3'}].

TableCells -> '$empty' : [].
TableCells -> TableCells open_table_cell Elements close_table_cell : '$1' ++ [{table_cell, '$4'}].
