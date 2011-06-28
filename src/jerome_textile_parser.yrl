% Textile parser. Possibly too strict.

Nonterminals
        Elements
        TableRow
        TextElement
        TextElements
        NonEmptyTextElements
        NonEmptyLinkElements
        LinkElement
        BlockTag
        TableCell
        TableCells
        HeaderCell
        HeaderCells.

Terminals
        block_tag
        punctuation
        single_star_even
        single_star_odd
        double_star_even
        double_star_odd
        single_underscore_even
        single_underscore_odd
        double_underscore_even
        double_underscore_odd
        newline
        text
        header_cell_start
        cell_delimiter
        url
        image
        double_quote
        double_quote_open
        double_quote_close
        caret_even
        caret_odd
        tilde_even
        tilde_odd.

Rootsymbol
        Elements.

Elements -> '$empty' : [].
Elements -> Elements TableRow : '$1' ++ ['$2'].
Elements -> Elements TextElement : '$1' ++ ['$2'].
Elements -> Elements newline : '$1' ++ ['$2'].
Elements -> Elements BlockTag : '$1' ++ ['$2'].

BlockTag -> block_tag TextElements newline : {block, '$1', '$2'}.

TableRow -> HeaderCells cell_delimiter newline : {table_header_row, '$1'}.
TableRow -> TableCells cell_delimiter newline : {table_row, '$1'}.

HeaderCells -> HeaderCell : ['$1'].
HeaderCells -> HeaderCells HeaderCell : '$1' ++ ['$2'].

HeaderCell -> header_cell_start TextElements : '$2'.

TableCells -> TableCell : ['$1'].
TableCells -> TableCells TableCell : '$1' ++ ['$2'].

TableCell -> cell_delimiter TextElements : '$2'.

TextElements -> '$empty' : [].
TextElements -> TextElements TextElement : '$1' ++ ['$2'].

NonEmptyTextElements -> TextElement : ['$1'].
NonEmptyTextElements -> NonEmptyTextElements TextElement : '$1' ++ ['$2'].

TextElement -> text : '$1'.
TextElement -> punctuation : '$1'.
TextElement -> double_quote : '$1'.
TextElement -> image : '$1'.
TextElement -> single_star_even NonEmptyTextElements single_star_odd : {strong, '$2'}.
TextElement -> double_star_even NonEmptyTextElements double_star_odd : {bold, '$2'}.
TextElement -> single_underscore_even NonEmptyTextElements single_underscore_odd : {em, '$2'}.
TextElement -> double_underscore_even NonEmptyTextElements double_underscore_odd : {italic, '$2'}.
TextElement -> double_quote_open NonEmptyLinkElements double_quote_close url : {hyperlink, '$2', '$4'}.
TextElement -> caret_even NonEmptyTextElements caret_odd : {superscript, '$2'}.
TextElement -> tilde_even NonEmptyTextElements tilde_odd : {subscript, '$2'}.

NonEmptyLinkElements -> LinkElement : ['$1'].
NonEmptyLinkElements -> NonEmptyLinkElements LinkElement : ['$1'].

LinkElement -> text : '$1'.
LinkElement -> punctuation : '$1'.
LinkElement -> single_star_even NonEmptyLinkElements single_star_odd : {strong, '$2'}.
LinkElement -> double_star_even NonEmptyLinkElements double_star_odd : {bold, '$2'}.
LinkElement -> single_underscore_even NonEmptyLinkElements single_underscore_odd : {em, '$2'}.
LinkElement -> double_underscore_even NonEmptyLinkElements double_underscore_odd : {italic, '$2'}.

