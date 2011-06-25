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
        single_star
        double_star
        single_underscore
        double_underscore
        newline
        text
        header_cell_start
        cell_delimiter
        url
        image
        double_quote
        caret
        tilde.

Rootsymbol
        Elements.

Elements -> '$empty' : [].
Elements -> Elements TableRow : '$1' ++ ['$2'].
Elements -> Elements NonEmptyTextElements : '$1' ++ '$2'.
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
TextElement -> single_star NonEmptyTextElements single_star : {strong, '$2'}.
TextElement -> double_star NonEmptyTextElements double_star : {bold, '$2'}.
TextElement -> single_underscore NonEmptyTextElements single_underscore : {em, '$2'}.
TextElement -> double_underscore NonEmptyTextElements double_underscore : {italic, '$2'}.
TextElement -> double_quote NonEmptyLinkElements double_quote url : {hyperlink, '$2', '$4'}.
TextElement -> caret NonEmptyTextElements caret : {superscript, '$2'}.
TextElement -> tilde NonEmptyTextElements tilde : {subscript, '$2'}.

NonEmptyLinkElements -> LinkElement : ['$1'].
NonEmptyLinkElements -> NonEmptyLinkElements LinkElement : ['$1'].

LinkElement -> text : '$1'.
LinkElement -> punctuation : '$1'.
LinkElement -> single_star NonEmptyLinkElements single_star : {strong, '$2'}.
LinkElement -> double_star NonEmptyLinkElements double_star : {bold, '$2'}.
LinkElement -> single_underscore NonEmptyLinkElements single_underscore : {em, '$2'}.
LinkElement -> double_underscore NonEmptyLinkElements double_underscore : {italic, '$2'}.
