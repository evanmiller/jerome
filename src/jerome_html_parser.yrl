% HTML parser

Nonterminals
        Elements.

Terminals
        open_bold
        close_bold
        open_italic
        close_italic
        open_underline
        close_underline
        open_subscript
        close_subscript
        open_superscript
        close_superscript
        open_url
        close_url
        newline
        text.

Rootsymbol
        Elements.

Elements -> '$empty' : [].
Elements -> Elements text : '$1' ++ ['$2'].
Elements -> Elements newline : '$1' ++ ['$2'].
Elements -> open_bold Elements close_bold : {bold, '$2'}.
Elements -> open_italic Elements close_italic : {italic, '$2'}.
Elements -> open_underline Elements close_underline : {underline, '$2'}.
Elements -> open_url Elements close_url : {hyperlink, '$1', '$2'}.
Elements -> open_superscript Elements close_superscript : {superscript, '$1'}.
Elements -> open_subscript Elements close_subscript : {subscript, '$1'}.
