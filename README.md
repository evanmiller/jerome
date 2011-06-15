Jerome: Erlang rich-text processor
==================================

Jerome is designed to read and write many rich-text formats, with support for
bold, italic, lists, tables, and hyperlinks. At present the number of supported
formats is small. Usage:

    % Read
    RichText = jerome:parse("/path/to/file.rtf", rtf)

    % Write
    {ok, IOList} = jerome:generate(RichText, html)


Available formats:

    Format     Read/Write?
    ------     -----------
    BBCode     Read + Write
    HTML       Write-only
    RTF        Read + Write
    Textile    Read + Write
