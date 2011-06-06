Jerome: Erlang rich-text processor
==================================

Jerome is designed to read and write many rich-text formats. Right now it only reads RTF and emits HTML. Usage:

    % Read
    RichText = jerome:read("/path/to/file.rtf", rtf)

    % Write
    jerome:write(RichText, "/path/to/file.html", html)
