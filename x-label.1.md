% X-LABEL(1) 0.1
% Sebastian Schwarz <seschwar@googlemail.com>
% 2009-06-06

# NAME

x-label -- X-Label email header editor

# SYNOPSIS

x-label *command* [*label*]...

# DESCRIPTION

x-label reads a single email message from standard input, searches
for the X-Label header field, modifies it according to the specified
commands and outputs the modified message with the new X-Label header
field at the bottom of the email header section to the standard output.

This is quite useful for adding labels with a MDA by piping mail
though x-label.

x-label removes any duplicate labels and combines multiple X-Label
header fields into a single one.

# COMMANDS

add
:   Adds the specified labels to the X-Label header field body.

remove
:   Removes the specified labels from the X-Label header field body.

set
:   Overwrites the X-Label header field body with the specified labels.

clear
:   Deletes the whole X-Label header field.

# SEE ALSO

`formail`(1).

