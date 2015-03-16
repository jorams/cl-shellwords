# cl-shellwords

CL-shellwords is a port of Ruby's shellwords.rb to Common Lisp. It can be used to escape strings for use on the (Bourne) shell, or to split command lines into segments according to the shell's escaping rules.

## Documentation

CL-shellwords defines the package `:cl-shellwords`, with the alias `:shellwords`. This package exports three functions, a condition and an accessor for that condition.

### SPLIT

```lisp
(split string)
```

Split `STRING` into a list of words, handling escaping the same way a shell like the Bourne shell does.

Whitespace normally acts as a word separator, except when preceded by a backslash or enclosed in single- or double quotes.

```lisp
Examples:
(split "example string")
;=> ("example"  "string")
(split "example\ escaped string")
;=> ("example escaped" "string")
(split "example 'escaped string'")
;=> ("example" "escaped string")
(split "example "escaped string"")
;=> ("example" "escaped string")
```

If `STRING` contains non-matching single- or double quotes, an error of type `UNMATCHED-QUOTE-ERROR` is signaled. `STRING` can be retrieved from the error object using `UNMATCHED-QUOTE-ERROR-STRING`.

### ESCAPE

```lisp
(escape string)
```

 Escape `STRING` so that it is safe when used as an argument in a shell like
the Bourne shell.

- If `STRING` is an empty string, a pair of single quotes is returned.
- An LF character is escaped by placing it in single quotes.
- All other special characters are escaped with a backslash.

Examples:
```lisp
(escape "")
;=> "''"
(escape "It's an example string")
;=> "It\s\ an\ example\ string"
(escape "NothingWrongHere")
;=> "NothingWrongHere"
(escape "{LF}")
;=> "'{LF}'" ({LF} = #\linefeed)
```

### JOIN

```lisp
(join sequence)
```
Join the elements of `SEQUENCE` together, separated by spaces. The elements are first passed to `ESCAPE` for proper escaping.

`SEQUENCE` should be a list or vector of strings.
