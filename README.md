# Synopsis

## `tokenize/2` and `tokenize/3`

```prolog
?- tokenize(`Would you tokenize this text?\nThank you!`, Tokens).
Tokens = [word(would), word(you), word(tokenize), word(this), word(text), punct(?), cntrl('\n'), word(thank), word(...)|...] 
```

## `tokenize_file/2` and `tokenize_file/3`

Given some file, e.g.,

```shell
$ cat test.txt
1. Show examples of tokenizer
2. Zip files
3. Upload to the swi-package list!
```

We can use `tokenize_file/3` to tokenize its contents:

```prolog
?- tokenize_file('test.txt', T, [spaces(true), to(strings)]), display(T).
[word("1"),punct("."),spc(" "),word("show"),spc(" "),word("examples"),spc(" "),word("of"),spc(" "),word("tokenizer"),cntrl("\n"),word("2"),punct("."),spc(" "),word("zip"),spc(" "),word("files"),cntrl("\n"),word("3"),punct("."),spc(" "),word("upload"),spc(" "),word("to"),spc(" "),word("the"),spc(" "),word("swi"),punct("-"),word("package"),spc(" "),word("list"),punct("!"),cntrl("\n")]
T = [word("1"), punct("."), spc(" "), word("show"), spc(" "), word("examples"), spc(" "), word("of"), spc(...)|...] 
```

`tokenize_file/2` is the same but with default options.

## `untokenize/2`

Given some list of tokens, we can read them into a list of character codes using
`untokenize/2`:

```prolog
?- Tokens = [word("one"),spc(" "),word("two"),spc(" "),word("three"),spc(" "),punct("!")], untokenize(Tokens, Codes), format(`~s~n`, [Codes]).
one two three !
Tokens = [word("one"), spc(" "), word("two"), spc(" "), word("three"), spc(" "), punct("!")],
Codes = [111, 110, 101, 32, 116, 119, 111, 32, 116|...] 
```

# Description

Module `tokenize` aims to provide a straightforward tool for tokenizing text into a simple format. It is the result of a learning exercise, and it is far from perfect. If there is sufficient interest from myself or anyone else, I'll try to improve it.

## Options

`tokenize_file/3` and `tokenize/3` both take an option list for their third argument. The two place versions of these predicates are equivalent to calling the three place predicates with an empty list of options, and simply uses the default options.

### Available options:

 `option_name(settings)` | description 
-------------------------|-------------
 cased(true|false)       | whether tokens preserve cases
