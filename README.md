# Synopsis

```prolog
?- tokenize(`Would you tokenize this text?\nThank you!`, Tokens).
Tokens = [word(would), word(you), word(tokenize), word(this), word(text), punct(?), cntrl('\n'), word(thank), word(...)|...] 
```

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

`tokenize_file/3` and `tokenize/3` both take an option list for their third argument. The two place versions of these predicates are equivalent to calling the three place predicates with an empty list of options, and amount to using the default options. I.e., `tokenize(Text, Tokens)` is equivalent to `tokenize(Text, Tokens, [cased(false), spaces(false), cntrl(true), punct(true), to(atoms), pack(false)])`.

### Available options:

 option name | possible values | description | default |
-------------|------------|--------------|--------
cased | true, false  | whether tokens preserve cases | false
spaces | true, false | whether spaces are included as tokens or omitted | false
cntrl  | true, false | whether control characters are | true
punct  | true, false | whether punctuation marks are included as tokens or omitted | true
to     | strings, atoms, chars, codes | set the type of representation used by the tokens | atoms
pack | true, false | whether to pack consecutive occurrences of identical tokens or simply repeat the tokens | false

### Examples of options:

```prolog
?- tokenize(`Example  text!!!`, T).
T = [word(example), word(text), punct(!), punct(!), punct(!)] 

?- tokenize(`Example  text!!!`, T, [cased(true)]).
T = [word('Example'), word(text), punct(!), punct(!), punct(!)] 

?- tokenize(`Example  text!!!`, T, [cased(true), spaces(true)]).
T = [word('Example'), spc(' '), spc(' '), word(text), punct(!), punct(!), punct(!)] 

?- tokenize(`Example  text!!!`, T, [cased(true), spaces(true), punct(false)]).
T = [word('Example'), spc(' '), spc(' '), word(text)] 

?- tokenize(`Example  text!!!`, T, [cased(true), spaces(true), pack(true)]).
T = [word('Example', 1), spc(' ', 2), word(text, 1), punct(!, 3)] 

?- tokenize(`Example  text!!!`, T, [cased(true), spaces(true), pack(true), to(strings)]).
T = [word("Example", 1), spc(" ", 2), word("text", 1), punct("!", 3)] 

?- tokenize(`Example  text!!!`, T, [cased(true), spaces(true), pack(true), to(codes)]).
T = [word([69, 120, 97, 109, 112, 108, 101], 1), spc([32], 2), word([116, 101, 120, 116], 1), punct([33], 3)]
```
