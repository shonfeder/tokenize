# `pack(tokenize) :-`

A modest tokenization library for SWI-Prolog, seeking a balance between
simplicity and flexibility.

[![CircleCI](https://circleci.com/gh/shonfeder/tokenize.svg?style=svg)](https://circleci.com/gh/shonfeder/tokenize)

## Synopsis

```prolog
?- tokenize(`\tExample  Text.`, Tokens).
Tokens = [cntrl('\t'), word(example), space(' '), space(' '), word(text), punct('.')]

?- tokenize(`\tExample  Text.`, Tokens, [cntrl(false), pack(true), cased(true)]).
Tokens = [word('Example', 1), space(' ', 2), word('Text', 1), punct('.', 1)]

?- tokenize(`\tExample  Text.`, Tokens), untokenize(Tokens, Text), format('~s~n', [Text]).
	example  text.
Tokens = [cntrl('\t'), word(example), space(' '), space(' '), word(text), punct('.')],
Text = [9, 101, 120, 97, 109, 112, 108, 101, 32|...]
```

## Description

Module `tokenize` aims to provide a straightforward tool for tokenizing text into a simple format. It is the result of a learning exercise, and it is far from perfect. If there is sufficient interest from myself or anyone else, I'll try to improve it.

It is packaged as an SWI-Prolog pack, available [here](http://www.swi-prolog.org/pack/list?p=tokenize). Install it into your SWI-Prolog system with the query

```prolog
?- pack_install(tokenize).
```

Please [visit the wiki](https://github.com/aBathologist/tokenize/wiki/tokenize.pl-options-and-examples) for more detailed instructions and examples, including a full list of options supported.

## Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md).
