# Synopsis

```prolog
?- tokenize(`\tExample  Text.`, Tokens).
Tokens = [cntrl('\t'), word(example), spc(' '), spc(' '), word(text), punct('.')] 

?- tokenize(`\tExample  Text.`, Tokens, [cntrl(false), pack(true), cased(true)]).
Tokens = [word('Example', 1), spc(' ', 2), word('Text', 1), punct('.', 1)] 

?- tokenize(`\tExample  Text.`, Tokens), untokenize(Tokens, Text), format('~s~n', [Text]).
	example  text.
Tokens = [cntrl('\t'), word(example), spc(' '), spc(' '), word(text), punct('.')],
Text = [9, 101, 120, 97, 109, 112, 108, 101, 32|...] 
```

# Description

Module `tokenize` aims to provide a straightforward tool for tokenizing text into a simple format. It is the result of a learning exercise, and it is far from perfect. If there is sufficient interest from myself or anyone else, I'll try to improve it.

Please [visit the wiki](https://github.com/aBathologist/tokenize/wiki) for more detailed instructions and examples, including [a full list of options supported](https://github.com/aBathologist/tokenize/wiki#options).

