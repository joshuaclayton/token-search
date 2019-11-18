# token-search

This is a library for efficient substring detection across a codebase.

## Motivation

[Unused] leverages [ctags]' token generation in conjunction with a tool to
search the file system (either [ripgrep] or [The Silver Searcher]).

During execution, Unused shells out from Haskell for each unique token and
searches the appropriate files for each token. This means each file searched is
searched thousands of times each run, which takes a significant amount of time.

[Unused]: https://unused.codes/
[ctags]: https://ctags.io/
[ripgrep]: https://github.com/BurntSushi/ripgrep
[The Silver Searcher]: https://geoff.greer.fm/ag/

## Approach

Instead of searching each file git tracks for each of potentially thousands of
tokens, `token-search` processes each file once.

With the tokens:

```
rem
or
lo
```

and the text:

```
lorem ipsum
dolor sit amet
```

`token-search` then:

1. Builds a trie with the three tokens
2. Iterates over each character in the text, while also:
   * creating a new copy of the trie
   * adding the created trie to a list of all non-terminated tries
   * walks each trie by the character
   * maintains a list of terminal nodes as tries are walked
   * increments a terminal node count for tokens as they're encountered

## Install

```sh
stack install
```

## Test

```sh
stack test
```

## License

Copyright 2019 Josh Clayton. See the [LICENSE](LICENSE).
