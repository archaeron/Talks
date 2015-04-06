# Repl

## Starting

```sh
ghci
```

## Useful Commands

| :reload      | :r | reload the current module set             |
| :type <expr> | :t | show the type of <expr>                   |
| :kind <type> | :k | show the kind of <type>                   |
| :info        | :i | display information about the given names |
| :print       | :p | Print the expression                      |
| :edit        | :e | Load file in system editor                |

## Loading source files

`:cd <dir>` changes the current directory to `<dir>`.
`:load <file>` load a file and set it to the current target set.
`:add <file>` add a file to the current target set.
`import <module>` or `:module +<module>` to load a module

## Multi line input

```
:{
let greet = do
	putStrLn "Hello World"
:}
```


## Language extensions and compiler pragmas

`:set` shows the currently set options.

### Setting an option

```
:set -XRankNTypes
:set -fno-warn-unused-do-bind
```
[Flag reference](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flag-reference.html)


## References

http://stackoverflow.com/a/21008377/1829891
http://dev.stephendiehl.com/hask/#ghci
https://downloads.haskell.org/~ghc/7.6.2/docs/html/users_guide/loading-source-files.html
