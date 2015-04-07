# GHCi Cheatsheet

## Starting

```sh
ghci
```

## Useful Commands

- `:reload` or `:r`, reload the current module set
- `:type <expr>` or `:t`, show the type of `<expr>`
- `:kind <type>` or `:k`, show the kind of <type>
- `:info` or `:i`, display information about the given names
- `:print` or `:p`, Print the expression
- `:edit` or `:e`, Load file in system editor
- `:quit` or `:q`, Exit ghci

## Loading source files

- `:cd <dir>`, changes the current directory to `<dir>`.
- `:load <file>` or `:l`, load a file and set it to the current target set.
- `:add <file>`, add a file to the current target set.
- `import <module>` or `:module +<module>`, load a module

## Multi line input

```haskell
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

http://dev.stephendiehl.com/hask/#ghci

http://stackoverflow.com/a/21008377/1829891

https://downloads.haskell.org/~ghc/7.6.2/docs/html/users_guide/loading-source-files.html
