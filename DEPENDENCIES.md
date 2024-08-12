# Stacky - Dependency list

## Build Dependencies

If nothing else is said, it is assumed that these are the *oldest* version that is compatible with the sources.

### Haskell package dependencies

These are the dependences you need to meet to build the interpreter using `make all`. For a guaranteed up to date list, see the `src/stacky.cabal` file.



| Type  | Package        | Version  | Repo Name        | Comments                 |
|:-----:|:--------------:|:--------:|:-----------------|:-------------------------|
| DNF   | GHC            | 9.4.5    | `ghc`            |                          |
| DNF   | Cabal          | 3.8.1.0  | `cabal-install`  |                          |
| Cabal | base           | 4.17.1.0 | n/a              | Comes with cabal package |
| Cabal | split          | 0.2.5    | `split`          |                          |
| Cabal | raw-strings-qq | 1.1      | `raw-strings-qq` |                          |
| Cabal | MissingH       | 1.6.0.1  | `MissingH`       |                          |
| Cabal | directory      | 1.3.8.5  | directory        |                          |

### Other dependencies

| Type | Package  | Version                 | Repo Name               | Comments |
|:----:|:--------:|:-----------------------:|:------------------------|:---------|
| DNF  | Pandoc   | 3.1.3                   | `pandoc`                |          |
| DNF  | PdfLaTeX | 3.141592653-2.6-1.40.25 | `texlive texlive-latex` |          |

## Run-time dependencies

These are the libraries the `stacky` binary executable depends on as given by the command:

```
ldd stacky | awk '{print $1}'
```

```
linux-vdso.so.1
libm.so.6
libgmp.so.10
libc.so.6
libffi.so.8
/lib64/ld-linux-x86-64.so.2
```
