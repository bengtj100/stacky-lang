# Stacky - A simple stack language

## Downloads

* [Releases](https://www.dropbox.com/scl/fo/w3r8zo3och43dybheyspl/AHpmO0q0heWiFuOWt2gSwCE?rlkey=pwz1f8j1yuqd00grj88re745o&st=xx54wmnf&dl=0)

## Links
* [Introduction](Introduction.md)
* [Language Reference](Reference.md)


## Dependencies

### Build Dependencies

| Type  | Package        | Version                 | Repo Name               | Comments                 |
|:-----:|:--------------:|:-----------------------:|:------------------------|:-------------------------|
| DNF   | GHC            | 9.4.5                   | `ghc`                   |                          |
| DNF   | Cabal          | 3.8.1.0                 | `cabal-install`         |                          |
| Cabal | base           | 4.17.1.0                | n/a                     | Comes with cabal package |
| Cabal | split          | 0.2.5                   | `split`                 |                          |
| Cabal | raw-strings-qq | 1.1                     | `raw-strings-qq`        |                          |
| Cabal | MissingH       | 1.6.0.1                 | `MissingH`              |                          |
| DNF   | Pandoc         | 3.1.3                   | `pandoc`                |                          |
| DNF   | PdfLaTeX       | 3.141592653-2.6-1.40.25 | `texlive texlive-latex` |                          |
|       |                |                         |                         |                          |

### Run-time dependencies

```
linux-vdso.so.1 (0x00007fbf8c44c000)
libm.so.6 => /lib64/libm.so.6 (0x00007fbf8c34f000)
libgmp.so.10 => /lib64/libgmp.so.10 (0x00007fbf8c2ab000)
libc.so.6 => /lib64/libc.so.6 (0x00007fbf8c0be000)
libffi.so.8 => /lib64/libffi.so.8 (0x00007fbf8c0ae000)
/lib64/ld-linux-x86-64.so.2 (0x00007fbf8c44e000)
```
