## Dialogui

**Dialogui** - library, providing a simple UI, interacting with user through command line.

### Screenshots

At the moment two "backends" of UI are available:

1. **TUI** (Text User Interface):

  <img src="./examples/hecho-tui.png" alt="TUI">

2. [Tk](https://en.wikipedia.org/wiki/Tk_%28software%29)-based **GUI**
(requires [dialogui-htk](https://github.com/astynax/dialogui-htk)):

  <img src="./examples/hecho-tk.png" alt="GUI">

### Installation

```shell
$ stack install
```

### Examples

Example applications with different implementations of UI can be builded this way:
```shell
$ stack build --flag dialogui:buildexamples
```
