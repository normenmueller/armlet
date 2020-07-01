# armlet

`armlet`, ARchimate Modeling LanguagE Transformations, is to be understood as
an accessory with which you can decorate models described in
[ArchiMate](https://pubs.opengroup.org/architecture/archimate3-doc/toc.html)
and represented by [Archi](https://www.archimatetool.com/).

# Installation

First of all, get `stack` if you don't have it already: see the [official stack
documentation](https://docs.haskellstack.org/en/stable/README/#how-to-install).
Note that stack is also included in the [Haskell
platform](http://hackage.haskell.org/platform/), and on Linux it is usually
available in your package manager.

If you have `git`, you can now clone the repository and build:

```shell
git clone https://github.com/normenmueller/armlet..git
cd armlet
git checkout <commit/tag/branch>
stack install
```

If you don't have `git`, just download the sources for your preferred
commit/branch/tag via the GitHub interface, and run `stack install` in the
directory that contains `stack.yaml` file.

This will install `armlet` executable to `$HOME/.local/bin`.

To install the bash completion system-wide:

```
armlet --bash-completion-script armlet >/etc/bash_completion.d/armlet
```

# Usage

Usage information is available via `armlet -h`.

# License

See [LICENSE](https://github.com/normenmueller/armlet/blob/master/LICENSE) for
details.

© 2020 Normen Müller

Contributors (per GPL, holders of copyright on their respective contributions):

<!-- BEGIN CONTRIBUTORS LIST -->
- Normen Müller

<!-- END CONTRIBUTORS LIST -->
