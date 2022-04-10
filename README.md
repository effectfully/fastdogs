WARNING
========

Warning: this is a very quick-and-dirty port of `haskdogs` to `fast-tags`.
This forks generates an `emacs` `TAGS` file by default and requires `cabal` for
installation. Support for `nix` is dropped.

You probably don't want to use this.

Fastdogs
========

Fastdogs is a shellscript-like tool which creates tag file for entire haskell
project directory. It takes into account first-level dependencies by recursively
scanning imports and adding matching projects to the dependency list. Next,
Fastdogs uses cabal or stack to unpack their sources into a temporary directory,
which is `~/.fastdogs` by default. Finally, fast-tags is called to produce the
`tags` file.

As a result, programmer can use his/her text editor supporting tags (e.g. vim)
to jump directly to definition of any standard or foreign function he/she uses.

Note, that fastdogs relies on some GNU programs as well as on Unix shell
commands such as 'cd', 'mkdir' and so on. Also it would run 'stack' and ghc-pkg'
in order to obtain package information.

INSTALL
-------

Check the dependencies. Currently they are: cabal, stack, fast-tags, GNU find,
which and shell.

Please follow stack's documentation(https://github.com/commercialhaskell/stack) to install stack.

Install fast-tags via

	$ stack install fast-tags

Install fastdogs via

	$ cabal install all

called from within the cloned repo.

Make sure that PATH contains path to your stack binaries directory ($HOME/.local/bin by default).

RUNNING
-------

1. Make sure yoy have installed fast-tags and put it in PATH.

2. cd to your Haskell project dir

       $ cd $HOME/my-haskell-project

3. Run fastdogs without arguments to generate tags file in Emacs-compatible format

       $ fastdogs

See also `fastdogs --help`.

The following error could be caused by (over)strict Haskell policy regarding
Unicode locale:

    fastdogs: fd:5: hGetContents: invalid argument (invalid byte sequence)

It usually happens when the program tries to print Unicode character to
non-unicode console. In order to overcome, try the following setting:

    export LANG=en_US.UTF8

TIPS
-----

* create tags for specific package

  ``echo 'import Control.Lens' | fastdogs -i -``
