stackage-cli
============

[![Build Status](https://travis-ci.org/fpco/stackage-cli.svg)](https://travis-ci.org/fpco/stackage-cli)

A command-line interface for leveraging stackage.

You must have `ghc`, `ghc-pkg`, and `cabal` on your $PATH. This program will make various calls to those executables on your behalf.

This package provides two executables: the `stackage` executable, and its alias, `stk`. The `stackage` command-line program will inspect your path for stackage plugins, and will dispatch to them. Anything on your $PATH prefixed `stackage-` that responds to the `--summary` flag is considered a stackage plugin.

(This package also provides a library, `Stackage.CLI`, which is intended to make the process of writing stackage plugins easier.)

This package no longer provides any plugins (which makes it rather useless on its own).
You can find the `init`, `purge`, and `upgrade` plugins in the `stackage-cabal` package.
You can find the `sandbox` plugin in the `stackage-sandbox` package.

## Further reading

See also: [this example on the stackage-cli wiki](https://github.com/fpco/stackage-cli/wiki/Example).
