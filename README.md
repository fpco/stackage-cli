stackage-cli
============

[![Build Status](https://travis-ci.org/fpco/stackage-cli.svg)](https://travis-ci.org/fpco/stackage-cli)

A command-line interface for leveraging stackage.

You must have `ghc`, `ghc-pkg`, and `cabal` on your $PATH. This program will make various calls to those executables on your behalf.

This package provides a number of executables. Primary among these is the `stackage` executable (and its alias `stk`). The `stackage` command-line program will inspect your path for stackage plugins, and will dispatch to them. Anything on your $PATH prefixed `stackage-` that responds to the `--summary` flag is considered a stackage plugin.

(This package also provides a library, `Stackage.CLI`, which is intended to make the process of writing stackage plugins easier.)

This package provides the following stackage plugins:

## stackage init

Downloads a `cabal.config` file from `stackage.org`. That's all it does! The `cabal.config` file constrains the package versions used to build your project. Stackage.org is constantly calculating the best compatible build plans for a big subset of Hackage.

## stackage purge

Calls `ghc-pkg unregister` on your package databases. (You will be prompted on a per-db basis; use `stackage purge --force` if you don't want to be prompted.) This is an easy way to clean out your sandbox.

## stackage upgrade

A `stackage purge` followed by `stackage init`.

## stackage sandbox

The sandbox plugin has a few commands of its own that mirror the above three stackage commands:

### stackage sandbox init

In addition to downloading a `cabal.config` file, this will also set up a shared sandbox (via `cabal.sandbox.config`) corresponding to whatever stackage snapshot is used. By default, the latest available Stackage LTS snapshot will be used.

Shared sandboxes are located at `$HOME/.stackage/sandboxes`. Be aware! Anything you `cabal install` to this sandbox will be available to any other projects that share the same sandbox. The `cabal.config` file should help make sure you don't install anything incompatible.

### stackage sandbox delete

Delete `cabal.config` and `cabal.sandbox.config`.

Unlike `stackage purge`, this does not alter any package databases. The shared sandbox will remain untouched. You can use `stackage sandbox delete NAME` to delete the shared sandbox called NAME (e.g. lts-2.0) from your system.

### stackage sandbox upgrade

Replace `cabal.config` and `cabal.sandbox.config` with newer versions and (possibly) a new shared sandbox.


## Further reading

See also: [this example on the stackage-cli wiki](https://github.com/fpco/stackage-cli/wiki/Example).
