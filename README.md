stackage-cli
============

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


Examples
--------

Let's say I want to see if I can compile my project against the latest in the LTS 1 series. For the sake of demonstration, let's pretend that `semigroups` is my project. :P

```
$ cabal unpack semigroups-0.16.2.2
$ cd semigroups-0.16.2.2
$ stackage sandbox init lts/1
Writing a default package environment file to
/home/dan/semigroups-0.16.2.2/cabal.sandbox.config
Using an existing sandbox located at
/home/dan/.stackage/sandboxes/ghc-7.8.4/lts-1.15
```

Cool, the latest in the series is lts-1.15, and I've already installed some things in that sandbox before.

```
$ cabal install --only-dependencies
Resolving dependencies...
Notice: installing into a sandbox located at
/home/dan/.stackage/sandboxes/ghc-7.8.4/lts-1.15
Configuring nats-1...
Building nats-1...
Installed nats-1
```

Some of the dependencies of my project were already installed in the sandbox. But `nats` was not. Cabal gives us a nice notice about where the sandbox is located when it performs the install. Note that it picked nats-1 because that's what our generated `cabal.config` told it to pick.

```
$ cat cabal.config | grep nats
             nats ==1,
```

When working with shared sandboxes, I recommend using `cabal build` and avoiding `cabal install`. Semigroups is actually already part of stackage. What happens if we cabal install it?

```
$ cabal install
...
Installed semigroups-0.16.2.2
```

As it happens, the version number in my local `semigroups.cabal` matches the constraint in `cabal.config`.

```
$ cat cabal.config | grep semigroups
             semigroups ==0.16.2.2,
```

So if you try to `cabal install` a local project into a shared sandbox, it will work, as long as the local `cabal.config` constraint agrees with the version number found in `yourproject.cabal`. You probably *don't* want to install local modifications into your *shared sandbox*, which is why I recommend using `cabal build` instead of `cabal install` to build your local projects.

Let's see what happens when we try to install an older version of semigroups:

```
$ cd ..
$ cabal unpack semigroups-0.16.2.1
$ cd semigroups-0.16.2.1
$ stackage sandbox init lts-1.15
Writing a default package environment file to
/home/dan/semigroups-0.16.2.1/cabal.sandbox.config
Using an existing sandbox located at
/home/dan/.stackage/sandboxes/ghc-7.8.4/lts-1.15
$ # notice it reused the same lts-1.15 sandbox
$ cabal install --only-dependencies
...
rejecting: semigroups-0.16.2.2/installed-c36..., 0.16.2.2 (global constraint
requires ==0.16.2.1)
rejecting: semigroups-0.16.2.1 (global constraint requires ==0.16.2.2)
...
```

First of all, `cabal install --only-dependencies` failed, because even though I specified *only dependencies*, cabal is mad at me for trying to install the wrong version of semigroups. Which I'm not... but whatever.

I can use `cabal install dep1 dep2 dep3` to manually install the dependencies I need. But since this is a shared sandbox, I already have those dependencies installed from last time.

Thankfully, cabal isn't as annoying about `cabal build`.

```
$ cabal build
```

It builds! But if you try to install...

```
$ cabal install
...
rejecting: semigroups-0.16.2.2/installed-c36..., 0.16.2.2 (global constraint
requires ==0.16.2.1)
rejecting: semigroups-0.16.2.1 (global constraint requires ==0.16.2.2)
...
```

It will stop you, because you're trying to install a version of `semigroups` that differs from the constraint in lts-1.15. This is good. We don't want to mess up the shared lts-1.15 sandbox.
