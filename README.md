# Hazel: Automatic Bazel rules for third-party Haskell dependencies

[![CircleCI](https://circleci.com/gh/FormationAI/hazel/tree/master.svg?style=svg)](https://circleci.com/gh/FormationAI/hazel/tree/master)

Hazel is a Bazel framework that manages build rules for third-party Haskell
dependencies.

Rather than manually writing BUILD files (and checking them into your source
repository), Hazel only requires you to specify the set of third-party
dependencies and their versions.  Hazel autogenerates everything else at build
time, including automatically downloading Cabal packages from Hackage,
parsing their .cabal files and creating Haskell build rules.

Hazel uses the [`rules_haskell`](https://github.com/tweag/rules_haskell)
package.

## Status
Hazel is still experimental, and its API is subject to change.  Most Hackage
packages will not yet build with it; however, a small number have been
verified so far as a proof of concept.

## Setting up a new project
First, run the `Stackage.hs` script to generate a list of all packages in a
particular LTS release:

```
./Stackage.hs lts-10.5 packages.bzl
```

That `packages.bzl` file should be checked into your repository.


Then, add some `rules_haskell`-related boilerplate to your `WORKSPACE` file,
as described in their
[`README`](https://github.com/tweag/rules_nixpkgs/blob/master/README.md):

```
RULES_NIXPKGS_SHA = "cd2ed701127ebf7f8f21d37feb1d678e4fdf85e5"
http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-" + RULES_NIXPKGS_SHA,
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/" + RULES_NIXPKGS_SHA + ".tar.gz"],
)

RULES_HASKELL_SHA = "67f2f87691f93a8a674283a1e1ab2ce46daf0f99"
http_archive(
    name = "io_tweag_rules_haskell",
    urls = ["https://github.com/tweag/rules_haskell/archive/" + RULES_HASKELL_SHA + ".tar.gz"],
    strip_prefix = "rules_haskell-" + RULES_HASKELL_SHA,
)

# Replace with a more recent commit, as appropriate
HAZEL_SHA = "f2d6128811aae75f927d062348dc9686072fe9ce"
http_archive(
    name = "ai_formation_hazel",
    strip_prefix = "hazel-" + HAZEL_SHA,
    urls = ["https://github.com/FormationAI/hazel/archive/" + HAZEL_SHA + ".tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_git_repository", "nixpkgs_package")

NIXPKGS_REVISION_SHA = "c33c5239f62b4855b14dc5b01dfa3e2a885cf9ca"
nixpkgs_git_repository(
    name = "nixpkgs",
    # A revision of 17.09 that contains ghc-8.2.2:
    revision = NIXPKGS_REVISION_SHA,
)

nixpkgs_package(
    name = "ghc",
    repository = "@nixpkgs",
    attribute_path = "haskell.packages.ghc822.ghc",
    # NOTE: this uses the ghc bindist template provided by Hazel
    build_file = "@ai_formation_hazel//:BUILD.ghc",
)

nixpkgs_package(
    name = "c2hs",
    repository = "@nixpkgs",
    attribute_path = "haskell.packages.ghc822.c2hs",
)

nixpkgs_package(
    name = "taglib",
    repository = "@nixpkgs",
    attribute_path = "taglib",
    # Inline the definition of a nonexistent build file
    build_file_content =
"""
package(default_visibility = ["//visibility:public"])

load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_cc_import")

filegroup (
  name = "lib",
  srcs = glob([
    "lib/libtag_c.so",
    "lib/libtag_c.dylib",
  ]),
)
""",
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

register_toolchains("@ghc//:ghc")
```

Finally, in `WORKSPACE`, load `packages.bzl` and feed its contents to `haskell_repositories` macro:

```
load("@ai_formation_hazel//:hazel.bzl", "hazel_repositories")
load("//:packages.bzl", "prebuilt_dependencies", "packages")

hazel_repositories(
    prebuilt_dependencies=prebuilt_dependencies,
    packages=packages
)
```

## Using Hazel in build rules
The `hazel_repositories` macro creates a separate [external
dependency](https://docs.bazel.build/versions/master/external.html) for each
package.  It downloads the corresponding Cabal tarball from Hackage
and construct build rules for compiling the components of that package.

For example:

```
# Build all components of a single package, as well as all of its dependencies:
bazel build @haskell_vector//:all

# Build all components of all third-party packages:
bazel build @all_hazel_packages//:all
```

To depend on a third-party package in a `BUILD` file, use the macros provided by `hazel.bzl`:

```
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_test")
load("@ai_formation_hazel//:hazel.bzl", "hazel_library")

haskell_library(
    name = "Foo",
    srcs = ["Foo.hs"],
    prebuilt_dependencies = ["base"],
)

haskell_test(
    name = "bar",
    srcs = ["Main.hs"],
    deps = [
        ":Foo",
        hazel_library("vector"),
    ],
    prebuilt_dependencies = ["base"],
)
```
