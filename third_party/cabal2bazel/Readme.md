Hazel uses some code originally hosted at
[google/cabal2bazel](https://github.com/google/cabal2bazel).
The current version of that code was taken from [this commit](https://github.com/google/cabal2bazel/commit/c208bf179c8e820b0733a4472a5825c4f86e8e60).

Some of that code is used nearly unchanged; other parts (in particular,
`cabal_package.bzl` are heavily modified.  Here is a (nonexhaustive) list of
some key differences:

#### Different semantics of `rules_haskell`
- `hsc` files can be passed directly to build rules.
- All source files need to be given a common `src_strip_prefix`; solve this
  by symlinking them all into a common location.
- Locate data files correctly in the rule's external repository.
- Work around `haskell_library` not supporting libraries without `srcs`.

#### Hazel repository structure
- The components of each package live in an autogenerated external repository
  (for example: `@haskell_{package}_{hash}//...`), rather than
  `//third_party/haskell/{package}/...`.
- Default packages are taken from the `packages.bzl` file, rather than being
  hard-coded; they also need to be passed via the `builtin_dependencies`
  attribute.
- Package versions (needed for the Cabal macros file) are passed around
  manually by Bazel functions (since Hazel knows them up-front) rather than
  being passed through a separate `cabal_haskell_library` rule.


### Other changed/reimplemented bits
- Flag resolution code (`hazel_base_repository/Flatten.hs`) is implemented
  differently, to not require knowing the versions of packages
- Supports `Cabal>=2.0`.
- Don't set `CURRENT_PACKAGE_KEY`; I'm not aware of any packages using it.
- Use Skylib (in particular, `paths`) to clean up some code.
