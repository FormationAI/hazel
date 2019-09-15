load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_toolchain",
  "haskell_doctest_toolchain",
)
load("@io_tweag_rules_haskell//haskell:c2hs.bzl", "c2hs_toolchain")
load("//tools:mangling.bzl", "hazel_binary")

exports_files([
    "hazel.bzl",
    "BUILD.ghc",
    "paths-template.hs",
    "cc_configure_custom.bzl",
])

haskell_doctest_toolchain(
    name = "doctest",
    doctest = hazel_binary("doctest"),
)

c2hs_toolchain(
  name = "c2hs",
  c2hs = "@c2hs//:bin",
)
