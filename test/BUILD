load("@io_tweag_rules_haskell//haskell:haskell.bzl",
     "haskell_doctest",
     "haskell_library",
     "haskell_test",
)
load("@ai_formation_hazel//:hazel.bzl", "hazel_library")

haskell_test(
    name = "vector-test",
    srcs = ["vector-test.hs"],
    deps = [
        hazel_library("base"),
        hazel_library("vector"),
    ],
    linkstatic = False,
)

haskell_test(
    name = "old-time-test",
    srcs = ["old-time-test.hs"],
    deps = [
        hazel_library("base"),
        hazel_library("old-time"),
    ],
)

haskell_library(
    name = "DoctestExample",
    srcs = ["DoctestExample.hs"],
    deps = [hazel_library("base")],
)

haskell_doctest(
    name = "doctest-test",
    deps = [":DoctestExample"],
)

load("//:third_party/cabal2bazel/bzl/cabal_paths.bzl", "cabal_paths")

cabal_paths(
    name = "foo-paths",
    package = "foo",
    version = [
        1,
        2,
    ],
)

haskell_test(
    name = "PathsTest",
    srcs = ["PathsTest.hs"],
    deps = [
        ":foo-paths",
        hazel_library("base"),
        hazel_library("filepath"),
    ],
)
