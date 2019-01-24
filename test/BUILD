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

haskell_test(
    name = "third-party-test",
    srcs = ["third-party-test.hs"],
    deps = [
        hazel_library("aeson"),
        hazel_library("aeson-extra"),
        hazel_library("cassava"),
        hazel_library("conduit"),
        hazel_library("entropy"),
        hazel_library("fuzzyset"),
        hazel_library("hsndfile"),
        hazel_library("htaglib"),
        hazel_library("http-client"),
        hazel_library("language-c"),
        hazel_library("lens"),
        hazel_library("network"),
        hazel_library("postgresql-libpq"),
        hazel_library("pretty-show"),
        hazel_library("text-metrics"),
        hazel_library("unix-compat"),
        hazel_library("unix-time"),
        hazel_library("vector"),
        hazel_library("wai-app-static"),
        hazel_library("zlib"),
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
