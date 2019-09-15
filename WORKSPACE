workspace(name = "ai_formation_hazel")

load("@bazel_tools//tools/build_defs/repo:http.bzl",
     "http_archive",
)

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-cd2ed701127ebf7f8f21d37feb1d678e4fdf85e5",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/cd2ed70.tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_git_repository", "nixpkgs_package")

nixpkgs_git_repository(
    name = "nixpkgs",
    revision = "c37ae1dcf669b5b262350ddbb75a40be166e7e88",
)

load("//:cc_configure_custom.bzl", "cc_configure_custom")

nixpkgs_package(
    name = "compiler",
    repository = "@nixpkgs",
    nix_file = "//:compiler.nix",
)

nixpkgs_package(
    name = "binutils",
    repository = "@nixpkgs",
    attribute_path = "binutils"
)

cc_configure_custom(
    name = "local_config_cc",
    gcc = "@compiler//:bin/cc",
    ld = "@binutils//:bin/ld",
)

RULES_HASKELL_SHA = "8221d03cd38cee9cd956dd8f3e48ea0fa4440770"

http_archive(
    name = "io_tweag_rules_haskell",
    urls = ["https://github.com/FormationAI/rules_haskell/archive/"
            + RULES_HASKELL_SHA + ".tar.gz"],
    strip_prefix = "rules_haskell-" + RULES_HASKELL_SHA,
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

nixpkgs_package(
    name = "ghc",
    repository = "@nixpkgs",
    attribute_path = "haskell.packages.ghc822.ghc",
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
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_cc_import")

cc_library(
  name = "taglib",
  srcs = [":lib"],
)

filegroup (
  name = "lib",
  srcs = glob([
    "lib/libtag_c.so*",
    "lib/libtag_c.dylib",
  ]),
)
""",
)

nixpkgs_package(
    name = "libsndfile",
    attribute_path = "libsndfile.out",
    repository = "@nixpkgs",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

cc_library(
  name = "libsndfile",
  srcs = [":lib"],
  deps = ["@libsndfile.dev"],
)

filegroup(
  name = "lib",
  srcs = glob([
    "lib/libsndfile.so*",
    "lib/libsndfile.dylib",
  ]),
)
"""
)

nixpkgs_package(
    name = "libsndfile.dev",
    repository = "@nixpkgs",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

cc_library(
  name = "libsndfile.dev",
  hdrs = [":headers"],
  strip_include_prefix = "include",
)

filegroup(
  name = "headers",
  srcs = glob([
    "include/*.h",
  ]),
)
"""
)

nixpkgs_package(
  name = "postgresql",
  repository = "@nixpkgs",
  build_file_content = """
package(default_visibility = ["//visibility:public"])

cc_library(
  name = "postgresql",
  srcs = [":lib"],
  hdrs = [":headers"],
  strip_include_prefix = "include",
)

filegroup (
  name = "lib",
  srcs = glob([
    "lib/libecpg.so*",
    "lib/libecpg.dylib",
  ]),
)

filegroup (
  name = "headers",
  srcs = glob([
    "include/*.h",
    "include/**/*.h",
  ]),
)
"""
)

nixpkgs_package(
  name = "glib_locales",
  repository = "@nixpkgs",
  attribute_path = "glibcLocales",
  build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
  name = "locale-archive",
  srcs = ["lib/locale/locale-archive"],
)
"""
)

register_toolchains(
    "@ghc//:ghc",
    "//:c2hs",
    "//:doctest",
)

load("//:hazel.bzl", "hazel_repositories",
     "hazel_custom_package_hackage",
     "hazel_custom_package_github",
)

hazel_custom_package_hackage(
  package_name = "zlib",
  version = "0.6.2",
)

hazel_custom_package_hackage(
  package_name = "vault",
  version = "0.3.1.1",
)

hazel_custom_package_hackage(
  package_name = "ghc-paths",
  version = "0.1.0.9",
)

hazel_custom_package_github(
  package_name = "text-metrics",
  github_user = "mrkkrp",
  github_repo = "text-metrics",
  repo_sha = "5d10b6f6ec4ff4b014e5e512f82d23e7606cc260",
)

hazel_custom_package_github(
  package_name = "conduit",
  github_user = "snoyberg",
  github_repo = "conduit",
  strip_prefix = "conduit",
  repo_sha = "34db9267bb4f9dbdee45623944900062e7995d09",
)

hazel_custom_package_github(
  package_name = "wai-app-static",
  github_user = "FormationAI",
  github_repo = "wai",
  strip_prefix = "wai-app-static",
  repo_sha = "9217512fae1d6c2317447b257f478005efb55ef7",
)

load("//:packages.bzl", "packages", "core_packages")

hazel_repositories(
    packages = packages,
    core_packages = core_packages,
    exclude_packages = [
      "conduit",
      "ghc-paths",
      "text-metrics",
      "vault",
      "wai-app-static",
      "zlib",
    ],
    extra_libs = {
      "pq": "@postgresql",
      "sndfile": "@libsndfile",
      "tag_c": "@taglib",
    },
)
