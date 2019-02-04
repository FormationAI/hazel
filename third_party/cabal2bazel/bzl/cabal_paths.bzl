# Copyright 2018 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Rules for auto-generating the Paths_* module needed in third-party.

Some third-party Haskell packages use a Paths_{package_name}.hs file
which is auto-generated by Cabal.  That file lets them
- Get the package's current version number
- Find useful data file

This file exports the "cabal_paths" rule for auto-generating that Paths module.
For usage information, see the below documentation for that rule.
"""
load("@bazel_skylib//:lib.bzl", "paths")
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

def _impl_path_module_gen(ctx):
  paths_file = ctx.new_file(ctx.label.name)

  base_dir = ctx.label.package + (
      ("/" + ctx.attr.data_dir) if ctx.attr.data_dir else "")

  ctx.template_action(
      template=ctx.file._template,
      output=paths_file,
      substitutions={
          "%{module}": ctx.attr.module,
          "%{base_dir}": paths.join(
              # TODO: this probably won't work for packages not in external
              # repositories.  See:
              # https://github.com/bazelbuild/bazel/wiki/Updating-the-runfiles-tree-structure
              "..", paths.relativize(ctx.label.workspace_root, "external"), base_dir),
          "%{version}": str(ctx.attr.version),
      },
  )
  return struct(files=depset([paths_file]))


_path_module_gen = rule(
    implementation=_impl_path_module_gen,
    attrs={
        "data_dir": attr.string(),
        "module": attr.string(),
        "version": attr.int_list(mandatory=True, non_empty=True),
        "_template": attr.label(allow_files=True, single_file=True,
                                default=Label(
            "@ai_formation_hazel//:paths-template.hs")),
    },
)

def cabal_paths(name=None, package=None, data_dir='',data=[], version=[], **kwargs):
  """Generate a Cabal Paths_* module.

  Generates a Paths_ module for use by Cabal packages, and compiles it into a
  haskell_library target that can be dependend on by other Haskell rules.

  Example usage:

    haskell_binary(
      name = "hlint",
      srcs = [..],
      deps = [":paths", ...],
    )

    cabal_paths(
      name = "paths",
      package = "hlint",
      version = [1, 18, 5],
      # Corresponds to the Cabal "data-dir" field.
      data_dir = "datafiles",
      # Corresponds to the Cabal "data-files" field, with data-dir prepended.
      data = ["datafiles/some/path", "datafiles/another/path"],
    )


  Args:
    name: The name of the resulting library target.
    package: The name (string) of the Cabal package that's being built.
    data_dir: The subdirectory (relative to this package) that contains the
      data files (if any).  If empty, assumes the data files are at the top
      level of the package.
    data: The data files that this package depends on to run.
    version: The version number of this package (list of ints)
  """
  module_name = "Paths_" + package.replace("-", "_")
  paths_file = module_name + ".hs"
  _path_module_gen(
      name = paths_file,
      module=module_name,
      data_dir=data_dir,
      version=version,
  )
  haskell_library(
      name=name,
      srcs = [paths_file],
      data = data,
      deps = [
          "@haskell_base//:base",
          "@haskell_filepath//:filepath",
      ],
      # TODO: run directory resolution.
      **kwargs)
