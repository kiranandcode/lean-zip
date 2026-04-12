import Lake
open System Lake DSL

package «lean-zip» where
  testDriver := "test"

require zipCommon from git "https://github.com/kim-em/lean-zip-common" @ "87480b0"

lean_lib Zip

lean_lib ZipTest where
  globs := #[.submodules `ZipTest]

@[default_target]
lean_exe test where
  root := `ZipTest

lean_exe bench where
  root := `ZipBench
