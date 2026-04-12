import Lake
open System Lake DSL

package «lean-zip» where

require zipCommon from git "https://github.com/kim-em/lean-zip-common" @ "87480b0"

lean_lib Zip

lean_exe «lean-zip» where
  root := `ZipCli
