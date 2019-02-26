#!/usr/bin/env bash

# TODO: Optimize the compilation script

cd ../

stack build && (path_exec=`stack path --local-install-root` ; cp $path_exec/bin/deBruijn ./deBruijn) || stack build --copy-bins --local-bin-path .