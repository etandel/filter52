#!/bin/env sh

BUILD_DIR=dist
TARGET="$BUILD_DIR/filter52"

mkdir --parent filter52 $TARGET

cabal build

cp "dist/build/filter52/filter52" "$TARGET"
cp "src/run-arch.sh" "$TARGET"
cp "README" "$TARGET"

tar -cvzf "$BUILD_DIR/filter52.tar.gz" $TARGET

