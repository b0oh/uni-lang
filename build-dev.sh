#!/usr/bin/env bash

./clean.sh

npm run build-dev || exit

HASH=$(md5 -r build/index.js | awk '{print $1}')
FILE=$HASH.js

cp build/index.js "build/$FILE"
sed "s/index.js/$FILE/g" src/index.html > build/index.html
