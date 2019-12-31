#!/usr/bin/env bash

./clean.sh

npm run build || exit

HASH=$(md5 -r build/index.js | awk '{print $1}')
FILE=$HASH.js

mv build/index.js "build/$FILE"
sed "s/index.js/$FILE/g" src/index.html > build/index.html
