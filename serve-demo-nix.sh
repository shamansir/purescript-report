#!/bin/sh
# cd ./test/games-samples
# sh ./make-json.sh
# cd ../../
cp -f ./test/games-samples/games-collection.json ./dist/
spago bundle --module Demo.WebApp --outfile ./web/app.js --platform browser --bundle-type app
node ./node_modules/parcel/lib/bin.js build ./web/app.html --no-cache
node ./node_modules/parcel/lib/bin.js serve ./web/app.html --no-cache --port 1238
