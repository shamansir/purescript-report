#!/bin/sh
# cd ./test/games-samples
# sh ./make-json.sh
# cd ../../
cp -f ./test/games-samples/games-collection.json ./dist/
spago bundle --module Demo.ReportWebApp --outfile ./web/demo-rep-app.js --platform browser --bundle-type app
node ./node_modules/parcel/lib/bin.js build ./web/demo-rep-app.html --no-cache
node ./node_modules/parcel/lib/bin.js serve ./web/demo-rep-app.html --no-cache --port 1338
