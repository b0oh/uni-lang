force:

build: force
	npm run build-dev

deps: force
	npm install

run: force
	node src/app.js
