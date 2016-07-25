# RSCoin Block Explorer

TODO: add some description here

## Installation

For development:
```sh
cd block-explorer
npm install
npm start
```

Visit `http://localhost:3000` in your browser.
and watch the magic!

For deploy (`npm run build` will be run automatically on `stack build`):
```sh
cd block-explorer
npm run build
npm run serve
```

## Available scripts

### watch

`npm start` or `npm run watch` will start a development server, which
hot-reloads your application when sources changes.

### serve

`npm run serve` serves your application without watching for changes or
hot-reloading.

### build

`npm run build` bundles and minifies your application to run in production mode.
