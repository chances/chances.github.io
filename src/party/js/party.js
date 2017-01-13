const Elm = require('./party.elm.js');

var app = Elm.Main.embed(document.body);

const haskind = require('haskind');
const Nothing = haskind.Data.Maybe.Nothing;
const Either = haskind.Data.Either;

const util = require('./util');
const elm = require('./elm/interop')(app);
const spotify = require('./spotify');

let log = util._curry(function log(message, value) {
  console.log(message, value);
  return value;
});

elm.updateToken(function (maybeAccessToken) {
  spotify.updateToken(
    elm.maybe(maybeAccessToken)
  );
});

elm.searchTracks(function (query) {
  log('search tracks from elm:', query);

  let logResults = log('track search results:');

  spotify.searchTracks(query, Nothing())
    .then(logResults)
    .then(elm.tracksResults.send);
});
