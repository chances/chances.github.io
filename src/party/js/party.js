const Elm = require('./party.elm.js');

var app = Elm.Main.embed(document.body);

app.ports.searchTracks.subscribe(function (query) {
  console.log('search from elm: ', query);

  // app.ports.searchResults.send(results);
});
