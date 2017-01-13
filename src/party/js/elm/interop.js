const haskind = require('haskind');
const Maybe = haskind.Data.Maybe;

module.exports = function (app) {
  return {
    updateToken: app.ports.updateToken.subscribe,
    searchTracks: app.ports.searchTracks.subscribe,

    tracksResults: app.ports.tracksResults,

    // Haskind interop
    maybe: function elmMaybeToHaskindMaybe(value) {
      return (value == null) ? Maybe.Nothing() : Maybe.Just(value);
    }
  }
};
