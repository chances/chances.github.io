const Promise = require('bluebird');
const Spotify = require('spotify-web-api-js');

const haskind = require('haskind');
const Maybe = haskind.Data.Maybe;
const Nothing = Maybe.Nothing;
const Either = haskind.Data.Either;

const util = require('./util');

const spotify = new Spotify();
spotify.setPromiseImplementation(Promise);

let accessToken = Nothing();

module.exports = {

  // Maybe TokenString -> Void
  updateToken: function setAccessToken(maybeToken) {
    accessToken = Maybe.fromMaybe(Nothing(), maybeToken);

    if (Maybe.isJust(maybeToken)) {
      spotify.setAccessToken(Maybe.fromJust(maybeToken));
    }
  },

  // String -> Maybe { limit: Maybe Number, offset: Maybe Number } ->
  //    Promise (Either SpotifyError (Paging Track))
  searchTracks: function searchTracks(query, maybeOptions) {
    // TODO: Handle maybeOptions
    query = query.split(" ").join("%20");

    return new Promise(function (resolve) {
      if (Maybe.isNothing(accessToken)) {
        resolve(Either.Left({
          error: "invalid_token",
          error_description: "No access token exists"
        }));
      } else {
        spotify.searchTracks(query)
          .then(function (data) {
            resolve(Either.Right(data));
          }).catch(function (error) {
            resolve(Either.Left(error));
          });
      }
    });
  }
};
