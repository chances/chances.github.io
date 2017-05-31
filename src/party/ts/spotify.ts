import Promise = require('bluebird');
import Spotify = require('spotify-web-api-js');

import { Data } from 'haskind';
const { Maybe, Either } = Data;
const Nothing = Maybe.Nothing;

import * as util from './util';

const spotify = new Spotify();
spotify.setPromiseImplementation(Promise);

let accessToken = Nothing();

// Maybe TokenString -> Void
export function updateAccessToken(maybeToken: Haskind.Maybe<string>) {
  accessToken = Maybe.fromMaybe(Nothing(), maybeToken);

  if (Maybe.isJust(maybeToken)) {
    spotify.setAccessToken(Maybe.fromJust(maybeToken));
  }
}

// String -> Maybe { limit: Maybe Number, offset: Maybe Number } ->
//    Promise (Either SpotifyError (Paging Track))
export function searchTracks(query: string, maybeOptions: any) {
  // TODO: Handle maybeOptions
  query = query.split(" ").join("%20");

  return new Promise(function (resolve: (result: any) => Haskind.Either) {
    if (Maybe.isNothing(accessToken)) {
      resolve(Either.Left({
        error: "invalid_token",
        error_description: "No access token exists"
      }));
    } else {
      spotify.searchTracks(query)
        .then(function (data: SpotifyApi.TrackSearchResponse) {
          resolve(Either.Right(data));
        }).catch(function (error: any) {
          resolve(Either.Left(error));
        });
    }
  });
}
