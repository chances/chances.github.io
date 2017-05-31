import Promise = require('bluebird');
import Spotify = require('spotify-web-api-js');

import { Maybe, Either } from 'monet'

import * as util from './util';

const spotify = new Spotify();
spotify.setPromiseImplementation(Promise);

let accessToken: Maybe<string> = Maybe.Nothing<string>();

// Maybe TokenString -> Void
export function updateAccessToken(maybeToken: Maybe<string>) {
  accessToken = maybeToken;

  if (maybeToken.isJust()) {
    spotify.setAccessToken(maybeToken.just());
  }
}

// String -> Maybe { limit: Maybe Number, offset: Maybe Number } ->
//    Promise (Either SpotifyError (Paging Track))
export function searchTracks(query: string, maybeOptions: any): Promise<Either<any, SpotifyApi.TrackSearchResponse>> {
  // TODO: Handle maybeOptions
  query = query.split(" ").join("%20");

  return new Promise(function (resolve: (result: Either<any, SpotifyApi.TrackSearchResponse>) => void) {
    if (accessToken.isNothing()) {
      resolve(Either.Left<any, any>({
        error: "invalid_token",
        error_description: "No access token exists"
      }));
    } else {
      spotify.searchTracks(query)
        .then(function (data: SpotifyApi.TrackSearchResponse) {
          resolve(Either.Right(data));
        }).catch(function (error: any) {
          resolve(Either.Left<any, any>(error));
        });
    }
  });
}
