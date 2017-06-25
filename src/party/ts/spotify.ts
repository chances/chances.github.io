import Promise = require('bluebird')
import Spotify = require('spotify-web-api-js')

import { Either, Maybe } from 'monet'

import * as util from './util'

const spotify = new Spotify()
spotify.setPromiseImplementation(Promise)

let accessToken: Maybe<string> = Maybe.Nothing<string>()

// Maybe TokenString -> Void
export function updateAccessToken(maybeToken: Maybe<string>) {
  accessToken = maybeToken

  if (maybeToken.isJust()) {
    spotify.setAccessToken(maybeToken.just())
  }
}

type EitherTrackResults = Either<any, SpotifyApi.TrackSearchResponse>

// String -> Maybe { limit: Maybe Number, offset: Maybe Number } ->
//    Promise (Either SpotifyError (Paging Track))
export function searchTracks(
  query: string,
  maybeOptions: Maybe<{limit: Maybe<number>, offset: Maybe<number>}>,
) {
  // TODO: Handle maybeOptions
  query = query.split(' ').join('%20')

  return new Promise<EitherTrackResults>(resolve => {
    if (accessToken.isNothing()) {
      resolve(Either.Left<any, SpotifyApi.TrackSearchResponse>({
        error: 'invalid_token',
        error_description: 'No access token exists',
      }))
    } else {
      const results = spotify.searchTracks(query)
        .then(data => resolve(Either.Right(data)))
        .catch(error => resolve(Either.Left(error) as EitherTrackResults))

      // const results = spotify.searchTracks(query)
      //   .then(data => resolve(Either.Right<any, SpotifyApi.TrackSearchResponse>(data)))
      //   .catch(error: any => resolve(Either.Left<any, SpotifyApi.TrackSearchResponse>(error)))

      resolve(results as PromiseLike<EitherTrackResults>)
    }
  })
}
