import { expect, skip, test, that } from './lib/expect'

import { Maybe } from 'monet'
import { EitherTrackResults, Options, searchTracks, updateAccessToken } from '../spotify'

test('searchTracks fails when access token is nothing', t => {
  searchTracks('Sugar Ray', Maybe.Nothing<Options>())
    .then(eitherResultsOrErr => {
      expect(that(eitherResultsOrErr.isLeft()).is.true)
      expect(that(eitherResultsOrErr.left().error).equals('invalid_token'))

      t.end()
    })
})

test('searchTracks fails when access token is set to nothing', t => {
  updateAccessToken(Maybe.Nothing<string>())

  searchTracks('Sugar Ray', Maybe.Nothing<Options>())
    .then(eitherResultsOrErr => {
      expect(that(eitherResultsOrErr.isLeft()).is.true)
      expect(that(eitherResultsOrErr.left().error).equals('invalid_token'))

      t.end()
    })
})

skip('searchTracks fails when access token is bad', t => {
  updateAccessToken(Maybe.Just('bad'))

  searchTracks('Sugar Ray', Maybe.Nothing<Options>())
    .then(eitherResultsOrErr => {
      expect(that(eitherResultsOrErr.isLeft()).is.true)
      expect(that(eitherResultsOrErr.left().error).equals('invalid_token'))

      t.end()
    })
})
