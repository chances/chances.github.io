import { Maybe } from 'monet'
import { expect, test, that } from './lib/expect'

import { EitherTrackResults, Options, searchTracks } from '../spotify'

test('searchTracks fails when access token is nothing', t => {
  searchTracks('Sugar Ray', Maybe.Nothing<Options>())
    .then(eitherResultsOrErr => {
      expect(that(eitherResultsOrErr.isLeft()).is.true)
      expect(that(eitherResultsOrErr.left().error).equals('invalid_token'))

      t.end()
    })
})
