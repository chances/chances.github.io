import test = require('tape')

import { Maybe } from 'monet'
import { expectation, where } from './lib/expect'

import { EitherTrackResults, Options, searchTracks } from '../spotify'

test('searchTracks fails when access token is nothing', t => {
  searchTracks('Sugar Ray', Maybe.Nothing<Options>())
    .then(eitherResultsOrErr => {
      expectation(t, () => where(eitherResultsOrErr.isLeft()).is.true)
      expectation(t, () => where(eitherResultsOrErr.left().error).equals('invalid_token'))

      t.end()
    })
})
