import { expect, test, that } from './lib/expect'

import { log } from '../util'

test('log returns logged value', t => {
  const value = 'foobar'

  expect(that(log('foo', value)).equals('foobar'))

  t.end()
})

test('log is curryable', t => {
  const fooLogger = log('foo')

  expect(that(fooLogger).is.an.instanceOf(Function))
  expect(that(fooLogger('foobar')).equals('foobar'))

  t.end()
})
