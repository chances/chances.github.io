import test = require('tape')
import { expect } from 'chai'

export const where = expect

export function expectation(
  t: test.Test,
  expectation: () => Chai.Assertion,
) {
  try {
    t.ok(expectation())
  } catch (e) {
    t.fail(e.message)
  }
}
