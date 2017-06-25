import tape = require('tape')
import * as chai from 'chai'

let topic: tape.Test

export function test(name: string, cb: tape.TestCase) {
  tape(name, t => {
    topic = t
    cb(t)
  })
}

export function expect(assertion: Chai.Assertion) {
  if (topic == null) {
    throw new Error('Test has no topic subject')
  }

  try {
    topic.ok(assertion)
  } catch (e) {
    topic.fail(e.message)
  }
}

export const that = chai.expect
