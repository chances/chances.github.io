import { Dispatch, Middleware, Store } from 'redux'

import { Action } from './actions'
import { State } from './state'
import * as util from './util'

export const logger =
(store: Store<State>) => (next: Dispatch<State>) => (action: Action) => {
  util.log('dispatching', action)
  const result = next(action)
  util.log('Next state', store.getState().toJson())
  return result
}
