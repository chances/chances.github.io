import { Dispatch, Middleware, Store } from 'redux'

import * as util from '../util'
import { Action } from './actions'
import { State } from './state'

export const logger =
(store: Store<State>) => (next: Dispatch<State>) => (action: Action) => {
  util.log('dispatching', action)
  const result = next(action)
  util.log('Next state', store.getState().toJson())
  return result
}
