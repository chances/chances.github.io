import { Dispatch, Middleware, Store } from 'redux'

import { Action } from '../actions'
import State from '../state'

type AsyncActionHandler = (store: Store<State>, action: Action) => void

export function async(asyncHandler: AsyncActionHandler): Middleware {
  return (store: Store<State>) => (next: Dispatch<State>) => (action: Action) => {
    asyncHandler(store, action)

    next(action)
  }
}
