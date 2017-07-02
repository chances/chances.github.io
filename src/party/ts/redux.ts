import AllActions from './redux/actions'
import allMiddleware from './redux/middleware'
import mainReducer from './redux/reducers'
import IState from './redux/state'

export { persistTransform } from './redux/state'
export { Action } from './redux/actions'
export { Reducer } from './redux/reducers'

export const middleware = allMiddleware

export type State = IState
export const Actions = AllActions
export const partyApp = mainReducer
