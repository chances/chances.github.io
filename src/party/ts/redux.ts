import AllActions from './redux/actions'
import * as AllMiddleware from './redux/middleware'
import mainReducer from './redux/reducers'
import IState from './redux/state'

export { persistTransform } from './redux/state'
export { Action } from './redux/actions'
export { Reducer } from './redux/reducers'

export type State = IState

const Actions = AllActions
export default Actions

export const Middleware = AllMiddleware

export const partyApp = mainReducer
