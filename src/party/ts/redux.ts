import AllActions from './redux/actions'
import * as AllMiddleware from './redux/middleware'
import mainReducer from './redux/reducers'

export { Action } from './redux/actions'
export { Reducer } from './redux/reducers'
export { State } from './redux/state'

const Actions = AllActions
export default Actions

export const Middleware = AllMiddleware

export const partyApp = mainReducer
