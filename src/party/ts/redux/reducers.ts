import { Maybe } from 'monet'

import Actions from './actions'
import { Action } from './actions'
import { initialState, State } from './state'

export type Reducer = (state: State, action: Action) => State | void
const reducers: Reducer[] = []

type ShowParty = typeof Actions.ShowParty.payload
type JoinParty = typeof Actions.JoinParty.payload

export default function reducer(state: State = initialState, action: Action) {
  switch (action.type) {
    case Actions.ShowParty.type:
      return state.mutate.party(Maybe.Just(action.payload as ShowParty))

    case Actions.JoinParty.type:
      return state.mutate.joining(Maybe.Just(action.payload as JoinParty))

    default:
      break
  }

  return state
}
