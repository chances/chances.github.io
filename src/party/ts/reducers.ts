import { Maybe } from 'monet'

import Actions from './actions'
import { Action } from './actions'
import { initialState, State } from './state'

export default function reducer(state: State = initialState, action: Action) {
  switch (action.type) {
    case Actions.ShowParty.type:
      state.party = Maybe.Just(action.payload as typeof Actions.ShowParty.payload)
      break

    default:
      return state
  }

  return state
}
