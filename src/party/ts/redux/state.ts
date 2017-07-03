import { Either, Maybe } from 'monet'
import { createTransform } from 'redux-persist'

import { Party } from '../api/party'
import curry from '../util'
import Actions from './actions'
import { JoinParty } from './middleware'

export const initialState = {
  firstLaunch: true,
  joining: Maybe.Nothing<JoinParty>(),
  party: Maybe.Nothing<Party>(),
}

type State = typeof initialState
export default State

type StateKeys = keyof State
type StateValues = State[keyof State]

// Monet wrapper

function paintWithMonet(key: StateKeys, state: any): StateValues {
  switch (key) {
    case 'joining':
    case 'party':
      return Maybe.fromNull(state)
    default:
      return state as StateValues
  }
}

function paintWithMonetInverse(key: StateKeys, state: StateValues): any {
  switch (key) {
    case 'joining':
    case 'party':
      return (state as Maybe<any>).orNull()
    default:
      return state
  }
}

// redux-persist serializer for monet

export const persistTransform = createTransform<StateValues, any>(
  (state, key: StateKeys) => paintWithMonetInverse(key, state),
  (serializedState, key: StateKeys) => paintWithMonet(key, serializedState),
  {
    blacklist: ['joining'],
  },
)
