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

let record: State = initialState

export function replace(state: State): State {
  return record = state
}

// Monet wrapper

export interface SerializedState {
  firstLaunch: boolean
  joining: JoinParty | null
  party: Party | null
}

function paintWithMonet(state: SerializedState): State {
  return {
    firstLaunch: state.firstLaunch,
    joining: Maybe.fromNull(state.joining),
    party: Maybe.fromNull(state.party),
  }
}

function paintWithMonetInverse(state: State): SerializedState {
  return {
    firstLaunch: state.firstLaunch,
    joining: state.joining.orNull(),
    party: state.party.orNull(),
  }
}

// Pseudo-mutation helpers

type StateKeys = keyof State
type StateValues = State[keyof State]

type MutateFunctions = {
  [F in StateKeys]: (value: State[F]) => State
}
interface MutatorFunction {
  [propName: string]: (value: any) => State
}

function generateMutators(): MutateFunctions {
  const mutatorObject: MutatorFunction = {}
  const clone = {...record}
  Object.keys(initialState).map((key: StateKeys) => {
    mutatorObject[key] = (value: any) => mutateKey(clone, key, value)
  })
  return mutatorObject as MutateFunctions
}
export const mutate = generateMutators()

function mutateKey(state: State, key: StateKeys, value: StateValues): State {
  state[key] = value
  return record = state
}

export const persistTransform = createTransform<State, SerializedState>(
  (state, key) => paintWithMonetInverse(state),
  (serializedState, key) => paintWithMonet(serializedState),
  {
    blacklist: ['joining'],
  },
)
