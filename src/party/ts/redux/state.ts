import { createTransform } from 'redux-persist'
import Immutable = require('immutable')
import { Maybe } from 'monet'

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

const mutator = Immutable.Record(initialState)
let record: Immutable.Map<string, any> = mutator()

export function replace(state: State): State {
  record = new mutator(state)
  return record.toJS()
}

// Pseudo-mutation helpers

type StateKeys = keyof State
type StateValues = typeof initialState[keyof typeof initialState]

type MutateFunctions = {
  [F in StateKeys]: (value: State[F]) => State
}
interface MutatorFunction {
  [propName: string]: (value: any) => State
}

function generateMutators(): MutateFunctions {
  const mutatorObject: MutatorFunction = {}
  Object.keys(initialState).map((key: StateKeys) => {
    mutatorObject[key] = (value: any) => mutateKey(record.toJS(), key, value)
  })
  return mutatorObject as MutateFunctions
}
export const mutate = generateMutators()

// export const mutate: MutateFunctions = {
//   firstLaunch: curry(
//     (state: State, value: boolean) => mutateKey(state, 'firstLaunch', value),
//   )(record.toJS()),
//   joining: curry(
//     (state: State, value: Maybe<JoinParty>) => mutateKey(state, 'joining', value),
//   )(record.toJS()),
//   party: curry(
//     (state: State, value: Maybe<JoinParty>) => mutateKey(state, 'party', value),
//   )(record.toJS()),
// }
function mutateKey(state: State, key: StateKeys, value: StateValues): State {
  state[key] = value
  record = new mutator(state)
  return record.toJS()
}

export const persistTransform = createTransform<State, State>(
  (stateToSerialize, key) => stateToSerialize,
  (serializedState: State, key) => serializedState,
  {
    blacklist: ['joining'],
  },
)
