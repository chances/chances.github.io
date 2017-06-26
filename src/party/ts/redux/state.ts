import { createTransform } from 'redux-persist'
import Immutable = require('immutable')
import { Maybe } from 'monet'

import { JoinParty, Party } from '../api/party'
import curry from '../util'

const initial = {
  firstLaunch: true,
  joining: Maybe.Nothing<JoinParty>(),
  party: Maybe.Nothing<Party>(),
}

type IState = typeof initial
export default IState

type StateKeys = keyof IState
type StateValues = typeof initial[keyof typeof initial]

type MutateFunctions = {
  [F in StateKeys]: (value: IState[F]) => IState
}

export class State implements IState {
  private static mutator = Immutable.Record(initial)
  private record: Immutable.Map<string, any>

  constructor(state?: IState) {
    this.record =
      state === undefined
        ? State.mutator()
        : State.mutator(state)
  }

  get firstLaunch(): boolean {
    return this.record.get('firstLaunch')
  }

  get joining(): Maybe<JoinParty> {
    return this.record.get('joining')
  }

  get party(): Maybe<Party> {
    return this.record.get('party')
  }

  get mutate(): MutateFunctions {
    return {
      firstLaunch: curry(
        (state: State, value: boolean) => mutate(state, 'firstLaunch', value),
      )(this),
      joining: curry(
        (state: State, value: Maybe<JoinParty>) => mutate(state, 'joining', value),
      )(this),
      party: curry(
        (state: State, value: Maybe<JoinParty>) => mutate(state, 'party', value),
      )(this),
    }
  }

  toJson(): IState {
    return this.record.toJS()
  }
}

function mutate(state: State, key: StateKeys, value: StateValues): IState {
  const s = state.toJson()
  s[key] = value
  return new State(s)
}

export const initialState = new State()

export const persistTransform = createTransform<State, IState>(
  (stateToSerialize, key) => stateToSerialize,
  (serializedState: IState, key) => new State(serializedState),
  {
    blacklist: ['joining'],
  },
)
