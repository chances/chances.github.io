import { Maybe } from 'monet'

import * as util from '../util'
import Actions from './actions'
import { Action } from './actions'

import IState from './state'
import { initialState } from './state'
import * as State from './state'

export type Reducer = (state: IState, action: Action) => IState | void
const reducers: Reducer[] = []

type Rehydrate = typeof Actions.Rehydrate.payload
type ShowParty = typeof Actions.ShowParty.payload
type JoinParty = typeof Actions.JoinParty.payload

export default function reducer(state = initialState, action: Action) {
  switch (action.type) {
    case Actions.Rehydrate.type:
      const payload = action.payload as Rehydrate
      return payload.firstLaunch == null
        ? state
        : {
          ...state,
          firstLaunch: payload.firstLaunch,
          party: payload.party,
        }

    case Actions.ShowParty.type:
      return {
        ...state,
        firstLaunch: false,
        joining: Maybe.Nothing<JoinParty>(),
        party: Maybe.Just(action.payload as ShowParty),
      }

    case Actions.JoinParty.type:
      return {
        ...state,
        joining: Maybe.Just(action.payload as JoinParty),
      }

    default:
      return state
  }
}
