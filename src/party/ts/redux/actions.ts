import { ActionCreator } from 'react-redux-typescript'
import { REHYDRATE } from 'redux-persist/constants'

import { JoinParty, Party } from '../api'
import State from './state'

function createAction<PayloadType>(name: string) {
  return new ActionCreator<typeof name, PayloadType>(name)
}

const Actions = {
  Rehydrate: createAction<State>(REHYDRATE),
  JoinParty: createAction<JoinParty>('JoinParty'),
  ShowParty: createAction<Party>('ShowParty'),
}

export default Actions

export type Action = typeof Actions[keyof typeof Actions]
