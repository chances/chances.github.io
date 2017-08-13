import { ActionCreator } from 'react-redux-typescript'
import { REHYDRATE } from 'redux-persist/constants'

import { Party, Request } from '../api'
import { JoinParty } from './middleware'
import State from './state'

function createAction<PayloadType>(name: string) {
  return new ActionCreator<typeof name, PayloadType>(name)
}

class RequestActionCreator<Ctor, PayloadType>
extends ActionCreator<string, PayloadType> {
  constructor(type: string, readonly request: Ctor) {
    super(type)
  }
}

function createRequestAction<For, Ctor>(name: string, request: Ctor) {
  return new RequestActionCreator<Ctor, For>(name, request)
}

const Actions = {
  Rehydrate: createAction<State>(REHYDRATE),
  JoinParty: createRequestAction<JoinParty, typeof JoinParty>('JoinParty', JoinParty),
  ShowParty: createAction<Party>('ShowParty'),
}

export default Actions

export type Action = typeof Actions[keyof typeof Actions]
