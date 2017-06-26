import { ActionCreator } from 'react-redux-typescript'

import { JoinParty, Party } from '../api/party'
import { RequestStatus } from '../api/request'

function createAction<PayloadType>(name: string) {
  return new ActionCreator<typeof name, PayloadType>(name)
}

const Actions = {
  JoinParty: createAction<JoinParty>('JoinParty'),
  ShowParty: createAction<Party>('ShowParty'),
}

export default Actions

export type Action = typeof Actions[keyof typeof Actions]
