import { ActionCreator } from 'react-redux-typescript'

import { Party, PartyCode } from './api/party'

function createAction<PayloadType>(name: string) {
  return new ActionCreator<typeof name, PayloadType>(name)
}

const Actions = {
  JoinParty: createAction<PartyCode>('JoinParty'),
  ShowParty: createAction<Party>('ShowParty'),
}

export default Actions

export type Action = typeof Actions[keyof typeof Actions]
