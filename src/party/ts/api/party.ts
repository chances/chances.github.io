import { Either, Maybe } from 'monet'
import { Dispatch, Middleware, Store } from 'redux'

import { post, RequestStatus, Response, ResponsePromise } from './request'
import { Track } from './track'

import { Action, Actions, State } from '../redux'

export type PartyCode = string

export interface Party {
  location: PartyLocation,
  room_code: PartyCode,
  ended: boolean,
  guests: string[],
  current_track?: Track,
}

interface PartyLocation {
  host_name: string
}

export interface JoinParty {
  partyCode: PartyCode,
  status: RequestStatus,
  response?: Response<Party>,
}

export function getParty(partyCode: PartyCode): ResponsePromise<Party> {
  return post<Party>('party/join', {
    data: {room_code: partyCode},
  })
}

let joinPartyPromise: ResponsePromise<Party>
export const joinParty: Middleware =
(store: Store<State>) => (next: Dispatch<State>) => (action: Action) => {
  if (action.type === Actions.JoinParty.type) {
    const payload = action.payload as typeof Actions.JoinParty.payload
    const shouldNotJoinParty = payload.response != null
    if (shouldNotJoinParty) {
      return
    }

    if (joinPartyPromise != null) {
      joinPartyPromise.cancel()
    }

    joinPartyPromise = getParty(payload.partyCode)
    joinPartyPromise.then(eitherParty => {
      store.dispatch(Actions.JoinParty.create({
        partyCode: payload.partyCode,
        status: RequestStatus.COMPLETED,
        response: eitherParty,
      }))
    })
  }
}
