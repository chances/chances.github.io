import { Either, Maybe } from 'monet'
import { Dispatch, Middleware, Store } from 'redux'

import { post } from './request'
import { Request, Response, ResponsePromise } from './request/primitives'
import { Track } from './track'

import { Action, Actions, State } from '../redux'

export interface Party {
  location: PartyLocation,
  room_code: string,
  ended: boolean,
  guests: string[],
  current_track?: Track,
}

interface PartyLocation {
  host_name: string
}

export interface JoinParty extends Request<Party> {
  partyCode: string
}

export function getParty(partyCode: string): ResponsePromise<Party> {
  return post<Party>('party/join', {
    data: {room_code: partyCode},
  })
}

let joinPartyPromise: ResponsePromise<Party>
export const joinParty: Middleware =
(store: Store<State>) => (next: Dispatch<State>) => (action: Action) => {
  if (action.type === Actions.JoinParty.type) {
    const payload = action.payload as typeof Actions.JoinParty.payload
    if (payload.response.isJust()) {
      return
    }

    if (joinPartyPromise != null) {
      joinPartyPromise.cancel()
    }

    joinPartyPromise = getParty(payload.partyCode)
    joinPartyPromise.then(eitherParty => {
      const requestResponse = Request.complete<Party>(eitherParty) as JoinParty
      requestResponse.partyCode = payload.partyCode

      store.dispatch(Actions.JoinParty.create(requestResponse))
    })
  }
}
