import { Either, Maybe } from 'monet'
import { Dispatch, Middleware, Store } from 'redux'

import { Party, post, Request, Response, ResponsePromise, Track } from '../../api'

import Actions from '../actions'
import { Action } from '../actions'
import State from '../state'

import { async } from './util'

export interface JoinParty extends Request<Party> {
  partyCode: string
}

export function getParty(partyCode: string): ResponsePromise<Party> {
  return post<Party>('party/join', {
    data: {room_code: partyCode},
  })
}

let joinPartyPromise: ResponsePromise<Party>

export const joinParty = async((store, action) => {
  if (action.type === Actions.JoinParty.type) {
    const payload = action.payload as JoinParty
    if (payload.response.isNothing()) {
      if (joinPartyPromise != null) {
        joinPartyPromise.cancel()
      }

      joinPartyPromise = getParty(payload.partyCode)
      joinPartyPromise.then(eitherParty => {
        const requestResponse = Request.complete<Party>(eitherParty) as JoinParty
        requestResponse.partyCode = payload.partyCode

        store.dispatch(Actions.JoinParty.create(requestResponse))
      })
    } else {
      payload.response
        .flatMap(eitherResponse => eitherResponse.toMaybe())
        .map(party => {
          store.dispatch(Actions.ShowParty.create(party))
        })
    }
  }
})
