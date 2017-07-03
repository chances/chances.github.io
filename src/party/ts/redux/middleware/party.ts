import { Either, Maybe } from 'monet'
import { Dispatch, Middleware, Store } from 'redux'

import { Errors, Party, post, Request, Response, ResponsePromise, Track } from '../../api'

import Actions from '../actions'
import { Action } from '../actions'
import State from '../state'

import { async } from './util'

export class JoinParty extends Request<Party> {
  constructor(public partyCode: string, response?: Response<Party>) {
    super(response)
  }
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
    payload.response.cata(
      () => {
        if (joinPartyPromise != null) {
          joinPartyPromise.cancel()
        }

        joinPartyPromise = getParty(payload.partyCode)
        joinPartyPromise.then(eitherParty => {
          store.dispatch(Actions.JoinParty.create(
            new JoinParty(
              payload.partyCode,
              eitherParty.leftMap(errors => {
                // Friendly Party not found error message
                if (errors.responseStatus === 404) {
                  errors.errors = [Errors.create(
                    'Not Found',
                    `Party ${payload.partyCode} not found`,
                  )]
                }
                return errors
              }),
            ),
          ))
        })
      },
      eitherParty => {
        eitherParty.toMaybe()
          .map(partyResource => {
            store.dispatch(Actions.ShowParty.create(partyResource.attributes))
          })
      },
    )
  }
})
