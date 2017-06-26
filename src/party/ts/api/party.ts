import { Maybe } from 'monet'

import { get, RequestStatus, Response, ResponsePromise } from './request'
import { Track } from './track'

export type PartyCode = string

export interface Party {
  location: PartyLocation,
  room_code: PartyCode,
  ended: boolean,
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

export function getParty(): ResponsePromise<Party> {
  return get<Party>('party')
}
