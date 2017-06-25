import { Maybe } from 'monet'

import { get, ResponsePromise } from './request'
import { Track } from './track'

export type PartyCode = string

export interface Party {
  location: PartyLocation,
  room_code: PartyCode,
  ended: boolean,
  current_track?: Track,
}

export interface PartyLocation {
  host_name: string
}

export function getParty(): ResponsePromise<Party> {
  return get<Party>('party')
}
