import { Either } from 'monet'
import Promise = require('bluebird')
import fetch = require('isomorphic-fetch')

let partyApiHost = 'https://party.chancesnow.me/'

const defaultOptions: RequestInit = {
  credentials: 'include',
}

export interface RequestParam {name: string, value: string}
export type ResponsePromise<T> = Promise<Either<any, T>>

export function get<T>(path: string, params?: RequestParam[]): ResponsePromise<T> {
  const url = partyApiHost + path +
    (params ? joinParams(params) : '')

  return new Promise(resolve => {
    fetch(new Request(url, defaultOptions)).then(response => {
      if (!response.ok) {
        response.json().then(data => {
          resolve(Either.Left('error') as Either<any, T>)
        })
      }

      response.json().then(data => {
        resolve(Either.Right(data))
      })
    })
  }) as ResponsePromise<T>
}

export function setPartyApiHost(host: string) {
  partyApiHost = host.endsWith('/')
    ? host
    : host + '/'
}

function joinParams(params: RequestParam[]): string {
  return params.map(param => {
    return encodeURIComponent(param.name) + '=' + encodeURIComponent(param.value)
  }).join('&')
}
