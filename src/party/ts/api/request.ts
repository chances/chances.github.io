import { Either } from 'monet'
import Promise = require('bluebird')
import fetch = require('isomorphic-fetch')

Promise.config({
  cancellation: true,
})

let partyApiHost = 'https://party.chancesnow.me/'

const defaultOptions: RequestInit = {
  credentials: 'include',
}

export interface RequestParam {name: string, value: string}
export type Response<T> = Either<Errors, T>
export type ResponsePromise<T> = Promise<Response<T>>
export enum RequestStatus {
  LOADING,
  COMPLETED,
}

export interface Error {
  status: number
  title: string
  detail: string
  meta: {
    cause: string | null,
    details: string | null,
  }
}
export interface Errors {
  responseStatus: number | null
  errors: Error[]
}

export function get<T>(path: string, params?: RequestParam[]): ResponsePromise<T> {
  return request<T>('get', path, undefined, params)
}

export function post<T>(path: string, body?: {data: any},
                        params?: RequestParam[]): ResponsePromise<T> {
  return request<T>('post', path, body, params)
}

function request<T>(method: 'get' | 'post', path: string, body?: {data: any},
                    params?: RequestParam[]): ResponsePromise<T> {
  const url = partyApiHost + path +
    (params ? joinParams(params) : '')
  const headers = body != null
    ? {'Content-Type': 'application/json'}
    : undefined
  const options: RequestInit = {
    ...defaultOptions,
    method,
    headers,
    body: body ? JSON.stringify(body) : undefined,
  }

  return new Promise(resolve => {
    fetch(new Request(url, options)).then(response => {
      response.json().then(data => {
        const result = response.ok
          ? Either.Right(data.data)
          : Either.Left<Errors>({
            responseStatus: response.status,
            ...data,
          })
        resolve(result)
      })
    }).catch(e => {
      resolve(Either.Left<Errors>({
        responseStatus: null,
        errors: [
          createError('Request Error', 'Party could not complete the request'),
        ],
      }))
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

function createError(title: string, detail: string, cause?: string, details?: string): Error {
  return {
    status: -1,
    title,
    detail,
    meta: {
      cause: cause ? cause : null,
      details: details ? details : null,
    },
  }
}
