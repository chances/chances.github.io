import { Either } from 'monet'
import Promise = require('bluebird')
import fetch = require('isomorphic-fetch')

import Errors from './request/errors'
import { ErrorType } from './request/errors'
import { ApiResponse, joinParams, RequestParam, ResponsePromise } from './request/primitives'
import * as p from './request/primitives'

Promise.config({
  cancellation: true,
})

let partyApiHost = 'https://party.chancesnow.me/'

export function setPartyApiHost(host: string) {
  partyApiHost = host.endsWith('/')
    ? host
    : host + '/'
}

const defaultOptions: RequestInit = {
  credentials: 'include',
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

  return new Promise<p.Response<T>>(resolve => {
    fetch(new Request(url, options)).then(response => {
      response.json().then((data: ApiResponse<T>) => {
        if (response.ok && !isErrorResponse(data)) {
          resolve(Either.Right(data.data) as p.Response<T>)
        } else if (isErrorResponse(data)) {
          resolve(Either.Left(new Errors(
            response.status,
            data.errors,
          )) as p.Response<T>)
        } else {
          resolve(Either.Left(new Errors(
            response.status,
            [ Errors.create(
              'Request Error',
              'Party received an invalid response',
            ) ],
          )) as p.Response<T>)
        }
      })
    }).catch(e => {
      resolve(Either.Left(new Errors(
        ErrorType.NULL_ERROR,
        [ Errors.defaultError ],
      )) as p.Response<T>)
    })
  })
}

function isErrorResponse<T>(responseData: ApiResponse<T>): responseData is p.ErrorResponse {
  return (responseData as p.ErrorResponse).errors !== undefined
}
