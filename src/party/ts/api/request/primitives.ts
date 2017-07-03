import { Either, Maybe } from 'monet'
import Promise = require('bluebird')

import Errors from './errors'
import { Error, nullErrorToNothing } from './errors'

export interface RequestParam {name: string, value: string}
export class Request<T> {
  static complete<T>(response: Response<T>) {
    return new Request(true, response)
  }

  response: Maybe<Response<T>>

  constructor(private completed: boolean = false, response?: Response<T>) {
    this.response = response
      ? Maybe.Just(response)
      : Maybe.Nothing<Response<T>>()
  }

  get isLoading(): boolean {
    return !this.completed
  }

  get isCompleted(): boolean {
    return this.completed && this.response.isJust()
  }

  get error(): Maybe<Error> {
    return this.response
    .map(eitherResponse => {
      return eitherResponse.cata<Error>(
        errorResponse => {
          return errorResponse.errors
            .filter(error => error.status === errorResponse.responseStatus)[0] ||
            errorResponse.errors[errorResponse.errors.length - 1] ||
            Errors.defaultError
        },
        response => Errors.nullError,
      )
    })
    .flatMap(nullErrorToNothing)
  }
}

export type ApiResponse<T> = DataResponse<T> | ErrorResponse
// JSON API Specification - Document Structure
// http://jsonapi.org/format/1.0/#document-structure
export interface DataResponse<T> {
  data: Data<T>
}
export interface Data<T> {
  id: string
  type: string
  attributes: T
  links: {
    self: string,
  }
}
// JSON API Specification - Errors
// http://jsonapi.org/format/1.0/#errors
export interface ErrorResponse {
  errors: Error[]
}

export type Response<T> = Either<Errors, Data<T>>
export type ResponsePromise<T> = Promise<Response<T>>

export function joinParams(params: RequestParam[]): string {
  return params.map(param => {
    return encodeURIComponent(param.name) + '=' + encodeURIComponent(param.value)
  }).join('&')
}
