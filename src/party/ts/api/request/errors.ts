import { Maybe } from 'monet'

export enum ErrorType {
  UNKNOWN_ERROR,
  NULL_ERROR,
  ERROR,
}

const defaultError = createError('Request Error', 'Party could not complete the request')
const nullError: Error = {
  ...defaultError,
  status: ErrorType.NULL_ERROR,
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

export default class Errors {
  static create = createError
  static defaultError = defaultError
  static nullError = nullError

  constructor(public responseStatus: ErrorType | number, public errors: Error[]) {}
}

export function nullErrorToNothing(err: Error): Maybe<Error> {
  return err.status !== ErrorType.NULL_ERROR
    ? Maybe.Just(err)
    : Maybe.Nothing<Error>()
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
