declare module 'haskind' {
  export const Data: {
    Maybe: Haskind.Maybe<any>
    Either: Haskind.Either
  }
}

declare namespace Haskind {
  namespace Data {
    type Maybe = _Just<any> | _Nothing;
    type _Just<T> = {
      (val: T): Haskind.Maybe<T>
      just: T
    }
    type _Nothing = {
      (): Haskind.Maybe<any>
      nothing: void
    }
  }

  interface Maybe<T> {
    Just: Data._Just<any>
    Nothing: Data._Nothing

    isJust(m: Maybe<T>): boolean
    isNothing(m: Maybe<T>): boolean
    fromJust(m: Maybe<T>): T
    fromJust<V>(m: Maybe<V>): V
    fromMaybe(def: T, m: Maybe<T>): T
    fromMaybe<V>(def: V, m: Maybe<V>): V
  }

  interface Either {
    Left: {
      (val: any): Haskind.Either
      left: any
    }
    Right: {
      (val: any): Haskind.Either
      right: any
    }

    either<T>(lfn: (a: any) => T, rfn: (a: any) => T, either: Either): T
    lefts<T>(eithers: Either[]): T[]
    rights<T>(eithers: Either[]): T[]
    isLeft(either: Either): boolean
    isRight(either: Either): boolean
    partitionEithers(eithers: Either[]): [any[], any[]]
  }
}
