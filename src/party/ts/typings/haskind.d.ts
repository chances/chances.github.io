declare module 'haskind' {
  export const Data: {
    Maybe: Haskind.Maybe
    Either: Haskind.Either
  }
}

declare namespace Haskind {
  namespace Data {
    type Maybe = _Just<any> | _Nothing;
    type _Just<T> = {
      (val: T): Haskind.Maybe
      just: T
    }
    type _Nothing = {
      (): Haskind.Maybe
      nothing: void
    }
  }

  interface Maybe {
    Just: Data._Just<any>
    Nothing: Data._Nothing

    isJust(m: Maybe): boolean
    isNothing(m: Maybe): boolean
    fromJust<T>(m: Maybe): T
    fromMaybe<T>(def: T, m: Maybe): T
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
