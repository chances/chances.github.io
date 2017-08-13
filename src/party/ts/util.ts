
export default function _curry(f: (...args: any[]) => any) {
  return (...args: any[]) => {
    return args.length < f.length
      ? _curry(
        args.reduce(
        (g: () => any, arg: any) => {
          return g.bind(null, arg)
        }, f),
      )
      : f.apply(null, args)
  }
}

export const log = _curry((message: string, value: any) => {
  if (process.env.NODE_ENV === 'development') {
    // tslint:disable-next-line:no-console
    console.log(message, value)
  }
  return value
})
