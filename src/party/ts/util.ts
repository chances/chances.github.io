
export default function _curry(f: (...args: any[]) => any) {
  return function (...args: any[]) {
    // var args = Array.prototype.slice.call(arguments, 0);
    return args.length < f.length
      ? _curry(args.reduce(
        function (g: () => any, arg: any) {
          return g.bind(null, arg)
        }, f)
      )
      : f.apply(null, args);
  }
}

export const log = _curry((message: string, value: any) => {
  console.log(message, value);
  return value;
})
