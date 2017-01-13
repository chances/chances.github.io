
let _curry = function (f) {
  return function () {
    var args = Array.prototype.slice.call(arguments, 0);
    return args.length < f.length ?
      _curry(args.reduce(function (g, arg) {return g.bind(null, arg)}, f)) :
      f.apply(null, args);
  }
};

module.exports = {
  _curry: _curry
}
