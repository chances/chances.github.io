"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function _curry(f) {
    return function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
        }
        // var args = Array.prototype.slice.call(arguments, 0);
        return args.length < f.length
            ? _curry(args.reduce(function (g, arg) {
                return g.bind(null, arg);
            }, f))
            : f.apply(null, args);
    };
}
exports.default = _curry;
exports.log = _curry(function (message, value) {
    console.log(message, value);
    return value;
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidXRpbC5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uL3RzL3V0aWwudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6Ijs7QUFDQSxnQkFBK0IsQ0FBMEI7SUFDdkQsTUFBTSxDQUFDO1FBQVUsY0FBYzthQUFkLFVBQWMsRUFBZCxxQkFBYyxFQUFkLElBQWM7WUFBZCx5QkFBYzs7UUFDN0IsdURBQXVEO1FBQ3ZELE1BQU0sQ0FBQyxJQUFJLENBQUMsTUFBTSxHQUFHLENBQUMsQ0FBQyxNQUFNO2NBQ3pCLE1BQU0sQ0FBQyxJQUFJLENBQUMsTUFBTSxDQUNsQixVQUFVLENBQVksRUFBRSxHQUFRO2dCQUM5QixNQUFNLENBQUMsQ0FBQyxDQUFDLElBQUksQ0FBQyxJQUFJLEVBQUUsR0FBRyxDQUFDLENBQUE7WUFDMUIsQ0FBQyxFQUFFLENBQUMsQ0FBQyxDQUNOO2NBQ0MsQ0FBQyxDQUFDLEtBQUssQ0FBQyxJQUFJLEVBQUUsSUFBSSxDQUFDLENBQUM7SUFDMUIsQ0FBQyxDQUFBO0FBQ0gsQ0FBQztBQVhELHlCQVdDO0FBRVksUUFBQSxHQUFHLEdBQUcsTUFBTSxDQUFDLFVBQUMsT0FBZSxFQUFFLEtBQVU7SUFDcEQsT0FBTyxDQUFDLEdBQUcsQ0FBQyxPQUFPLEVBQUUsS0FBSyxDQUFDLENBQUM7SUFDNUIsTUFBTSxDQUFDLEtBQUssQ0FBQztBQUNmLENBQUMsQ0FBQyxDQUFBIn0=