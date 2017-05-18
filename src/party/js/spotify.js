"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var Promise = require("bluebird");
var Spotify = require("spotify-web-api-js");
var haskind_1 = require("haskind");
var Maybe = haskind_1.Data.Maybe, Either = haskind_1.Data.Either;
var Nothing = Maybe.Nothing;
var spotify = new Spotify();
spotify.setPromiseImplementation(Promise);
var accessToken = Nothing();
// Maybe TokenString -> Void
function updateAccessToken(maybeToken) {
    accessToken = Maybe.fromMaybe(Nothing(), maybeToken);
    if (Maybe.isJust(maybeToken)) {
        spotify.setAccessToken(Maybe.fromJust(maybeToken));
    }
}
exports.updateAccessToken = updateAccessToken;
// String -> Maybe { limit: Maybe Number, offset: Maybe Number } ->
//    Promise (Either SpotifyError (Paging Track))
function searchTracks(query, maybeOptions) {
    // TODO: Handle maybeOptions
    query = query.split(" ").join("%20");
    return new Promise(function (resolve) {
        if (Maybe.isNothing(accessToken)) {
            resolve(Either.Left({
                error: "invalid_token",
                error_description: "No access token exists"
            }));
        }
        else {
            spotify.searchTracks(query)
                .then(function (data) {
                resolve(Either.Right(data));
            }).catch(function (error) {
                resolve(Either.Left(error));
            });
        }
    });
}
exports.searchTracks = searchTracks;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic3BvdGlmeS5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uL3RzL3Nwb3RpZnkudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6Ijs7QUFBQSxrQ0FBcUM7QUFDckMsNENBQStDO0FBRS9DLG1DQUErQjtBQUN2QixJQUFBLDRCQUFLLEVBQUUsOEJBQU0sQ0FBVTtBQUMvQixJQUFNLE9BQU8sR0FBRyxLQUFLLENBQUMsT0FBTyxDQUFDO0FBSTlCLElBQU0sT0FBTyxHQUFHLElBQUksT0FBTyxFQUFFLENBQUM7QUFDOUIsT0FBTyxDQUFDLHdCQUF3QixDQUFDLE9BQU8sQ0FBQyxDQUFDO0FBRTFDLElBQUksV0FBVyxHQUFHLE9BQU8sRUFBRSxDQUFDO0FBRTVCLDRCQUE0QjtBQUM1QiwyQkFBa0MsVUFBeUI7SUFDekQsV0FBVyxHQUFHLEtBQUssQ0FBQyxTQUFTLENBQUMsT0FBTyxFQUFFLEVBQUUsVUFBVSxDQUFDLENBQUM7SUFFckQsRUFBRSxDQUFDLENBQUMsS0FBSyxDQUFDLE1BQU0sQ0FBQyxVQUFVLENBQUMsQ0FBQyxDQUFDLENBQUM7UUFDN0IsT0FBTyxDQUFDLGNBQWMsQ0FBQyxLQUFLLENBQUMsUUFBUSxDQUFTLFVBQVUsQ0FBQyxDQUFDLENBQUM7SUFDN0QsQ0FBQztBQUNILENBQUM7QUFORCw4Q0FNQztBQUVELG1FQUFtRTtBQUNuRSxrREFBa0Q7QUFDbEQsc0JBQTZCLEtBQWEsRUFBRSxZQUFpQjtJQUMzRCw0QkFBNEI7SUFDNUIsS0FBSyxHQUFHLEtBQUssQ0FBQyxLQUFLLENBQUMsR0FBRyxDQUFDLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyxDQUFDO0lBRXJDLE1BQU0sQ0FBQyxJQUFJLE9BQU8sQ0FBQyxVQUFVLE9BQXdDO1FBQ25FLEVBQUUsQ0FBQyxDQUFDLEtBQUssQ0FBQyxTQUFTLENBQUMsV0FBVyxDQUFDLENBQUMsQ0FBQyxDQUFDO1lBQ2pDLE9BQU8sQ0FBQyxNQUFNLENBQUMsSUFBSSxDQUFDO2dCQUNsQixLQUFLLEVBQUUsZUFBZTtnQkFDdEIsaUJBQWlCLEVBQUUsd0JBQXdCO2FBQzVDLENBQUMsQ0FBQyxDQUFDO1FBQ04sQ0FBQztRQUFDLElBQUksQ0FBQyxDQUFDO1lBQ04sT0FBTyxDQUFDLFlBQVksQ0FBQyxLQUFLLENBQUM7aUJBQ3hCLElBQUksQ0FBQyxVQUFVLElBQW9DO2dCQUNsRCxPQUFPLENBQUMsTUFBTSxDQUFDLEtBQUssQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDO1lBQzlCLENBQUMsQ0FBQyxDQUFDLEtBQUssQ0FBQyxVQUFVLEtBQVU7Z0JBQzNCLE9BQU8sQ0FBQyxNQUFNLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyxDQUFDLENBQUM7WUFDOUIsQ0FBQyxDQUFDLENBQUM7UUFDUCxDQUFDO0lBQ0gsQ0FBQyxDQUFDLENBQUM7QUFDTCxDQUFDO0FBbkJELG9DQW1CQyJ9