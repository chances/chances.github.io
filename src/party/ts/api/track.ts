import { Data } from 'haskind';
const { Maybe } = Data;
const { Just, Nothing } = Data.Maybe;

export interface Track {
  id: string,
  name: string,
  artists: TrackArtist[],
  images: Image[],
  endpoint: string,
  began_playing: string,
  duration: number,
  contributor?: string,
  contributor_id?: string
}

export interface TrackArtist {
  id: string,
  name: string
}

export interface Image {
  width: number,
  height: number,
  url: string
}

export function firstArtistName(artists: TrackArtist[]) {
  return artists.length > 0
    ? artists[0].name
    : "";
}

export function largestImage(images: Image[]): Haskind.Maybe<Image> {
  if (images.length === 0) return Nothing();

  let largestImage = images[0];
  for (var imageIndex = 1; imageIndex < images.length; imageIndex++) {
    var image = images[imageIndex];
    if (image.width * image.height > largestImage.width * largestImage.height) {
      largestImage = image;
    }
  }
  return Just(largestImage);
}
