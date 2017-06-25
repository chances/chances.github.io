import { expect, test, that } from './lib/expect'

import { firstArtistName, largestImage, Track } from '../api/track'

test('firstArtistName returns empty string with zero artists', t => {
  expect(that(firstArtistName([])).equals(''))

  t.end()
})

test('firstArtistName returns first artist', t => {
  const track: Track = {
    id: '',
    name: 'Awakening',
    artists: [{id: '', name: 'Submotion Orchestra'}],
    images: [
      {
        width: 1000,
        height: 1000,
        url: 'https://ninjatune.net/images/releases/alium-main.jpg',
      },
    ],
    endpoint: '',
    began_playing: '',
    duration: 120,
    contributor: 'Jake Perkins',
  }

  expect(that(firstArtistName(track.artists)).equals('Submotion Orchestra'))

  t.end()
})

test('largestImage returns nothing with zero images', t => {
  const track: Track = {
    id: '',
    name: 'Awakening',
    artists: [{id: '', name: 'Submotion Orchestra'}],
    images: [
      {
        width: 1000,
        height: 1000,
        url: 'https://ninjatune.net/images/releases/alium-main.jpg',
      },
      {
        width: 500,
        height: 500,
        url: 'https://ninjatune.net/images/releases/alium-main.jpg',
      },
    ],
    endpoint: '',
    began_playing: '',
    duration: 120,
    contributor: 'Jake Perkins',
  }
  track.images = []

  const image = largestImage(track.images)

  expect(that(image.isNothing()).is.true)

  t.end()
})

test('largestImage returns just the largest image', t => {
  const track: Track = {
    id: '',
    name: 'Awakening',
    artists: [{id: '', name: 'Submotion Orchestra'}],
    images: [
      {
        width: 500,
        height: 500,
        url: 'https://ninjatune.net/images/releases/alium-main.jpg',
      },
      {
        width: 1000,
        height: 1000,
        url: 'https://ninjatune.net/images/releases/alium-main.jpg',
      },
    ],
    endpoint: '',
    began_playing: '',
    duration: 120,
    contributor: 'Jake Perkins',
  }

  const image = largestImage(track.images)

  expect(that(image.isJust()).is.true)
  expect(that(image.just().width).equals(1000))
  expect(that(image.just().height).equals(1000))
  expect(that(image.just().url).equals('https://ninjatune.net/images/releases/alium-main.jpg'))

  t.end()
})
