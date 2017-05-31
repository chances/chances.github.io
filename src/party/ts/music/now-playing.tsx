import { Component, h } from 'preact'
import { } from 'immutable'

import * as api from '../api/track'
import * as track from '../components/track'

interface State {
  currentTrack: api.Track
}

export default class NowPlaying extends Component<{}, State> {
  state = {
    currentTrack: {
      id: "",
      name: "Worries",
      artists: [{id: "", name: "Submotion Orchestra"}],
      images: [
        {
          width: 1000,
          height: 1000,
          url: "https://ninjatune.net/images/releases/alium-main.jpg"
        }
      ],
      endpoint: "",
      began_playing: "",
      duration: 120,
    }
  };

  render({}, {currentTrack}: State) {
    return track.block("nowPlaying", "Now Playing", currentTrack)
  }
}
