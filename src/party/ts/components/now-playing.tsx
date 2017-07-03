import { Component, h } from 'preact'

import header from './logo'

import CurrentTrack from './music/current-track'
import TrackList from './music/track-list'

interface State {
  tvMode: boolean
}

export default class NowPlaying extends Component<{}, State> {
  state = {
    tvMode: false,
  }

  render({}, {tvMode}: State) {
    return <div id="content">
      <CurrentTrack />
      <TrackList id="upNext" name="Up Next" />
    </div>
  }
}
