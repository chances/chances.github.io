import { Component, h } from 'preact'

import header from './logo'

import TrackList from './music/track-list'
import NowPlaying from './music/now-playing'

interface State {
  tvMode: boolean
}

export default class Party extends Component<{}, State> {
  state = {
    tvMode: false
  }

  render({}, {tvMode}: State) {
    return h("main", {}, header(false, tvMode).concat([
      <div id="content">
        <NowPlaying />
        <TrackList id="upNext" name="Up Next" />
      </div>,
      <div>
        <a href="/party/old.html" style="cursor: pointer; font-size: 14pt; color: white;">Prettier Mockup</a>
      </div>
    ]))
  }
}
