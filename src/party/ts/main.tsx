import { render, h } from 'preact'
import * as util from './util'

import NowPlaying from './music/now-playing'
import TrackList from './components/track-list'

let foo = util.log('Foo:', { jank: 'free' })

let main = document.querySelector('main')
if (main !== null) {
  main.remove()
  render(
    <main>
      <h2>chancesnow.me/party</h2>
      <div style="display: flex; flex-direction: column; align-items: center; justify-content: center;">
        <img src="/assets/images/Party-logo-invert.png" alt="Party" style="max-width: 300px; margin: 0 0 -1rem 0;" />
        <p style="margin-top: 0;">Prototype</p>
      </div>
      <div id="content">
        <NowPlaying />
        <TrackList id="upNext" name="Up Next" />
      </div>
      <div>
        <a href="/party/old.html" style="cursor: pointer; font-size: 14pt; color: white;">Prettier Mockup</a>
      </div>
    </main>,
    document.body
  )
}
