import { render, h } from 'preact'
import * as util from './util'

import Splash from './components/splash'
import Party from './components/party'

// let foo = util.log('Foo:', { jank: 'free' })

let main = document.querySelector('main')
if (main !== null) {
  main.remove()

  render(<Splash />, document.body)
}
