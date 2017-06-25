import { h, render } from 'preact'
import * as util from './util'

import Party from './components/party'
import Splash from './components/splash'

// let foo = util.log('Foo:', { jank: 'free' })

const main = document.querySelector('main')
if (main !== null) {
  main.remove()

  render(<Splash />, document.body)
}
