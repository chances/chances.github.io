import { h, render } from 'preact'
import { applyMiddleware, createStore } from 'redux'

import { setPartyApiHost } from './api/request'
import { logger } from './middleware'
import partyApp from './reducers'
import * as util from './util'

import Party from './components/party'
import Splash from './components/splash'

const store = createStore(
  partyApp,
  applyMiddleware(logger),
)

setPartyApiHost(util.log('Party API Host:', 'http://app.local:3005'))

const main = document.querySelector('main')
if (main !== null) {
  main.remove()

  render(<Splash />, document.body)
}
