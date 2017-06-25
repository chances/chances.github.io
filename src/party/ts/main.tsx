import { h, render } from 'preact'
import { Provider } from 'preact-redux'
import { applyMiddleware, createStore } from 'redux'

import { setPartyApiHost } from './api/request'
import { logger } from './middleware'
import partyApp from './reducers'
import * as util from './util'

import Party from './components/party'
import Splash from './containers/splash'

const store = createStore(
  partyApp,
  applyMiddleware(logger),
)

setPartyApiHost(util.log('Party API Host:', 'http://app.local:3005'))

const main = document.querySelector('main')
if (main !== null) {
  main.remove()

  render(
    <Provider store={store}>
      <Splash />
    </Provider>,
    document.body,
  )
}
