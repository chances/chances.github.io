import localForage = require('localforage')
import { h, render } from 'preact'
import { Provider } from 'preact-redux'
import { applyMiddleware, compose, createStore } from 'redux'
import { persistStore } from 'redux-persist'

import * as api from './api'
import { Middleware, partyApp, persistTransform } from './redux'
import * as util from './util'

import Party from './components/party'
import Splash from './containers/splash'

declare var window: {
  __REDUX_DEVTOOLS_EXTENSION_COMPOSE__?: typeof compose,
}

import 'preact/devtools'
const composeEnhancers = process.env.NODE_ENV === 'development'
  ? window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ || compose
  : compose

const store = createStore(
  partyApp,
  composeEnhancers(
    applyMiddleware(Middleware.logger, ...api.middleware),
  ),
)

persistStore(store, {
  storage: localForage,
  transforms: [persistTransform],
})

api.setPartyApiHost(util.log('Party API Host:', 'http://app.local:3005'))

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
