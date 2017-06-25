import { Component, h } from 'preact'
import { connect } from 'preact-redux'

import Actions from '../actions'
import { State } from '../state'
import * as util from '../util'

import JoinForm from '../components/join-form'
import header from '../components/logo'

interface SplashState {
  firstLaunch: boolean,
  error?: null,
}

interface SplashDispatch {
  joinParty(partyCode: string): void
}

class Splash extends Component<SplashDispatch, SplashState> {
  state = {
    firstLaunch: false,
  }

  render({joinParty}: SplashDispatch, {firstLaunch}: SplashState) {
    return h('main', {}, header(true).concat([
      <div id="content">
        <JoinForm onJoinSubmitted={joinParty} />
      </div>,
      <div>
        <p>Made with love in Portland, Oregon.</p>
      </div>,
    ]))
  }
}

const SplashContainer = connect<SplashState, SplashDispatch, {}>(
  (state: State) => {
    return {
      firstLaunch: state.firstLaunch,
    }
  },
  dispatch => {
    return {
      joinParty: partyCode => {
        dispatch(Actions.JoinParty.create(partyCode))
      },
    }
  },
)(
  Splash,
)

export default SplashContainer
