import { Component, h } from 'preact'
import { connect } from 'preact-redux'

import { JoinParty, RequestStatus } from '../api'
import { Actions, State } from '../redux'

import JoinForm from '../components/join-form'
import header from '../components/logo'

interface SplashProps {
  firstLaunch: boolean
  isJoining: boolean
  partyCode: string
  joinParty(partyCode: string): void
}

function joinParty(partyCode: string): JoinParty {
  return {
    partyCode,
    status: RequestStatus.LOADING,
  }
}

class Splash extends Component<SplashProps, {}> {

  render({joinParty, isJoining, partyCode}: SplashProps, {}) {
    return h('main', {}, header(true).concat([
      <div id="content">
        <JoinForm onJoinSubmitted={joinParty} joining={isJoining} partyCode={partyCode} />
      </div>,
      <div>
        <p>Made with love in PDX.</p>
      </div>,
    ]))
  }
}

const SplashContainer = connect<{}, SplashProps, {}>(
  (state: State) => {
    return {
      firstLaunch: state.firstLaunch,
      isJoining: state.joining.isJust(),
      partyCode: state.joining.isJust() ? state.joining.just().partyCode : '',
    }
  },
  dispatch => {
    return {
      joinParty: partyCode => {
        dispatch(Actions.JoinParty.create(joinParty(partyCode)))
      },
    }
  },
)(
  Splash,
)

export default SplashContainer
