import { Maybe } from 'monet'
import { Component, h } from 'preact'
import { connect } from 'preact-redux'
import { Dispatch } from 'redux'

import { Request } from '../api'
import { Actions, State } from '../redux'

import JoinForm from '../components/join-form'

type SplashProps = SplashStateProps & SplashDispatchProps
interface SplashStateProps {
  firstLaunch: boolean
  partyCode: Maybe<string>
  isJoining: boolean
  error: Maybe<string>
}
interface SplashDispatchProps {
  joinParty(partyCode: string): void
}

class Splash extends Component<SplashProps, {}> {
  render({joinParty, isJoining, partyCode, error}: SplashProps, {}) {
    return <JoinForm
      onJoinSubmitted={joinParty}
      isJoining={isJoining}
      partyCode={partyCode}
      error={error}
    />
  }
}

function stateProps(state: State): SplashStateProps {
  const isJoining = state.joining.cata(() => false, request => request.isLoading)
  const maybeError = state.joining
    .flatMap(request => request.error)
    .map(err => err.detail)

  return {
    ...state,
    partyCode: state.joining.map(joinRequest => joinRequest.partyCode),
    isJoining,
    error: maybeError,
  }
}

const JoinParty = Actions.JoinParty.request

function dispatchProps(dispatch: Dispatch<any>): SplashDispatchProps {
  return {
    joinParty: partyCode => {
      dispatch(Actions.JoinParty.create(new JoinParty(partyCode)))
    },
  }
}

const SplashContainer =
  connect<SplashStateProps, SplashDispatchProps, {}>(stateProps, dispatchProps)(Splash)

export default SplashContainer
