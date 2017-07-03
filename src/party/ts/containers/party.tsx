import { Maybe } from 'monet'
import { Component, h } from 'preact'
import { connect } from 'preact-redux'
import { Dispatch } from 'redux'

import * as api from '../api'
import { Actions, State } from '../redux'

import header from '../components/logo'
import NowPlaying from '../components/now-playing'
import Splash from './splash'

type PartyProps = PartyStateProps & PartyDispatchProps
interface PartyStateProps {
  firstLaunch: boolean
  tvMode: boolean
  party: Maybe<api.Party>
}
interface PartyDispatchProps {
  // TODO: Party dispatch properties
  joinParty(partyCode: string): void
}

export class Party extends Component<PartyProps, {}> {
  render(props: PartyProps, {}) {
    return h('main', {}, header(props.party.isNothing(), props.tvMode).concat(
      props.party.cata(
        () => [
          <div id="content">
            <Splash />
          </div>,
          <div>
            <p>Made with love in PDX.</p>
          </div>,
        ],
        party => [
          <nav id="musicMenu" class="menu secondary">
            <ul>
              <li class="selected"><a href="#nowPlaying">Now Playing</a></li>
              <li><a href="#history">History</a></li>
              <li id="upNextMenuItem"><a href="#upNext">Up Next</a></li>
              <li><a href="#contribute">Contribute</a></li>
            </ul>
          </nav>,
          <NowPlaying />,
          <nav id="mainMenu" class="menu">
            <ul>
              <li class="selected"><a href="#music">Music</a></li>
              <li><a href="#guests">Guests</a></li>
            </ul>
          </nav>,
        ],
      ),
    ))
  }
}

function stateProps(state: State): PartyStateProps {
  const isJoining = state.joining.cata(() => false, request => request.isLoading)
  const maybeError = state.joining
    .flatMap(request => request.error)
    .map(err => err.detail)

  return {
    firstLaunch: state.firstLaunch,
    tvMode: state.tvMode,
    party: state.party,
  }
}

type JoinParty = typeof Actions.JoinParty.payload

function dispatchProps(dispatch: Dispatch<any>): PartyDispatchProps {
  return {
    joinParty: partyCode => {
      // TODO: Party dispatch properties
    },
  }
}

const PartyContainer =
  connect<PartyStateProps, PartyDispatchProps, {}>(stateProps, dispatchProps)(Party)

export default PartyContainer
