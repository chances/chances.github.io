import { Component, h } from 'preact'

import header from './logo'
import Spinner from './spinner'

interface Props {
  partyCode?: string
  joining?: boolean
  onJoinSubmitted(partyCode: string): void
}

export default class JoinForm extends Component<Props, {}> {
  private partyCode: HTMLInputElement

  render({partyCode, joining}: Props, {}) {
    const spinnerHidden = !(joining != null && joining)
    return (
      <form id="join" onSubmit={this.onJoinSubmitted}>
        <h1>Join a Party</h1>
        <p>Help mix things up and join the fun!</p>
        <div class="form-group">
          <label for="partyCode">Party code:</label>
          <div class="pill-group">
            <input
              ref={this.refPartyCode}
              id="partyCode"
              type="text"
              value={partyCode}
              placeholder="Ab7j"
              autofocus={true}
              autocomplete="off"
              maxLength={4}
              onFocus={this.focusBlurJoinForm}
              onBlur={this.focusBlurJoinForm}
            />
            <input
              type="submit"
              value="Join"
              title="Join the party"
              onFocus={this.focusBlurJoinForm}
              onBlur={this.focusBlurJoinForm}
            />
          </div>
          <Spinner hidden={spinnerHidden} />
        </div>
      </form>
    )
  }

  private refPartyCode = (node: HTMLInputElement) => {
    this.partyCode = node
  }

  private focusBlurJoinForm(e: Event) {
    const input = e.currentTarget as HTMLInputElement
    const pillGroup = input.parentElement as HTMLElement

    if (e.type === 'focus') {
      pillGroup.classList.add('focus')
    } else {
      pillGroup.classList.remove('focus')
    }
  }

  private onJoinSubmitted = (e: Event) => {
    e.preventDefault()

    if (this.partyCode && this.partyCode.value) {
      this.props.onJoinSubmitted(this.partyCode.value)
    }
  }
}
