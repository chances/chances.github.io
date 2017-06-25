import { Component, h } from 'preact'

import header from './logo'
import Spinner from './spinner'

interface Props {
  joining?: boolean
  onJoinSubmitted(partyCode: string): void
}

interface State {
  partyCode: string
}

export default class JoinForm extends Component<Props, State> {
  state = {
    partyCode: '',
  }

  render({onJoinSubmitted}: Props, {partyCode}: State) {
    const spinnerHidden = !(joining != null && joining)
    return (
      <form id="join" onSubmit={this.onJoinSubmitted}>
        <h1>Join a Party</h1>
        <p>Help mix things up and join the fun!</p>
        <div class="form-group">
          <label for="partyCode">Party code:</label>
          <div class="pill-group">
            <input
              id="partyCode"
              name="room_code"
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
              onFocus={this.focusBlurJoinForm}
              onBlur={this.focusBlurJoinForm}
            />
          </div>
          <Spinner hidden={spinnerHidden} />
        </div>
      </form>
    )
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
    const form = e.currentTarget as HTMLFormElement
    const partyCode = form.elements.namedItem('partyCode') as HTMLInputElement | null

    if (partyCode && partyCode.value) {
      this.props.onJoinSubmitted(partyCode.value)
    }
  }
}
