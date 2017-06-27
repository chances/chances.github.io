import { Component, h } from 'preact'

import header from './logo'
import Spinner from './spinner'

interface Props {
  partyCode?: string
  joining?: boolean
  onJoinSubmitted(partyCode: string): void
}

const whitespace = /[\s\^]/g
const alphaNum = /[a-zA-z0-9]/
function stripNonAlphaNumeric(s: string) {
  let result = ''
  for (let char = 0; char < s.length; char++) {
    const c = s.charAt(char)
    result += alphaNum.test(c) ? c : ''
  }
  return result
}

export default class JoinForm extends Component<Props, {}> {
  private partyCode: HTMLInputElement
  private submitButton: HTMLInputElement

  render({partyCode, joining}: Props, {}) {
    const spinnerHidden = !(joining != null && joining)
    const submitHidingClass = spinnerHidden && partyCode &&
      partyCode.trim() ? '' : 'hiding'

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
              disabled={!spinnerHidden}
              onKeyUp={this.partyCodeKey}
              onFocus={this.focusBlurJoinForm}
              onBlur={this.focusBlurJoinForm}
            />
            <input
              ref={this.refSubmitButton}
              class="hiding"
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

  private refSubmitButton = (node: HTMLInputElement) => {
    this.submitButton = node
  }

  private partyCodeKey = (e: Event) => {
    const input = e.currentTarget as HTMLInputElement
    const value = stripNonAlphaNumeric(
      input.value.trim().replace(whitespace, ''),
    )
    input.value = value

    if (!this.submitButton) {
      return
    }

    const showSubmit = value.length > 0

    if (showSubmit) {
      this.submitButton.classList.remove('hiding')
    } else if (!this.submitButton.classList.contains('hiding')) {
      this.submitButton.classList.add('hiding')
    }
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

    if (this.partyCode && this.partyCode.value && this.partyCode.value.trim()) {
      if (!this.submitButton.classList.contains('hiding')) {
        this.submitButton.classList.add('hiding')
      }
      this.partyCode.blur()
      this.submitButton.blur()

      this.props.onJoinSubmitted(this.partyCode.value)
    }
  }
}
