import { Maybe } from 'monet'
import { Component, h } from 'preact'

import header from './logo'
import Spinner from './spinner'

interface Props {
  partyCode: Maybe<string>
  isJoining: boolean
  error: Maybe<string>
  onJoinSubmitted(partyCode: string): void
}

const whitespace = /[\s\\\^\[\]`]/g
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

  render(props: Props, {}) {
    const submitHidingClass = props.isJoining
      ? 'hiding'
      : props.partyCode.cata(
        () => 'hiding',
        partyCode => partyCode.length ? '' : 'hiding',
      )

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
              value={props.partyCode.cata(() => '', partyCode => partyCode)}
              placeholder="Ab7j"
              autofocus={true}
              autocomplete="off"
              maxLength={4}
              disabled={props.isJoining}
              onInput={this.partyCodeInputChange}
              onFocus={this.focusBlurJoinForm}
              onBlur={this.focusBlurJoinForm}
            />
            <input
              ref={this.refSubmitButton}
              class={submitHidingClass}
              type="submit"
              value="Join"
              title="Join the party"
              onFocus={this.focusBlurJoinForm}
              onBlur={this.focusBlurJoinForm}
            />
          </div>
          <Spinner hidden={!props.isJoining} />
        </div>
        <p class="error">{props.error.cata(() => '', errStr => errStr)}</p>
      </form>
    )
  }

  private refPartyCode = (node: HTMLInputElement) => {
    this.partyCode = node
  }

  private refSubmitButton = (node: HTMLInputElement) => {
    this.submitButton = node
  }

  private partyCodeInputChange = (e: Event) => {
    const input = e.currentTarget as HTMLInputElement
    const value = stripNonAlphaNumeric(
      input.value.trim().replace(whitespace, ''),
    )
    input.value = value

    if (!this.submitButton) {
      return
    }

    this.submitVisible = value.length > 0
  }

  private set submitVisible(showSubmit: boolean) {
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
      this.partyCode.blur()
      this.submitButton.blur()

      this.props.onJoinSubmitted(this.partyCode.value)
    }
  }
}
