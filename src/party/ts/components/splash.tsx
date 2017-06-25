import { Component, h } from 'preact'

import header from './logo'

interface State {
  firstLaunch: boolean,
  error?: null,
}

export default class Splash extends Component<{}, State> {
  state = {
    firstLaunch: false,
  }

  render({}, {firstLaunch}: State) {
    return h('main', {}, header(true).concat([
      <div id="content">
        <form id="join" onSubmit={this.joinSubmitted}>
          <h1>Join a Party</h1>
          <p>Help mix things up and join the fun!</p>
          <div class="form-group">
            <label for="partyCode">Party code:</label>
            <div class="pill-group">
              <input
                id="partyCode"
                name="room_code"
                type="text"
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
          </div>
        </form>
      </div>,
      <div>
        <p>Made with love in Portland, Oregon.</p>
      </div>,
    ]))
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

  private joinSubmitted(e: Event) {
    // TODO: Implement join via AJAX to Party API
  }
}
