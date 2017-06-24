import { Component, h } from 'preact'

import header from './logo'

interface State {
  firstLaunch: boolean,
  error?: null,
}

export default class Splash extends Component<{}, State> {
  state = {
    firstLaunch: false
  }

  private joinSubmitted(e: Event) {

  }

  render({}, {firstLaunch}: State) {
    return h("main", {}, header(true).concat([
      <div id="content">
        <form id="join" onSubmit={this.joinSubmitted}>
          <h1>Join a Party</h1>
          <p>Enter the four character party code to help mix things up.</p>
          <div class="form-group">
            <label for="partyCode">Party code:</label>
            <div class="pill-group">
              <input id="partyCode" name="room_code" type="text" placeholder="Ab7j" maxLength={4} autofocus autocomplete="off" />
              <input type="submit" value="Join" />
            </div>
          </div>
        </form>
      </div>,
      <div>
        <p>Made with love in Portland, Oregon.</p>
      </div>
    ]))
  }
}
