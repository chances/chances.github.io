import Immutable = require('immutable')
import { Maybe } from 'monet'

import { Party } from './api/party'

// tslint:disable-next-line:interface-name
export interface IState {
  firstLaunch: boolean,
  party: Maybe<Party>
}

export class State implements IState {
  private mutator: Immutable.Record.Class
  private record: Immutable.Map<string, any>

  constructor() {
    this.mutator = Immutable.Record({
      firstLaunch: false,
      party: Maybe.Nothing<Party>(),
    })
    this.record = this.mutator()
  }

  get firstLaunch(): boolean {
    return this.record.get('firstLaunch')
  }
  set firstLaunch(firstLaunch: boolean) {
    this.record = this.record.set('firstLaunch', firstLaunch)
  }

  get party(): Maybe<Party> {
    return this.record.get('party')
  }
  set party(party: Maybe<Party>) {
    this.record = this.record.set('party', party)
  }

  toJson(): IState {
    return this.record.toJS()
  }
}

export const initialState = new State()
