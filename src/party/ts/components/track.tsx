import { Component, h } from 'preact'
import { } from 'immutable'
import { Data } from 'haskind';
const { Maybe } = Data;

import * as api from '../api/track'
import { TabProps } from './tab'

interface TrackProps {
  value: api.Track
  type: "div" | "li"
}

type Props = TrackProps & TabProps

export function listItem(track: api.Track) {
  return <Track type="li" value={track} />
}

export function block(id: string, name: string, track: api.Track) {
  let heading = <h2>{name}</h2>
  return <Track type="div" value={track} id={id} heading={heading} />
}

export default class Track extends Component<Props, void> {
  render({id, heading}: Props) {
    let {images, name, artists} = this.props.value;
    let image = api.largestImage(images)

    let content = [
      <div class="song-info">
        <span class="title">{name}</span>
        <span class="artist">{api.firstArtistName(artists)}</span>
      </div>
    ];

    if (Maybe.isJust(image)) {
      content.unshift(
        <img src={Maybe.fromJust<api.Image>(image).url} />
      )
    }

    if (heading) {
      content.unshift(heading);
    }

    // Hacky props merging with TabProps
    let props: JSX.HTMLAttributes & TabProps = { class: "track" };
    if (id) props.id = id;
    return h(this.props.type, props, content)
  }
}
