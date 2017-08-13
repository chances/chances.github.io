import { Maybe } from 'monet'
import { Component, h } from 'preact'

import * as api from '../../api/track'
import { TabProps } from '../tab'

interface TrackProps {
  value: api.Track
  type: 'div' | 'li'
}

type Props = TrackProps & TabProps

export function listItem(track: api.Track) {
  return <Track type="li" value={track} />
}

export function block(id: string, name: string, track: api.Track) {
  const heading = <h2>{name}</h2>
  return <Track type="div" value={track} id={id} heading={heading} />
}

export default class Track extends Component<Props, void> {
  render({id, heading}: Props) {
    const {images, name, artists, contributor} = this.props.value
    const image = api.largestImage(images)

    const requestedBy = contributor
        ? <span class="requested-by">Added by {contributor}</span>
        : null

    const content = [
      <div class="song-info">
        <span class="title">{name}</span>
        <span class="artist">{api.firstArtistName(artists)}</span>
        {requestedBy}
      </div>,
    ]

    if (image.isJust()) {
      content.unshift(
        <img src={image.just().url} />,
      )
    }

    if (heading) {
      content.unshift(heading)
    }

    // Hacky props merging with TabProps
    const props: JSX.HTMLAttributes & TabProps = { class: 'track' }
    if (id) {
      props.id = id
    }
    return h(this.props.type, props, content)
  }
}
