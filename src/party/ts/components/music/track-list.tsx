import { Component, h } from 'preact'

import * as api from '../../api/track'
import * as track from './track'

interface TrackListParams {
  id: string
  name: string
}

export default class TrackList extends Component<TrackListParams, api.Track[]> {
  state = [
    {
      id: "",
      name: "Awakening",
      artists: [{id: "", name: "Submotion Orchestra"}],
      images: [
        {
          width: 1000,
          height: 1000,
          url: "https://ninjatune.net/images/releases/alium-main.jpg"
        }
      ],
      endpoint: "",
      began_playing: "",
      duration: 120,
      contributor: "Jake Perkins"
    },
    {
      id: "",
      name: "Run",
      artists: [{id: "", name: "Air"}],
      images: [
        {
          width: 1000,
          height: 1000,
          url: "https://i.scdn.co/image/671f5bc32e02cbca2568054b0f578e63b9e2fe31"
        }
      ],
      endpoint: "",
      began_playing: "",
      duration: 120,
      contributor: "Bradley Rasmussen"
    },
    {
      id: "",
      name: "Black Sands",
      artists: [{id: "", name: "Bonobo"}],
      images: [
        {
          width: 1000,
          height: 1000,
          url: "https://upload.wikimedia.org/wikipedia/en/4/49/Bonobo_-_Black_Sands.jpg"
        }
      ],
      endpoint: "",
      began_playing: "",
      duration: 120
    }
  ];

  render({id, name}: TrackListParams, tracks: api.Track[]) {
    return (
      <div id={id}>
        <h2>{name}</h2>
        <ul class="tracks">
          {
            tracks.map(t => (
              track.listItem(t)
            ))
          }
        </ul>
      </div>
    );
  }
}
