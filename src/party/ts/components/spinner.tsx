import { Component, h } from 'preact'

interface Props {
  hidden?: boolean
  color?: string
  size?: number
}

export default class Spinner extends Component<Props, {}> {
  get hidden() {
    return this.props.hidden != null
      ? this.props.hidden
      : true
  }

  get color() {
    return this.props.color != null
      ? this.props.color
      : '#FCFCFC'
  }

  get size() {
    return this.props.size != null
      ? this.props.size
      : 40
  }

  render(props: Props, {}) {
    const dur = '0.7s'

    return (
      <span class={'spinner' + (this.hidden ? ' hiding' : '')}>
        <svg
          x="0px"
          y="0px"
          width={this.size + 'px'}
          height={this.size + 'px'}
          viewBox="0 0 50 50"
          style="enable-background: new 0 0 50 50"
        >
          <path
            class="spinner"
            fill={this.color}
            // tslint:disable-next-line:max-line-length
            d="M25.251,6.461c-10.318,0-18.683,8.365-18.683,18.683h4.068c0-8.071,6.543-14.615,14.615-14.615V6.461z"
          >
            <animateTransform
              attributeType="xml"
              attributeName="transform"
              type="rotate"
              from="0 25 25"
              to="360 25 25"
              dur={dur}
              repeatCount="indefinite"
            />
          </path>
        </svg>
      </span>
    )
  }
}
