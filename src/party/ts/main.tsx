import { render, h } from 'preact'
import * as util from './util'

let foo = util.log('Foo:', { jank: 'free' })

render(
  <p>Foobar</p>,
  document.body
)
