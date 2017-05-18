import { render, h } from 'preact'
import * as util from './util'

let foo = util.log('Foo:', { jank: 'free' })

let main = document.querySelector('main')
if (main !== null) {
  main.remove()
  render(
    <main>
      <h2>chancesnow.me/party</h2>
      <p>Foobar</p>
    </main>,
    document.body
  )
}
