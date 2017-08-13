import { Component, h } from 'preact'

export default function header(isSplash: boolean, tvMode: boolean = false) {
  const header = []

  if (isSplash) {
    header.push(
      <div id="logo">
        <img
          src="/assets/images/party/Party-logo-invert.png"
          alt="Party"
        />
        <p style="margin-top: 0;">Prototype</p>
      </div>,
    )
  }

  if (tvMode) {
    header.push(
      <h2>chancesnow.me/party</h2>,
    )
  }

  return header
}
