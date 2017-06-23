import { Component, h } from 'preact'

export default function header(isSplash: boolean, tvMode: boolean = false) {
  let header = [];

  if (isSplash) {
    header.push(
      <div style="display: flex; flex-direction: column; align-items: center; justify-content: center;">
        <img src="/assets/images/party/Party-logo-invert.png" alt="Party" style="max-width: 300px; margin: 0 0 -1.5rem 0;" />
        <p style="margin-top: 0;">Prototype</p>
      </div>,
    )
  }

  if (tvMode) {
    header.push(
      <h2>chancesnow.me/party</h2>
    )
  }

  return header;
}
