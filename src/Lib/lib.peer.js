"use strict"

export const initPeer = options => () => new Peer(options)

export const onData = peer => f => () =>
  peer.on("connection", conn =>
    conn.on("open", () =>
      conn.on("data", data => f(data)())
    )
  )

export const connect = peer => id => () => peer.connect(id)

export const onOpen = conn => f => () => conn.on("open", () => f())

export const send = conn => data => () => conn.send(data)
