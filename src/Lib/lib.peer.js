"use strict"

export const initPeer = host => port => path => () =>
  new Peer({
    host: host,
    port: port,
    path: path,
  })

export const onData = peer => onData => () =>
  peer.on("connection", conn =>
    conn.on("open", () =>
      conn.on("data", data =>
        onData(data)()
      )
    )
  )

export const sendData = peer => data => () => {
  let req = new XMLHttpRequest()
  req.onload = function() {
    JSON.parse(this.responseText).forEach(x => {
      if (x != peer.id) {
        let conn = peer.connect(x)
        conn.on("open", () => {
          conn.send(data)
        })
      }
    })
  }
  req.open("GET", "https://" + peer.options.host + ":" + peer.options.port + peer.options.path + "peerjs/peers")
  req.send()
}
