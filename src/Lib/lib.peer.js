exports.initPeer = function(host) {
  return function(port) {
    return function(path) {
      return function() {
        return new Peer({
          host: host,
          port: port,
          path: path,
          secure: false,
        })
      }
    }
  }
}

exports.onData = function(peer) {
  return function(onData) {
    return function() {
      peer.on("connection", conn => {
        conn.on("open", () => {
          conn.on("data", data => {
            onData(data)()
          })
        })
      })
    }
  }
}

exports.sendData = function(peer) {
  return function(data) {
    return function() {
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
      req.open("GET", "http://" + peer.options.host + ":" + peer.options.port + peer.options.path + "peerjs/peers")
      req.send()
    }
  }
}
