const fs = require("fs")
const { PeerServer } = require("peer")

const peerServer = PeerServer({
  path: "/board",
  port: 443,
  allow_discovery: true,
  ssl: {
    key: fs.readFileSync("key"),
    cert: fs.readFileSync("cert"),
  },
})
