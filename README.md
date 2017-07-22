# graham

`graham` is a toy project written in Haskell that consists of two services:

- `producer` - publishes messages received on websockets to a message queue.
- `consumer` - broadbasts messages from the message queue to clients connected via websockets.
