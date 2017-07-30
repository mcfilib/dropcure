# dropcure

`dropcure` is a toy project written in Haskell that consists of two services and
one library.

- `producer` - publishes messages received on websockets to a message queue.
- `consumer` - broadbasts messages from the message queue to clients connected via websockets.
- `common` - shared code or both services.
