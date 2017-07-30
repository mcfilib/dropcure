# dropcure

`dropcure` is a toy project written in Haskell that consists of two services and
one library.

- `producer` - publishes messages received on websockets to a message queue.
- `consumer` - broadbasts messages from the message queue to clients connected via websockets.
- `common` - shared code or both services.

## Usage

The instructions below assume that you are
running [Ubuntu](https://www.ubuntu.com/),
have [Stack](https://docs.haskellstack.org/en/stable/README/) installed and have
the [Docker](https://www.docker.com/) daemon running.

```
$ make up                             # start necessary serices
$ WEBSOCKET_PORT=8001 consumer client # run the consumer client
$ WEBSOCKET_PORT=8000 producer client # run the producer client
```

### Dependencies

- `docker`
- `stack`
- `ubuntu`

## Development

### Tasks

```
% make
build                          Build producer and consumer Docker containers
clean                          Clean up build artefacts
help                           Print available tasks
install                        Compile producer and consumer
up                             Start services
```
