# dropcure

`dropcure` is a toy project written in Haskell that consists of two services and
one library.

- `producer` - publishes messages received on websockets to a message queue.
- `consumer` - broadbasts messages from the message queue to clients connected via websockets.
- `common` - shared code or both services.

## Overview

Both `producer` and `consumer` follow a similar pattern in that each implements
a client and a server as a subcommand.

### producer

#### Server

The `producer` server operates as follows:

- Wait for RabbitMQ service to become available.
- Connect to RabbmitMQ server and create queue and channel if it doesn't exist.
- Disconnect from RabbitMQ.
- On new websocket connection:
  - Send handshake message.
  - Repeatedly publish any message to RabbitMQ.

#### Client

The `producer` client is intended to be used for debugging purposes and operates
as follows:

- Connect to server.
- Receive handshake.
- Every `n` seconds emit a static message (`"beep"`).

### consumer

#### Sever

The `consumer` server operates as follows:

- Initialise an empty mapping between unique identifiers and websocket.
- Wait for RabbitMQ service to become available.
- Connect to RabbmitMQ server and create queue and channel if it doesn't exist.
- Subscribe to the queue.
- On new websocket connection:
  - Create unique identifier.
  - Add connection to mapping for given identifier.
- On websocket disconnection:
  - Remove connection from mapping.
- On new message in the queue:
  - Enumerate all connections in mapping and emit message via websockets.

#### Client

The `consumer` client is intended to be used for debugging purposes and operates
as follows:

- Connect to server.
- Receive handshake.
- On new message emitted by the server:
  - Print it to `stdout`.

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
