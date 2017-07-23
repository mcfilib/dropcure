FROM ubuntu:16.04
MAINTAINER Philip Cunningham <hello@filib.io>

RUN apt-get update && apt-get install -y libgmp-dev netbase
ADD ./tmp/producer /
ENTRYPOINT ["/producer", "server"]