FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get -y -q install curl gcc libgmp-dev binutils libnuma-dev autoconf automake build-essential zlib1g-dev libgmp10 git wget lsb-release  software-properties-common gnupg2  apt-transport-https libtinfo-dev

# install gpg keys
ARG GPG_KEY=7784930957807690A66EBDBE3786C5262ECB4A3F
RUN gpg --batch --keyserver keys.openpgp.org --recv-keys $GPG_KEY

# install ghcup
RUN \
    curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup && \
    ghcup config set gpg-setting GPGStrict

ARG GHC=9.2.2
ARG CABAL=latest

# install GHC and cabal
RUN \
    ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
    ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL}
