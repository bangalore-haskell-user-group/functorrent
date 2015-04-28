# A bittorrent client.

[![Join the chat at https://gitter.im/vu3rdd/functorrent](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/vu3rdd/functorrent?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![Build Status](https://travis-ci.org/vu3rdd/functorrent.svg?branch=master)](https://travis-ci.org/vu3rdd/functorrent)

## Building

Functorrent can be build with [Cabal](https://www.haskell.org/cabal/) sandbox or
[Nix](https://nixos.org/nix/).

### Cabal sandbox

Sandboxes give you per project independent containers, just like Python's
virtualenv.


    $ git clone https://github.com/vu3rdd/functorrent && cd functorrent
    $ cabal sandbox init
    $ cabal install --only-dependencies --enable-tests
    $ cabal build # binaries in ./dist/built/functorrent/*

### Building with Nix

``$ nix-shell``` at the root of the source code repo should drop you into a
shell which has all the package dependencies installed.


    $ nix-shell --pure
    [...]
    [nix-shell] $ cabal configure && cabal build

## Goals

- Become more profient with Haskell.
- Implement something non-trivial with Haskell (crypto, file operations, network
  operations, concurrency, bit twiddling, DHT).
- Follow the spec - https://wiki.theory.org/BitTorrentSpecification
- Easy for newbies like me to read and understand along side the spec.
- doctest and quickcheck tests.
- Follow Haskell Style Guide - https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md

## Current Status

- can decode torrent files (bencoding)
- talk to the tracker and get the peer list
- the `main' program takes a torrent file (in the local file system) as input and
  prints the {ip,port} for each peer, after talking to the tracker.

## TODO

*    Test suite.
*    Peer protocol.
*    Get the file download working in the simplest possible way.
*    Concurrency (threads per peer)
*    other advanced features of Bit Torrent (like DHT).
