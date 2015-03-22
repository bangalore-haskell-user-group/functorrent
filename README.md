# A bittorrent client.

[https://travis-ci.org/vu3rdd/functorrent](file:https://travis-ci.org/vu3rdd/functorrent.svg?branch=master)

## building

I suggest using cabal sandbox.

### Steps:

clone the repo; cd functorrent;

    $ cabal sandbox init
    $ wget http://www.stackage.org/lts/cabal.config
    $ cabal install --only-dependencies --enable-tests
    $ cabal build # binaries in ./dist/built/functorrent/*

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
