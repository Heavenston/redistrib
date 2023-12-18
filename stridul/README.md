# Stridul

Reliable (or not), connection less communication library.

## Streams

Streams are,,, byte-streams,,, identified by a 32bit id, ids should be
generated at least partially randomly (random top bits, app-depedent lower bits).
Streams being connection-less if a remote client doesn't have the start of the
stream, it is basically useless.

## Stridulation
From [Wikipedia](https://fr.wikipedia.org/wiki/Stridulation)

## TODOs

- [ ] Have better api than the Socket / SocketDriver api
- [ ] Figure-out what happens when packets sequence number wraps around as I forgot about that during most development
- [ ] "Garbage collection" of unsused streams after some time
- [ ] Limit the amount of streams a single remote addr can create
- [ ] Unreliable communication accessible for Users (Single UDP messages only ?)
- [ ] A bunch of // FIXPERF messages with maybe possible performance paths
- [ ] Implement a Congestion window that resizes based on lost/successful packets
