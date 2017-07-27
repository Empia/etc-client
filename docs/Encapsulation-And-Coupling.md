# Goals and Intents for packages 

## rlpx

This is a partial implementation of [RLPx](https://github.com/ethereum/devp2p/blob/master/rlpx.md#network-formation) This package encapsulates the code necessary to establish a connection with a remote host or handle a connection from a remote host. They negotiate the handshake establish session keys over which encrypted messages are exchanged and are able to 'deframe' bytes from the wire into fixed size messages identified by a leading `Int`.

The entry point is the `RLPxConnectionHandler` actor and the API consists of accepting an incoming AKKA Tcp `Connection` or attempting to create a connection given a uri. The actor accepts outgoing messages in the form of a byte array and propagates incoming message to the parent actorRef

### rlpx coupling
Coupled with 
