---
author: 'Peter Hajdu'
title: 'Phantom types'
...

# Domain

``` haskell
socket :: Either ConnectionError Socket
bind :: Socket -> Either ConnectionError Socket
listen :: Socket -> Either ConnectionError Socket
accept :: Socket -> Either ConnectionError Connection
```

 * problems

# Restrict with types

``` haskell
socket :: Either ConnectionError Socket
bind :: Socket -> Either ConnectionError BoundSocket
listen :: BoundSocket -> Either ConnectionError ListeningSocket
accept :: ListeningSocket -> Either ConnectionError Connection
```

 * problems

# Boilerplate

``` haskell
data Socket = Socket {fd :: FileDescriptor, opts :: [SocketOption]}
data BoundedSocket = BoundedSocket {fd :: FileDescriptor, opts :: [SocketOption]}
data ListeningSocket = ListeningSocket {fd :: FileDescriptor, opts :: [SocketOption]}
```

# Phantom types

``` haskell
data Socket s = Socket {fd :: FileDescriptor, opts :: [SocketOption]}
```

 * Is it used?

# Phantom types

``` haskell
data OpenSocket
data BoundSocket
data ListeningSocket

data Socket s = Socket {fd :: FileDescriptor, opts :: [SocketOption]}
```

 * OpenSocket ?
 * OpenSocket instances?
 * Socket ?
 * Socket OpenSocket ?

# Using phantom types

``` haskell
socket :: Either ConnectionError (Socket OpenSocket)
bind :: (Socket OpenSocket) -> Either ConnectionError (Socket BoundSocket)
listen :: (Socket BoundSocket) -> Either ConnectionError (Socket ListeningSocket)
accept :: (Socket ListeningSocket) -> Either ConnectionError Connection
```

  * Socket Int ?

# Kinds

 * ``` * ```
 * ``` * -> * ```
 * Int ?
 * Map ?
 * Map Int ?

# DataKinds

``` haskell
data SocketState =
    Open
  | Bound
  | Listening

data Socket {s :: SocketState} = Socket {fd :: FileDescriptor, opts :: [SocketOption]}
```

  * :t Open
  * :k 'Open

# Domain

``` haskell
socket :: Either ConnectionError (Socket 'Open)
bind :: (Socket 'Open) -> Either ConnectionError (Socket 'Bound)
listen :: (Socket 'Bound) -> Either ConnectionError (Socket 'Listening)
accept :: (Socket 'Listening) -> Either ConnectionError Connection
```

# Misc

 * smart constructors
 * no runtime overhead

# Examples

 * door
 * signed messages

