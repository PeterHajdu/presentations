# Domain

``` haskell
socket :: Either ConnectionError Socket
bind :: Socket -> Either ConnectionError Socket
listen :: Socket -> Either ConnectionError Socket
accept :: Socket -> Either ConnectionError Connection
```

# Domain

``` scala
def socket(): Either[ConnectionError, Socket]
def bind(socket: Socket): Either[ConnectionError, Socket]
def listen(socket: Socket): Either[ConnectionError, Socket]
def accept(socket: Socket): Either[ConnectionError, Connection]
```

# Restrict with types

``` haskell
socket :: Either ConnectionError Socket
bind :: Socket -> Either ConnectionError BoundSocket
listen :: BoundSocket -> Either ConnectionError ListeningSocket
accept :: ListeningSocket -> Either ConnectionError Connection
```

# Restrict with types

``` scala
def socket(): Either[ConnectionError, Socket]
def bind(socket: Socket): Either[ConnectionError, BoundSocket]
def listen(socket: BoundSocket): Either[ConnectionError, ListeningSocket]
def accept(socket: ListeningSocket): Either[ConnectionError, Connection]
```

# Boilerplate

``` haskell
data Socket = Socket {fd :: FileDescriptor, opts :: [SocketOption]}
data BoundedSocket = BoundedSocket {fd :: FileDescriptor, opts :: [SocketOption]}
data ListeningSocket = ListeningSocket {fd :: FileDescriptor, opts :: [SocketOption]}
```

# Boilerplate

``` scala
class Socket(fd: FileDescriptor, opts: List[SocketOption])
class BoundSocket(fd: FileDescriptor, opts: List[SocketOption])
class ListeningSocket(fd: FileDescriptor, opts: List[SocketOption])
```

# DataKinds

``` haskell
data SocketState =
    Open
  | Bound
  | Listening

data Socket {s :: SocketState} = Socket {...}
```

# Phantom types

``` scala
sealed trait SocketState
trait Open extends SocketState
trait Bound extends SocketState
trait Listening extends SocketState

class Socket[T <: SocketState](fd: FileDescriptor, opts: List[SocketOption])
```

# Phantom types

``` scala
def socket(): Either[ConnectionError, Socket[Open]]
def bind(socket: Socket[Open]): Either[ConnectionError, Socket[Bound]]
def listen(socket: Socket[Bound]): Either[ConnectionError, Socket[Listening]]
def accept(socket: Socket[Listening]): Either[ConnectionError, Connection]
```

