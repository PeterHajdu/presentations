---
author: 'Peter Hajdu'
title: 'Phantom types in scala'
...

# Domain

``` scala
def socket(): Either[ConnectionError, Socket]
def bind(socket: Socket): Either[ConnectionError, Socket]
def listen(socket: Socket): Either[ConnectionError, Socket]
def accept(socket: Socket): Either[ConnectionError, Connection]
```

# Restrict with types

``` scala
def socket(): Either[ConnectionError, Socket]
def bind(socket: Socket): Either[ConnectionError, BoundSocket]
def listen(socket: BoundSocket): Either[ConnectionError, ListeningSocket]
def accept(socket: ListeningSocket): Either[ConnectionError, Connection]
```

# Boilerplate

``` scala
class Socket(fd: FileDescriptor, opts: List[SocketOption])
class BoundSocket(fd: FileDescriptor, opts: List[SocketOption])
class ListeningSocket(fd: FileDescriptor, opts: List[SocketOption])
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

# Misc

 * smart constructors
 * no runtime overhead

# Examples

 * door
 * signed messages

