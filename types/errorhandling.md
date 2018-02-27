---
author: 'Peter Hajdu'
title: 'Error handling'
...

# In C

``` c
int accept_connection(struct sockaddr_in source) {
  const int sock = socket();
  if (socket < 0) {
    return -1;
  }

  if (bind(sock)) < 0) {
    return -2;
  }

  const int connection = accept(sock);
  if (connection < 0) {
    return -3;
  }

  return connection;
}
```

# In languages with exceptions

``` c++
int accept_connection(struct sockaddr_in source) {
  const int sock = socket();
  if (socket < 0) {
    throw UnableToCreateSocket();
  }

  if (bind(sock)) < 0) {
    throw UnableToBindSocket();
  }

  const int connection = accept(sock);
  if (connection < 0) {
    throw UnableToAcceptConnection();
  }

  return connection;
}
```

# nullptr and friends

``` java
if (someobject != null) {
  ...
}
```

``` c
int length(const char* str) {
  if (str == NULL) {
    ...
  }
}
```

# nullptr and friends

 * Algol W 1965, Tony Hoare
 * _my billion-dollar mistake_

# What is a total function?

 * f: A -> B

# What is a partial function?

 * f: A -> B
 * accept_connection: Source -> Connection
 * runtime error
 * avoid writing/calling partial functions

# Static type checking and error handling

 * runtime errors -> compile time errors
 * cheaper to fix

# Error handling with exceptions

``` c++
int accept_connection(struct sockaddr_in source) {
  const int sock = socket();
  if (socket < 0) {
    throw UnableToCreateSocket();
  }

  if (bind(sock)) < 0) {
    throw UnableToBindSocket();
  }

  const int connection = accept(sock);
  if (connection < 0) {
    throw UnableToAcceptConnection();
  }

  return connection;
}
```

# Either

  * accept_connection: Source -> Connection
  * accept_connection: Source -> ConnectionError
  * accept_connection: Source -> Either[ConnectionError, Connection]

# Either in scala

``` scala
sealed abstract class Either[+A, +B] extends Product with Serializable
final case class Left[+A, +B](value: A) extends Either[A, B]
final case class Right[+A, +B](value: B) extends Either[A, B]
```

# Either example

``` scala
sealed trait ConnectionError
object UnableToCreateSocket extends ConnectionError
object UnableToBindSocket extends ConnectionError
object UnableToAcceptConnection extends ConnectionError
....

def socket(): Either[ConnectionError, Socket]
def bind(socket: Socket): Either[ConnectionError, BoundSocket]
def accept(socket: BoundSocket): Either[ConnectionError, Connection]
```

# Either example

``` scala
def single_connection(source: Source): Either[ConnectionError, Connection] =
  socket() match {
    case Right(socket) => bind(socket) match {
      case Right(boundSocket) => accept(boundSocket)
      case err => err
    }
    case err => err
  }
```

# Either example

``` scala
def single_connection(source: Source): Either[ConnectionError, Connection] =
  socket().flatMap(bind).flatMap(accept)
```

# Either example

``` scala
def single_connection(source: Source): Either[ConnectionError, Connection] =
  for {
    sock <- socket()
    boundSocket <- bind(sock)
    connection <- accept(boundSocket)
  } yield connection
```

# Either example

``` scala
def parseMessage(buffer: Array[Byte],
                 pubkey: PublicKey): Either[ParseError, Message] =
  parse(buffer) match {
    case Some(message) =>
      if checkSignature(message, pubkey) Right(message)
      else Left(InvalidSignature)
    case _ => Left(InvalidMessage)
  }
```

# Try

  * java and scala
  * Either for exceptional languages

# Try

``` scala
sealed abstract class Try[+T] extends Product with Serializable
final case class Success[+T](value: T) extends Try[T]
final case class Failure[+T](exception: Throwable) extends Try[T]
```

# Try

``` scala
def spamUsingExceptionalJava(url: Url): Try[List[EmailAddresses]] = Try {
  val rawData: Array[Byte] = javaGet(url)
  val page: Html = javaParsePage(rawData)
  extractEmailAddresses(page)
}

spamUsingExceptionalJava("http://hup.hu") match {
  case Success(addresses) => spam(addresses)
  case Failure(thr) => log(thr.message)
}
```

