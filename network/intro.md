---
author: 'Peter Hajdu'
title: 'Introduction to networking'
...

# History

  * 1969 First four nodes of the ARPANET
  * 1971 First e-mail
  * 1973 ethernet
  * 1973 FTP
  * 1974 TCP specification
  * 1983 ARPANET uses IP / TCP
  * government-sponsored (ARPANET in the US, CYCLADES in France)
  * vendor-developed with proprietary standards
  * 1984 the OSI model is published

# OSI

  * protocol layering
  * responsibilities and services are divided into layers
  * entity interacts directly only with the layer immediately beneath it
  * entity provides facilities for use by the layer above it

# OSI model layers

```
7   Application   High-level APIs, including resource sharing, remote file access
6   Presentation  Translation of data between a networking service and an application;
                  including character encoding, data compression and encryption/decryption
5   Session       Managing communication sessions, i.e., continuous exchange of information
                  in the form of multiple back-and-forth transmissions between two nodes
4   Transport     Reliable transmission of data segments between points on a network,
                  including segmentation, acknowledgement and multiplexing
3   Network       Structuring and managing a multi-node network, including addressing,
                  routing and traffic control
2   Data link     Reliable transmission of data frames between two nodes connected by a
                  physical layer
1   Physical      Transmission and reception of raw bit streams over a physical medium
```

# Protocol examples, PDUs

```
5 Application   HTTP, XMPP      Data
4 Transport     TCP, UDP, SCTP  Segments, Datagrams
3 Network       IP, ICMP        Packets
                  ARP
2 Data link     Ethernet        Frames
1 Physical
```

# Layering example HTTP

```
5 Application   |GET index.html|
4 Transport     |TCP header|HTTP Payload|
3 Network       |IP header|TCP header|HTTP Payload|
2 Data link     |Ethernet header|IP header|TCP header|HTTP Payload|Ethernet frame end|
1 Physical
```

 * encapsulation
 * decapsulation?
