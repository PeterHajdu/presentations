---
author: 'Peter Hajdu'
title: 'Ethernet'
...

# IP basics

 * hosts communicate by exchanging IP packets
 * every host is identified
 * IPv4 address
 * netmask
 * network address

# IP address

 * 192.168.12.123
 * 255.255.255.0 = 11111111.11111111.11111111.00000000
 * 192.168.12.123/24
 * network / broadcast?

# IP address

 * 192.168.12.0
 * 192.168.12.255
 * ipcalc

# IP / Ethernet

 * hosts communicate directly if they are on the same network

# Ethernet

 * hosts communicate by exchanging frames
 * every host is uniquely identified (MAC 6 octet)
 * unicast / broadcast (ff:ff:ff:ff:ff:ff)
 * NIC / MAC / promiscuous mode

# Ethernet II frame

```
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|      1        |       2       |       3        |       4       |       5        |       6     |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                      Destination Address                                      |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                         Source Address                                        |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|           EtherType           |                                                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                                                               +
|                                                                                               |
+                                            Payload                                            +
|                                                                                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                              CRC                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
```

 * MAC address (Media Access Control)
 * CRC (cyclic error-correcting codes)

# ARP

 * Address Resolution Protocol
 * translates IP addresses to MAC addresses
 * request / reply
 * forwarding table (arp -n)

# ARP frame in case of Ethernet / IPv4

 * relations to ethernet frame

```
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|      1        |       2       |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|  Hardware type (HTYPE)        |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|  Protocol type (PTYPE)        |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|  HLEN         | PLEN          |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|     Operation                 |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                               |
| Sender hardware address (SHA) |
|                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
| Sender protocol address (SHA) |
|                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                               |
| Target hardware address (SHA) |
|                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
| Target protocol address (SHA) |
|                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

# Exercise 1

 * Install relevant tools before starting the exercises: tshark, wireshark, arpspoof, arpscan, ipcalc
 * Disable every network management services to avoid confusion. (systemctl stop NetworkManager, disable docker -> reboot)
 * Choose a network that is just enough for two hosts.
 * Connect two machines with an ethernet cable.
 * Identify NIC that is connected to the other machine. (mii-tool)
 * Give an IP address from the given network to each machine. (ip addr add <IP/NETMASK> dev <NIC>)
 * Check your forwarding table. (arp -n) (It should be empty.)
 * Start sniffing on the relevant NIC. (tshark -i <NIC>, ip link set <NIC> up)
 * Ping the other machine.
 * Stop sniffing.

 * What is the MAC address of your pair?
 * What is the operation code of an ARP request?
 * What is the operation code of an ARP reply?
 * What is the hardware type of ethernet?
 * What is the protocol type of IPv4?

# Switch

 * interconnects network hosts
 * maintains FIB (forwarding information base)

# Exercise 2

 * Delete the previous address from your interface. (ip addr del)
 * Connect all machines to the switch.
 * Choose a network that is just enough for the hosts.
 * Give an IP address from the network for each host on the network.
 * Try pinging every other host on the network.
 * Check your forwarding table.
 * In groups of 3 decide who takes which role: Alice, Bob, Malory
 * Alice starts an ssh server.
 * Bob starts an ssh connection to the server. (Accept the host key.)
 * How could Malory inpersonate Alice?
 * Malory adds Alice's IP address to his interface.
 * Malory starts an SSH server.
 * Malory starts arp spoofing the network with Alices address.
 * Check arp forwarding tables.
 * Bob starts another ssh connection.
 * Is it possible to detect arp spoofing?
 * arpscan
