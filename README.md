About
---

`filter52` is a simple DNS server written in Haskell that proxies DNS queries to other name servers, but filters out IPs (IPv4) that start with "52".


Building
---

`cabal build` will generate the binary `./sdist/filter52`.
Alternatively, you may use CMake and just run `make`.


Running
---

Generally, running the server requires 2 steps:

  - Changing your default resolver to 127.0.0.1;
  - Running the server as root passing as its single argument a file containing a list of name servers (see the included resolv.conf file as an example).

System specific instructions follow:

### Arch Linux

A "pure" Arch setup will use `netctl` for networking.
If that's your case  - that is, you don't use network-manager, wicd etc. -, the DNS "surgery" is done via `/etc/resolv.conf` and you can use the `run-arch.sh` script that automatically configures `filter52` and restores the system's original DNS settings when it shuts down.

### Ubuntu

Because by default desktop Ubuntu already ships with a local resolver (`dnsmask`), you must disable it before running `filter52`:

  - Comment the `dns=dnsmasq` line at `/etc/NetworkManager/NetworkManager.conf`;
  - `sudo restart network-manager`

After you close `filter52`, you must do the inverse so your system returns to its original settings: uncomment the dnsmasq line and restart network-manager.


Author
---

`filter52` was created by Elias Tandel, though the code is extensivelly based on [hosts-server](https://hackage.haskell.org/package/hosts-server).


License
---

`filter52` is licensed under GPLv3. See LICENSE for more details.

