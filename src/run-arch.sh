#!/usr/bin/sh

RESOLVCONF=/etc/resolv.conf
cp $RESOLVCONF{,.backup}
trap "mv $RESOLVCONF{.backup,}" EXIT
echo "nameserver 127.0.0.1" > $RESOLVCONF
./filter52 $RESOLVCONF
