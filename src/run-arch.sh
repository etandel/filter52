#!/usr/bin/sh

RESOLVCONF=/etc/resolv.conf
BACKUP="$RESOLVCONF".backup

cp $RESOLVCONF $BACKUP
trap "mv $BACKUP $RESOLVCONF" EXIT
echo "nameserver 127.0.0.1" > $RESOLVCONF
./filter52 $BACKUP
