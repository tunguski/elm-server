#!/bin/bash

echo "Start script"

ps auxwf | grep node

for i in $(seq 1 10); do curl http://localhost:8080 | tee "$i.log" ; done

sleep 10

for i in $(seq 1 10); do grep -Fxq "/$i" "$i.log" || ( echo "File $i.log contains improper data"; cat "$i.log"; ) ; done

