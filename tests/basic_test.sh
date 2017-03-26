#!/bin/bash

echo "Start script"

for i in $(seq 1 10)
do
    curl http://localhost:8000 | tee "$i.log"
done

sleep 10

for i in $(seq 1 10)
do
    grep -Fxq "/$i" "$i.log" || ( echo "File $i.log contains improper data"; cat "$i.log"; )
done

