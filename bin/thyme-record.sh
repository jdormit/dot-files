#!/bin/bash

while true; do
    /home/jeremy/bin/thyme-track.sh
    date="$(date +%F_%T)"
    echo "$date -- Logged activity"
    sleep 30s
done

      
