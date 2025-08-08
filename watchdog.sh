#!/bin/bash

while true; do
    # Check if the sbt process is running
    if ! pgrep -f "sbt" > /dev/null; then
        echo "sbt process not found. Restarting..."
        nohup sbt -J-Xmx512m "runMain YorubaRestService" > yorudi.log 2>&1 & echo $!/
    fi
    # Wait for 60 seconds before checking again
    sleep 60
done
