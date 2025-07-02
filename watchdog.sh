#!/bin/bash

while true; do
    # Check if the sbt process is running
    if ! pgrep -f "sbt" > /dev/null; then
        echo "sbt process not found. Restarting..."
        sbt run &
    fi
    # Wait for 60 seconds before checking again
    sleep 60
done
