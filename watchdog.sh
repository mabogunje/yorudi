#!/bin/bash

while true; do
    # Check if the sbt process is running
    if ! pgrep -f "sbt run" > /dev/null; then
        echo "sbt process not found. Restarting..."
        sbt run &
    fi
    # Wait for 5 seconds before checking again
    sleep 5
done
