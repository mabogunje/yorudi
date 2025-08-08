#!/bin/bash

while true; do
    # Check if the sbt process is running
    if ! pgrep -f "sbt" > /dev/null; then
        echo "sbt process not found. Restarting..."
        JAVA_OPTS="-Xmx256m" sbt "runMain YorubaRestService" &
    fi
    # Wait for 60 seconds before checking again
    sleep 60
done
