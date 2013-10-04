#!/bin/sh

while true; do { echo -e 'HTTP/1.1 200 OK\r\n'; cat index.html; } | sudo nc -l ::1 80; done

