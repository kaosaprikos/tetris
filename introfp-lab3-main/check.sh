#!/usr/bin/env bash

echo "=== Part A ==="
if [ -f CheckPartA.hs ]; then runghc CheckPartA.hs; else echo "Part A missing"; fi

echo "=== Part B ==="
if [ -f CheckPartB.hs ]; then runghc CheckPartB.hs; else echo "Part B missing"; fi

echo "=== Part C ==="
if [ -f CheckPartC.hs ]; then runghc CheckPartC.hs; else echo "Part C missing"; fi

