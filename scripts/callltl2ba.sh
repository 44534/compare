#!/usr/bin/env bash

set -m

trap 'kill %%' TERM


ltl2ba -F /dev/stdin | autfilt -H & wait
