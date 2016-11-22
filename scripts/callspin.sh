#!/usr/bin/env bash

set -m

trap 'kill %%' TERM

spin -F /dev/stdin | autfilt -H & wait
