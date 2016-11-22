#!/usr/bin/env bash
BIN_GOAL="goal"
option="$1"

GOAL_SCRIPT="\$nba = load -c HOAF /dev/stdin; 
             \$dpa = determinization -m piterman ${option} \$nba; 
             save \$dpa -c HOAF /dev/stdout;"

exec "${BIN_GOAL}" batch "${GOAL_SCRIPT}"
