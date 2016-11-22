#!/usr/bin/env bash
BIN_GOAL="goal"

IFS= read -r form

GOAL_SCRIPT="\$nba = translate QPTL -t nbw -sg \"$form\"; 
             save \$nba -c HOAF /dev/stdout;"
             
exec $BIN_GOAL batch /dev/stdin <<< ${GOAL_SCRIPT}
