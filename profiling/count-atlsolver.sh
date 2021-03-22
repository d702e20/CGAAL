#!/bin/bash

#TODO; do better parser for atlsolver args (model, formula, threads, etc)
if [ $# -eq 0 ]; then
    printf "Error: Please specify output count log file\n"
    printf "usage: ./count-atlsolver.sh <count_log_file>\n"
    exit 1
fi

# run atlsolver and redirect stderr to tmp
./target/release/atl-checker solver --formula docs/lcgs/working-examples/Mexican_Standoff/Mexican_Standoff_p1_is_alive_till_he_aint.json --model docs/lcgs/working-examples/Mexican_Standoff/Mexican_Standoff_4_1hp.lcgs --model-type lcgs 2> $1.tmp

# grep matching prefix stderr into given output
grep -e "^worker" $1.tmp > $1

# remove tmp
rm $1.tmp
