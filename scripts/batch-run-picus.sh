#!/bin/bash
# usage: this.sh <timeout:seconds> <log_path> <target_folder>
# example: ./batch-run-picus.sh z3 600 ./logs/test/ ./benchmarks/circomlib-cff5ab6/

solver=$1
otime=$2
logpath=$3
targetfolder=$4

mkdir -p ${logpath}

for fp in ${targetfolder}/*.r1cs
do
	fn=$(basename ${fp})
	bn="${fn%.*}"
    bp="${fp%.*}"
	echo "=================== checking: ${fn} ==================="
	echo "====   start: $(date -u)"
	st="$(date -u +%s)"

    timeout ${otime} racket ./picus.rkt --timeout 5000 --solver ${solver} --r1cs ${fp} > ${logpath}/${bn}.log 2>&1

	et="$(date -u +%s)"
	echo "====     end: $(date -u)"
	ct="$(($et-$st))"
	echo "==== elapsed: ${ct} seconds"
done
