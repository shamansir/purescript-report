#!/bin/bash
if [ $# -ge 1 ]; then
    spago run -m Report.Cli --backend-args "$1 $2 $3 $4 $5 $6 $7 $8 $9"
else
    spago run -m Report.Cli
fi