#!/bin/sh
NO=`cat cnt`
echo `expr $NO + 1` > cnt
NO=`printf %04d $NO`
EXPERIMENT_NAME=$1
cp -r "EXPERIMENTS/base" "EXPERIMENTS/$NO - $EXPERIMENT_NAME"
