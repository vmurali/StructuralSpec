#!/bin/bash

if [ -z "$1" ]
then
  LIB=single
else
  LIB=$1
fi

cd bsv && for file in ../Programs/*
do
  ln -sf ${file} ./memory.vmh
  if [[ ${file} != ../Programs/median* && ${file} != ../Programs/multiply* && ${file} != ../Programs/qsort* && ${file} != ../Programs/towers* && ${file} != ../Programs/vvadd* ]]
  then
    echo ${file}
    cd ${LIB} && ./a.out && cd ..
  fi
done
rm memory.vmh

echo $LIB
