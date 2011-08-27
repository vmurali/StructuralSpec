#!/bin/bash

if [ -z "$1" ]
then
  LIB=single
else
  LIB=$1
fi

cd bsv && for file in ../Programs/*.b
do
  if [[ ${file} == ../Programs/median* || ${file} == ../Programs/multiply* || ${file} == ../Programs/qsort* || ${file} == ../Programs/towers* || ${file} == ../Programs/vvadd* ]]
  then
    ln -sf ${file} ./memory.vmh
    echo ${file}
    cd ${LIB} && ./a.out && cd ..
  fi
done
rm -f memory.vmh

echo $LIB
