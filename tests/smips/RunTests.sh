#!/bin/bash

if [ -z "$1" ]
then
  LIB=single
else
  LIB=$1
fi

cd bsv && for file in ../programs/*
do
  ln -sf ${file} ./memory.vmh
  if [[ ${file} == ../programs/smips* ]]
  then
    echo ${file}
    cd ${LIB} && ./a.out && cd ..
  fi
done
rm memory.vmh

echo $LIB
