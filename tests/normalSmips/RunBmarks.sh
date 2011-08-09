#!/bin/bash

for file in ../smips/programs/*
do
  ln -sf ${file} ./memory.vmh
  if [[ ${file} != ../smips/programs/smips* ]]
  then
    echo ${file}
    cd bsv && ./a.out && cd ../
  fi
done
rm memory.vmh
