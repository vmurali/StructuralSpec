#!/bin/bash

for file in ../smips/tests/*
do
  ln -sf ${file} ./memory.vmh
  if [[ ! ${file} == ../smips/tests/smips* ]]
  then
    echo ${file}
    cd bsv && ./a.out && cd ../
  fi
done
