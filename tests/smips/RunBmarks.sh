#!/bin/bash

for file in tests/*
do
  ln -sf ${file} ./memory.vmh
  if [[ ! ${file} == tests/smips* ]]
  then
    echo ${file}
    cd bsv && ./a.out && cd ../
  fi
done
