#!/bin/bash

cd bsv && for file in ../../smips/tests/*
do
  ln -sf ${file} ./memory.vmh
  if [[ ${file} == ../../smips/tests/smips* ]]
  then
    echo ${file}
    cd multi && ./a.out && cd ../
  fi
done
