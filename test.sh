#!/bin/bash

TMP_OUT=$(mktemp)
TMP_ERR=$(mktemp)

for file in ./good/*.myl
do

echo "Run test: " 
echo ${file%.myl}

./interpreter $file > $TMP_OUT 2 > $TMP_ERR

cat $TMP_ERR

if diff ${file%myl}out $TMP_OUT > /dev/null;
then echo -e "OK IN OUTPUT"
else echo -e "ERROR IN OUTPUT"
fi

if diff ${file%myl}err $TMP_ERR > /dev/null;
then echo -e "OK IN ERROR"
else echo -e "ERROR IN ERROR"
fi

done