#! /bin/bash

declare -a arr
declare -a inputs
arr=("help.out" "length-flag.out" "length-no-args.out" "no-flag.out" "no-input.out")
inputs=("--help" "--length a aa aaa aaaa aaaaa aaaaaa" "--length" "a aa aar aard aardv aardva" "")

numTests=5
beep=1

echo "Testing differences between expected and actual outputs"
echo "No difference listed under a test indicates the test has passed"
echo

for (( c=0; c<$numTests; c++ ))
do  
   echo "Testing: ${arr[$c]}"
   OUTPUT_CMD="stack exec aardvark -- ${inputs[$c]}"
   DIFF_OUTPUT=$(diff <($OUTPUT_CMD) "./test/expected-outputs/${arr[$c]}")
   LEN=${#DIFF_OUTPUT}
   # Diff will at least leave a newline in its output, so we check if there's more than that newline
   if [[ LEN -gt 1 ]]
    then
      echo "Test failed: ${arr[$c]}"
      exit 1
    else
      echo "Test passed: ${arr[$c]}"
   fi
done
