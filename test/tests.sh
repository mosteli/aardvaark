#! /bin/bash

declare -a arr
declare -a inputs
arr=("help" "one" "plus" "sub" "mul" "div" "nested" "unnested" "floatunnest" "iftrue" "iffalse" "iflte" "NaNPlusNaN" "NaNPlusFloat")
inputs=("--help" "1" "(1+3)" "(1-3)" "(1*3)" "(1.0/3)" "(1+(1*(1+(4.0/5))))" "(1/3)+(2/5)" "(1/3)+(2.0+4)" "iftruethen1else3" "iffalsethen1else3" "if1<=3then1else3" "NaN+NaN" "NaN+3.0")
# Not sure why the Bash script doesn't like spaces in the input strings...

numTests=14
beep=1

echo "Testing differences between expected and actual outputs"
echo "No difference listed under a test indicates the test has passed"
echo

for (( c=0; c<$numTests; c++ ))
do
   echo "Testing Lexer Output: ${arr[$c]}"
   OUTPUT_CMD="stack exec aardvark-exe -- -l ${inputs[$c]}"
   DIFF_OUTPUT=$(diff <($OUTPUT_CMD) "./test/expected-outputs/${arr[$c]}.lex.out")
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

for (( c=0; c<$numTests; c++ ))
do
   echo "Testing Parser Output: ${arr[$c]}"
   OUTPUT_CMD="stack exec aardvark-exe -- -p ${inputs[$c]}"
   DIFF_OUTPUT=$(diff <($OUTPUT_CMD) "./test/expected-outputs/${arr[$c]}.parse.out")
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


for (( c=0; c<$numTests; c++ ))
do
   echo "Testing Program Evaluation: ${arr[$c]}"
   OUTPUT_CMD="stack exec aardvark-exe -- ${inputs[$c]}"
   DIFF_OUTPUT=$(diff <($OUTPUT_CMD) "./test/expected-outputs/${arr[$c]}.out")
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
