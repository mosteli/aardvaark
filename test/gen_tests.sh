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

cd "expected-outputs"

for (( c=0; c<$numTests; c++ ))
do
   echo "Testing Lexer Output: ${arr[$c]}"
   OUTPUT_CMD="stack exec aardvark-exe -- -l \"${inputs[$c]}\" > ${arr[$c]}.lex.out"
   eval $OUTPUT_CMD
done

for (( c=0; c<$numTests; c++ ))
do
   echo "Testing Parser Output: ${arr[$c]}"
   OUTPUT_CMD="stack exec aardvark-exe -- -p \"${inputs[$c]}\" > ${arr[$c]}.parse.out"
   eval $OUTPUT_CMD
done


for (( c=0; c<$numTests; c++ ))
do
   echo "Testing Program Evaluation: ${arr[$c]}"
   OUTPUT_CMD="stack exec aardvark-exe -- \"${inputs[$c]}\" > ${arr[$c]}.out"
   eval $OUTPUT_CMD
done

cd ..
