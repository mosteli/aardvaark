#! /bin/bash

echo "Testing differences between expected and actual outputs."
echo "All differences between expected and actual outputs will be printed."
echo

cd ./test/lexer-outputs

for file in ../inputs/*.in; do
  cd ..
  name=$(basename $file)
  noextname=${name%.*}
  echo $noextname
  cd lexer-outputs

  echo "Testing Lexing"
  eval stack exec aardvark-exe \-\- -l -r $file > temp.test
  diff temp.test $noextname.lex > diff.output
  size=$(wc -c <"diff.output")
  if [ $size -gt 0 ]; then
      echo "Failed test."
      echo "Expected:"
      cat $noextname.lex
      echo "Actual:"
      cat temp.test
      exit 2
  fi
  rm temp.test

  echo "Testing parsing"
  cd ../parser-outputs
  eval stack exec aardvark-exe \-\- -p -r $file > temp.test
  diff temp.test $noextname.parsed > diff.output
  size=$(wc -c <"diff.output")
  if [ $size -gt 0 ]; then
      echo "Failed test."
      echo "Expected:"
      cat $noextname.parsed
      echo "Actual:"
      cat temp.test
      exit 2
  fi
  rm temp.test

  echo "Testing evaluation"
  cd ../eval-outputs
  eval stack exec aardvark-exe \-\- -r $file > temp.test
  diff temp.test $noextname.evaluated > diff.output
  size=$(wc -c <"diff.output")
  if [ $size -gt 0 ]; then
      echo "Failed test."
      echo "Expected:"
      cat $noextname.evaluated
      echo "Actual:"
      cat temp.test
      exit 2
  fi
  rm temp.test
done

exit
