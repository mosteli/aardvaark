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
  eval stack exec aardvark-exe \-\- -l -r $file > temp.test
  diff temp.test $noextname.lex
  rm temp.test
  cd ../parser-outputs
  eval stack exec aardvark-exe \-\- -p -r $file > temp.test
  diff temp.test $noextname.parsed
  rm temp.test
  cd ../eval-outputs
  eval stack exec aardvark-exe \-\- -r $file > temp.test
  diff temp.test $noextname.evaluated
  rm temp.test
done

exit
