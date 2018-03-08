#! /bin/bash
cd ./test/lexer-outputs/

for file in ../inputs/*.in; do
  name=$(basename $file)
  noextname=${name%.*}
  echo $noextname
  echo $name
  cd lexer-outputs
  eval stack exec aardvark-exe \-\- -l -r $file > $noextname.lex
  cd ../parser-outputs
  eval stack exec aardvark-exe \-\- -p -r $file > $noextname.parsed
  cd ../eval-outputs
  eval stack exec aardvark-exe \-\- -r $file > $noextname.evaluated
  cd ..
done

cd ..

exit
