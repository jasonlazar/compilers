#/bin/sh
./tony < $1 > a.ll
llvm-as-9 < a.ll -o=plot.dot | opt-9 -analyze -view-cfg
dot -Tpng plot.dot -o plot.png
