#/bin/sh
./tony < $1 > a.ll
clang-9 a.ll libs/edsger_lib-master/lib.a -o a.out -lgc
