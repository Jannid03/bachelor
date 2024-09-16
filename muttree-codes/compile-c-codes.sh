# eob.so
g++ -I/usr/share/R/include -DNDEBUG -Irbgl_trimmed_boost_1_49_0 -fpic -O3 -pipe -g -c edmonds_optimum_branching_max.cpp -o eobmax.o
g++ -I/usr/share/R/include -DNDEBUG -Irbgl_trimmed_boost_1_49_0 -fpic -O3 -pipe -g -c edmonds_optimum_branching_min.cpp -o eobmin.o
g++ -shared -o eob.so eobmax.o eobmin.o -L/usr/lib/R/lib -lR
rm -f eobmax.o eobmin.o 

# single-cell-lib.so
R CMD SHLIB single-cell-lib.c

rm -f *.o
