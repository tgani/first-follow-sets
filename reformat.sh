#!/bin/sh
for ff in *.cpp *.h
do
    clang-format $ff > $ff.reformatted
    if [ $? -eq 0 ]
    then
        mv $ff $ff.before-formatting
        cp $ff.reformatted $ff
    else
        echo clang-format failed
        rm $ff.reformatted
    fi
done

	
		  
