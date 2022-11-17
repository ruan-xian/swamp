for filename in `ls ./test_inputs | grep .swamp`
do
	echo $filename
	echo -n "\t"
	cat "./test_inputs/${filename}" | grep "Expected value:"
	echo -n "\t"
	./calc < "./test_inputs/${filename}"
done