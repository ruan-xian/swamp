for filename in `ls ./test_cases | grep .swamp`
do
	echo $filename
	echo -n "\t"
	cat "./test_cases/${filename}" | grep "Expected value:"
	echo -n "\t"
	./calc < "./test_cases/${filename}"
done