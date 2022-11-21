for filename in `ls ./test_cases | grep .swamp`
do
	echo $filename
	echo -en "\t"
	cat "./test_cases/${filename}" | grep "Expected value:"
	echo -en "\t"
	./calc < "./test_cases/${filename}"
done