for filename in `ls ./test_cases | grep .swamp`
do
	echo $filename
	printf "\t"
	cat "./test_cases/${filename}" | grep "Expected value:"
	printf "\t"
	./calc < "./test_cases/${filename}"
done