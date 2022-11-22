for filename in `ls ./test_cases | grep .swamp`
do
	echo $filename
	printf "\t"
	./print_test.native < "./test_cases/${filename}"
done