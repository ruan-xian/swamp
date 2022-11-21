for filename in `ls ./test_cases | grep .swamp`
do
	echo $filename
	echo -n "\t"
	./print_test.native < "./test_cases/${filename}"
done