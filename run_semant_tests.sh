for filename in `ls ./test_cases | grep .swamp`
do
	echo $filename
	printf "\t"
	./swamp.native -s < "./test_cases/${filename}"
done