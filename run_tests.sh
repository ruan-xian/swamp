echo "Expected results: not_found 12 21"
for filename in `ls ./test_inputs | grep .swamp`
do
	./calc < "./test_inputs/${filename}"
done