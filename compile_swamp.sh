#!/bin/sh

# Regression testing script for Swamp
# Step through a list of files
#  Compile, run, and check the output of each expected-to-work test
#  Compile and check the error of each expected-to-fail test

# Path to the LLVM interpreter
LLI="lli"
(which "$LLI" > /dev/null) || LLI="/usr/local/opt/llvm@14/bin/lli"
(which "$LLI" > /dev/null) || LLI="/opt/homebrew/opt/llvm/bin/lli"
(which "$LLI" > /dev/null) || LLI="/opt/homebrew/Cellar/llvm@14/14.0.6/bin/lli"

# Path to the LLVM compiler
LLC="llc"
(which "$LLC" > /dev/null) || LLC="/usr/local/opt/llvm@14/bin/llc"
(which "$LLC" > /dev/null) || LLC="/opt/homebrew/opt/llvm/bin/llc"
(which "$LLC" > /dev/null) || LLC="/opt/homebrew/Cellar/llvm@14/14.0.6/bin/llc"

# Path to the C compiler
CC="cc"

# Path to the swamp compiler. 
SWAMP="./swamp"
CFUNCS="./irgen.o"

# Set time limit for all operations
ulimit -t 30

basename=`echo $1 | sed 's/.*\\///
							s/.swamp//'`

echo -n "Compiling $basename..."

eval "$SWAMP" "-l $1" ">" "${basename}.ll" &&
eval "$LLC" "-relocation-model=pic" "${basename}.ll" ">" "${basename}.s" &&
eval "$CC" "-o" "${basename}.exe" "${basename}.s" "$CFUNCS"

rm -f ${basename}.ll; 
rm -f ${basename}.s;

printf "Done\n"

exit 0