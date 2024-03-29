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

globallog=testall.log
rm -f $globallog
error=0
globalerror=0
globalerrorcount=0

keep=0
sast=0

Usage() {
    echo "Usage: autotest.sh [options] [.swamp files]"
    echo "-k    Keep intermediate files"
	echo "-s	Test sast output"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

# RunFail <args>
# Report the command, run it, and expect an error
RunFail() {
    echo $* 1>&2
    eval $* && {
	SignalError "failed: $* did not report an error"
	return 1
    }
    return 0
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.swamp//'`
    reffile=`echo $1 | sed 's/.swamp$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

	if [ "$sast" -eq 0 ]
    then
    generatedfiles="$generatedfiles ${basename}.ll ${basename}.out ${basename}.s ${basename}.exe" &&
    Run "$SWAMP" "-l $1" ">" "${basename}.ll" &&
    Run "$LLC" "-relocation-model=pic" "${basename}.ll" ">" "${basename}.s" &&
    Run "$CC" "-o" "${basename}.exe" "${basename}.s" "$CFUNCS" &&
    Run "./${basename}.exe" > "${basename}.out" &&
    Compare ${basename}.out ${reffile}.out ${basename}.diff
	else
    generatedfiles="$generatedfiles ${basename}.sast" &&
    Run "$SWAMP" "-s $1" ">" "${basename}.sast" &&
    Compare ${basename}.sast ${reffile}.sast ${basename}.diff
	fi

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
	globalerrorcount=$((globalerrorcount + 1))
    fi
}

CheckFail() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.swamp//'`
    reffile=`echo $1 | sed 's/.swamp$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.err ${basename}.diff" &&
    RunFail "$SWAMP -s" "<" $1 "2>" "${basename}.err" ">>" $globallog &&
    Compare ${basename}.err ${reffile}.err ${basename}.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
	globalerrorcount=$((globalerrorcount + 1))
    fi
}

while getopts kdpsh c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
	s) # Run in SAST mode
		sast=1
		;;
	h) # Help
	    Usage
	    ;;
    esac
done

shift `expr $OPTIND - 1`

LLIFail() {
  echo "Could not find the LLVM interpreter \"$LLI\"."
  echo "Check your LLVM installation and/or modify the LLI variable in autotest.sh"
  exit 1
}

which "$LLI" >> $globallog || LLIFail

if [ ! -f "$CFUNCS" ]
then
    echo "Could not find irgen.o"
    echo "Remember to run make!"
    exit 1
fi

if [ $# -ge 1 ]
then
    files=$@
else
    files="test_cases/test-*.swamp test_cases/bad-*.swamp"
fi

for file in $files
do
    case $file in
	*test-*)
	    Check $file 2>> $globallog
	    ;;
	*bad-*)
	    CheckFail $file 2>> $globallog
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done

if [ $globalerrorcount -eq 0 ] ; then
	echo "All cases passed!"
else
	echo "\nFAILED $globalerrorcount cases"
fi

exit $globalerror