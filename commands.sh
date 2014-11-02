# Useful commands

# Copy all hs files from text package renamed.
for i in $(find ../ext/text/|grep hs$|grep Data); do NAME=$(echo $i | tr '/' '.' | sed 's/.*text.//'); cp $i txtsrc/$NAME; done

# Warnings breakdown.
grep -o 'data-warning=.[a-zA-Z]\+' deploy/Data.Text.*html | cut -d\" -f2 | sort | uniq -c | sort -nrk1

# Parse failures breakdown
# by error
cat out | grep ParseFailed | grep -o ').*' | sort | uniq -c | sort -nk1
# by package
cat out | grep ParseFailed  | grep -o '/home/[^\\]*' | grep -o '^[a-zA-Z/-]*[0-9\.]*' | sort | uniq -c | sort -nk1

# Running
./nemnem-genstatic/dist/build/nemnem-genstatic/nemnem-genstatic files.txt +RTS -p

# To get common defines
find pull |grep hs$ | xargs grep '^#if' | cut -d: -f2 | sort -u

grep 'compat(a,b,c)' out | grep -o '/home.*line [0-9]\+' | cut -d\  -f1,5 | tr ' ' ':' > compatlocs
for i in $(cat compatlocs); do F=$(echo $i | cut -d: -f1); L=$(echo $i | cut -d: -f2); echo === $F; sed -n ${L}p $F; done > problematic
