# Useful commands

# Copy all hs files from text package renamed.
for i in $(find ../ext/text/|grep hs$|grep Data); do NAME=$(echo $i | tr '/' '.' | sed 's/.*text.//'); cp $i txtsrc/$NAME; done

# Warnings breakdown.
grep -o 'data-warning=.[a-zA-Z]\+' deploy/Data.Text.*html | cut -d\" -f2 | sort | uniq -c | sort -nrk1

# Parse failures breakdown
# by error
cat out3 | grep ParseFailed | grep -o ').*' | sort | uniq -c | sort -nk1
# by package
cat out3 | grep ParseFailed  | grep -o '/home/[^\\]*' | grep -o '^[a-zA-Z/-]*' | sort | uniq -c | sort -nk1

# Running
./nemnem-genstatic/dist/build/nemnem-genstatic/nemnem-genstatic files.txt +RTS -p
