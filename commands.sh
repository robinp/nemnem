# Useful commands

# Copy all hs files from text package renamed.
for i in $(find ../ext/text/|grep hs$|grep Data); do NAME=$(echo $i | tr '/' '.' | sed 's/.*text.//'); cp $i txtsrc/$NAME; done

# Warnings breakdown.
grep -o 'data-warning=.[a-zA-Z]\+' deploy/Data.Text.*html | cut -d\" -f2 | sort | uniq -c | sort -nrk1
