git reset --hard origin/main

find -not -wholename './.git*' | rename 's/facai/harvest/g'

find -not -wholename './.git*' -not -name rename.sh -not -name CHANGELOG.md | \
while read f ; do
echo '
1,$s/facai/harvest/g
1,$s/Facai/Harvest/g
1,$s/:as fk/:as hk/g
1,$s/fk\//hk\//g
1,$s/:as f\]/:as h\]/g
1,$s/(f\//(h\//g
1,$s/\[fk\//\[hk\//g
wq
' | ed $f
done
