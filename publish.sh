#bin/bash

echo "ENV -> "$1
echo "Bump version to -> "$2
echo "Comment -> "$3

read -p "Is this a expected publish info (y/n)? " answer

if [ ${answer:0:1} != "y" ] ; then
  echo "Exiting "
  exit 1
fi

echo "<PUBLISH> Running UT"
stack test

if [ $? -ne 0 ]
then
  echo "<PUBLISH> Failing on UT"
  exit 2
fi

# updating version on api endpoint
echo "<PUBLISH> Update Version"
#echo $rp_cmd
sed -i '' -e "s/Version \".*\"/Version \"$2\"/g"  app/Main.hs

if [ $? -ne 0 ]
then
  echo "<PUBLISH> Failed to update version"
  exit 3
fi

echo "<PUBLISH> Tagging"
git add app/Main.hs
git add swagger.json
git commit -m "bump version to-> < $2 >"
git tag -a $1$2 -m "$3"

if [ $? -ne 0 ]
then
  echo "<PUBLISH> Failed to add Tag"
  exit 4
fi

echo "<PUBLISH> Pushing Tag"
git push --tag

if [ $? -ne 0 ]
then
  echo "<PUBLISH> Failed to push Tag"
  exit 5
fi

echo "<PUBLISH> Pushing "
git push origin HEAD

if [ $? -ne 0 ]
then
  echo "<PUBLISH> Failed to push Tag"
  exit 6
fi

echo "<PUBLISH> Done ! "

# Generate change log file 
# echo "<CHANGE LOG> Update Change Log "
# python generate_changelogs.py CHANGELOGS.json $1$2
echo "<CHANGE LOG> Upload Change Log "
scp ChangeLog.md root@simplicity.vip:/root/absbox.org/ChangeLog.md
#scp CHANGELOGS.json root@simplicity.vip:/root/absbox.org/CHANGELOGS.json

