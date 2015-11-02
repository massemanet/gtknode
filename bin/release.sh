#!/bin/bash

function usage () {
    echo "$1"
    exit
}

APPSRC=`echo $PWD/src/*.app.src`
BRANCH=$(git rev-parse --abbrev-ref HEAD)
TAG=$(git name-rev --tags --name-only $(git rev-parse HEAD))
OVSN=$(git describe | cut -f1 -d"-")

[ -f $APPSRC ]            || usage "No .app.src file"
[ "$BRANCH" != "master" ] && usage "Not on master"
[ "$TAG" != "undefined" ] && usage "Already tagged"
[ "$OVSN" = "" ]          && usage "No old version"

if [ -z "$1" ]; then
    size="patch"
else
    size=$1
fi

MAJOR=`echo $OVSN | cut -f1 -d"."`
MINOR=`echo $OVSN | cut -f2 -d"."`
PATCH=`echo $OVSN | cut -f3 -d"."`
if [ "$size" == "major" ]; then
    NVSN=$(($MAJOR + 1)).0.0
elif [ "$size" == "minor" ]; then
    NVSN=$MAJOR.$(($MINOR + 1)).0
elif [ "$size" == "patch" ]; then
    NVSN=$MAJOR.$MINOR.$(($PATCH + 1))
else
    usage "$0 major|minor|patch"
fi
echo $OVSN"->"$NVSN

sed  "s/[0-9]\.[0-9]\.[0-9]/$NVSN/" < $APPSRC > $$ && mv $$ $APPSRC
git add $APPSRC
git commit -m"v$NVSN"
git log --name-only --no-merges | grep -Ev '^[ ]+$$|git-svn-id' > ChangeLog
echo " Mats Cronqvist <masse@cronqvi.st>" > AUTHORS
git log | grep Author | grep -Evi "vagrant|no author|mats cronqvist" \
  | sort -u | cut -c8- >> AUTHORS
git add ChangeLog AUTHORS
git commit --amend --reuse-message HEAD

git tag -a -m"$NVSN" $NVSN && echo "git push && git push --tags"
