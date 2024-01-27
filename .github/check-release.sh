#!/bin/bash
set -e

# Perform pre-release checks.
# Args:
# version number
# maintainer email address

CABAL_FILE=$(ls *.cabal)
CHANGELOG_FILE="CHANGELOG.md"

die () {
    echo
    echo >&2 "$@"
    echo
    exit 1
}

# Main
[ "$#" -eq 2 ] || die "Specify release version number and maintainer's email address."

RELEASE_VERSION="$1"
MAINTAINER_EMAIL="$2"

echo "Running pre-release checks for version ${RELEASE_VERSION}"

# Ensure version number in cabal file matches release version.
CABAL_VERSION=$(awk '$1=="version:"{print $2}' "${CABAL_FILE}")

if [ -z ${CABAL_VERSION} ];
then
  die "Couldn't find version number in ${CABAL_FILE}"
fi

CABAL_VERSION_VALID=true
if [ ${CABAL_VERSION} != ${RELEASE_VERSION} ];
then
  CABAL_VERSION_VALID=false
  echo "You need to update the version number in ${CABAL_FILE} to ${RELEASE_VERSION}"
fi

# Ensure git url is correct in cabal file.
CABAL_GIT_URL=$(awk '$1=="bug-reports:"{print $2}' "${CABAL_FILE}")

if [ -z ${CABAL_GIT_URL} ];
then
  die "Couldn't find bug-reports field in ${CABAL_FILE}"
fi

REPO_GIT_URL=$(git config --get remote.origin.url)
EXPECTED_GIT_URL="${REPO_GIT_URL%.git}"

CABAL_GIT_URL_VALID=true
if [ ${CABAL_GIT_URL} != ${EXPECTED_GIT_URL} ];
then
  CABAL_GIT_URL_VALID=false
  echo "You need to change the bug-reports field in ${CABAL_FILE} to ${EXPECTED_GIT_URL}"
fi

# Ensure homepage field exists in cabal file and is correct.
CABAL_HOMEPAGE=$(awk '$1=="homepage:"{print $2}' "${CABAL_FILE}")

if [ -z ${CABAL_HOMEPAGE} ];
then
  die "Couldn't find homepage field in ${CABAL_FILE}"
fi

CABAL_HOMEPAGE_VALID=true
if [ ${CABAL_HOMEPAGE} != ${EXPECTED_GIT_URL} ];
then
  CABAL_HOMEPAGE_VALID=false
  echo "You need to change the homepage field in ${CABAL_FILE} to ${EXPECTED_GIT_URL}"
fi

# Ensure maintainer email address is correct in cabal file.
CABAL_EMAIL=$(awk '$1=="maintainer:"{print $2}' "${CABAL_FILE}")

if [ -z ${CABAL_EMAIL} ];
then
  die "Couldn't find maintainer field in ${CABAL_FILE}"
fi

CABAL_EMAIL_VALID=true
if [ ${CABAL_EMAIL} != ${MAINTAINER_EMAIL} ];
then
  CABAL_EMAIL_VALID=false
  echo "You need to change the maintainer field in ${CABAL_FILE} to ${MAINTAINER_EMAIL}"
fi

# Ensure source-repository entry exists and is correct.
EXPECTED_SOURCE_REPOSITORY_LOCATION="${EXPECTED_GIT_URL#https:\/\/}"
CABAL_SOURCE_REPOSITORY=$(awk '$1=="source-repository"{print $1}' "${CABAL_FILE}")

if [ -z ${CABAL_SOURCE_REPOSITORY} ];
then
  die "Couldn't find source-repository field in ${CABAL_FILE}"
fi

FULL_GIT_URL="git://${EXPECTED_SOURCE_REPOSITORY_LOCATION}.git"
CABAL_SOURCE_REPOSITORY_LOCATION=$(awk '$1=="location:"{print $2}' "${CABAL_FILE}")

CABAL_SOURCE_REPOSITORY_VALID=true
if [ ${CABAL_SOURCE_REPOSITORY_LOCATION} != "${FULL_GIT_URL}" ];
then
  CABAL_SOURCE_REPOSITORY_VALID=false
  echo "You need to change the source-repository's location in ${CABAL_FILE} to ${FULL_GIT_URL}"
fi

# Ensure changelog contains an entry for this release version.
CHANGELOG_VERSION=$(grep -Eo "##\s+${RELEASE_VERSION}" "${CHANGELOG_FILE}" || true)

CHANGELOG_VERSION_VALID=true
if [ -z "${CHANGELOG_VERSION}" ];
then
  CHANGELOG_VERSION_VALID=false
  echo "You need to add an entry in ${CHANGELOG_FILE} for version ${RELEASE_VERSION}"
fi

CHECKS=(
  "$CABAL_VERSION_VALID"
  "$CABAL_GIT_URL_VALID"
  "$CABAL_HOMEPAGE_VALID"
  "$CABAL_EMAIL_VALID"
  "$CABAL_SOURCE_REPOSITORY_VALID"
  "$CHANGELOG_VERSION_VALID"
)

if [[ ${CHECKS[*]} == *false* ]];
then
  die "Please correct errors and commit them before trying again."
fi

echo "Everything looks good."
echo
