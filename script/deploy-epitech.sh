#!/usr/bin/env bash

git checkout $TRAVIS_BRANCH

if [ "$TRAVIS_BRANCH" = "master" ]
then
    ssh-keyscan git.epitech.eu >> ~/.ssh/known_hosts
    git push --repo=git@git.epitech.eu:/florent.poinsard@epitech.eu/FUN_deBruijn_2018 --force
    exit 0
fi

exit 0