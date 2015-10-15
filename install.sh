#!/usr/bin/env bash

for d in `ls .`;
do
    ( stow $d )
done
