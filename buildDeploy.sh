#!/bin/bash

set -e

git pull
stack install
cd config/
./deploy.sh
