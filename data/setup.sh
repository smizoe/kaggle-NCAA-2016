#!/bin/bash
set -eu
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# we assume the data is downloaded in the following place
# curl -o /tmp/march-machine-learning-mania-2016-v1.zip

unzip "${DIR}/data_orig" /tmp/march-machine-learning-mania-2016-v1.zip

for file in ${DIR}/data_orig/*
do
  ln -s ${file} "${DIR}/$(basename ${file})"
done

${DIR}/data_src/get_budget.sh
${DIR}/data_src/get_kaggle_forum_data.sh
${DIR}/data_src/reset_sqlite.sh
