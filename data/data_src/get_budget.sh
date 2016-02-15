#!/bin/bash
set -eu
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# curl -o ${DIR}/NCAA_budget.json http://usatoday30.usatoday.com/sports/graphics/DataTables/2012/0515-NCAAFinance/NCAA.js?_=1455471431712

if ! gem list bundler | grep -q bundler ; then
  gem install bundler
fi

cd ${DIR}
bundle install --path=vendor/bundle

curl -o /tmp/budget_data_src.html http://sports.usatoday.com/ncaa/finances/
GENERATE_HEADER='--print-header'
for number in $(cat /tmp/budget_data_src.html |grep sportsTableData | grep -o -E '\[.+\]' | jq '.[] | .school_id |tonumber')
do
  curl -o "/tmp/budget_${number}.html" "http://sports.usatoday.com/ajaxservice/ncaa/finances__school__${number}__"
  < "/tmp/budget_${number}.html" bundle exec ${DIR}/extract_budgets.rb ${GENERATE_HEADER}
  sleep 1
  GENERATE_HEADER=''
done

