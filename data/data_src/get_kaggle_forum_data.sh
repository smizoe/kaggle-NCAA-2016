#!/bin/bash

set -eu
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${DIR}"
URLS=(
https://www.kaggle.com/blobs/download/forum-message-attachment-files/3681/TeamConferences.csv
https://www.kaggle.com/blobs/download/forum-message-attachment-files/3682/TeamCoaches.csv
https://www.kaggle.com/blobs/download/forum-message-attachment-files/3689/SeedRoundSlots.csv
https://www.kaggle.com/blobs/download/forum-message-attachment-files/3679/RegularSeasonCompactResults2016ThruDay94.csv
https://www.kaggle.com/blobs/download/forum-message-attachment-files/3680/RegularSeasonDetailedResults2016ThruDay94.csv
https://www.kaggle.com/blobs/download/forum-message-attachment-files/3691/MasseyOrdinals.zip
https://www.kaggle.com/blobs/download/forum-message-attachment-files/3692/MasseyOrdinals2016ThruDay94.csv
https://www.kaggle.com/blobs/download/forum-message-attachment-files/3686/SBRLines.csv
https://www.kaggle.com/blobs/download/forum-message-attachment-files/3687/ThePredictionTrackerPointspreads.csv
https://www.kaggle.com/blobs/download/forum-message-attachment-files/3690/GameFlowDetails.zip
https://www.kaggle.com/blobs/download/forum-message-attachment-files/3684/TeamGeog.csv
https://www.kaggle.com/blobs/download/forum-message-attachment-files/3685/TourneyGeog.csv
)

for (( indx=0; indx<${#URLS[@]}; indx++))
do
  curl -L -O "${URLS[indx]}"
  sleep 1
done
