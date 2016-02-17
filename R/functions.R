library(RSQLite)
library(mice)
library(infotheo)
library(parallel)
library(caret)

drv <- dbDriver("SQLite")
con  <- dbConnect(drv, dbname="../data/database.sqlite")
tblnames <- c("Expense","SchoolNameConv","Seasons","RegularSeasonCompactResults","Teams","RegularSeasonDetailedResults","TourneyCompactResults","Revenue","TourneyDetailedResults","TourneySeeds","SampleSubmission","TourneySlots", "TeamCoaches", "TeamConferences", "MasseyOrdinals")

for(tblname in tblnames){
  assign(tblname, dbGetQuery(con,paste("SELECT * FROM ", tblname)))
}

add.z.scores <- function(df, feature.names, agg.group.by.cols, na.rm=T){
  result <- df
  for(name in feature.names){
    stats <- result %>% group_by_(.dots=agg.group.by.cols) %>%
      summarise_(avg=paste("mean(", name, ",na.rm=", na.rm,")", sep=""), dev=paste("sd(", name, ",na.rm=", na.rm, ")", sep=""))
    indices <-
      sapply(1:dim(result)[1], function(indx){
        indicator <- Reduce(function(acc, keyname){
          acc & (with(result, get(keyname)[indx]) == with(stats, get(keyname)))
        }, agg.group.by.cols, logical(dim(stats)[1]) | T)
        indx <- which(indicator)
        stopifnot(length(indx) == 1)
        indx
      })
    result[[paste(name, "z", sep=".")]] <- with(stats, (result[[name]] - avg[indices])/dev[indices])
  }
  result
}

