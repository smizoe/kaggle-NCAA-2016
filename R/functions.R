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

available.more.than.or.eq.10.year <- MasseyOrdinals %>%
  group_by(sys_name) %>%
    summarise(num.available=n_distinct(season,na_rm=T)) %>%
    filter(num.available >= 10) %>%
    with(sys_name)

casted.massey.ordinals <- dcast(MasseyOrdinals %>% filter(sys_name %in% available.more.than.or.eq.10.year), season + rating_day_num + team ~ sys_name, value.var="orank")

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

assign.coach <- function(years, days, team.ids, mc.cores=getOption("mc.cores", 2L)){
  mcmapply(function(year, day, team.id){
    result <- TeamCoaches %>%
      filter(season == year, team_id == team.id, day >= first_day, day <= last_day) %>%
        select(coach_name)
    if(dim(result)[1] == 0)
      NA
    else
      result$coach_name
  }, years, days, team.ids, mc.cores=mc.cores)
}

get.massey.ordinals <- function(seasons, day.nums, teams, mc.cores=getOption("mc.cores", 2L)) {
  na.data.frame <- as.data.frame(matrix(rep(NA, times=dim(casted.massey.ordinals)[2] - 3),nrow=1))
  names(na.data.frame) <- names(casted.massey.ordinals)[-(1:3)]

  first.day.of.year <- casted.massey.ordinals %>%
    select(season, rating_day_num) %>% unique %>%
      group_by(season) %>%
       top_n(1, -rating_day_num)
  first.rating.year <- min(first.day.of.year$season)

  result <- as.data.frame(t(mcmapply(function(season.in, day.num, team.in){
             first.rating.day.of.year <- (first.day.of.year %>% filter(season == season.in))$rating_day_num
             if(season.in < first.rating.year || day.num < first.rating.day.of.year)
               cbind(data.frame(season=season.in, rating_day_num=day.num, team=team.in),na.data.frame)
             else
               casted.massey.ordinals %>%
## game day must be later than the rating day; we can't use information from future!
                 filter(season == season.in, rating_day_num <= day.num, team == team.in) %>%
                 top_n(1,-rating_day_num)
  }, seasons, day.nums, teams, mc.cores=mc.cores, SIMPLIFY=T)))
  result$season <- as.integer(result$season)
  result$rating_day_num <- as.integer(result$rating_day_num)
  result$team <- as.integer(result$team)
  for(name in names(result)[-(1:3)]){
    result[[name]] <- as.integer(result[[name]])
  }
  result
}
