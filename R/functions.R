library(RSQLite)
library(mice)
library(infotheo)
library(parallel)
library(caret)

num.cores <- 3
drv <- dbDriver("SQLite")
con  <- dbConnect(drv, dbname="../data/database.sqlite")
tblnames <- c("Expense","SchoolNameConv","Seasons","RegularSeasonCompactResults","Teams","RegularSeasonDetailedResults","TourneyCompactResults","Revenue","TourneyDetailedResults","TourneySeeds","SampleSubmission","TourneySlots", "TeamCoaches", "TeamConferences", "MasseyOrdinals")
table.keys <- list(MasseyOrdinals=c("season", "rating_day_num", "sys_name", "team"))
for(tblname in tblnames){
  assign(tblname, data.table(dbGetQuery(con,paste("SELECT * FROM ", tblname))))
  if(!is.null(table.keys[[tblname]]))
    setkeyv(get(tblname), table.keys[[tblname]])
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
  na.data.frame <- as.data.frame(matrix(rep(NA, times=length(available.more.than.or.eq.10.year)),nrow=1))
  names(na.data.frame) <- available.more.than.or.eq.10.year

  #first.day.of.year <- MasseyOrdinals %>% filter(sys_name %in% available.more.than.or.eq.10.year) %>%
  #  group_by(season, sys_name) %>% summarize(first.rating.date=min(ifelse(!is.na(orank), rating_day_num, NA), na.rm=T))

  first.rating.year <- min(MasseyOrdinals$season)

  season.wise.data <- list()
  for(season.key in as.character(unique(seasons))){
    if(is.null(season.wise.data[[season.key]]))
      season.wise.data[[season.key]] <- list()
    df <- MasseyOrdinals %>% filter(season == as.integer(season.key)) %>%
      select(sys_name, team, rating_day_num, orank)
    setkey(df, team, rating_day_num)
    season.wise.data[[season.key]] <- df
  }
  result <- as.data.frame(t(mcmapply(function(season.in, day.num, team.in){

             vec <- NULL
             if(season.in < first.rating.year)
               vec <- na.data.frame
             else{
               vec <- as.data.frame(matrix(
                             {
                                 #df <- MasseyOrdinals %>%
                                 #            filter(season == season.in, sys_name %in% available.more.than.or.eq.10.year, day.num >= rating_day_num, team == team.in) %>%
                                 #            select(sys_name, rating_day_num, orank) %>% group_by(sys_name) %>%
                                 #            top_n(1, - rating_day_num)
                                 df <- season.wise.data[[as.character(season.in)]] %>% filter(day.num >= rating_day_num, team == team.in) %>%
                                   group_by(sys_name) %>% top_n(1, -rating_day_num)
                                 with(df, sapply(available.more.than.or.eq.10.year, function(name) {
                                                   selected <- orank[sys_name == name]
                                                   if(length(selected) > 0) selected else NA
                                 }))
                             }, nrow=1))
               names(vec) <- available.more.than.or.eq.10.year
             }
             cbind(data.frame(season=season.in, rating_day_num=day.num, team=team.in),vec)
  }, seasons, day.nums, teams, mc.cores=mc.cores, SIMPLIFY=T)))
  result$season <- as.integer(result$season)
  result$rating_day_num <- as.integer(result$rating_day_num)
  result$team <- as.integer(result$team)
  for(name in names(result)[-(1:3)]){
    result[[name]] <- as.integer(result[[name]])
  }
  result
}
