
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

get.massey.ordinals.factory <- function(available.more.than.or.eq.10.year){
  function(seasons, day.nums, teams, mc.cores=getOption("mc.cores", 2L)) {
    na.data.frame <- as.data.frame(matrix(rep(NA, times=length(available.more.than.or.eq.10.year)),nrow=1))
    names(na.data.frame) <- available.more.than.or.eq.10.year

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

}

## df: data.frame that contains game-level data
## targets: a character vector contains prediction target
## features: a list whose element is a character vector.
##           each element of the list is named with one of 'target',
##           and the vector contains the feature names to be used to predict the target
mk.game.stats.predictor <- function(df, targets, features, max.depth=4, mc.cores=getOption("mc.cores", 2L), seed=NULL) {
  make.model <- function(target){
    feature.names <- features[[target]]
    pred.formula <- as.formula(paste(target, paste(feature.names,sep=" + "),sep=" ~ "))

    train(pred.formula, data=df[c(target, feature.names)], method="rf")
  }
  if(!is.null(seed)){
    set.seed(seed, "L'Ecuyer")
    mc.reset.stream()
  }
  mclapply(targets, make.model, mc.cores=mc.cores)
}

load.or.create <- function(obj.name, fun, filename=gsub("\\.", "_", obj.name), store.dir="saved"){
  path <- paste(store.dir, filename, sep="/")
  parent <- parent.env(environment())
  if(file.exists(path))
    load(path, envir=parent)
  else{
    assign(obj.name, fun(), envir=parent)
    save(list=obj.name, envir=parent, file=path)
  }
}

mk.rename.nse <- function(original, prefix=NULL, suffix=NULL, sep="."){
  if(all(is.null(c(prefix, suffix))))
    new.name <- original
  if(!is.null(prefix) && !is.null(suffix))
    new.name <- paste(prefix, original, suffix, sep=sep)
  if(is.null(prefix))
    new.name <- paste(original, suffix, sep=sep)
  if(is.null(suffix))
    new.name <- paste(prefix, original, sep=sep)
  names(original) <- new.name
  original
}
