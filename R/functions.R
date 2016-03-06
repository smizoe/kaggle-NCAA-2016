
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
## targets: a list or a dataframe contains prediction target. the list must be named and the names are used in selecting features
## features: a list whose element is a character vector.
##           each element of the list is named with one of 'target',
##           and the vector contains the feature names to be used to predict the target
mk.game.stats.predictor <- function(df, targets, features, mc.cores=getOption("mc.cores", 2L)) {
  require(doMC)
  registerDoMC(cores = mc.cores)

  make.model <- function(target, name){
    feature.names <- features[[name]]
    pred.formula <- as.formula(paste(name, paste(feature.names,sep=" + "),sep=" ~ "))
    data <- df[feature.names]
    data[[name]] <- target
    train(pred.formula, data=data, method="rf")
  }

  mapply(make.model, targets, names(targets))
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

##
## - pos.neg.feature.list: a list of features that should take
##   + 1 (or positive value) when it is winners' value
##   + -1 (or negative value) when losers'
##   + and 0 when it's irrelevant.
##   each element is a character vector of length 2, whose 1st element specifies
##   the name of winners' feature and the other specifies that of losers'.
##   e.g., if an element in list is c("Wteam", "Lteam"), it means
##   we would like to get a set of features which takes 1 when it appears as "Wteam"
##   and the other values when it does not.
mk.matrix.from.raw <- function(data, pos.neg.feature.list){
  result <- data
  for(name in names(pos.neg.feature.list)) {
    vec <- pos.neg.feature.list[[name]]
## if one feature appears in both side (winner and loser),
## we put 0 in the corresponding feature
    w.name <- vec[1]
    l.name <- vec[2]
    values <- Filter(function(x) !is.na(x), unique(data[[w.name]], data[[l.name]]))
    new.features <- sapply(values, function(val){
             w.data <- data[[w.name]]
             l.data <- data[[l.name]]
             is.winners <- w.data == val & l.data != val
             is.losers  <- l.data == val & w.data != val
             use.raw <- typeof(w.data) == "double"
             ifelse(is.winners, if(use.raw) w.data else 1 ,
                    ifelse(is.losers, if(use.raw) l.data else -1 , 0) )
    })
    colnames(new.features) <- paste(name, as.character(values), sep="_")
    result <- cbind(result, new.features)
  }
  result
}
