source("initialization.R")
add.features <- function(data, cores=3){
  revenue.names <- names(Revenue)[-(1:3)]
  expense.names <- names(Expense)[-(1:3)]
  discretized <- discretize(data %>% select_(.dots=paste(rep(c(revenue.names, expense.names), times=2), rep(1:2, each=length(c(revenue.names, expense.names))), sep=".")), "equalwidth", 20)
  for(name in names(discretized)){
    discretized[[name]][discretized[[name]] <0] <- NA
## TODO: find a better imputation
    med <- median(discretized[[name]], na.rm=T)
    discretized[[name]][is.na(discretized[[name]])] <- med
  }
  names(discretized) <- paste(rep(c(revenue.names, expense.names), times=2), "disc", rep(1:2, each=length(c(revenue.names, expense.names))), sep=".")
  data <- cbind(data, discretized)
  two.side.features <- c("Coach", "Team", "Seed", paste(c(revenue.names, expense.names), "disc", sep="."), available.more.than.or.eq.10.year)
  data <- mk.matrix.from.raw(data, sapply(two.side.features, function(x) paste(x, 1:2, sep="."), simplify=F, USE.NAMES=T), cores=cores)
## TODO: add features
##  # add features that can be inferred from team level stats (e.g., non-game stats)
##  rebounds.avg <- with(data, c(mean(c(or.1 , or.2)), mean(c(dr.1,dr.2))))
##  ft.avg  <- with(data, c(mean(c(ftm.1, ftm.2)), mean(c(fta.1, fta.2))))
##  fg.avg  <- with(data, c(mean(c(fgm.1, fgm.2)), mean(c(fga.1, fga.2))))
##  fg3.avg <- with(data, c(mean(c(fgm3.1, fgm3.2)), mean(c(fga3.1, fga3.2))))
##  ast.avg <- with(data, mean(c(ast.1, ast.2)))
##  score.avg <- with(data, mean(c(score.1, score.2)))
##  pf.avg  <- with(data, mean(c(pf.1, pf.2)))
##
##  # we add game level stats features by log(team 1's skillfulness/team 2's skillfulness).
##
##  additional.feature.targets <- with(data,
##    list(log.fgr.ratio={
##           denom.base <- fg.avg[2]/10
##           numer.base <- fg.avg[1]/10
##           fgr.1 <- (fgm.1 + numer.base) / (fga.1 + denom.base)
##           fgr.2 <- (fgm.2 + numer.base) / (fga.2 + denom.base)
##           log( fgr.1 / fgr.2 )
##         },
##         log.scoring.ratio={
##           base <- score.avg/10
##           log( (score.1 + base) / (score.2 + base))
##         },
##         log.rebound.ratio={
##           denom.base <-  sum(rebounds.avg) / 10 # number of total rebounds
##           numer.base <- rebounds.avg[2] / 10 # number of dr /10
##           dr.ratio.1 <- (dr.1 + numer.base) / (dr.1+ or.2 + denom.base)
##           dr.ratio.2 <- (dr.2 + numer.base) / (dr.2 + or.1 + denom.base)
##           log( dr.ratio.1 / dr.ratio.2)
##         },
##         log.ast.ratio={
##           base <- ast.avg / 10
##           log((ast.1+base)/ (ast.2 + base))
##         },
##         log.fgm.ratio={
##           base <- fg.avg[1] / 10
##           log((fgm.1 + base) / (fgm.2 + base))
##         },
##         log.ftm.ratio={
##           base <- ft.avg[1] / 10
##           log((ftm.1 + base) / (ftm.2 + base))
##         },
##         log.fgr3.ratio={
##           denom.base <- fg3.avg[2] / 10
##           numer.base <- fg3.avg[1] / 10
##           fgr3.1 <- (fgm3.1 + numer.base) / (fga3.1 + denom.base)
##           fgr3.2 <- (fgm3.2 + numer.base) / (fga3.2 + denom.base)
##           log(fgr3.1 / fgr3.2)
##         },
##         log.fta.ratio={
##           base <- ft.avg[2] / 10
##           log((fta.1 + base) / (fta.2 + base))
##         },
##         log.pf.ratio={
##           base <- pf.avg
##           log((pf.1 + base)/ (pf.2 + base))
##         },
##         log.ftr.ratio={
##           denom.base <- ft.avg[2] / 10
##           numer.base <- ft.avg[1] / 10
##           ftr.1 <- (ftm.1 + numer.base) / (fta.1 + denom.base)
##           ftr.2 <- (ftm.2 + numer.base) / (fta.2 + denom.base)
##           log(ftr.1 / ftr.2)
##         })
##    )
##  predictor.list <- list()
##  features.to.be.used <- c("Season", names(data)[grepl(paste("^(", paste(paste(two.side.features,"_",sep=""), collapse="|"), ")", sep=""), names(data))])
##  for(name in names(additional.feature.targets))
##    predictor.list[[name]] <- features.to.be.used
##  models <- mk.game.stats.predictor(data, additional.feature.targets, predictor.list, cores)
  to.remove <- setdiff(names(data)[1:142], c("Season", "won.by.1"))
  to.select <- setdiff(names(data), to.remove)
  data$won.by.1 <- factor(ifelse(data$won.by.1 == 1, "win", "loss"))
  data %>% select_(.dots=to.select)
}

train.all <- function(data, cores=3, number=10){
  require(caret)
  require(caretEnsemble)

  factors <- names(data)[!grepl("\\.disc_", names(data)) & grepl("_", names(data))]
  factors.validity <-as.data.frame(data %>% select_(.dots=factors) %>% mclapply(function(col) col %in% c(1, -1), mc.cores=cores))
  split.part <- split(factors.validity, floor((1:dim(factors.validity)[1])/(dim(factors.validity)[1]/cores + 1)))
  #splitter <- unlist(mclapply(split.part, function(x) Reduce(paste, x, ""), mc.cores=cores))
  splitter <- unlist(mclapply(split.part, function(x) Reduce(function(x,y) x+y, x, 0), mc.cores=cores))
  control <- trainControl(method="cv", number=number, savePredictions="final", classProbs=T, summaryFunction=mnLogLoss, index=createFolds(splitter, k=number, returnTrain=T), verboseIter=T, p=0.9)
  rm(split.part, splitter, factors, factors.validity)
  require(doParallel)
  registerDoParallel(cores=cores)
  caretList(won.by.1 ~., data=data, metric="logLoss", trControl=control, methodList=c("glm", "xgbLinear", "svmLinear"), continue_on_fail=T)
}

run <- function(cores=3, spec=""){
  for(year in 2012:2015){
    data  <- add.features(valid.raw.data.for(year), cores)
    data <- as.data.frame(data)
    model <- train.all(data, cores)
    save(data, model, file=paste("saved/models/model_and_data_", spec, year, sep=""))
    rm(data)
    rm(model)
  }
}
