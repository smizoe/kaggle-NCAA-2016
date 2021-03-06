source("initialization.R")
library(mice)
library(GGally)
library(infotheo)
{
## check if a team with a high revenue wins or not
#games.with.revenue<- RegularSeasonCompactResults %>% inner_join(Teams,by=c("Wteam"="Team_Id")) %>% inner_join(Revenue, by=c("Team_Name", "Season"="year"))
  wins  <- RegularSeasonCompactResults %>% group_by(Wteam, Season) %>% summarise(wins=n())
  losses <- RegularSeasonCompactResults %>% group_by(Lteam, Season) %>% summarise(losses=n())
  win.loss <- wins %>% full_join(losses, by=c("Wteam" = "Lteam", "Season")) %>%
    inner_join(Teams, by = c("Wteam"="Team_Id"))
  win.loss$wins[is.na(win.loss$wins)] <- 0
  win.loss$losses[is.na(win.loss$losses)] <- 0

  raw.revenue.cols <- names(Revenue)[-(1:3)]
  win.loss.revenue <-  win.loss %>% full_join(Revenue, by = c("Team_Name", "Season"="year"))
  win.loss.revenue <- add.z.scores(win.loss.revenue, raw.revenue.cols, "Season")

  freq <- discretize(win.loss.revenue %>% select_(.dots=raw.revenue.cols))
  names(freq) <- paste(raw.revenue.cols,"freq", sep=".")
  width <- discretize(win.loss.revenue %>% select_(.dots=raw.revenue.cols), disc="equalwidth")
  names(width) <- paste(raw.revenue.cols, "width", sep=".")
  win.loss.revenue <- cbind(win.loss.revenue, freq, width)

  revenue.col.names <- c("wins","losses", raw.revenue.cols, paste(raw.revenue.cols, "z", sep="."), paste(raw.revenue.cols, "freq", sep="."), paste(raw.revenue.cols, "width", sep="."))
  ggpairs(win.loss.revenue, which(names(win.loss.revenue) %in% revenue.col.names))
  with(subset(win.loss.revenue, !is.na(wins) & !is.na(total_revenue)), cor(wins, total_revenue, method="kendall"))
  non.na.win.loss.revenue <- subset(win.loss.revenue, Reduce(function(x,y) x & !is.na(get(y)), revenue.col.names, TRUE))
  sort(sapply(revenue.col.names[-1], function(label) with(non.na.win.loss.revenue, cor(wins, get(label), use="complete.obs", method="kendall"))))
  sort(mutinformation(non.na.win.loss.revenue)["wins",])

## check if a team with a high expense wins or not
  raw.expense.cols <- names(Expense)[-(1:3)]
  win.loss.expense <- win.loss %>% full_join(Expense, by=c("Team_Name", "Season"="year"))
  win.loss.expense <- add.z.scores(win.loss.expense, c("coaching", "scholarships", "building", "others", "total_expense"), "Season")

  freq <- discretize(win.loss.expense %>% select_(.dots=raw.expense.cols))
  names(freq) <- paste(raw.expense.cols,"freq", sep=".")
  width <- discretize(win.loss.expense %>% select_(.dots=raw.expense.cols), disc="equalwidth")
  names(width) <- paste(raw.expense.cols, "width", sep=".")
  win.loss.expense <- cbind(win.loss.expense, freq, width)

  expense.col.names <- c("wins" , "losses", raw.expense.cols, paste(raw.expense.cols, rep(c("z", "freq", "width"), each=length(raw.expense.cols)),sep="."))
  ggpairs(win.loss.expense, which(names(win.loss.expense) %in% expense.col.names))
  with(subset(win.loss.expense, !is.na(wins) & !is.na(total_expense)), cor(wins, total_expense, method="kendall"))
  non.na.win.loss.expense <- subset(win.loss.expense, Reduce(function(x,y) x & !is.na(get(y)), expense.col.names, TRUE))
  sort(sapply(expense.col.names[-1], function(label) with(non.na.win.loss.expense, cor(wins, get(label), method="kendall"))))
  sort(mutinformation(non.na.win.loss.expense)["wins",])

}
{
# check if # of wins differ if a school is private (or does not disclose their revenue and expense)
  disclosed.or.not <- win.loss.expense %>% group_by(Team_Name) %>% summarize(disclosed=any(!is.na(total_expense)))
  win.loss.vs.diclosure <- win.loss %>% inner_join(disclosed.or.not)
  total.instances <- win.loss.vs.diclosure %>% group_by(disclosed) %>% summarize(total.cnt = n())
  wins.dist <- win.loss.vs.diclosure %>% inner_join(total.instances) %>% group_by(disclosed, wins)  %>% summarize(ratio=n()/min(total.cnt))
  qplot(wins, ratio, data=wins.dist, colour=disclosed)
## win rate distribution is almost similar => hopefully imptation works well
}

## check if there are strong conferences
{
  win.rate.conference <- win.loss %>% inner_join(TeamConferences, by=c("Season"="season", "Wteam" = "team_id")) %>% mutate(win.rate=wins/ as.numeric(wins+losses))
  qplot(Season, win.rate, data=subset(win.rate.conference, Season > 2005), geom="boxplot", facets=~conference, group=Season)
  win.rate.conference.median <- win.rate.conference %>% group_by(Season, conference) %>% summarize(win.rate.median=median(win.rate))
  #qplot(conference, win.rate.median, data=win.rate.conference.median, geom=c("point", "text"),label=Season)
  qplot(conference, win.rate.median, data=win.rate.conference.median)
# seems conference makes a difference
# althogh members on a team completely change in 4 years, once the team is recognized as strong, it can get good members
}
{
 coaches.exploratory <- TeamCoaches %>% group_by(coach_name) %>%
  summarize(days= sum(last_day-first_day + 1),last=max(season), num.teams=n_distinct(team_id))
  coaches.exploratory %>% arrange(desc(last), desc(days)) %>% print(n=400)
  coaches.exploratory %>% arrange(desc(last), desc(num.teams), desc(days)) %>% print(n=400)
# if num.teams == 1, the coach almost belongs to the team
# it's better to use coaches with num.teams > 1 and days  > 1540 ( 10 years)
# or for each year, we need to filter by 'last' and take coaches with large num.teams and large 'days'
  target.coaches.names <- (coaches.exploratory %>% filter(num.teams > 3, days > 1540, last==2016))$coach_name
}
{
  num.ordinals.per.system <- MasseyOrdinals %>% group_by(season, sys_name) %>% summarise(num.issues=n_distinct(rating_day_num, na_rm=T))
  num.available.year <- MasseyOrdinals %>% group_by(sys_name) %>% summarise(num.available=n_distinct(season,na_rm=T))
  qplot(num.available,data=num.available.year, binwidth=1) #10 available years seem to be good
  qplot(season,num.issues,data=num.ordinals.per.system,facets=~sys_name)
  plot(cnt, data= num.ordinals.per.system %>% group_by(season, num.issues) %>% summarise(cnt=n_distinct(sys_name)), facets=~season)
}
{
  game.stats <- data.frame(won=rep(c(1,0),each=dim(RegularSeasonDetailedResults)[1]))
  mk.int.ratio <- function(numer, denom, mul=1000){as.integer(numer/as.numeric(denom) * mul)}
  stats.names <- c("score", "or", "dr", "ast", "to", "stl", "blk", "pf", "fta", "ftm", "fga", "fgm", "fga3", "fgm3")
  for(stat in stats.names){
    game.stats[[stat]] <- with(RegularSeasonDetailedResults, c(get(paste("W",stat,sep="")),get(paste("L", stat, sep=""))))
  }
  game.stats <- game.stats %>% mutate(ftr=mk.int.ratio(ftm,fta), fgr=mk.int.ratio(fgm,fga), fgr3=mk.int.ratio(fgm3,fga3))
  stats.names <- c(stats.names, "ftr", "fgr", "fgr3")
  mut.info.mat <- mutinformation(game.stats)
  sort(mut.info.mat[1,])
#          or          fga         fga3           to         fgm3          stl          blk          ftr           pf          fta
#  0.0004890482 0.0039307952 0.0047942828 0.0131200171 0.0146148895 0.0153338918 0.0223315624 0.0348963509 0.0377914211 0.0480063823
#          fgr3          ftm          fgm          ast           dr        score          fgr          won
#  0.0563663413 0.0576521190 0.0701691565 0.0758421555 0.0849355723 0.1346779446 0.1370217270 0.6931471806
## good predictors are ftr to fgr
  correl.vec <- mclapply(stats.names, function(x) cor(game.stats$won, game.stats[[x]], use="complete.obs", method="kendall"), mc.cores=num.cores)
  correl.vec <- as.numeric(correl.vec)
  names(correl.vec) <- stats.names
  sort(correl.vec)


#         pf          to        fga3         fga          or           ftr    fgm3         stl         blk         fta        fgr3         ftm         fgm         ast          dr         fgr
#  -0.22409294 -0.12832937 -0.07892090 -0.07121675 -0.02110140  0.116917760.14064298  0.14116008  0.17487293  0.24901759  0.25701006  0.27398312  0.30062050  0.31296617  0.33273590  0.40394462
#        score
#   0.40582467

  selected <- c("ftr", "pf", "fta", "fgr3", "ftm", "fgm", "ast", "dr", "score", "fgr")
  first.year <- 2003 # to use massey ordinals
  final.year <- 2015
  all.team <- unique(TeamConferences$team_id)
  final.day <- 154
#  massey.ordinals.reduced <- with(data.frame(se=rep(first.year:final.year, each =length(all.team) * (final.day +1)),
#                               te=rep(all.team, times=(final.day +1) * (final.year - first.year +1)),
#                               da=rep(0:final.day,times=length(all.team)*(final.year - first.year +1))
#                               ),
#                    get.massey.ordinals(se,da,te, mc.cores=num.cores)
#                    )
#  save(massey.ordinals.reduced, file="saved/massey_ordinals_reduced")
#  load("saved/massey_ordinals_reduced")
  seed.info <- TourneySeeds %>% mutate(seed = ifelse(substring(Seed, nchar(Seed)) %in% c("a", "b"), "play-in", Seed)) %>% select(Season, Team, seed)
  game.stat.with.team <- game.stats %>% select_(.dots=selected) %>%
    mutate(team_id=with(RegularSeasonDetailedResults, c(Wteam, Lteam)),
           coach=with(RegularSeasonDetailedResults,c(assign.coach(Season, Daynum, Wteam), assign.coach(Season, Daynum, Lteam))),
           season=with(RegularSeasonDetailedResults, rep(Season, times=2)),
           Daynum=with(RegularSeasonDetailedResults, rep(Daynum, times=2)),
           loc=with(RegularSeasonDetailedResults, c(wloc, ifelse(wloc=="H", "A", "N")))) %>%
    filter(season >= first.year) %>%
    inner_join(TeamConferences, c("season", "team_id")) %>%
    inner_join(massey.ordinals.reduced,
               c("season", team_id="team", Daynum = "rating_day_num")) %>%
    left_join(seed.info, c(season = "Season", team_id="Team"))
  # use coaches who experienced more than 1 team
  save(game.stat.with.team, file="saved/game_stat_with_team")
  load("saved/game_stat_with_team")
  with(game.stat.with.team, game.stat.with.team[["coach"]][!(coach %in% target.coaches.names)]  <- "_renamed")
  with(game.stat.with.team, game.stat.with.team[["seed"]][is.na(seed)] <- "non-tourney")
  char.features <- c("coach", "conference", "loc", "seed")
  for(label in char.features)
    game.stat.with.team[[label]] <- factor(game.stat.with.team[[label]])
  ordinal.sys.names <- names(massey.ordinals.reduced)[-(1:3)]
  correlation.target.features <- c(char.features, "team_id", ordinal.sys.names)
  mutinfo.game.stats <- t(sapply(selected, function(label) mutinformation(game.stat.with.team %>% select_(.dots=c(label, correlation.target.features)))[1,]))
  mutinfo.ordinals <- t(sapply(names(massey.ordinals.reduced)[-(1:3)], function(label){mutinformation(game.stat.with.team %>% select_(.dots=c(label, char.features, "team_id", selected)))[1,]}))
  correl.game.stats <- as.data.table(
                         t(matrix(mcmapply(function(label, feature) cor(game.stat.with.team[[label]], game.stat.with.team[[feature]], use="complete.obs", method="kendall"),
                                           rep(selected, each=length(ordinal.sys.names)), ordinal.sys.names, mc.cores=num.cores), nrow=length(ordinal.sys.names)))
                         )
  names(correl.game.stats) <- ordinal.sys.names
  row.names(correl.game.stats) <- selected
  save(mutinfo.game.stats, mutinfo.ordinals, correl.game.stats, file="saved/game_stats_var_importance")
  load("saved/game_stats_var_importance")
# check which team attribute is a good predictor for game stats
  qplot(x=against, y=value, data=as.data.table(mutinfo.game.stats/mutinfo.game.stats[,"ftr"]) %>% select(-ftr) %>% mutate(against=row.names(mutinfo.game.stats)) %>% melt("against"), facets=~variable)+theme(axis.text.x = element_text(angle =90, hjust = 1))
  qplot(x=variable, y= value, data=as.data.table(mutinfo.ordinals/mutinfo.ordinals[,1]) %>% select(-1) %>% mutate(against=rownames(mutinfo.ordinals)) %>% melt("against"), facets= ~against)+theme(axis.text.x = element_text(angle =90, hjust = 1))
  #as a result we should use the followings to impute ordinals: coach, conference, seed, team, ftr, fgr, fgr3, score
  qplot(x=against, y=value, data=correl.game.stats %>% mutate(against=row.names(correl.game.stats)) %>% melt("against"), facets=~variable)+theme(axis.text.x = element_text(angle =90, hjust = 1))
# coach, ordinals and seed works well
}
