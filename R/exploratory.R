source("functions.R")

{
## check if a team with a high revenue wins or not
#games.with.revenue<- RegularSeasonCompactResults %>% inner_join(Teams,by=c("Wteam"="Team_Id")) %>% inner_join(Revenue, by=c("Team_Name", "Season"="year"))
  wins  <- RegularSeasonCompactResults %>% group_by(Wteam, Season) %>% summarise(wins=n())
  losses <- RegularSeasonCompactResults %>% group_by(Lteam, Season) %>% summarise(losses=n())
  win.loss <- wins %>% full_join(losses, by=c("Wteam" = "Lteam", "Season")) %>%
    inner_join(Teams, by = c("Wteam"="Team_Id"))
  win.loss$wins[is.na(win.loss$wins)] <- 0
  win.loss$losses[is.na(win.loss$losses)] <- 0

  win.loss.revenue <-  win.loss %>% full_join(Revenue, by = c("Team_Name", "Season"="year"))
  win.loss.revenue <- add.z.scores(win.loss.revenue, c("ticket_sales", "contributions", "rights", "student_fees", "school_funds", "other", "total_revenue"), "Season")
  revenue.col.names <- c("wins","losses", "ticket_sales", "contributions", "rights", "student_fees", "school_funds", "other", "total_revenue", "ticket_sales.z", "contributions.z", "rights.z", "student_fees.z", "school_funds.z", "other.z", "total_revenue.z")
  ggpairs(win.loss.revenue, which(names(win.loss.revenue) %in% revenue.col.names))
  with(subset(win.loss.revenue, !is.na(wins) & !is.na(total_revenue)), cor(wins, total_revenue, method="kendall"))
  non.na.win.loss.revenue <- subset(win.loss.revenue, Reduce(function(x,y) x & !is.na(get(y)), revenue.col.names, TRUE))
  sort(sapply(revenue.col.names[-1], function(label) with(non.na.win.loss.revenue, cor(wins, get(label), method="kendall"))))
  sort(mutinformation(non.na.win.loss.revenue)["wins",])

## check if a team with a high expense wins or not
  win.loss.expense <- win.loss %>% full_join(Expense, by=c("Team_Name", "Season"="year"))
  win.loss.expense <- add.z.scores(win.loss.expense, c("coaching", "scholarships", "building", "others", "total_expense"), "Season")
  expense.col.names <- c("wins" , "losses", "coaching", "scholarships", "building", "others", "total_expense", "coaching.z", "scholarships.z", "building.z", "others.z", "total_expense.z")
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
  correl.vec <- mclapply(stats.names, function(x) cor(game.stats$won, game.stats[[x]] ,method="kendall"), mc.cores=3)
  correl.vec <- as.numeric(correl.vec)
  names(correl.vec) <- stats.names
  sort(correl.vec)
#         pf          to        fga3         fga          or        fgm3         stl         blk         fta        fgr3         ftm         fgm         ast          dr         fgr
#  -0.22409294 -0.12832937 -0.07892090 -0.07121675 -0.02110140  0.14064298  0.14116008  0.17487293  0.24901759  0.25701006  0.27398312  0.30062050  0.31296617  0.33273590  0.40394462
#        score
#   0.40582467
## ftr is not there since sometimes fta == 0

  game.stat.with.team <- game.stats %>% select(ftr, pf, fta, fgr3, ftm, fgm, ast, dr, score, fgr) %>% mutate(team=with(RegularSeasonDetailedResults, c(Wteam, Lteam)))
}
