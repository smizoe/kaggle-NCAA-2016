library(RSQLite)
library(mice)
library(infotheo)
library(parallel)
library(caretEnsemble)
library(GGally)
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
TourneyDetailedResults <- TourneyDetailedResults %>% rename(wloc=Wloc)
source("functions.R")

available.more.than.or.eq.10.year <- MasseyOrdinals %>%
  group_by(sys_name) %>%
    summarise(num.available=n_distinct(season,na_rm=T)) %>%
    filter(num.available >= 10) %>%
    with(sys_name)

casted.massey.ordinals <- dcast(MasseyOrdinals %>% filter(sys_name %in% available.more.than.or.eq.10.year), season + rating_day_num + team ~ sys_name, value.var="orank")

get.massey.ordinals <- get.massey.ordinals.factory(available.more.than.or.eq.10.year)

load.or.create("full.coach.table", function(){
    game.stats.all <- union(RegularSeasonDetailedResults %>% select(Season, Daynum, Wteam, Lteam),
                            TourneyDetailedResults %>% select(Season, Daynum, Wteam, Lteam))
    with(game.stats.all,
         data.table(Season=rep(Season, times=2),
                    Daynum=rep(Daynum, times=2),
                    Team=c(Wteam, Lteam),
                    Coach=assign.coach(rep(Season, times=2),
                                       rep(Daynum, times=2),
                                       c(Wteam, Lteam), num.cores
                                       )
                    )
         )
  }
)
load.or.create("massey.ordinals.reduced", function(){
  first.year <- 2003 # to use massey ordinals
  final.year <- 2015
  all.team <- unique(TeamConferences$team_id)
  final.day <- 154
  result <- with(data.frame(se=rep(first.year:final.year, each =length(all.team) * (final.day +1)),
                               te=rep(all.team, times=(final.day +1) * (final.year - first.year +1)),
                               da=rep(0:final.day,times=length(all.team)*(final.year - first.year +1))
                               ),
                    get.massey.ordinals(se,da,te, mc.cores=num.cores)
                    )
  for(name in names(result)[-(1:3)])
    result[[name]] <- factor(result[[name]], levels=sort(unique(result[[name]])), ordered=T)
  result
})

valid.raw.data.for <- function(tournament.year){
  game.results.raw <- union(
    RegularSeasonDetailedResults %>% filter(Season <= tournament.year),
    TourneyDetailedResults %>% filter(Season < tournament.year)
  )
  game.results <- with(game.results.raw,
                       data.table(won.by.1=as.numeric(Wteam < Lteam), Season=Season, Daynum=Daynum, wloc=wloc, Team.1=pmin(Wteam, Lteam), Team.2=pmax(Wteam, Lteam)))
  stats <- c("score", "fgm", "fga", "fgm3", "fga3", "ftm", "fta", "or", "dr", "ast", "to", "stl", "blk", "pf")
  for(name in stats){
    game.results[[paste(name,"1", sep=".")]] <- with(game.results.raw, ifelse(game.results$won.by.1 == 1, get(paste("W", name,sep="")), get(paste("L", name, sep=""))))
    game.results[[paste(name,"2", sep=".")]] <- with(game.results.raw, ifelse(game.results$won.by.1 == 0, get(paste("W", name,sep="")), get(paste("L", name, sep=""))))
  }

## add coach and ordinals
  game.results <- game.results %>% inner_join(full.coach.table %>% rename(Team.1=Team), c("Season", "Daynum", "Team.1")) %>% # add coach names to both team won and lost
      rename(Coach.1=Coach) %>%
      inner_join(full.coach.table %>% rename(Team.2=Team), c("Season", "Daynum", "Team.2")) %>%
      rename(Coach.2=Coach) %>%
      left_join(massey.ordinals.reduced %>% rename(Season=season, Daynum=rating_day_num, Team.1=team), c("Season", "Daynum", "Team.1"), copy=T) %>% # add ordinals to them
      rename_(.dots=mk.rename.nse(available.more.than.or.eq.10.year, suffix="1")) %>%
      left_join(massey.ordinals.reduced %>% rename(Season=season, Daynum=rating_day_num, Team.2=team), c("Season", "Daynum", "Team.2"), copy=T) %>%
      rename_(.dots=mk.rename.nse(available.more.than.or.eq.10.year, suffix="2"))

## restrict coach names
  coaches.exploratory <- TeamCoaches %>% group_by(coach_name) %>%
  summarize(days= sum(last_day-first_day + 1),last=max(season), num.teams=n_distinct(team_id))
  target.coaches.names <- (coaches.exploratory %>% filter(num.teams > 3, days > 1540, last==2016))$coach_name
  for(name in paste("Coach", 1:2, sep="."))
    game.results[[name]][!(game.results[[name]] %in% target.coaches.names)] <- "non_target"

## restrict ordinals to top30 (impute NA with worst value)
  for(name in available.more.than.or.eq.10.year){
    bound <- min(c(30, max(as.integer(massey.ordinals.reduced[[name]]), na.rm=T)+ 1))
    for(num in 1:2){
      target.name <- paste(name, num, sep=".")
      game.results[[target.name]] <- as.integer(game.results[[target.name]])
      game.results[[target.name]][is.na(game.results[[target.name]])] <- bound
      game.results[[target.name]][game.results[[target.name]] > bound] <- bound
    }
  }

## revenue and expense
  revenue.and.expense <- Teams %>% inner_join(Revenue %>% select(-conference), c("Team_Name")) %>% inner_join(Expense %>% select(-conference), c("Team_Name", "year"))
  revenue.colnames <- names(Revenue)[-(1:3)]
  expense.colnames <- names(Expense)[-(1:3)]
  r.e.colnames <- c(revenue.colnames, expense.colnames)
  game.results <- game.results %>% left_join(revenue.and.expense %>% rename(Team.1=Team_Id, Season=year), c("Team.1", "Season")) %>%
    rename_(.dots=mk.rename.nse(r.e.colnames, suffix="1")) %>%
      left_join(revenue.and.expense %>% rename(Team.2=Team_Id, Season=year), c("Team.2", "Season")) %>%
      rename_(.dots=mk.rename.nse(r.e.colnames, suffix="2"))

## seed info
  seed.info <- TourneySeeds %>% mutate(seed = ifelse(substring(Seed, nchar(Seed)) %in% c("a", "b"), "play_in", Seed)) %>% select(Season, Team, seed)
  game.results <- game.results %>% left_join(seed.info %>% rename(Team.1=Team), c("Season", "Team.1")) %>% rename(Seed.1=seed) %>%
    left_join(seed.info %>% rename(Team.2=Team), c("Season", "Team.2")) %>% rename(Seed.2=seed)
  for(name in c("Seed.1", "Seed.2")){
    game.results[[name]][is.na(game.results[[name]])] <- "non_seed"
    game.results[[name]] <- factor(game.results[[name]])
  }

## restrict teams to stronger teams
  target.teams <- (TourneySeeds %>% filter(Season >= 2003) %>% group <- by(Team) %>% summarize(cnt=n()) %>% filter(cnt >= 6) %>% arrange(cnt))$Team
  for(name in paste("Team", 1:2, sep="."))
    game.results[[name]][!(game.results[[name]] %in% target.teams)] <- 0
  game.results
}
