library(RSQLite)
library(mice)
library(infotheo)
library(parallel)
library(caret)
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
  with(data.frame(se=rep(first.year:final.year, each =length(all.team) * (final.day +1)),
                               te=rep(all.team, times=(final.day +1) * (final.year - first.year +1)),
                               da=rep(0:final.day,times=length(all.team)*(final.year - first.year +1))
                               ),
                    get.massey.ordinals(se,da,te, mc.cores=num.cores)
                    )
})

valid.raw.data.for <- function(tournament.year){
  game.results <- union(
    RegularSeasonDetailedResults %>% filter(Season <= tournament.year),
    TourneyDetailedResults %>% filter(Season < tournament.year)
  ) %>% inner_join(full.coach.table %>% rename(Wteam=Team), c("Season", "Daynum", "Wteam")) %>% # add coach names to both team won and lost
      rename(W.Coach=Coach) %>%
      inner_join(full.coach.table %>% rename(Lteam=Team), c("Season", "Daynum", "Lteam")) %>%
      rename(L.Coach=Coach) %>%
      left_join(massey.ordinals.reduced %>% rename(Season=season, Daynum=rating_day_num, Wteam=team), c("Season", "Daynum", "Wteam"), copy=T) %>% # add ordinals to them
      rename_(.dots=mk.rename.nse(available.more.than.or.eq.10.year, "W")) %>%
      left_join(massey.ordinals.reduced %>% rename(Season=season, Daynum=rating_day_num, Lteam=team), c("Season", "Daynum", "Lteam"), copy=T) %>%
      rename_(.dots=mk.rename.nse(available.more.than.or.eq.10.year, "L"))
  revenue.and.expense <- Teams %>% inner_join(Revenue %>% select(-conference), c("Team_Name")) %>% inner_join(Expense %>% select(-conference), c("Team_Name", "year"))
  revenue.colnames <- names(Revenue)[-(1:3)]
  expense.colnames <- names(Expense)[-(1:3)]
  r.e.colnames <- c(revenue.colnames, expense.colnames)
  game.results %>% left_join(revenue.and.expense %>% rename(Wteam=Team_Id, Season=year), c("Wteam", "Season")) %>%
    rename_(.dots=mk.rename.nse(r.e.colnames, "W")) %>%
      left_join(revenue.and.expense %>% rename(Lteam=Team_Id, Season=year), c("Lteam", "Season")) %>%
      rename_(.dots=mk.rename.nse(r.e.colnames, "L"))
}
