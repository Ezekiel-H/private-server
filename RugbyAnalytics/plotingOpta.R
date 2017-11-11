library(readr)
library(dplyr)
library(tidyr)
library(reshape2)
library(highcharter)
library(lubridate)
library(stringr)

setwd("C:/Users/Ezekiel/Desktop/R/private-server/RugbyAnalytics")

options(stringsAsFactors = FALSE)


rugbyAwayEurope <- read_rds("rugbyAwayEurope.RDS")
rugbyHomeEurope <- read_rds("rugbyHomeEurope.RDS")
rugbyAwayT14 <- read_rds("rugbyAwayT14.RDS")
rugbyHomeT14 <- read_rds("rugbyHomeT14.RDS")

rugbyAwayEurope <- rugbyAwayEurope %>% mutate("Home/Away" = "Away", "Competition" = "European")

rugbyHomeEurope <- rugbyHomeEurope %>% mutate("Home/Away" = "Home", "Competition" = "European")

rugbyAwayT14 <- rugbyAwayT14 %>% mutate("Home/Away" = "Away", "Competition" = "T14")

rugbyHomeT14 <- rugbyHomeT14 %>% mutate("Home/Away" = "Home", "Competition" = "T14")

rugbybase <- bind_rows(rugbyAwayEurope, rugbyAwayT14, rugbyHomeEurope, rugbyHomeT14)

rugbybase$Team <- ifelse(rugbybase$`Home/Away` == "Away", rugbybase$Away, rugbybase$Home)
rugbybase$Opposition <- ifelse(rugbybase$`Home/Away` == "Home", rugbybase$Away, rugbybase$Home)

rugbybase$weeks <- substr(rugbybase$Date.Time, 5, nchar(rugbybase$Date.Time))

rugbybase$weeks <- as.Date(rugbybase$weeks, "%d %B %Y")

rugbybase$week <- isoweek(rugbybase$weeks)-isoweek('2017-08-20')

rugbybase$MissedTackles <- as.numeric(rugbybase$MissedTackles)
rugbybase$Turnovers <- as.numeric(rugbybase$Turnovers)



rugbybase$PositionGroup <- ifelse(rugbybase$Position %in% c("2","16"), "Hooker", 
                                              ifelse(rugbybase$Position %in% c("1","3","17","23"), "Prop",
                                                     ifelse(rugbybase$Position %in% c("4","5"), "2ndRow",
                                                            ifelse(rugbybase$Position %in% c("6","7","8"), "BackRow",
                                                                   ifelse(rugbybase$Position %in% c("9"), "HalfBack",
                                                                          ifelse(rugbybase$Position %in% c("10"), "FlyHalf",
                                                                                 ifelse(rugbybase$Position %in% c("12","13"), "Midfeilders",
                                                                                        ifelse(rugbybase$Position %in% c("11","14","15"), "OutsideBacks","Reserve"
                                                                                        ))))))))


###--------------------------------------------###


x <- as.data.frame(
  rugbybase %>%
  filter(Home == "Stade Francais") %>%
  group_by("Oposition" =Away, week) %>%
  summarise(Score = min(Score)) %>%
  mutate(Played = "Home")
  )

y <- as.data.frame(
  rugbybase %>%
  filter(Away == "Stade Francais") %>%
  group_by("Oposition" = Home, week) %>%
  summarise(Score = min(Score)) %>%
  mutate(Played = "Away")
  )
  
Oposition <- bind_rows(x, y)

Oposition$homeScore <- as.numeric(str_extract(Oposition$Score, "[0-9]+"))

Oposition$awayScore <- as.numeric(substr(Oposition$Score, as.numeric(nchar(Oposition$homeScore) + 4), nchar(Oposition$Score)))

Oposition$Winner <- ifelse(Oposition$awayScore>Oposition$homeScore&Oposition$Played == "Home",
                           Oposition$Oposition,
                           ifelse(Oposition$awayScore<Oposition$homeScore&Oposition$Played == "Away",
                                  Oposition$Oposition,
                                  "Stade Francais"))

stadeplayerbase <- as.data.frame(readRDS(file = "stadeplayerbase.RDS"))

stadeplayerbase <- left_join(stadeplayerbase, Oposition, by = "week")


stadeplayertable <- stadeplayerbase %>% select(Names, Position, week, Played, homeScore, awayScore, Oposition, Winner, Mins, Tackles, MissedTackles, Turnovers)

stadeplayertable$Names <- trimws(stadeplayertable$Names)


saveRDS(stadeplayertable, "stadeplayertable.RDS")
###--------------------------------------------###
# 
# quickgraphbase <- rugbybase %>% select(Team, Names, Position, Score, as.numeric(Mins), as.numeric(Tackles), as.numeric(MissedTackles), as.numeric(Turnovers), week, Competition)
# quickgraphbase$MissedTackles <- as.numeric(quickgraphbase$MissedTackles)
# 
# quickgraphbase <- quickgraphbase %>% mutate(TackleP = (Tackles/(Tackles + MissedTackles))) %>% filter(Competition == "T14")
# 
# grahp1 <- quickgraphbase %>% group_by(Team, week) %>% summarise(TackleP = mean(TackleP, na.rm = TRUE))
# hchart(grahp1, "line", hcaes(x = week, y = TackleP, group = Team))
#   
# 
# ##Need to rerun away T14 for new week
# 
# #make a plan of attack and work on it
# 
# 
# props <- quickgraphbase %>% filter(Position == "1" | Position == "3")
# hchart(props, "column", hcaes(x = Team, y = Tackles, group = Names, color = Team)) %>%
#   hc_plotOptions(column = list(stacking = "normal"))

###--------------------------------------------###


PositionAveragesTable <- rugbybase %>% select(Team, Position, PositionGroup, week, Tackles, Mins, MissedTackles, Turnovers)

#add new group (plus subs) Hookers 2,16 Props 1,3,17,23, 2nd Row 4,5,  Back row 6,7,8, Half Back, Fly Half, Midfeilders 12, 13, Outside Backs 11, 14, 15 


### week

#team
Averages.TeamAverage <- PositionAveragesTable %>% 
  group_by(Team, week) %>%
  summarise(Tackles.TeamAverage = mean(Tackles, na.rm = TRUE),
            TacklesPerMin.TeamAverage = mean(Tackles/Mins*80, na.rm = TRUE),
            TacklePercentage.TeamAverage = mean(Tackles/(Tackles + MissedTackles), na.rm = TRUE)
            )
#competition
Averages.CompetitionAverage <- PositionAveragesTable %>% 
  group_by(week) %>%
  summarise(Tackles.CompetitionAverage = mean(Tackles, na.rm = TRUE),
            TacklesPerMin.CompetitionAverage = mean(Tackles/Mins*80, na.rm = TRUE),
            TacklePercentage.CompetitionAverage = mean(Tackles/(Tackles + MissedTackles), na.rm = TRUE)
  )
#team Position
Averages.TeamAverage.Position <- PositionAveragesTable %>% 
  group_by(Team, PositionGroup, week) %>%
  summarise(Tackles.TeamAverage.Position = mean(Tackles, na.rm = TRUE),
            TacklesPerMin.TeamAverage.Position = mean(Tackles/Mins*80, na.rm = TRUE),
            TacklePercentage.TeamAverage.Position = mean(Tackles/(Tackles + MissedTackles), na.rm = TRUE)
  )
#competition Position
Averages.CompetitionAverage.Position <- PositionAveragesTable %>% 
  group_by(PositionGroup, week) %>%
  summarise(Tackles.CompetitionAverage.Position = mean(Tackles, na.rm = TRUE),
            TacklesPerMin.CompetitionAverage.Position = mean(Tackles/Mins*80, na.rm = TRUE),
            TacklePercentage.CompetitionAverage.Position = mean(Tackles/(Tackles + MissedTackles), na.rm = TRUE)
  )


### summary week

stadeplayerbase <- rugbybase %>%
  select(Names, Team, Position, PositionGroup, week, Tackles, Mins, MissedTackles, Turnovers) %>%
  filter(Team == "Stade Francais")

stadeplayerbase <-  left_join(stadeplayerbase, Averages.TeamAverage, by = c("Team", "week"))
stadeplayerbase <-  left_join(stadeplayerbase, Averages.CompetitionAverage, by = c("week"))
stadeplayerbase <-  left_join(stadeplayerbase, Averages.TeamAverage.Position, by = c("Team", "PositionGroup", "week"))
stadeplayerbase <-  left_join(stadeplayerbase, Averages.CompetitionAverage.Position, by = c("PositionGroup", "week"))

saveRDS(stadeplayerbase, "stadeplayerbase.RDS")


### no week
stadeplayerbase$Names <- trimws(stadeplayerbase$Names)


stadeplayertotals <- stadeplayerbase %>%
  group_by(Names, Team, PositionGroup) %>%
  summarise(Tackles = mean(Tackles, na.rm = TRUE), TacklesWeighted = mean(Tackles/Mins*80, na.rm = TRUE), TacklePercentage = mean(Tackles/(Tackles + MissedTackles), na.rm = TRUE))


#########


# stadeplayertotals <- rugbybase %>%
#   select(Names, Team, Position, PositionGroup, Tackles, Mins, MissedTackles, Turnovers) %>%
#   filter(Team == "Stade Francais")%>%
#   group_by(Names, Team, PositionGroup) %>%
#   summarise(Tackles = mean(Tackles, na.rm = TRUE), TacklesWeighted = mean(Tackles/Mins*80, na.rm = TRUE), TacklePercentage = mean(Tackles/(Tackles + MissedTackles), na.rm = TRUE))
  

Totals.Averages.TeamAverage <- PositionAveragesTable %>% 
  group_by(Team) %>%
  summarise(Tackles.TeamAverage = mean(Tackles, na.rm = TRUE),
            TacklesPerMin.TeamAverage = mean(Tackles/Mins*80, na.rm = TRUE),
            TacklePercentage.TeamAverage = mean(Tackles/(Tackles + MissedTackles), na.rm = TRUE)
  )
#competition
Totals.Averages.CompetitionAverage <- PositionAveragesTable %>% 
  summarise(Tackles.CompetitionAverage = mean(Tackles, na.rm = TRUE),
            TacklesPerMin.CompetitionAverage = mean(Tackles/Mins*80, na.rm = TRUE),
            TacklePercentage.CompetitionAverage = mean(Tackles/(Tackles + MissedTackles), na.rm = TRUE)
  )
Totals.Averages.CompetitionAverage$Team <- "Stade Francais"

#team Position
Totals.Averages.TeamAverage.Position <- PositionAveragesTable %>% 
  group_by(Team, PositionGroup) %>%
  summarise(Tackles.TeamAverage.Position = mean(Tackles, na.rm = TRUE),
            TacklesPerMin.TeamAverage.Position = mean(Tackles/Mins*80, na.rm = TRUE),
            TacklePercentage.TeamAverage.Position = mean(Tackles/(Tackles + MissedTackles), na.rm = TRUE)
  )
#competition Position
Totals.Averages.CompetitionAverage.Position <- PositionAveragesTable %>% 
  group_by(PositionGroup) %>%
  summarise(Tackles.CompetitionAverage.Position = mean(Tackles, na.rm = TRUE),
            TacklesPerMin.CompetitionAverage.Position = mean(Tackles/Mins*80, na.rm = TRUE),
            TacklePercentage.CompetitionAverage.Position = mean(Tackles/(Tackles + MissedTackles), na.rm = TRUE)
  )


#########

stadeplayertotals <-  left_join(stadeplayertotals, Totals.Averages.TeamAverage, by = c("Team"))
stadeplayertotals <-  left_join(stadeplayertotals, Totals.Averages.CompetitionAverage, by = c("Team"))
stadeplayertotals <-  left_join(stadeplayertotals, Totals.Averages.TeamAverage.Position, by = c("Team", "PositionGroup"))
stadeplayertotals <-  left_join(stadeplayertotals, Totals.Averages.CompetitionAverage.Position, by = c("PositionGroup"))



saveRDS(stadeplayertotals, "stadeplayertotals.RDS")

################






Totals.CompetitionTAverage <- PositionAveragesTable %>% 
  summarise(Tackles.CompetitionTAverage = mean(Tackles, na.rm = TRUE),
            TacklesPerMin.CompetitionTAverage = mean(Tackles/Mins*80, na.rm = TRUE),
            TacklePercentage.CompetitionTAverage = mean(Tackles/(Tackles + MissedTackles), na.rm = TRUE)
  )  



Totals.PositionGroupAveragesTotal <- PositionAveragesTable %>% 
  group_by(PositionGroup) %>%
  summarise(Tackles.CompetitionAverage.Position = mean(Tackles, na.rm = TRUE),
            TacklesPerMin.TeamAverage = mean(Tackles/Mins*80, na.rm = TRUE),
            TacklePercentage.CompetitionAverage.Position = mean(Tackles/(Tackles + MissedTackles), na.rm = TRUE)
  ) 
Totals.PositionGroupAveragesTotal$Team <- "Stade Francais"



###############




Comparison <- rugbybase %>% 
  select(Names, Position, week, Team, Opposition, Mins, Tackles, MissedTackles, Turnovers)

Comparisontotals <- Comparison %>%
  group_by(Names, Team) %>%
  summarise(Tackles = mean(Tackles, na.rm = TRUE), TacklesWeighted = mean(Tackles/Mins*80, na.rm = TRUE), TacklePercentage = mean(Tackles/(Tackles + MissedTackles), na.rm = TRUE))



saveRDS(Comparison, "Comparison.RDS")

saveRDS(Comparisontotals, "Comparisontotals.RDS")
