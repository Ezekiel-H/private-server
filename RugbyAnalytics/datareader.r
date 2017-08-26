library(dplyr)

#read in data and save as RDS file


ForAgainst <- read.csv(file = "Assets/Data/ForAgainst.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
TeamOverview <- read.csv(file = "Assets/Data/TeamOverview.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)


TeamSummary <- read.csv(file = "Assets/Data/TeamSummary.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
ShirtNo <- read.csv(file = "Assets/Data/ShirtNo.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
Carries <- read.csv(file = "Assets/Data/Carries.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

AttackC <- read.csv(file = "Assets/Data/AttackC.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
AttackQ <- read.csv(file = "Assets/Data/AttackQ.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

Defence <- read.csv(file = "Assets/Data/Defence.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
DefenceTotals <- read.csv(file = "Assets/Data/DefenceTotals.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

TacklesMade <- read.csv(file = "Assets/Data/TacklesMade.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
TacklesMissed <- read.csv(file = "Assets/Data/TacklesMissed.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
Misc <- read.csv(file = "Assets/Data/Misc.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

# Kicks <- read_excel("C:/Users/ehaggart001/Desktop/R/RUGBY/Assets/Data/StadeFrancais.xlsx", sheet = "Kicks")
# Pens <- read_excel("C:/Users/ehaggart001/Desktop/R/RUGBY/Assets/Data/StadeFrancais.xlsx", sheet = "Pens")
# Turrnovers <- read_excel("C:/Users/ehaggart001/Desktop/R/RUGBY/Assets/Data/StadeFrancais.xlsx", sheet = "Turnovers")
# Handling <- read_excel("C:/Users/ehaggart001/Desktop/R/RUGBY/Assets/Data/StadeFrancais.xlsx", sheet = "Handling")

PriorPlayer <- read.csv(file = "Assets/Data/PriorPlayer.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)


PlayerData <- read.csv(file = "Assets/Data/stadeScrape.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

PriorPlayer <- full_join(PriorPlayer, PlayerData, by = "Player")

#PlayerData$firstlower <- strsplit(PlayerData$name, " ")

######

#playerprofiling


CarriesPlayer <- Carries %>% filter(Type == "Total")

joinner <- merge(DefenceTotals, CarriesPlayer, by = "Player")
joinner <- merge(TeamOverview, joinner, by = "Player", all = TRUE)

joinner <- merge(PriorPlayer, joinner, by = "Player", all = TRUE)
joinner <- merge(Misc, joinner, by = "Player", all = TRUE)

joinner <- joinner %>% mutate(lastlower = sapply(joinner$NameLast, tolower))


saveRDS(joinner, file = "playerprofile.rds")



######

##manipulate some tables yo

Totals <- joinner %>%
  summarise("MissedTackles" = mean(PercentageMade, na.rm = TRUE), "MissedPassiveTackles" = mean(PercentagePassiveMissed, na.rm = TRUE), "AverageTacklesPerGame" = mean(round(Total.Complete/Total.Mins*80, 2), na.rm = TRUE))

PositionT <- joinner %>%
  group_by(Position) %>%
  summarise("MissedTackles" = mean(PercentageMade, na.rm = TRUE), "MissedPassiveTackles" = mean(PercentagePassiveMissed, na.rm = TRUE), "AverageTacklesPerGame" = mean(round(Total.Complete/Total.Mins*80, 2), na.rm = TRUE))

Totals <- add_column(Totals, Position = "Totals")

Totals <- bind_rows(Totals, PositionT)

saveRDS(Totals, file = "totals.rds")

