library(readr)
library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)

setwd("C:/Users/Ezekiel/Desktop/R/private-server/RugbyAnalytics")

options(stringsAsFactors = FALSE)

# RugbyAwayEC <- as.data.frame(read_csv("Assets/Data/Paris/RugbyAwayEC.csv"))
# RugbyHomeEC <- read_csv("Assets/Data/Paris/RugbyHomeEC.csv")
# rugbyAwayEurope <- read_csv("Assets/Data/Paris/rugbyAwayEurope.csv")
# rugbyAwayEurope <- read_csv("Assets/Data/Paris/rugbyAwayEurope.csv")
# 
# 
# Rugby2 <- read_csv("Assets/Data/Paris/rugby(2).csv")
# Rugby3 <- read_csv("Assets/Data/Paris/rugby(3).csv")
# 
# rugby <- cbind(Rugby2, Rugby3)
# 
# rugby
# 
# rugby <- as.data.frame(Rugby$player)
# 
# rugby <- as.character(rugby)
# rugby <- strsplit(Rugby$player, ".")
# rugby <- as.data.frame(rugby)
# 
# as.data.frame(rugby) %>% separate(rugby, into = paste("V", 1:24, sep = "."))
# 
# x <- separate(rugby, `Rugby$player`, into = "a", sep = ".")

###-----------------------------------###

rugby <- read_csv("Assets/Data/Paris/rugbyAwayEurope.csv")

rugby <- as.data.frame(rugby)

x <- strsplit(as.character(rugby$element), "[.]")

x <- as.data.frame(x)

names(x) <- lapply(x[1, ], as.character)

x <- x[-1,] 


y <- t(x)
names <- 1:23
names(y) <- print(names)
y <- as.data.frame(y)
y$team <- sapply(strsplit(rownames(y), "Minutes"), "[", 1)
y <- data.frame(lapply(y, as.character), stringsAsFactors=FALSE)
rownames(y) <- c()


rugby <- rugby %>% select(Home, Away, Score, `Date/Time`)
rugby <- data.frame(lapply(rugby, as.character), stringsAsFactors=FALSE)



w <- bind_cols(rugby, y)

z <- melt(w, id = c("Home", "Away", "Score", "Date.Time"))


z$numbers <- as.numeric(str_extract(z$value, "[0-9]+"))
z$Names <- (str_extract(z$value, "[aA-zZ ]+"))

z <- z %>% mutate("Position" = (as.numeric(str_extract(z$variable, "[0-9]+"))-1))

z <- z %>% select(Home, Away, Score, Date.Time, Names, Position, variable, numbers, value)

z$numbers <- ifelse(z$Position < 9, substr(z$numbers,1, nchar(z$numbers)-1), substr(z$numbers,1,nchar(z$numbers)-2))


w <- group_by(y)

#using numbers we can pull out the specific data

z$Turnovers <- substr(z$numbers, nchar(z$numbers), nchar(z$numbers))

z$MissedTackles <- substr(z$numbers, nchar(z$numbers)-1, nchar(z$numbers)-1)

z$Tackles <- as.numeric(ifelse(nchar(z$numbers)>5, substr(z$numbers, nchar(z$numbers)-3, nchar(z$numbers)-2), substr(z$numbers, nchar(z$numbers)-2, nchar(z$numbers)-2)))

z$Mins <- as.numeric(ifelse(nchar(z$numbers)>4, substr(z$numbers, 1, 2), substr(z$numbers, 1, 1)))


rugbyAwayEurope <- z

saveRDS(rugbyAwayEurope, "rugbyAwayEurope.RDS")

###-----------------------------------###
# 
# write.csv(x, file = "DataSet.csv")
# 
# 
# 
# 
# x %>% separate(abc, into = c('name', 'number'), sep = -4, convert = TRUE)
# 
# 
# x
# 
# data.frame(t(sapply(Rugby$player, [,2], function(y) strsplit(y,split=":")[[1]])))