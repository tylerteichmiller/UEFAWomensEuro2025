###############################################################################
#
# This builds our dataset
#
###############################################################################
# libraries
library(tidyverse)
library(rvest)
###############################################################################
# 2025 Data
#  Group A
homea2025 <- c("Iceland", "Switzerland", "Norway", "Switzerland", "Finland",
               "Norway")
awaya2025 <- c("Finland", "Norway", "Finland", "Iceland",
               "Switzerland", "Iceland")
groupa25 <- as.factor(rep("A", times=6))
yeara25 <- as.factor(rep(2025, times=6))
a2025 <- data.frame(HomeTeam = homea2025, AwayTeam = awaya2025,
                    Group = groupa25, Year = yeara25)
#  Group B
homeb2025 <- c("Belgium", "Spain", "Spain", "Portugal", "Italy", "Portugal")
awayb2025 <- c("Italy", "Portugal", "Belgium", "Italy", "Spain", "Belgium")
groupb25 <- as.factor(rep("B", times=6))
b2025 <- data.frame(HomeTeam = homeb2025, AwayTeam = awayb2025, 
                    Group = groupb25, Year = yeara25)
#  Group C
homec2025 <- c("Denmark", "Germany", "Germany", "Poland", "Sweden", "Poland")
awayc2025 <- c("Sweden", "Poland", "Denmark", "Sweden", "Germany", "Denmark")
groupc25 <- as.factor(rep("C", times=6))
c2025 <- data.frame(HomeTeam = homec2025, AwayTeam = awayc2025,
                    Group = groupc25, Year = yeara25)
#  Group D
homed2025 <- c("Wales", "France", "England", "France", "Netherlands", "England")
awayd2025 <- c("Netherlands", "England", "Netherlands", "Wales", "France",
               "Wales")
groupd25 <- as.factor(rep("D", times=6))
d2025 <- data.frame(HomeTeam = homed2025, AwayTeam = awayd2025, 
                    Group = groupd25, Year = yeara25)
# Put 2025 Together
all2025 <- full_join(full_join(a2025, b2025), full_join(c2025, d2025))
# World Ranking as of 6 March, 2025 (most recent ranking)
url <- "https://en.wikipedia.org/wiki/UEFA_Women%27s_Euro_2025"
html <- read_html(url)
table <- html %>% 
  html_nodes("table")
a2025a <- table[10][[1]] %>% html_table()
b2025a <- table[11][[1]] %>% html_table()
c2025a <- table[12][[1]] %>% html_table()
d2025a <- table[13][[1]] %>% html_table()
teams25 <- full_join(full_join(a2025a,b2025a),full_join(c2025a, d2025a))$Team
ranking25 <- data.frame(Team = teams25, 
                        Rank = c(23, 15, 13, 25, 2, 22, 20, 14, 3, 28, 12, 6, 
                                 11, 4, 31, 10))
all2025 <- all2025 %>% left_join(ranking25, by = join_by(HomeTeam == Team)) %>%
  rename(HomeRank = Rank)
all2025 <- all2025 %>% left_join(ranking25, by = join_by(AwayTeam == Team)) %>%
  rename(AwayRank = Rank)
all2025$GameID <- c(1:24)
###############################################################################
# 2022 Data
#  Group A
homea2022 <- c("England", "Norway", "Austria", "England", "Northern Ireland",
               "Austria")
awaya2022 <- c("Austria", "Northern Ireland", "Northern Ireland", "Norway",
               "England", "Norway")
homegoalsa2022 <- c(1,4,2,8,0,1)
awaygoalsa2022 <- c(0,1,0,0,5,0)
groupa22 <- as.factor(rep("A", times=6))
yeara22 <- as.factor(rep(2022, times=6))
a2022 <- data.frame(HomeTeam = homea2022, AwayTeam = awaya2022,
                    HomeGoals = homegoalsa2022, AwayGoals = awaygoalsa2022,
                    Group = groupa22, Year = yeara22)
#  Group B
homeb2022 <- c("Spain", "Germany", "Denmark", "Germany", "Finland", "Denmark")
awayb2022 <- c("Finland", "Denmark", "Finland", "Spain", "Germany", "Spain")
homegoalsb2022 <- c(4,4,1,2,0,0)
awaygoalsb2022 <- c(1,0,0,0,3,1)
groupb22 <- as.factor(rep("B", times=6))
b2022 <- data.frame(HomeTeam = homeb2022, AwayTeam = awayb2022, 
                    HomeGoals = homegoalsb2022, AwayGoals = awaygoalsb2022, 
                    Group = groupb22, Year = yeara22)
#  Group C
homec2022 <- c("Portugal", "Netherlands", "Sweden", "Netherlands",
               "Switzerland", "Sweden")
awayc2022 <- c("Switzerland", "Sweden", "Switzerland", "Portugal",
               "Netherlands", "Portugal")
homegoalsc2022 <- c(2,1,2,3,1,5)
awaygoalsc2022 <- c(2,1,1,2,4,0)
groupc22 <- as.factor(rep("C", times=6))
c2022 <- data.frame(HomeTeam = homec2022, AwayTeam = awayc2022,
                    HomeGoals = homegoalsc2022, AwayGoals = awaygoalsc2022,
                    Group = groupc22, Year = yeara22)
#  Group D
homed2022 <- c("Belgium", "France", "Italy", "France", "Iceland", "Italy")
awayd2022 <- c("Iceland", "Italy", "Iceland", "Belgium", "France", "Belgium")
homegoalsd2022 <- c(1,5,1,2,1,0)
awaygoalsd2022 <- c(1,1,1,1,1,1)
groupd22 <- as.factor(rep("D", times=6))
d2022 <- data.frame(HomeTeam = homed2022, AwayTeam = awayd2022, 
                    HomeGoals = homegoalsd2022, AwayGoals = awaygoalsd2022, 
                    Group = groupd22, Year = yeara22)
# Put 2022 Together
all2022 <- full_join(full_join(a2022, b2022), full_join(c2022, d2022))
# World Ranking as of 17 June, 2022 (last before competition)
url <- "https://en.wikipedia.org/wiki/UEFA_Women%27s_Euro_2022"
html <- read_html(url)
table <- html %>% 
  html_nodes("table")
a2022a <- table[9][[1]] %>% html_table()
b2022a <- table[16][[1]] %>% html_table()
c2022a <- table[23][[1]] %>% html_table()
d2022a <- table[30][[1]] %>% html_table()
names(a2022a)[2] <- "Team"
names(b2022a)[2] <- "Team"
names(c2022a)[2] <- "Team"
names(d2022a)[2] <- "Team"
teams22 <- full_join(full_join(a2022a,b2022a),full_join(c2022a, d2022a))$Team
ranking22 <- data.frame(Team = teams22, 
                        Rank = c(8, 21, 11, 47, 5, 7, 15, 29, 2, 4, 20, 30, 3,
                                 19, 17, 14))
ranking22 <- ranking22 %>% 
  mutate(Team = str_replace(Team, " \\(H\\)", ""))
all2022 <- all2022 %>% left_join(ranking22, by = join_by(HomeTeam == Team)) %>%
  rename(HomeRank = Rank)
all2022 <- all2022 %>% left_join(ranking22, by = join_by(AwayTeam == Team)) %>%
  rename(AwayRank = Rank)
###############################################################################
# 2017 Data
#  Group A
homea2017 <- c("Netherlands", "Denmark", "Norway", "Netherlands", "Belgium",
               "Norway")
awaya2017 <- c("Norway", "Belgium", "Belgium", "Denmark", "Netherlands",
               "Denmark")
homegoalsa2017 <- c(1,1,0,1,1,0)
awaygoalsa2017 <- c(0,0,2,0,2,1)
groupa17 <- as.factor(rep("A", times=6))
yeara17 <- as.factor(rep(2017, times=6))
a2017 <- data.frame(HomeTeam = homea2017, AwayTeam = awaya2017,
                    HomeGoals = homegoalsa2017, AwayGoals = awaygoalsa2017,
                    Group = groupa17, Year = yeara17)
#  Group B
homeb2017 <- c("Italy", "Germany", "Sweden", "Germany", "Russia", "Sweden")
awayb2017 <- c("Russia", "Sweden", "Russia", "Italy", "Germany", "Italy")
homegoalsb2017 <- c(1,0,2,2,0,2)
awaygoalsb2017 <- c(2,0,0,1,2,3)
groupb17 <- as.factor(rep("B", times=6))
b2017 <- data.frame(HomeTeam = homeb2017, AwayTeam = awayb2017, 
                    HomeGoals = homegoalsb2017, AwayGoals = awaygoalsb2017,
                    Group = groupb17, Year = yeara17)
#  Group C
homec2017 <- c("Austria", "France", "Iceland", "France", "Switzerland",
               "Iceland")
awayc2017 <- c("Switzerland", "Iceland", "Switzerland", "Austria",
               "France", "Austria")
homegoalsc2017 <- c(1,1,1,1,1,0)
awaygoalsc2017 <- c(0,0,2,1,1,3)
groupc17 <- as.factor(rep("C", times=6))
c2017 <- data.frame(HomeTeam = homec2017, AwayTeam = awayc2017,
                    HomeGoals = homegoalsc2017, AwayGoals = awaygoalsc2017,
                    Group = groupc17, Year = yeara17)
#  Group D
homed2017 <- c("Spain", "England", "Scotland", "England", "Portugal",
               "Scotland")
awayd2017 <- c("Portugal", "Scotland", "Portugal", "Spain", "England", "Spain")
homegoalsd2017 <- c(2,6,1,2,1,1)
awaygoalsd2017 <- c(0,0,2,0,2,0)
groupd17 <- as.factor(rep("D", times=6))
d2017 <- data.frame(HomeTeam = homed2017, AwayTeam = awayd2017,
                    HomeGoals = homegoalsd2017, AwayGoals = awaygoalsd2017,
                    Group = groupd17, Year = yeara17)
# Put 2017 Together
all2017 <- full_join(full_join(a2017, b2017), full_join(c2017, d2017))
# World Ranking as of 23 June, 2017 (last before competition)
url <- "https://en.wikipedia.org/wiki/UEFA_Women%27s_Euro_2017"
html <- read_html(url)
table <- html %>% 
  html_nodes("table")
a2017a <- table[10][[1]] %>% html_table()
b2017a <- table[17][[1]] %>% html_table()
c2017a <- table[24][[1]] %>% html_table()
d2017a <- table[31][[1]] %>% html_table()
names(a2017a)[2] <- "Team"
names(b2017a)[2] <- "Team"
names(c2017a)[2] <- "Team"
names(d2017a)[2] <- "Team"
d2017a <- d2017a %>% mutate(
  Pts = ifelse(Pts == "3[a]", 3, 9))
teams17 <- full_join(full_join(a2017a,b2017a),full_join(c2017a, d2017a))$Team
ranking17 <- data.frame(Team = teams17, 
                        Rank = c(12, 15, 22, 11, 2, 9, 25, 18, 24, 3, 17, 19,
                                 5, 13, 21, 38))
ranking17 <- ranking17 %>% 
  mutate(Team = str_replace(Team, " \\(H\\)", ""))
all2017 <- all2017 %>% left_join(ranking17, by = join_by(HomeTeam == Team)) %>%
  rename(HomeRank = Rank)
all2017 <- all2017 %>% left_join(ranking17, by = join_by(AwayTeam == Team)) %>%
  rename(AwayRank = Rank)
###############################################################################
# 2013 Data
#  Group A
homea2013 <- c("Italy", "Sweden", "Italy", "Finland", "Sweden", "Denmark")
awaya2013 <- c("Finland", "Denmark", "Denmark", "Sweden", "Italy", "Finland")
homegoalsa2013 <- c(0,1,2,0,3,1)
awaygoalsa2013 <- c(0,1,1,5,1,1)
groupa13 <- as.factor(rep("A", times=6))
yeara13 <- as.factor(rep(2013, times=6))
a2013 <- data.frame(HomeTeam = homea2013, AwayTeam = awaya2013,
                    HomeGoals = homegoalsa2013, AwayGoals = awaygoalsa2013,
                    Group = groupa13, Year = yeara13)
#  Group B
homeb2013 <- c("Norway", "Germany", "Norway", "Iceland", "Germany",
               "Netherlands")
awayb2013 <- c("Iceland", "Netherlands", "Netherlands", "Germany", "Norway",
               "Iceland")
homegoalsb2013 <- c(1,0,1,0,0,0)
awaygoalsb2013 <- c(1,0,0,3,1,1)
groupb13 <- as.factor(rep("B", times=6))
b2013 <- data.frame(HomeTeam = homeb2013, AwayTeam = awayb2013,
                    HomeGoals = homegoalsb2013, AwayGoals = awaygoalsb2013,
                    Group = groupb13, Year = yeara13)
#  Group C
homec2013 <- c("France", "England", "England", "Spain", "France", "Russia")
awayc2013 <- c("Russia", "Spain", "Russia", "France", "England", "Spain")
homegoalsc2013 <- c(3,2,1,0,3,1)
awaygoalsc2013 <- c(1,3,1,1,0,1)
groupc13 <- as.factor(rep("C", times=6))
c2013 <- data.frame(HomeTeam = homec2013, AwayTeam = awayc2013,
                    HomeGoals = homegoalsc2013, AwayGoals = awaygoalsc2013,
                    Group = groupc13, Year = yeara13)
# Put 2013 Together
all2013 <- full_join(full_join(a2013, b2013), c2013)
# World Ranking as of 21 June, 2013 (last before competition)
url <- "https://en.wikipedia.org/wiki/UEFA_Women%27s_Euro_2013"
html <- read_html(url)
table <- html %>% 
  html_nodes("table")
a2013a <- table[8][[1]] %>% html_table()
b2013a <- table[15][[1]] %>% html_table()
c2013a <- table[22][[1]] %>% html_table()
names(a2013a)[1] <- "Team"
names(b2013a)[1] <- "Team"
names(c2013a)[1] <- "Team"
teams13 <- full_join(full_join(a2013a,b2013a),c2013a)$Team
ranking13 <- data.frame(Team = teams13, 
                        Rank = c(5, 12, 13, 21, 11, 2, 15, 14, 6, 18, 22, 7))
all2013 <- all2013 %>% left_join(ranking13, by = join_by(HomeTeam == Team)) %>%
  rename(HomeRank = Rank)
all2013 <- all2013 %>% left_join(ranking13, by = join_by(AwayTeam == Team)) %>%
  rename(AwayRank = Rank)
###############################################################################
# 2009 Data
#  Group A
homea2009 <- c("Ukraine", "Finland", "Ukraine", "Netherlands", "Finland",
               "Denmark")
awaya2009 <- c("Netherlands", "Denmark", "Denmark", "Finland", "Ukraine",
               "Netherlands")
homegoalsa2009 <- c(0,1,1,1,0,1)
awaygoalsa2009 <- c(2,0,2,2,1,2)
groupa09 <- as.factor(rep("A", times=6))
yeara09 <- as.factor(rep(2009, times=6))
a2009 <- data.frame(HomeTeam = homea2009, AwayTeam = awaya2009,
                    HomeGoals = homegoalsa2009, AwayGoals = awaygoalsa2009,
                    Group = groupa09, Year = yeara09)
#  Group B
homeb2009 <- c("Germany", "Iceland", "France", "Iceland", "Germany", "Norway")
awayb2009 <- c("Norway", "France", "Germany", "Norway", "Iceland", "France")
homegoalsb2009 <- c(4,1,1,0,1,1)
awaygoalsb2009 <- c(0,3,5,1,0,1)
groupb09 <- as.factor(rep("B", times=6))
b2009 <- data.frame(HomeTeam = homeb2009, AwayTeam = awayb2009,
                    HomeGoals = homegoalsb2009, AwayGoals = awaygoalsb2009,
                    Group = groupb09, Year = yeara09)
#  Group C
homec2009 <- c("Italy", "Sweden", "Italy", "England", "Russia", "Sweden")
awayc2009 <- c("England", "Russia", "Sweden", "Russia", "Italy", "England")
homegoalsc2009 <- c(2,3,0,3,0,1)
awaygoalsc2009 <- c(1,0,2,2,2,1)
groupc09 <- as.factor(rep("C", times=6))
c2009 <- data.frame(HomeTeam = homec2009, AwayTeam = awayc2009, 
                    HomeGoals = homegoalsc2009, AwayGoals = awaygoalsc2009,
                    Group = groupc09, Year = yeara09)
# Put 2009 Together
all2009 <- full_join(full_join(a2009, b2009), c2009)
# World Ranking as of 26 June, 2009 (last before competition)
# obtained from https://inside.fifa.com/fifa-world-ranking/women?dateId=ranking_20220617
url <- "https://en.wikipedia.org/wiki/UEFA_Women%27s_Euro_2009"
html <- read_html(url)
table <- html %>% 
  html_nodes("table")
teams09 <- full_join(full_join((table[4][[1]] %>% html_table()),
                     (table[11][[1]] %>% html_table())),
                     table[18][[1]] %>% html_table())$Team
ranking09 <- data.frame(Team = teams09, 
                        Rank = c(18, 17, 6, 16, 3, 8, 10, 19, 4, 13, 9, 15))
all2009 <- all2009 %>% left_join(ranking09, by = join_by(HomeTeam == Team)) %>%
  rename(HomeRank = Rank)
all2009 <- all2009 %>% left_join(ranking09, by = join_by(AwayTeam == Team)) %>%
  rename(AwayRank = Rank)
###############################################################################
# 2005 Data
#  Group A
homea2005 <- c("Sweden", "England", "England", "Sweden", "England", "Finland")
awaya2005 <- c("Denmark", "Finland", "Denmark", "Finland", "Sweden", "Denmark")
homegoalsa2005 <- c(1,3,1,0,0,2)
awaygoalsa2005 <- c(1,2,2,0,1,1)
groupa05 <- as.factor(rep("A", times=6))
yeara05 <- as.factor(rep(2005, times=6))
a2005 <- data.frame(HomeTeam = homea2005, AwayTeam = awaya2005,
                    HomeGoals = homegoalsa2005, AwayGoals = awaygoalsa2005,
                    Group = groupa05, Year = yeara05)
#  Group B
homeb2005 <- c("Germany", "France", "Germany", "Norway", "France", "Norway")
awayb2005 <- c("Norway", "Italy", "Italy", "France", "Germany", "Italy")
homegoalsb2005 <- c(1,3,4,1,0,5)
awaygoalsb2005 <- c(0,1,0,1,3,3)
groupb05 <- as.factor(rep("B", times=6))
b2005 <- data.frame(HomeTeam = homeb2005, AwayTeam = awayb2005,
                    HomeGoals = homegoalsb2005, AwayGoals = awaygoalsb2005,
                    Group = groupb05, Year = yeara05)
# Put 2005 Together
all2005 <- full_join(a2005, b2005)
# World Ranking as of 25 March, 2005 (last before competition)
# obtained from https://inside.fifa.com/fifa-world-ranking/women?dateId=ranking_20220617
url <- "https://en.wikipedia.org/wiki/UEFA_Women%27s_Euro_2005"
html <- read_html(url)
table <- html %>% 
  html_nodes("table")
teams05 <- full_join((table[3][[1]] %>% html_table()),
                     (table[10][[1]] %>% html_table()))$Team
ranking05 <- data.frame(Team = teams05, Rank = c(6, 16, 7, 14, 1, 3, 5, 10))
all2005 <- all2005 %>% left_join(ranking05, by = join_by(HomeTeam == Team)) %>%
  rename(HomeRank = Rank)
all2005 <- all2005 %>% left_join(ranking05, by = join_by(AwayTeam == Team)) %>%
  rename(AwayRank = Rank)

###############################################################################
# All Years (except 2025) in one Dataset
uefa <- full_join(full_join(all2005, all2009), 
                  full_join(all2013, full_join(all2017, all2022)))
unique(uefa$AwayTeam)
unique(uefa$HomeTeam)

