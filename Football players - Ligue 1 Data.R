setwd('R practice')

install.packages('readxl')
library(readxl)

ligue1 <- read_excel("~/Desktop/Ligue1.xlsx")
ligue1 <- data.frame(ligue1)

table(is.na(ligue1))
#keep in mind there is 7005 blocks that have no data. 
#I decided not to remove them (clean the data) because most of these "NA" values
#are found in somewhat unsignificant stats (at least i haven't used them) and because
#i dont know the players. 
#But you could be potentially missing out on a promising unknown player. 
#I'd ask why some of the data is missing (maybe they havent played idk)



#objective: find promising young strickers in ligue 1 based on various stats
#to find them I will create a new data.frame where all ligue 1 players 
#will be sorted by the following stats:

# - younger than 23 years old
# - their goal ratio per minutes played being lower than 3.0
# - having played more than 720 minutes/8 games (for data to be more reliable)
# - and to have scored more than 3 goals

#once these players are sorted out I will add the following stats 
#inside the new data.frame where they are stored to compare them:

# - player name
# - team
# - nationality
# - foot
# - height
# - weight
# - position 
# - age
# - market value 
# - minutes played
# - matches played
# - goals
# - assists
# - total shots
# - shots on target %
# - penalites

# + some custom stats:

# - actual game time
# - goal ratio per minute
# - shot ration per game
# - shooting clinicality


youngsters <- data.frame(ligue1$Player[(ligue1$Age < 24) & 
                                         (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                         (ligue1$Minutes.played > 720) &
                                         (ligue1$Goals > 3)],
                         ligue1$Team[(ligue1$Age < 24) & 
                                       (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                       (ligue1$Minutes.played > 720) &
                                       (ligue1$Goals > 3)],
                         ligue1$Birth.country[(ligue1$Age < 24) & 
                                                (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                                (ligue1$Minutes.played > 720) &
                                                (ligue1$Goals > 3)],
                         ligue1$Foot[(ligue1$Age < 24) & 
                                       (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                       (ligue1$Minutes.played > 720) &
                                       (ligue1$Goals > 3)],
                         ligue1$Height[(ligue1$Age < 24) & 
                                         (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                         (ligue1$Minutes.played > 720) &
                                         (ligue1$Goals > 3)],
                         ligue1$Weight[(ligue1$Age < 24) & 
                                         (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                         (ligue1$Minutes.played > 720) &
                                         (ligue1$Goals > 3)],
                         ligue1$Position[(ligue1$Age < 24) & 
                                           (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                           (ligue1$Minutes.played > 720) &
                                           (ligue1$Goals > 3)],
                         ligue1$Age[(ligue1$Age < 24) & 
                                      (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                      (ligue1$Minutes.played > 720) &
                                      (ligue1$Goals > 3)],
                         ligue1$Market.value[(ligue1$Age < 24) & 
                                               (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                               (ligue1$Minutes.played > 720) &
                                               (ligue1$Goals > 3)],
                         ligue1$Minutes.played[(ligue1$Age < 24) & 
                                                 (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                                 (ligue1$Minutes.played > 720) &
                                                 (ligue1$Goals > 3)],
                         ligue1$Matches.played[(ligue1$Age < 24) & 
                                                 (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                                 (ligue1$Minutes.played > 720) &
                                                 (ligue1$Goals > 3)],
                         ligue1$Goals[(ligue1$Age < 24) & 
                                        (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                        (ligue1$Minutes.played > 720) &
                                        (ligue1$Goals > 3)],
                         ligue1$Assists[(ligue1$Age < 24) & 
                                          (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                          (ligue1$Minutes.played > 720) &
                                          (ligue1$Goals > 3)],
                         ligue1$Shots.total...41[(ligue1$Age < 24) & 
                                                   (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                                   (ligue1$Minutes.played > 720) &
                                                   (ligue1$Goals > 3)],
                         ligue1$Shots.on.target..[(ligue1$Age < 24) & 
                                                    (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                                    (ligue1$Minutes.played > 720) &
                                                    (ligue1$Goals > 3)],
                         ligue1$Penalties.taken[(ligue1$Age < 24) & 
                                                  (ligue1$Minutes.played/ligue1$Goals/100 < 3.0) &
                                                  (ligue1$Minutes.played > 720) &
                                                  (ligue1$Goals > 3)])

names(youngsters) <- c('player', 'team','nation', 'foot', 'height', 'weight',
                       'position', 'age', 'value', 'min.played', 'match.played', 
                       'goals', 'assists', 'totalshots', 'shots.on.target%', 'penalties')

youngsters$actual.game.time <- youngsters$min.played/90
youngsters$goalratio.permin <- youngsters$min.played/youngsters$goals/100
youngsters$shotratio.pergame <- youngsters$totalshots/youngsters$actual.game.time
youngsters$clinicalshot <- youngsters$totalshots/youngsters$goals

#most impressive/promising: J.Maja

#pros:
#J.Maja seems to be the most promising young striker with 6 goals and 2 assists
#in only 9 "actual games"(847 mins) + an impresive goal ration per minute of 1.4 
#(only second after mbappe) while having the best clinical shooting out of the 
#youngsters with 1/3 shots ending with a goal, and the best accuracy with 66% 
#of shots on target. 

#cons:
#However, for his impresive stats he has played less than half 
#of the games in the season, so to make a better judgement it can be argued that
#more play time is needed of him to account for time period bias.

