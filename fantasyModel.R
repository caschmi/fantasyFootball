library(nlme)
library(dplyr)

fantasy = read.csv("fantasyFootball.csv")

fantasy$occassion = fantasy$Year - 2016


#change correlation structure -- need to change data frame to include "occasion"
modelFantasy = lme(Performance ~ Age + AgeKnot + Rk + FantPos + Division, 
                    data = fantasy, 
                    random = ~1 + Age + AgeKnot | PlayerID) 

fantasy = fantasy %>% group_by(Player) %>% mutate(maxAge = max(Age), 
                                                  minAge = min(Age))

fantasy_28 = fantasy %>% filter(maxAge > 28) %>% filter(minAge < 28)

fantasy_28 = fantasy_28 %>% ungroup()

modelFantasy28 = modelFantasy = lme(Performance ~ Age + AgeKnot + Rk + FantPos + Division, 
                                    data = fantasy, 
                                    random = ~1 + Age + AgeKnot | PlayerID)
