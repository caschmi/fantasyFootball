library(nlme)
library(dplyr)

fantasy = read.csv("fantasyFootball.csv")

fantasy$occassion = fantasy$Year - 2016

fantasy$Agesquared <- (fantasy$Age)^2
fantasy$AgeKnotsquared <- (fantasy$AgeKnot.Centered)^2


#change correlation structure -- need to change data frame to include "occasion"
modelFantasy = lme(log(Performance + 5.3) ~ Age.Centered + Agesquared + AgeKnot.Centered + AgeKnotsquared + Rk + FantPos + Division + Age.Centered*FantPos + AgeKnot.Centered*FantPos, 
                    data = fantasy, 
                    random = ~1 + Age | PlayerID) 

summary(modelFantasy)


# Data set creation of only individuals who have ages before and after 28

fantasy = fantasy %>% group_by(Player) %>% mutate(maxAge = max(Age), 
                                                  minAge = min(Age))

fantasy_28 = fantasy %>% filter(maxAge > 28) %>% filter(minAge < 28)

fantasy_28 = fantasy_28 %>% ungroup()

# modelFantasy28 = modelFantasy = lme(Performance ~ Age + AgeKnot + Rk + FantPos + Division, 
                                    # data = fantasy, 
                                    # random = ~1 + Age + AgeKnot | PlayerID)
hist(fantasy$Performance)
hist(log(fantasy$Performance + 5.3))
hist(fantasy$Age)
