library(nlme)
library(dplyr)

fantasy = read.csv("fantasyFootball.csv")

fantasy$occassion = fantasy$Year - 2016

fantasy$Agesquared <- (fantasy$Age)^2
fantasy$AgeKnotsquared <- (fantasy$AgeKnot.Centered)^2
fantasy$logPerformance <- log(fantasy$Performance + 5.3)

#models for first scientific question: ----
#Does the trajectory of a player’s performance change after age 28?

#first analysis: all players, unadjusted model
model1 <- lme(logPerformance ~ Age.Centered + Agesquared + AgeKnot.Centered + AgeKnotsquared,
              data = fantasy,
              random = ~1 + Age.Centered | PlayerID)
summary(model1)

#model without squared terms
model2 <- lme(logPerformance ~ Age.Centered + AgeKnot.Centered,
              data = fantasy,
              random = ~1 + Age.Centered | PlayerID)
summary(model2)

anova(model1, model2)

#second analysis: all players, adjusted model
#change correlation structure -- need to change data frame to include "occasion"
modelFantasy1 = lme(logPerformance ~ Age.Centered + Agesquared + AgeKnot.Centered + AgeKnotsquared + Rk + FantPos + Division + Age.Centered*FantPos + AgeKnot.Centered*FantPos, 
                    data = fantasy, 
                    random = ~1 + Age.Centered | PlayerID) 

#final model without squared terms
modelFantasy = lme(logPerformance ~ Age.Centered + AgeKnot.Centered + Rk + FantPos + Division + Age.Centered*FantPos + AgeKnot.Centered*FantPos, 
                   data = fantasy, 
                   random = ~1 + Age.Centered | PlayerID) 

anova(modelFantasy1, modelFantasy)




summary(modelFantasy)
anova(modelFantasy)


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

#models for second scientific question: ----
#Does the performance of running back’s decline after age 28?

#filter data to only include running backs
fantasy_rb <- fantasy %>% filter(FantPos == "RB")

#first model: only running backs, unadjusted
model1 <- lme(logPerformance ~ Age.Centered + Agesquared + AgeKnot.Centered + AgeKnotsquared,
              data = fantasy,
              random = ~1 + Age | PlayerID)
summary(model1)



