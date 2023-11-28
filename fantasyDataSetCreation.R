fantasy = read.csv("fantasy_merged_7_17.csv")

### Creating outcome variable with performance point calculation
attach(fantasy)
fantasy$Performance <- 0.1*(RushYds+RecYds) + 6*(RushTD+RecTD) - 2*FL + 0.04*Yds + 4*TD - 2*Int

fantasy$AgeKnot = (Age > 28) * (Age - 28)

detach(fantasy)

write.csv(fantasy, "fantasyFootball.csv")
