library(dplyr)

fantasy = read.csv("fantasy_merged_7_17.csv")

### Creating outcome variable with performance point calculation
attach(fantasy)
fantasy$Performance <- 0.1*(RushYds+RecYds) + 6*(RushTD+RecTD) - 2*FL + 0.04*Yds + 4*TD - 2*Int

fantasy$AgeKnot = (Age > 28) * (Age - 28)

fantasy = fantasy %>% mutate(Division = case_when(Tm %in% c("MIA", "BUF", "NYJ", "NWE") ~ "AFC East",
                                                   Tm %in% c("BAL", "PIT", "CLE", "CIN") ~ "AFC North",
                                                   Tm %in% c("JAX", "HOU", "IND", "TEN") ~ "AFC South",
                                                   Tm %in% c("KAN", "LAC", "OAK", "LVR", "DEN") ~ "AFC West",
                                                   Tm %in% c("PHI", "DAL", "WAS", "NYG") ~ "NFC East",
                                                   Tm %in% c("DET", "MIN", "GNB", "CHI") ~ "NFC North",
                                                   Tm %in% c("NOR", "ATL", "TAM", "CAR") ~ "NFC South",
                                                   Tm %in% c("SFO", "SEA", "LAR", "ARI") ~ "NFC West",
                                                   TRUE ~ "NONE"
)
)

detach(fantasy)

# write.csv(fantasy, "fantasyFootball.csv")