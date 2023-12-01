library(dplyr)
library(ggplot2)
library(gridExtra)
library(knitr)

### Visualization of all positions ### 
fantasy = read.csv("fantasyFootball.csv")

ggplot(fantasy, aes(y = Performance, x = Age, group = FantPos)) + 
  geom_smooth(aes(color = FantPos), method = 'loess') +
  theme_bw() + 
  ggtitle("LOESS Smoothed Curve for Fantasy Football Performance")

ggplot(fantasy, aes(y = logPerformance, x = Age, group = FantPos)) + 
  geom_smooth(aes(color = FantPos), method = 'loess') +
  theme_bw() + 
  ggtitle("LOESS Smoothed Curve for Fantasy Football Performance")

### Creating sample of 20 players ###
set.seed(11032023)

fantasy_sample = fantasy[fantasy$Player %in% slice_sample(fantasy, n = 20)$Player, ] %>%
  arrange(Player)

### Spaghetti plot ###
#png()
ggplot(fantasy_sample, aes(y = Performance, x = Age, group = Player)) +
  geom_point() +
  geom_line() + 
  theme_bw() + 
  ggtitle("Performance of random sample of 20 players over age ")
#dev.off()



### Count of players in each position in the random sample above ###
table(fantasy_sample$FantPos)

### Random sample, 20 from each position ###
sample = fantasy[fantasy$Player %in% slice_sample(fantasy, n = 20, by = FantPos)$Player, ] %>%
  arrange(Player)




### Visualization -- Running backs ###
RB = sample %>% filter(FantPos == "RB")

RB_smooth = ggplot(RB, aes(y = Performance, x = Age)) + 
  geom_smooth(method = 'loess') +
  theme_bw() + 
  ggtitle("LOESS RB trajectory over age")

RB_spah = ggplot(RB, aes(y = Performance, x = Age, group = Player)) +
  geom_point() +
  geom_line() + 
  theme_bw() + 
  ggtitle("Performance of 20 RB's over age ")




### Visualization -- Quarterbacks ###
QB = sample %>% filter(FantPos == "QB")

QB_smooth = ggplot(QB, aes(y = Performance, x = Age)) + 
  geom_smooth(method = 'loess') +
  theme_bw() +
  ggtitle("LOESS QB trajectory over age")

QB_spagh = ggplot(QB, aes(y = Performance, x = Age, group = Player)) +
  geom_point() +
  geom_line() + 
  theme_bw() + 
  ggtitle("Performance of 20 QB's over age ") 




### Visualization -- Tight End ###
TE = sample %>% filter(FantPos == "TE")

TE_smooth = ggplot(TE, aes(y = Performance, x = Age)) + 
  geom_smooth(method = 'loess') +
  theme_bw() +
  ggtitle("LOESS TE trajectory over age")

TE_spagh = ggplot(TE, aes(y = Performance, x = Age, group = Player)) +
  geom_point() +
  geom_line() + 
  theme_bw() + 
  ggtitle("Performance of 20 TE's over age ")




### Visualization -- WIde Receiver ###
WR = sample %>% filter(FantPos == "WR")

WR_smooth = ggplot(RB, aes(y = Performance, x = Age)) + 
  geom_smooth(method = 'loess') +
  theme_bw() +
  ggtitle("LOESS WR trajectory over age")

WR_spagh = ggplot(WR, aes(y = Performance, x = Age, group = Player)) +
  geom_point() +
  geom_line() + 
  theme_bw() + 
  ggtitle("Performance of 20 WR's over age ")




### Filtered Visualization ### 
fantasy = fantasy %>% group_by(Player) %>% mutate(maxAge = max(Age), 
                                                  minAge = min(Age))

fantasy_28 = fantasy %>% filter(maxAge > 28) %>% filter(minAge < 28)

fantasy_28 = fantasy_28 %>% ungroup()

set.seed(11032023)

fantasy_28_sample = fantasy_28[fantasy_28$Player %in% slice_sample(fantasy_28, n = 20)$Player, ] %>%
  arrange(Player)

### Spaghetti plot ###

#png()
ggplot(fantasy_28_sample, aes(y = Performance, x = Age, group = Player)) +
  geom_point() +
  geom_line() + 
  theme_bw() + 
  ggtitle("Performance of 20 players within target age range") +
  labs(caption = "Target age range is players who played in the NFL before and after they turned 28.")
#dev.off()



### Descriptive statistics, grouped ###
summary_table <- fantasy %>%
  group_by(FantPos) %>%
  summarize(
    Age = paste(round(mean(Age, na.rm = TRUE), 2), " (", round(sd(Age, na.rm = TRUE), 2), ")"),
    Rank = paste(round(mean(Rk, na.rm = TRUE), 2), " (", round(sd(Rk, na.rm = TRUE), 2), ")"),
    Performance = paste(round(mean(Performance, na.rm = TRUE), 2), " (", round(sd(Performance, na.rm = TRUE), 2), ")")
  )

transposed_summary_table <- as.data.frame(t(summary_table))
colnames(transposed_summary_table) <- paste(transposed_summary_table[1, ], "(N, Mean (SD))")
transposed_summary_table <- transposed_summary_table[-1, ]

transposed_summary_table %>%
  kable()



### Checking normality of response ###
#png()
hist(fantasy$Performance, main = "Distribution of Performance from 2017 to 2023", xlab = "Performance") 
#dev.off()
