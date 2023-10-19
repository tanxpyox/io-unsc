library(tidyverse)
library(stargazer)
library(magrittr)
library(cepiigeodist)

df <- read_csv("mip.csv")
country_codes <- read_csv("cow-cc.csv")

find_distance <- function(x){
  dest = (country_codes %>% filter(CCode == x))[1,"StateAbb"] %>% as.character()
  return((dist_cepii %>% filter(iso_o == "USA", iso_d == dest))[1, "dist"] %>% as.numeric())
}


df %<>% filter(!is.na(UNRES))
df$fatality <- as.numeric(df$Fatalities)
df$distance <- sapply(df$Location, find_distance)
df$log_distance <- log(df$distance)
df$polity <- df$`State B polity2`
df$trade_flow <- ifelse(df$`Trade flow1` <=0, NA, df$`Trade flow1`)

r1 <- glm(UNRES ~ polity, data = df, family = "binomial")
r2 <- glm(UNRES ~ polity + log_distance, data = df, family = "binomial")
r3 <- glm(UNRES ~ polity + log_distance + log(trade_flow), data = df, family = "binomial")

df$humanitarian <- grepl("Humanitarian", df$Objective) %>% as.numeric()
df$defend <- grepl("own", df$Objective) %>% as.numeric()

r4 <- glm(UNRES ~ humanitarian, data = df, family = "binomial")
r5 <- glm(UNRES ~ humanitarian + defend, data = df, family = "binomial")

stargazer(r1, r2, r3, r4, r5, type = "text")
