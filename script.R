library(tidyverse)
library(stargazer)
library(magrittr)
library(cepiigeodist)

df <- read_csv("mip.csv")
country_codes <- read_csv("cow-cc.csv")
ally <- read_csv("alliance_v4.1_by_directed_yearly.csv") %>% filter(ccode1 == 2)
unsc_member <- read_csv("DPPA-SCMEMBERSHIP.csv")



convert <- function(x){
  if (is.na(x)) {
    return(NA_real_)
  } else if (x == "USSR (Union of Soviet Socialist Republics)"){
    return(365)
  } else if(x == "United Arab Republic"){
    return(651)
  } else if(x == "United Kingdom of Great Britain and Northern Ireland"){
    return(200)
  } else if (x == "Venezuela (Bolivarian Republic of)") {
    return (101)
  } else if (x == "Syrian Arab Republic"){
    return(652)
  } else if (x == "United Republic of Tanzania") {
    return(510)
  } else if (x == "Bolivia (Plurinational State of)") {
    return(145)
  } else if (x == "German FR (German Federal Republic)") {
    return(255)
  }  else if (x == "German DR (German Democratic Republic)") {
    return(255)
  } else if (x == "Ukrainian SSR (Ukrainian Soviet Socialist Republic)") {
    return(369)
  } else if (x == "Cabo Verde") {
    return(402)
  } else if (x == "Czechia") {
    return(316)
  } else if (x == "Russian Federation") {
    return(365)
  } else if (x == "Republic of Korea") {
    return(730)
  } else if (x == "Iran (Islamic Republic of)") {
    return(630)
  } else if (x == "Viet Nam") {
    return(816)
  } else if (x == "Saint Vincent and the Grenadines") {
    return(57)
  } else {
    (country_codes %>% dplyr::filter(StateNme == x))[1,"CCode"] %>% as.numeric()
  }
}

unsc_member$ccode <- sapply(unsc_member$security_council_member, convert)
unsc_member %<>% na.omit()

unsc_member$isally = NA

isally <- function(ccode, year){
  t <- ally %>% dplyr::filter(ccode2 == ccode, year == year)
  return(nrow(t)>0)
}

for(i in 1:nrow(unsc_member)){
  unsc_member$isally[i] = isally(unsc_member$ccode[i], unsc_member$year[i])
}

yearly_unsc_allies <- unsc_member %>% group_by(year) %>% summarise(ally_count = sum(isally))
yearly_unsc_allies$plurality <- yearly_unsc_allies$ally_count >= 9

find_distance <- function(x){
  dest = (country_codes %>% filter(CCode == x))[1,"StateAbb"] %>% as.character()
  return((dist_cepii %>% filter(iso_o == "USA", iso_d == dest))[1, "distcap"] %>% as.numeric())
}

df$year = df$styear

df <- left_join(df, yearly_unsc_allies, by = "year")

df %<>% filter(!is.na(UNRES))
df$fatality <- as.numeric(df$Fatalities)
df$distance <- sapply(df$Location, find_distance)
df$log_distance <- log(df$distance)
df$target_polity <- df$`State B polity2`
df$trade_flow <- ifelse(df$`Trade flow1` <=0, NA, df$`Trade flow1`)
df$threat <- -scale(df$target_polity) -scale(df$distance)
df$iraq = df$`State B` == "IRQ"
df$cold_war = as.numeric(df$styear %in% 1946:1989)
df$MENA = as.numeric(df$Location %in% 600:699)
df$target_rGDPpc <- df$StateBrGDP / df$`State B tpop`

df %>% filter(`Oil Dummy` == 1) %>% select(`State B`) %>% as.vector()

r1 <- lm(UNRES ~ threat + log(trade_flow) + `Oil Dummy`+ factor(styear), data = df)
# r12<- lm(UNRES ~ threat + target_rGDPpc + factor(styear), data = df)
# r15 <- lm(UNRES ~ target_polity + factor(styear), data = df)
# r2 <- lm(UNRES ~ target_polity + log(distance) + log(trade_flow) + `Oil Dummy` + factor(styear), data = df)
r3 <- lm(UNRES ~ target_polity + scale(distance) + log(trade_flow) + `Oil Dummy` + factor(styear), data = df)

df$humanitarian <- grepl("Humanitarian", df$Objective) %>% as.numeric()
df$`own interest` <- grepl("own", df$Objective) %>% as.numeric()

r4 <- lm(UNRES ~ humanitarian + log(trade_flow) + `Oil Dummy`+ factor(styear), data = df)
r5 <- lm(UNRES ~ `own interest` + log(trade_flow) + `Oil Dummy`+ factor(styear), data = df)

stargazer(r1, r3, r4, r5, type = "text", omit = "year",
          add.lines = list(c("Time FEs", rep("Y",5))))


r6 <- lm(UNRES ~ ally_count+ log(trade_flow) + `Oil Dummy`+ factor(styear), data = df %>% filter(year <= 2012))
r7 <- lm(UNRES ~ plurality+ log(trade_flow) + `Oil Dummy`+ factor(styear), data = df %>% filter(year <= 2012))
r8 <- lm(UNRES ~ threat + humanitarian + `own interest` + ally_count + log(trade_flow) + `Oil Dummy`, data = df %>% filter(year <= 2012))
stargazer(r1, r3, r4, r5, r6, r7, r8, type = "html", omit = "year",
          add.lines = list(c("Time FEs", rep("Y",4), rep("N", 3))), out = "o.html")
