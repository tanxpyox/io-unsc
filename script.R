library(tidyverse)
library(stargazer)
library(magrittr)
library(cepiigeodist)
library(future.apply)
mip <- read_csv("mip.csv")
df <- mip %>% mutate(dispnum = as.numeric(dispnum),
                     ccode = gsub("^([0-9]+).*", "\\1", `State B code`) %>%
                       as.numeric)



country_codes <- read_csv("cow-cc.csv")
ally <- read_csv("alliance_v4.1_by_directed_yearly.csv") %>% filter(ccode1 == 2)
unsc_member <- read_csv("DPPA-SCMEMBERSHIP.csv")

library(readxl)
polity <- read_excel("data/p5v2018.xls") %>% dplyr::select(ccode, year, polity2)

mid <- read_csv("data/MIDB 5.0.csv") %>%
  filter(styear >= 1946,
         ccode != 2)

mid %<>% filter(!(dispnum %in% na.omit(df$dispnum)))

df <- bind_rows(df, mid)

df %<>% mutate(
  intervene = case_when(
    is.na(UNRES) ~ 0,
    UNRES == 0 ~ 2,
    UNRES == 1 ~ 1
  ) %>% factor(levels = 0:2)
)

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

library(countrycode)

find_distance <- function(x){
  # x = gsub("^([0-9]+),.*", "\\1", x)
  dest = countrycode(x, origin = "cown", destination = "iso3c")
  return((dist_cepii %>% filter(iso_o == "USA", iso_d == dest))[1, "distcap"] %>% as.numeric())
}

df$year = df$styear

df <- left_join(df, yearly_unsc_allies, by = "year")
df <- left_join(df, polity, by = c("year", "ccode"))

df$Location <- ifelse(is.na(df$Location), df$ccode, as.numeric(gsub("^([0-9]+).*", "\\1", df$Location)))

# df %<>% filter(!is.na(UNRES))
# df$fatality <- as.numeric(df$Fatalities)
df$distance <- future_sapply(df$Location, find_distance)
df$log_distance <- log(df$distance)
df$target_polity <- df$polity2
df$trade_flow <- ifelse(df$`Trade flow1` <=0, NA, df$`Trade flow1`)
df$threat <- -scale(df$target_polity) -scale(df$distance)
df$iraq = df$`State B` == "IRQ"
df$cold_war = as.numeric(df$styear %in% 1946:1989)
df$MENA = as.numeric(df$Location %in% 600:699)
df$target_rGDPpc <- df$StateBrGDP / df$`State B tpop`
df$humanitarian <- grepl("Humanitarian", df$Objective) %>% as.numeric()
df$own_interest <- grepl("own", df$Objective) %>% as.numeric()
# df %>% filter(`Oil Dummy` == 1) %>% select(`State B`) %>% as.vector()

library(mice)

df %<>% mutate(
  threat = as.numeric(threat)
) %>% rename(
  own_interest = `own interest`,
  oil_dummy = `Oil Dummy`
)

mice_df <- mice(df %>% dplyr::select(UNRES, humanitarian, own_interest, ally_count, trade_flow, oil_dummy, styear, target_polity, distance))

reg_df <- complete(mice_df)

reg_df$threat <- - scale(reg_df$target_polity) - scale(reg_df$distance)
reg_df$plurality <- reg_df$ally_count >= 9
reg_df$intervene <- df$intervene
### Basic linear models

r1 <- lm(UNRES ~ threat + log(trade_flow) + oil_dummy + factor(styear), data = reg_df %>% filter(intervene != 0))
# r12<- lm(UNRES ~ threat + target_rGDPpc + factor(styear), data = reg_df)
# r15 <- lm(UNRES ~ target_polity + factor(styear), data = reg_df)
# r2 <- lm(UNRES ~ target_polity + log(distance)  + factor(styear), data = reg_df)
r3 <- lm(UNRES ~ scale(target_polity) + scale(distance)  + log(trade_flow) + oil_dummy + factor(styear), data = reg_df %>% filter(intervene != 0))


r4 <- lm(UNRES ~ humanitarian + log(trade_flow) + oil_dummy + factor(styear), data = reg_df%>% filter(intervene != 0))
r5 <- lm(UNRES ~ own_interest  + log(trade_flow) + oil_dummy + factor(styear), data = reg_df%>% filter(intervene != 0))

stargazer(r1, r3, r4, r5, type = "text", omit = "year",
          add.lines = list(c("Time FEs", rep("Y",5))))


r6 <- lm(UNRES ~ ally_count + log(trade_flow) + oil_dummy, data = reg_df %>% filter(styear <= 2012, intervene != 0))
r7 <- lm(UNRES ~ plurality + log(trade_flow) + oil_dummy, data = reg_df %>% filter(styear <= 2012, intervene != 0))
r8 <- lm(UNRES ~ threat + humanitarian + own_interest + ally_count + log(trade_flow) + oil_dummy , data = reg_df %>% filter(styear <= 2012, intervene != 0))

stargazer(r1, r3, r4, r5, r6, r7, r8, type = "html", omit = "year",
          add.lines = list(c("Time FEs", rep("Y",4), rep("N", 3))), out = "o.html")



### Logits

r1 <- glm(UNRES ~ threat + log(trade_flow) + oil_dummy + factor(styear),
          family = "binomial", data = reg_df %>% filter(intervene != 0))
# r12<- lm(UNRES ~ threat + target_rGDPpc + factor(styear), data = df)
# r15 <- lm(UNRES ~ target_polity + factor(styear), data = df)
# r2 <- lm(UNRES ~ target_polity + log(distance)  + factor(styear), data = df)
r3 <- glm(UNRES ~ scale(target_polity) + scale(distance) + log(trade_flow) + oil_dummy + factor(styear),
          family = "binomial", data = reg_df %>% filter(intervene != 0))

r4 <- glm(UNRES ~ humanitarian + log(trade_flow) + oil_dummy + factor(styear),  family = "binomial", data = reg_df %>% filter(intervene != 0))
r5 <- glm(UNRES ~ own_interest + log(trade_flow) + oil_dummy + factor(styear),  family = "binomial", data = reg_df %>% filter(intervene != 0))

stargazer(r1, r3, r4, r5, type = "text", omit = "year",
          add.lines = list(c("Time FEs", rep("Y",5))))


r6 <- glm(UNRES ~ ally_count+ log(trade_flow) + oil_dummy,  family = "binomial", data = reg_df %>% filter(styear <= 2012, intervene != 0))
r7 <- glm(UNRES ~ plurality+ log(trade_flow) + oil_dummy,  family = "binomial", data = reg_df %>% filter(styear <= 2012, intervene != 0))
r8 <- glm(UNRES ~ threat + humanitarian + own_interest + ally_count + log(trade_flow) + oil_dummy, family = "binomial",  data = reg_df %>% filter(styear <= 2012, intervene != 0))

stargazer(r1, r3, r4, r5, r6, r7, r8, type = "html", omit = "year",
          add.lines = list(c("Time FEs", rep("Y",4), rep("N", 3))), out = "o_logit.html")

### Multilevel

library(lme4)

r1 <- glmer(UNRES ~ threat + log(trade_flow) + oil_dummy  + (1 | UNRegT),
          family = "binomial", data = reg_df %>% filter(intervene != 0))
# r12<- lm(UNRES ~ threat + target_rGDPpc + factor(styear), data = df)
# r15 <- lm(UNRES ~ target_polity + factor(styear), data = df)
# r2 <- lm(UNRES ~ target_polity + log(distance)  + factor(styear), data = df)
r3 <- glmer(UNRES ~ scale(target_polity) + scale(distance) + log(trade_flow) + oil_dummy + (1 | UNRegT),
          family = "binomial", data = reg_df %>% filter(intervene != 0))

r4 <- glmer(UNRES ~ humanitarian + log(trade_flow) + oil_dummy  + (1 | UNRegT) ,  family = "binomial", data = reg_df %>% filter(intervene != 0))
r5 <- glmer(UNRES ~ own_interest + log(trade_flow) + oil_dummy  + (1 | UNRegT),  family = "binomial", data = reg_df %>% filter(intervene != 0))

stargazer(r1, r3, r4, r5, type = "text", omit = "year",
          add.lines = list(c("Time FEs", rep("Y",5))))


r6 <- glmer(UNRES ~ ally_count + log(trade_flow) + oil_dummy + (1 | UNRegT),  family = "binomial", data =  reg_df %>% filter(styear <= 2012, intervene != 0))
r7 <- glmer(UNRES ~ plurality + log(trade_flow) + oil_dummy + (1 | UNRegT),  family = "binomial", data =  reg_df %>% filter(styear <= 2012, intervene != 0))
r8 <- glmer(UNRES ~ threat + humanitarian + own_interest + ally_count + log(trade_flow) + oil_dummy  + (1 | UNRegT), family = "binomial",  data =  reg_df %>% filter(styear <= 2012, intervene != 0))


stargazer(r1, r3, r4, r5, r6, r7, r8,
          add.lines = list(c("Region REs", rep("Y",7))),
          type = "html", omit = "year", out = "o_logit_mixed.html")


### ologit

library(MASS)

r1 <- polr(intervene ~ threat, # + factor(styear),
           Hess = T, data = df)
# r12<- lm(UNRES ~ threat + target_rGDPpc + factor(styear), data = df)
# r15 <- lm(UNRES ~ target_polity + factor(styear), data = df)
# r2 <- lm(UNRES ~ target_polity + log(distance)  + factor(styear), data = df)
r3 <- polr(intervene ~ target_polity + scale(distance),
           Hess = T, data = df)

# r4 <- ologit(UNRES ~ humanitarian + factor(styear),  family = "binomial", data = df)
# r5 <- ologit(UNRES ~ own_interest + factor(styear),  family = "binomial", data = df)
#
# stargazer(r1, r3, r4, r5, type = "text", omit = "year",
#           add.lines = list(c("Time FEs", rep("Y",5))))
#
#
# r6 <- ologit(UNRES ~ ally_count,  family = "binomial", data = df %>% filter(year <= 2012))
# r7 <- ologit(UNRES ~ plurality,  family = "binomial", data = df %>% filter(year <= 2012))
# r8 <- ologit(UNRES ~ threat + humanitarian + own_interest + ally_count , family = "binomial",  data = df %>% filter(year <= 2012), Hess = T)

stargazer(r1, r3, type = "html", omit = "year", out = "o_ologit.html")



