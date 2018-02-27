data <- read.csv("JEL_ web.csv",header=T,sep=",", stringsAsFactors = F)
ita_eurooppa <- data[c(44,45,56,78,104,106,115,137,155,156),]
ita_eurooppa <- cbind(ita_eurooppa,eu_2004=c(1,1,1,1,1,1,1,1,1,1))
ita_eurooppa <- cbind(ita_eurooppa,non_eu=c(0,0,0,0,0,0,0,0,0,0))
ita_eurooppa <- cbind(ita_eurooppa,eu_before=c(0,0,0,0,0,0,0,0,0,0))
naapurit <- data[c(5,19,22,23,76,143,144,169,173,185),]
naapurit <- cbind(naapurit,eu_2004=c(0,0,0,0,0,0,0,0,0,0))
naapurit <- cbind(naapurit,non_eu=c(1,1,1,1,1,1,1,1,1,1))
naapurit <- cbind(naapurit,eu_before=c(0,0,0,0,0,0,0,0,0,0))
eu <- data[c(12,15,46,49,55,58,60,63,70,81,86,105,127,140,157),]
eu <- cbind(eu,eu_2004=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
eu <- cbind(eu,non_eu=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
eu <- cbind(eu,eu_before=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
kaikki <- rbind(ita_eurooppa,naapurit,eu)
ita_eurooppa[ita_eurooppa == "."] <- NA
naapurit[naapurit == "."] <- NA
kaikki[kaikki == "."] <- NA

anti_sd. <- as.numeric(kaikki$anti_sd)
mcap. <- as.numeric(kaikki$mcap)
firms. <- as.numeric(kaikki$firms)
privo. <- as.numeric(kaikki$privo)
cr. <- as.numeric(kaikki$cr)
gdp2004 <- log(as.numeric(kaikki$gdp_pop_ppp2004))

#regression

linear.1 <- lm(anti_sd. ~ eu_2004+non_eu,data=kaikki)
linear.2 <- lm(mcap. ~ eu_2004+non_eu,data=kaikki)
linear.3 <- lm(firms. ~ eu_2004+non_eu,data=kaikki)
linear.4 <- lm(privo. ~ eu_2004+non_eu,data=kaikki)
linear.5 <- lm(cr. ~ eu_2004+non_eu,data=kaikki)

stargazer(linear.1, linear.2,linear.3,linear.4, linear.5, title="Financial institutions and EU membership", align=TRUE,
          covariate.labels=c("EU members since 2004","Non-EU members"))

#t-test

eu2004_anti_sd <- as.numeric(ita_eurooppa$anti_sd)
eu2004_mcap <- as.numeric(ita_eurooppa$mcap)
eu2004_firms <- as.numeric(ita_eurooppa$firms)
eu2004_privo <- as.numeric(ita_eurooppa$privo)
eu2004_cr <- as.numeric(ita_eurooppa$cr)
eu2004_gdp2004 <- log(as.numeric(ita_eurooppa$gdp_pop_ppp2004))

neighbors_anti_sd <- as.numeric(naapurit$anti_sd)
neighbors_mcap <- as.numeric(naapurit$mcap)
neighbors_firms <- as.numeric(naapurit$firms)
neighbors_privo <- as.numeric(naapurit$privo)
neighbors_cr <- as.numeric(naapurit$cr)
neighbors_gdp2004 <- log(as.numeric(naapurit$gdp_pop_ppp2004))

t.test(eu2004_anti_sd,neighbors_anti_sd)
