setwd("~/Dropbox/Taloustieteen maisteri/Financial Development/Essay")
poverty <- read.csv("poverty.csv",header=T,stringsAsFactors = F)
private.credit <- read.csv("private.credit.csv",header=T,stringsAsFactors = F)
control <- read.csv("control.csv",header=T,stringsAsFactors = F)

private.credit <- private.credit[private.credit$year>1984,]
control[control==".."] <- NA
poverty <- poverty[order(poverty$country,poverty$year),]

control2 <- control[!is.na(control$gdp.per.capita),]
control2 <- transform(control2, lg2=ave(year, country, 
                                     FUN=function(x) c(x-first(x))))
control2 <- transform(control2, growth.gdp=ave(as.numeric(gdp.per.capita), country, 
                                                  FUN=function(x) c(log(x)-log(first(x)))))

library(plyr)
library(dplyr)

# Growth of headcount index

poverty1 <- poverty
poverty1$headcount[poverty1$headcount==0.00] <- NA
poverty1 <- poverty1[!is.na(poverty1$headcount),]
poverty1 <- transform(poverty1, lag1=ave(year, country, 
                           FUN=function(x) c(x-first(x))))
poverty1 <- transform(poverty1, growth.headcount=ave(headcount, country, 
                         FUN=function(x) c((log(x)-log(first(x))))))

# Growth of poverty gap

poverty2 <- poverty
poverty2$pov.gap[poverty2$pov.gap==0.00] <- NA
poverty2 <- poverty2[!is.na(poverty2$pov.gap),]
poverty2 <- transform(poverty2, lag2=ave(year, country, 
                                         FUN=function(x) c(x-first(x))))
poverty2 <- transform(poverty2, growth.pov.gap=ave(pov.gap, country, 
                                                   FUN=function(x) c(log(x)-log(first(x)))))

# Growth of squared poverty gap

poverty3 <- poverty
poverty3$squared[poverty3$squared==0.00] <- NA
poverty3 <- poverty3[!is.na(poverty3$squared),]
poverty3 <- transform(poverty3, lag3=ave(year, country, 
                                         FUN=function(x) c(x-first(x))))
poverty3 <- transform(poverty3, growth.squared=ave(squared, country, 
                                                 FUN=function(x) c(log(x)-log(first(x)))))


# Combining data frames

countries <- factor(poverty$country)
levels(countries)

combined <- merge(poverty, control2, all.y = TRUE)
mydata <- merge(combined, private.credit, all.y = TRUE)
mydata <- mydata[mydata$country=="Armenia"    |    mydata$country==    "Azerbaijan"   | mydata$country==     "Bangladesh"     |  mydata$country==  "Belarus"           
                 |mydata$country== "Bolivia"      |    mydata$country==  "Brazil"       |    mydata$country==  "Bulgaria"   |   mydata$country==     "Cambodia"          
                 |mydata$country=="Chile"        |  mydata$country==    "Colombia"     |  mydata$country==    "Costa Rica"     |  mydata$country==  "Cote d'Ivoire"     
                 |mydata$country== "Croatia"      |  mydata$country==    "Dominican Republic" | mydata$country=="Ecuador"       |  mydata$country==   "El Salvador"       
                 |mydata$country=="Georgia"      |   mydata$country==   "Guatemala"    |   mydata$country==   "Honduras"       | mydata$country==   "Hungary"           
                 |mydata$country== "Iran"         |  mydata$country==    "Jamaica"      |   mydata$country==   "Kazakhstan"     | mydata$country==   "Kosovo"            
                 |mydata$country== "Kyrgyz Republic"  | mydata$country== "Macedonia"    |  mydata$country==    "Madagascar"     | mydata$country==   "Malaysia"          
                 |mydata$country== "Mauritania"     |  mydata$country==  "Mexico"       |  mydata$country==    "Moldova"        |  mydata$country==  "Mongolia"          
                 |mydata$country== "Montenegro"     |  mydata$country==  "Nicaragua"    |  mydata$country==    "Niger"          | mydata$country==   "Pakistan"          
                 |mydata$country== "Panama"         |  mydata$country==  "Paraguay"     |  mydata$country==    "Peru"           |  mydata$country==  "Philippines"       
                 |mydata$country== "Poland"         | mydata$country==   "Romania"      |  mydata$country==    "Russia"         |  mydata$country==  "Serbia"            
                 |mydata$country== "South Africa"   |  mydata$country==  "Sri Lanka"    |  mydata$country==    "Tajikistan"     |  mydata$country==  "Thailand"          
                 |mydata$country== "Tunisia"        |  mydata$country==  "Turkey"       |  mydata$country==    "Uganda"         |  mydata$country==  "Ukraine"           
                 |mydata$country== "Uruguay"        |  mydata$country==  "Venezuela"    |   mydata$country==   "Vietnam"        | mydata$country==   "Zambia" , ]
  
mydata$sum.exports.imports <- as.numeric(mydata$exports) + as.numeric(mydata$imports)
mydata[mydata=="NaN" | mydata=="Inf" | mydata=="-Inf"] <- NA

# Cross-country

gdp.growth <- as.vector(by(mydata, as.factor(mydata$country), FUN=function(mydata) c(tail(mydata$growth.gdp/mydata$lg2,n=1))))

poverty[poverty=="NaN" | poverty=="Inf" | poverty=="-Inf"] <- NA

headcount.growth <- as.vector(by(poverty1, as.factor(poverty1$country), FUN=function(poverty1) c(tail(poverty1$growth.headcount/poverty1$lag1,n=1))))
initial.headcount <- as.vector(by(poverty1, as.factor(poverty1$country), FUN=function(poverty1) c(log(first(poverty1$headcount)))))

pov.gap.growth <- as.vector(by(poverty2, as.factor(poverty2$country), FUN=function(poverty2) c(tail(poverty2$growth.pov.gap/poverty2$lag2,n=1))))
initial.pov.gap <- as.vector(by(poverty2, as.factor(poverty2$country), FUN=function(poverty2) c(log(first(poverty2$pov.gap)))))

squared.growth <- as.vector(by(poverty3, as.factor(poverty3$country), FUN=function(poverty3) c(tail(poverty3$growth.squared/poverty3$lag3,n=1))))
initial.squared <- as.vector(by(poverty3, as.factor(poverty3$country), FUN=function(poverty3) c(log(first(poverty3$squared)))))

priv.cred <- ddply(mydata, .(country), summarize,  priv.credit=mean(pcrdbofgdp, na.rm=T)/100)

# Control variables

mydata2 <- mydata[!is.na(mydata$gdp.deflator),]
mydata2 <- transform(mydata2, growth.gdp.deflator=ave(as.numeric(gdp.deflator), country, 
                                                    FUN=function(x) c(log(x)-log(first(x)))))

growth.gdp.defl <- as.vector(by(mydata2, as.factor(mydata2$country), FUN=function(mydata2) c(tail(mydata2$growth.gdp.deflator/mydata2$lg2,n=1))))
growth.gdp.defl[growth.gdp.defl=="Inf" | growth.gdp.defl=="-Inf"] <- 0

exports.imports <- ddply(mydata, .(country), summarize,  exports.imports=mean(sum.exports.imports, na.rm=T)/100)

gov.expendit <- ddply(mydata, .(country), summarize,  gov.expendit=mean(as.numeric(gov.exp), na.rm=T)/100)


fit1 <- lm(headcount.growth~gdp.growth+priv.cred$priv.credit+growth.gdp.defl+
             log(exports.imports$exports.imports)+log(gov.expendit$gov.expendit)+initial.headcount)
fit1.1 <- lm(headcount.growth~gdp.growth+priv.cred$priv.credit+initial.headcount)

summary(fit1)

fit2 <- lm(pov.gap.growth~gdp.growth+priv.cred$priv.credit+growth.gdp.defl+
             log(exports.imports$exports.imports)+log(gov.expendit$gov.expendit)+initial.pov.gap)
fit2.1 <- lm(pov.gap.growth~gdp.growth+priv.cred$priv.credit+initial.pov.gap)

summary(fit2)

fit3 <- lm(squared.growth~gdp.growth+priv.cred$priv.credit+growth.gdp.defl+
             log(exports.imports$exports.imports)+log(gov.expendit$gov.expendit)+initial.squared)
fit3.1 <- lm(squared.growth~gdp.growth+priv.cred$priv.credit+initial.squared)

summary(fit3)

mean.gdp <- ddply(mydata, .(country), summarize,  mean.gdp=mean(as.numeric(gdp.per.capita), na.rm=T))

library(stargazer)

stargazer(fit1,fit2,fit3)
stargazer(fit1.1,fit2.1,fit3.1)

