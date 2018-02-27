int <- read.csv("interest_rates.csv",header=T,sep=',',stringsAsFactors = F)

plot(ts(int$call_rates,start=1980,frequency = 12),xlab='Year',ylab='Interest rate',col='red',main="Call rates, uncollaterarized overnight/average",xaxt='n')
axis(1, at = seq(1980, 2016, by = 2))
abline(h=0,col='grey')

set.seed(1)
x = ts(hinnat2$all_commodities, frequency = 12, start = c(1980, 1))
y = ts(hinnat2$services, frequency = 12, start = c(1980, 1))
ts.plot(x, y, gpars = list(col = c("blue", "red")))

hinnat <- read.csv("prices.csv",header=T,sep=',',stringsAsFactors = F)
xz = zoo(ts(hinnat$all_commodities, frequency = 12, start = c(1980, 1)))
yz = zoo(ts(hinnat$services, frequency = 12, start = c(1980, 1)))
plot(xz,xlab='Year',ylab='Price Index Change',col='blue',main="Producer and services price indexes, year-on-year change",sub="blue: commodities, red: services",xaxt='n')
axis(1, at = seq(1980, 2016, by = 2))
lines(yz, col = "red")