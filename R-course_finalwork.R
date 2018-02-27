########################
# R 2016 - Harjoitustyö

########################
# Tehtävä 1

# a)

summamuuttuja <- function(muuttujat, aineisto, max_puuttuvat=1) {
  aineisto <- data.frame(aineisto)
  muuttujat <- paste(muuttujat)
  puuttuvat <- apply(subset(aineisto, select=muuttujat), 1, function(row) sum(is.na(row)))
  tulos <- rowMeans(subset(aineisto, select=muuttujat), na.rm=T)
  tulos[puuttuvat > max_puuttuvat] <- NA
  return(tulos)
}
  
# b)

hsl <- read.csv2('asiakastyytyvaisyys.csv', stringsAsFactors = FALSE)
hsl$tyyt_kulj <- summamuuttuja(muuttujat=c('K1A1','K1A2','K1A3'), aineisto=hsl, max_puuttuvat=1)

mean(hsl$tyyt_kulj, na.rm=T)
sd(hsl$tyyt_kulj, na.rm=T)

hist(hsl$tyyt_kulj, col='beige')
table(hsl$tyyt_kulj)

hist(hsl$tyyt_kulj, breaks=14, col='beige')

# c)

hsl$tyytyvaisyys <- summamuuttuja(muuttujat=c('K1A4','K1A5','K1A6','K2A2','K2A3','K2A4','K2A5','K2A6'), aineisto=hsl, max_puuttuvat=5)
quantile(hsl$tyytyvaisyys, na.rm=T)
median(hsl$tyytyvaisyys, na.rm=T)
boxplot(hsl$tyytyvaisyys, col='pink', ylab='tyytyvaisyys')

# d)

hsl$T17 <- factor(hsl$T17, labels=c('kylla','ei'))
mean(hsl$tyytyvaisyys[hsl$T17=='kylla'], na.rm=T)
mean(hsl$tyytyvaisyys[hsl$T17=='ei'], na.rm=T)
boxplot(hsl$tyytyvaisyys~hsl$T17, col=c('lightblue','lightgreen'), ylab='tyytyvaisyys')

########################
# Tehtävä 2

# a)

hsl$ALUE <- factor(hsl$ALUE)
levels(hsl$ALUE) <- list('sisainen'=c(49,91,92,245,257,753),'seutu'=c(2010))

# b)

table(hsl$K1A3, hsl$ALUE)
round(prop.table(table(hsl$K1A3, hsl$ALUE),1)*100,2)
round(prop.table(table(hsl$K1A3, hsl$ALUE),2)*100,2)

# c)

t.test(hsl$K1A3[hsl$ALUE == 'sisainen'], hsl$K1A3[hsl$ALUE == 'seutu'])

########################
# Tehtävä 3

# a)

hsl$kuukausi <- format(as.Date(hsl$PAIVAMAARA,format="%Y-%m-%d"),"%m")
hsl$kuukausi <- as.numeric(hsl$kuukausi)

# b)

KeskiarvoJaVali <- function(x, p) {
  s <- sd(x, na.rm=T)
  n <- sum(!is.na(x))
  t <- qt(p = p, lower.tail = T, df = n-1)
  y <- mean(x, na.rm = T)
  return(c(y, y - s/sqrt(n)*t, y + s/sqrt(n)*t))
}

# c)

osa_aineisto <- hsl[hsl$LINJA == 8,]
x <- sapply(1:12, function(kuu_nro) { osa_aineisto$tyytyvaisyys[osa_aineisto$kuukausi==kuu_nro] })
arvot <- sapply(1:12, function(kuu_nro) KeskiarvoJaVali(x=x[[kuu_nro]],p=0.975))
arvot

# d)

plot(osa_aineisto$kuukausi, osa_aineisto$tyytyvaisyys, xlab="kuukausi", ylab="tyytyvaisyys")
axis(side=1,at=1:11)
segments(x0=1:11, y0=arvot[1,], y1=arvot[2,], col='red', lwd=2)

########################
# Tehtävä 4

# a)

plot(hsl$tyyt_kulj, hsl$tyytyvaisyys)

# b)

plot(sapply(hsl$tyyt_kulj, function(x) { x+rnorm(1,0,0.2) }), sapply(hsl$tyytyvaisyys, function(x) { x+rnorm(1,0,0.2) }), xlab='',ylab='', pch='.')

# c)

fit <- lm(hsl$tyytyvaisyys~hsl$tyyt_kulj)
summary(fit)

# d)

curve(2.8280+0.3439*x, from=0, to=5, add=T, col="blue")

########################
# Tehtävä 5

# a)

pallot_pekka <- function(n) {
  theta <- sample(6,n,replace=T)
  valkoinen_nostettu <- rbinom(n,3,prob=theta/6)
  return(table(theta[valkoinen_nostettu==2])/sum(valkoinen_nostettu==2)*100)
}

# b)

pallot_maija <- function(n) {
  theta <- sample(rbinom(n,6,prob=1/2),n,replace=T)
  valkoinen_nostettu <- rbinom(n,3,prob=theta/6)
  return(table(theta[valkoinen_nostettu==2])/sum(valkoinen_nostettu==2)*100)
}

# c)

pallot_pekka(100000)
pallot_maija(100000)




