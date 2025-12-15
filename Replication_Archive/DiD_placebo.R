
set.seed(54321)

library(readstata13)
library(estimatr)
library(sandwich)

source("functionDefs.R")
require(pbapply)
require(BMisc)

sessionInfo()



dat <- read.dta13("./CountryEPData.dta", convert.factors=F)
dat <- subset(dat, subset=dat$ccode %nin% c(5,9,12))
dat <- subset(dat, subset=dat$ep_election!=2019)
datIP <- read.dta13("./CountryEPData_covIP.dta")
dat <- merge(dat, datIP, by=c("ccode", "year", "ep_election"))
dat <- as.data.frame(dat)
dat <- subset(dat, subset = dat$year %in% unique(dat$ep_election))



###################### Placebo DiD ##################


plac <- data.frame(matrix(nrow=length(unique(dat$country)), ncol=2))
colnames(plac) <- c("country", "est")
j <- 1
for(i in sort(unique(dat$country))){
   print(i)
   dat$D <- ifelse(dat$country==i, 1, 0)
   dat$fD <- ifelse(dat$country==i, 1999, 0)
   plac[j,1] <- i
   out <- mp.spatt(rr_votes ~ D, data=dat, tname="year", 
      panel=T, idname="ccode", first.treat.name="fD", 
      bstrap=F, xformla = ~1 + RAIIP  + E_eubadIP + eu_pos_meanIP + 
   ch_imp_fns + opencIP + S_uegenIP)
   plac[j,"est"] <- out$aggte$simple.att
   j <- j+1
}


pdf("DiD_placebo.pdf", width = 5, height = 3.1, pointsize = 10)
par(mar = c(3,3,1,1))
par(mgp = c(0, 0.4, 0), tcl = -0.35)

plot(density(plac$est[plac$country!="United Kingdom"], bw=3, kernel="gauss"), 
   xlim=c(-20,20), ylim=c(0, 0.08), 
   lwd=0.5, main="", xlab="", ylab="")
grid(col="gray95", lty=1, lwd=1.2)
lines(density(plac$est[plac$country!="United Kingdom"], bw=3, kernel="gauss"),
      lwd=1.9, col="#00AFBB")
segments(x0=plac$est[plac$country=="United Kingdom"], y0=0, 
         x1=plac$est[plac$country=="United Kingdom"], y1=0.058, 
         col="#CC0066", lwd=2.7)
text(plac$est[plac$country=="United Kingdom"], 0.0655, 
     "UK\n estimate", col="#CC0066")
mtext("Density", 2, line=1.8)
mtext("Placebo estimates [%pts]", 1, line=1.7)

box()

print(median(plac$est[-9]))
print(plac$est[9])
print(mean(plac$est[9] <= plac$est[-9]))

dev.off()
