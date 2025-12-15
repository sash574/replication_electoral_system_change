
library(readstata13)

source("functionDefs.R")

load("scm_est.Rdata")

dat <- read.dta13("CountryEPData.dta", convert.factors=F)
dat <- subset(dat, subset=dat$ccode %nin% c(5,9,12))
dat <- subset(dat, subset=dat$ep_election!=2019)

x <- seq(1979, 2014, 5)
y1 <- res[[1]]$data.treat$rr_votes
yeu8 <- aggregate(dat$rr_votes, list(dat$ep_election), mean, na.rm=T)$x

wm <- read.csv("westminster.csv")[, c("year", "westminster_vote")]
wm <- wm[wm$year <= 2015, ]


gaps <- res[[1]]$gaps$rr_votes


mtext_adj <- 0.9


pdf(file='IntroPlot.pdf', width = 5, height = 3, pointsize = 10)

par(mar=c(2, 2.9, 1, 1))
plot(x=x, y=y1, axes=F, xlab="", ylab="", type="n",
   ylim=c(-1, 30), yaxs="i", 
   xlim=c(x[1]-1, 2016), xaxs="i")

# abline(v=x, col="gray90", lty=1, lwd=1.1)

for (i in seq(0,30,5)){
   segments(x0=1978, y0=i, x1=2016, y1=i, 
      col="gray96", lty=1, lwd=1.1)
}
for (i in seq(0,30,10)){
   segments(x0=1978, y0=i, x1=2016, y1=i, 
      col="gray92", lty=1, lwd=1.1)
}
for(i in x){
   segments(x0=i, y0=-1, x1=i, y1=30,
      col="gray92", lty=1, lwd=1.1)
}



par(mgp=c(0, 0.25, 0))
axis(1, at=x, labels=x, tcl=-0.25, cex.axis=mtext_adj)
# Westminster elections axis
# axis(3, at = wm$year, cex.axis = 0.7, lwd = 0.8, tcl = -0.2, 
#    col.axis = "#00AFBB", col ="#00AFBB")

par(mgp=c(0, 0.35, 0))
axis(2, las=2, tcl=-0.25, cex.axis=mtext_adj)
mtext("Right-wing populist vote [%]", 2, line=1.6, cex=mtext_adj)


segments(1999, -1, 1999, 30, col="gray60", lwd=1.3)
text(1989, 23, "Introduction of PR in\nUK European elections", col="gray30", cex=0.9)
arrows(x0=1993.5, y0=21.4, x1=1998.4, y1=18, col="gray50", lwd=1.1, length=0.1)

box(lwd = 0.9, col = "gray20")

lines(x=x, y=yeu8, lwd=1.5, col="#00aebbd7", lty = 2)
points(x=x, y=yeu8, cex=1.2, pch=21, col="#00AFBB", bg="#00aebb98")

lines(wm$year, wm$westminster_vote, lwd=1.5, col="#00aebbe6", lty = 4)
points(wm$year, wm$westminster_vote, cex=1, pch=19, col="#00aebbfd")


lines(x=x, y=y1, lwd=1.4, col="#CC0066")
points(x=x, y=y1, cex=1.3, pch=15, col="#CC0066")

text(2003.5, 19.7, "United Kingdom", pos=4, col="#CC0066", cex=0.92)

text(2003, 1.5, "Westminster elections", pos=4, col="#00AFBB", cex=0.92)
text(1980, 4.4, "EU8 average", pos=4, col="#00AFBBCC", cex=0.88)




dev.off()





