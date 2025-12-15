
library(readstata13)


load("scm_est.Rdata")

dat2 <- read.dta13("CountryEPData.dta", convert.factors=F)
dat2 <- subset(dat2, dat2$ccode!=11, convert.factors=F)

wm <- read.csv("westminster.csv")[, c("year", "westminster_vote")]
wm <- wm[wm$year <= 2015, ]


x <- seq(1979, 2014, 5)
y0 <- res[[1]]$data.synth$rr_votes
y1 <- res[[1]]$data.treat$rr_votes
yeu8 <- aggregate(dat2$rr_votes, list(dat2$ep_election), mean, na.rm=T)$x

gaps <- res[[1]]$gaps$rr_votes

# load("x_scm_est_FR.Rdata")
# y0fr <- resFR[[1]]$data.synth$rr_votes


mtext_adj <- 0.9


pdf(file='scm_plots_2panel.pdf', width = 7.7, height = 3.6, pointsize=10)

par(mar=c(2.2, 2.9, 1, 1))
par(mfrow=c(1,2))
plot(x=x, y=y1, axes=F, xlab="", ylab="", type="n",
   ylim=c(-1, 30.2), yaxs="i", 
   xlim=c(x[1]-1, x[length(x)]+0.6), xaxs="i")

for (i in seq(0,30,5)){
   segments(x0=1978, y0=i, x1=2014, y1=i, 
      col="gray96", lty=1, lwd=1.1)
}
for (i in seq(0,30,10)){
   segments(x0=1978, y0=i, x1=2014, y1=i, 
      col="gray92", lty=1, lwd=1.1)
}
for(i in x){
   segments(x0=i, y0=-1, x1=i, y1=30,
      col="gray92", lty=1, lwd=1.1)
}


par(mgp=c(0, 0.25, 0))
axis(1, at=x, labels=x, tcl=-0.25, cex.axis=mtext_adj)
par(mgp=c(0, 0.35, 0))
axis(2, las=2, tcl=-0.25, cex.axis=mtext_adj)
mtext("Right-wing populist vote [%]", 2, line=1.6, cex=mtext_adj)

mtext("a", side = 3, line = -0.2, at = 1974, font = 2, cex = 1.4)


segments(1999, -1, 1999, 30, col="gray60", lwd=1.2)
text(1989, 22.5, "Introduction of PR", col="gray30", cex=0.9)
arrows(x0=1993.5, y0=21.4, x1=1998.4, y1=18, col="gray50", lwd=1.1, length=0.1)

lines(x=x, y=yeu8, lwd=1.3, col="#bebebeb7", lty=2)
text(1984, 3.4, "EU8", col="#bebebeb7", cex = 0.7)

lines(wm$year, wm$westminster_vote, lwd = 1.3, col="#bebebeb7", lty = 4)
text(2010.5, 3.3, "Westminster", col="#bebebeb7", cex = 0.7)

lines(x=x, y=y1, lwd=1.5, col="#CC0066")
points(x=x, y=y1, cex=1.3, pch=15, col="#CC0066")
text(2003.5, 19.7, "United Kingdom", pos=4, col="#CC0066", cex=0.92)

lines(x=x, y=y0, lwd=1.5, col="#00AFBB")
points(x=x, y=y0, cex=1.35, pch=19, col="#00AFBB")
text(2001, 0.6, "Synthetic UK", pos=4, col="#00AFBB", cex=0.95)




y1p <- res$placebo$rr_votes$data.treat
y0p <- res$placebo$rr_votes$data.synth
gapsP <- y1p-y0p


plot(x=x, y=gaps, axes=F, xlab="", ylab="", type="n",
   ylim=c(-15, 25.2), yaxs="i", 
   xlim=c(x[1]-1, x[length(x)]+0.5), xaxs="i")

for (i in seq(-15,30,5)){
   segments(x0=1978, y0=i, x1=2014, y1=i, 
      col="gray96", lty=1, lwd=1.1)
}
for (i in seq(-15,30,10)){
   segments(x0=1978, y0=i, x1=2014, y1=i, 
      col="gray92", lty=1, lwd=1.1)
}
for(i in x){
   segments(x0=i, y0=-15, x1=i, y1=30,
      col="gray92", lty=1, lwd=1.1)
}


par(mgp=c(0, 0.25, 0))
axis(1, at=x, labels=x, tcl=-0.25, cex.axis=mtext_adj)
par(mgp=c(0, 0.35, 0))
axis(2, las=2, tcl=-0.25, cex.axis=mtext_adj, at=seq(-15, 25, 5))
mtext("Estimated gap right-wing populist vote [%pts]", 2, line=1.6, cex=mtext_adj)

segments(1999, -15, 1999, 25, col="gray50", lwd = 1.2)
segments(1979, 0, 2014, 0, col="gray50", lwd = 1.2)


for(i in 2:8){
   lines(x=x, y=gapsP[,i], lwd=1.3, col="gray75", lty=1)
   points(x=x, y=gapsP[,i], cex=0.5, pch=19, col="gray75")
}
lines(x=x, y=gaps, lwd=1.6, col="#CC0066", lty=1)
points(x=x, y=gaps, cex=1.2, col="#CC0066", pch=15)

mtext("b", side = 3, line = -0.2, at = 1974, font = 2, cex = 1.4)

text(2000, 21, "United Kingdom", pos=4, col="#CC0066", cex=0.92)
text(1999, -12, "Placebos", pos=4, col="gray60", cex=0.92)


dev.off()
