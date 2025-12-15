
library(readstata13)

make_plot <- function(){
    mtext_adj <- 0.9
    plot(x=x, y=y1, axes=F, xlab="", ylab="", type="n",
    ylim=c(-1, 30.2), yaxs="i", xlim=c(x[1]-1, x[length(x)]+1), xaxs="i")
    for (i in seq(0,30,5)) segments(x0=1978, y0=i, x1=2014, y1=i, col="gray96", lty=1, lwd=1.1)
    for (i in seq(0,30,10)) segments(x0=1978, y0=i, x1=2014, y1=i, col="gray92", lty=1, lwd=1.1)
    for(i in x) segments(x0=i, y0=-1, x1=i, y1=30, col="gray92", lty=1, lwd=1.1)
    par(mgp=c(0, 0.25, 0))
    axis(1, at=x, labels=x, tcl=-0.25, cex.axis=mtext_adj)
    par(mgp=c(0, 0.35, 0))
    axis(2, las=2, tcl=-0.25, cex.axis=mtext_adj)
    mtext("Right-wing populist vote [%]", 2, line=1.6, cex=mtext_adj)
    lines(x=x, y=yeu8, lwd=1.3, col="#bebebeb7", lty=2)
    text(1984, 3.4, "EU8", col="#bebebeb7", cex = 0.7)
    lines(wm$year, wm$westminster_vote, lwd = 1.3, col="#bebebeb7", lty = 4)
    text(2010.5, 3.3, "Westminster", col="#bebebeb7", cex = 0.7)
    segments(1999, -1, 1999, 30, col="gray50", lwd=1.2)
    lines(x=x, y=y1, lwd=1.5, col="#cc0066e1")
    points(x=x, y=y1, cex=1.1, pch=15, col="#cc0066dc")
    text(2003.5, 19.7, "UK", pos=4, col="#cc0066ef", cex=0.92)
}

load("scm_est.Rdata")

dat2 <- read.dta13("CountryEPData.dta", convert.factors=F)
dat2 <- subset(dat2, dat2$ccode!=11, convert.factors=F)

wm <- read.csv("./elections/westminster/westminster.csv")[, c("year", "westminster_vote")]
wm <- wm[wm$year <= 2015, ]

x <- seq(1979, 2014, 5)
y0 <- res[[1]]$data.synth$rr_votes
y1 <- res[[1]]$data.treat$rr_votes
yeu8 <- aggregate(dat2$rr_votes, list(dat2$ep_election), mean, na.rm=T)$x

load("x_scm_est_FR.Rdata")
y0fr <- resFR$data.synth$rr_votes



pdf(file='x_SCM_FR_plot.pdf', width = 7, height = 3.5, pointsize=10)

par(mar=c(2.2, 2.9, 1, 1))
par(mfrow=c(1,2))

make_plot()
lines(x=x, y=y0, lwd=1.5, col="#00AFBB")
points(x=x, y=y0, cex=1.35, pch=19, col="#00AFBB")
text(2001, 0.6, "Synthetic UK", pos=4, col="#00AFBB", cex=0.95)
mtext("a", side = 3, line = -0.2, at = 1974, font = 2, cex = 1.4)

make_plot()
lines(x=x, y=y0fr, lwd=2, col="#00AFBB", lty = 2)
points(x=x, y=y0fr, cex=1.2, pch=19, col="#00AFBB")
text(2001, 0.6, "Synthetic UK", pos=4, col="#00AFBB", cex=0.95)
mtext("b", side = 3, line = -0.2, at = 1974, font = 2, cex = 1.4)

dev.off()
