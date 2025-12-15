# 
# SCM sensitivity checks
# B: country jacknife
# 


set.seed(675321)
library(MSCMT)
library(readstata13)
library(parallel)
library(gtools)

sessionInfo()

cl <- makeCluster(8)

source("functionDefs.R")



#####################################################################
# 
# B: Sequential exclusion of donor countries
# 
#####################################################################

seeds <- sample(83726:997834, 10, replace=F)

dat <- read.dta13("./CountryEPData.dta", convert.factors=F)
dat <- subset(dat, subset=dat$ccode %nin% c(5,6,9,12))
dat <- subset(dat, subset=dat$ep_election!=2019)
datIP <- read.dta13("./CountryEPData_covIP.dta")
dat <- merge(dat, datIP, by=c("ccode", "year", "ep_election"))
dat <- as.data.frame(dat)
dat.full <- subset(dat, select=c("rr_votes", "E_eubadIP", "E_NatDem_satisfiedIP", "unempIP", "opencIP", "S_uegenIP",  "rightgovIP", "ch_imp_fns", "eu_pos_meanIP", "ccode", "country", "ep_election"))

countries <- sort(unique(dat.full$country[dat.full$country!="United Kingdom"]))
K <- length(countries)

gaps2 <- matrix(NA, ncol=8, nrow=0)
loss <- list()

for (j in countries){
    cat("----------------", j, "----------------")
    dat <- dat.full[dat.full$country!=j,]
    dat <- listFromLong(dat, unit.variable="ccode",
        unit.names.variable="country",
        time.variable="ep_election")
    treatment_id <- "United Kingdom"
    controls_id  <- setdiff(colnames(dat[[1]]), treatment_id)
    times_dep  <- cbind("rr_votes" = c(1979,1994))
    times_pred <- cbind(
                    "E_eubadIP" = c(1979,1994),
                    "E_NatDem_satisfiedIP" = c(1979,1994),
                    "unempIP" = c(1979,1994),
                    "opencIP" = c(1979,1994),
                    "ch_imp_fns" = c(1979,1994),
                    "S_uegenIP" = c(1979,1994),
                    "rightgovIP" = c(1979,1994),
                    "rr_votes" = c(1994)
    )
    res <- mscmt(dat, treatment_id, controls_id, times_dep, times_pred,
        inner.optim = "wnnlsOpt",
        outer.optim=  "DEoptC",
        outer.par = list(lb=1e-9, opt.separate = TRUE),
        seed = seeds, check.global = TRUE, 
        placebo = FALSE, return.ts = FALSE)
    y1 <- res$data.treat$rr_votes
    y0 <- res$data.synth$rr_votes
    gaps2 <- rbind(gaps2, y1-y0)
    loss[[j]] <- sqrt(res$loss.v)
}



# Plot

pdf(file='SCM_sensitivity_B.pdf', width = 6.2, height = 3.8, pointsize=10)
par(mar=c(0.3, 0.3, 0.3, 0.3), mfrow = c(2,4), oma=c(1.5,2.7,0,0.3))

x <- seq(1979, 2014, 5)

for (k in 1:K){
    plot(NULL, axes=F, xlab="", ylab="",
    ylim=c(-0.8, 23), yaxs="i", 
    xlim=c(x[1]-2, x[length(x)]+2), xaxs="i")
    abline(h=0,  col="gray92", lty=1, lwd=1.2)
    abline(h=5,  col="gray92", lty=1, lwd=1.2)
    abline(h=10, col="gray92", lty=1, lwd=1.2)
    abline(h=15, col="gray92", lty=1, lwd=1.2)
    abline(h=20, col="gray92", lty=1, lwd=1.2)

    text(1986.5, 18.7, "pre reform", cex = 0.8, col = "#969595c4")
    rect(1979, -2, 1996, 18, col = "#9695953f", border = NA)

    lines(x, gaps2[k,], col="#cc0066d2", lwd = 1.5)
    points(x, gaps2[k,], col="#cc0066d2", cex = 1.4, pch = 19)
    
    if (k %in% c(5,7)){
        par(mgp=c(0, 0.3, 0))
        axis(1, at=x, labels=x, tcl=-0.25, cex=0.9, col = "gray20")
    }
    if (k %in% c(1,5)){
        par(mgp=c(0, 0.35, 0))
        axis(2, las=2, tcl=-0.25, cex=0.8, col = "gray20")

    }
    box(lwd = 0.9, col = "gray20")
    mtext(paste0("excl. ", countries[k]), 3, line = -1.4, cex = 0.76, font = 3)

    e <- round(loss[[k]],3)
    txt <- bquote(e[y](W) == .(e))
    legend(1977, 4.8, legend=txt, bty = "n", cex = 0.87, xjust = 0)

}

mtext("RWP vote gap [%]", 2, line=1.5, cex=0.85, outer = T)


dev.off()



