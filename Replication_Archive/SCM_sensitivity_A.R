# 
# SCM sensitivity checks
# A: permutations of X matrix columns
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
# A: Combinatorial covariate sensitivity analysis
# Estimates models w/ all possible combinations of X vector inputs
# 
#####################################################################


dat <- read.dta13("./CountryEPData.dta", convert.factors=F)
dat <- subset(dat, subset=dat$ccode %nin% c(5,6,9,12))
dat <- subset(dat, subset=dat$ep_election!=2019)

datIP <- read.dta13("./CountryEPData_covIP.dta")
dat <- merge(dat, datIP, by=c("ccode", "year", "ep_election"))


dat <- as.data.frame(dat)
dat <- subset(dat, select=c("rr_votes", "E_eubadIP", "E_NatDem_satisfiedIP", "unempIP", "opencIP", "S_uegenIP",  "rightgovIP", "ch_imp_fns", "eu_pos_meanIP", "migr_inflIP", "ccode", "country", "ep_election"))

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
                "eu_pos_meanIP" = c(1979,1994),
                "migr_inflIP" = c(1984,1994),
                "rr_votes" = c(1994)
)
K <- ncol(times_pred)

seeds <- sample(83726:997834, 10, replace=F)

# Main model
res <- mscmt(dat, treatment_id, controls_id, times_dep, times_pred,
    inner.optim = "wnnlsOpt",
    outer.optim=  "DEoptC",
    outer.par = list(lb=1e-9, opt.separate = TRUE),
    seed = seeds, check.global = TRUE, cl = cl,
    placebo = FALSE, return.ts = FALSE)

loss <- sqrt(res$loss.v)
print(loss)


# all covariate combinations
gaps <- matrix(NA, ncol=9, nrow=0)
for (k in 1:K){
    sel <- combinations(10, k, repeats.allowed=F)
    J <- nrow(sel)
    for (j in 1:J){    
        times_pred_sel <- times_pred[, sel[j,], drop=F]
        print(times_pred_sel)

        res <- mscmt(dat, treatment_id, controls_id, times_dep, times_pred_sel,
            inner.optim = "wnnlsOpt",
            outer.optim=  "DEoptC",
            outer.par = list(lb=1e-9, opt.separate = TRUE),
            seed = seeds, check.global = TRUE, cl = cl,
            placebo = FALSE, return.ts = FALSE, verbose = F)

        y1 <- res$data.treat$rr_votes
        y0 <- res$data.synth$rr_votes
        gaps <- rbind(gaps, c(y1-y0, sqrt(res$loss.v)))
    }
}

print(dim(gaps))



# Plot

pdf(file='SCM_sensitivity_A.pdf', width = 5, height = 3.3, pointsize=10)
par(mar=c(1.2, 2.8, 1.2, 1))

x <- seq(1979, 2014, 5)

plot(NULL, axes=F, xlab="", ylab="",
   ylim=c(-5, 25.2), yaxs="i", 
   xlim=c(x[1]-1, x[length(x)]+0.4), xaxs="i")

for (i in seq(-10,30,5)){
   segments(x0=1978, y0=i, x1=2014, y1=i, 
      col="gray95", lty=1, lwd=1.2)
}
for (i in seq(-10,30,10)){
   segments(x0=1978, y0=i, x1=2014, y1=i, 
      col="gray90", lty=1, lwd=1.2)
}
for(i in x){
   segments(x0=i, y0=-5, x1=i, y1=30,
      col="gray90", lty=1, lwd=1.2)
}

text(1986.5, 16.7, "pre reform", cex = 0.8, col = "#969595c4")
rect(1979, -10, 1995.5, 16, col = "#c2c2c259", border = NA)


# Only plot gaps for SCM fits where loss is less than 20x loss of full model
gaps.plot <- gaps[gaps[, 9] < loss*20, 1:8]
print(nrow(gaps) - nrow(gaps.plot))

K <- nrow(gaps.plot)
for (k in 1:K){
    lines(x, gaps.plot[k,], col="#b4b3b31e", lwd = 0.8)
}

lines(x, apply(gaps.plot, 2, quantile, 0.05), col="#cc0066e3", lwd = 1.8, lty = 2)
lines(x, apply(gaps.plot, 2, quantile, 0.95), col="#cc0066e3", lwd = 1.8, lty = 2)
lines(x, apply(gaps.plot, 2, mean), col="#cc0066e3", lwd = 2)
points(x, apply(gaps.plot, 2, mean), col="#cc0066e7", cex = 1.1, pch = 19)

par(mgp=c(0, 0.3, 0))
axis(1, at=x, labels=x, tcl=-0.25, cex=0.9)
par(mgp=c(0, 0.35, 0))
axis(2, las=2, tcl=-0.25, cex=0.9)
mtext("RWP vote gap [%]", 2, line=1.5, cex=0.9)


dev.off()
