
set.seed(54321)

library(readstata13)
library(sandwich)
library(lmtest)
library(car)
library(multcomp)

source("functionDefs.R")

sessionInfo()


dat <- read.dta13("./CountryEPData.dta", convert.factors=F)
dat <- subset(dat, subset=dat$ccode %nin% c(5,9,12))
dat <- subset(dat, subset=dat$ep_election!=2019)
datIP <- read.dta13("./CountryEPData_covIP.dta")
dat <- merge(dat, datIP, by=c("ccode", "year", "ep_election"))
dat <- dat[is.na(dat$rr_votes)==FALSE,]
dat$D <- ifelse(dat$ccode==11, 1, 0)
dat$fD <- ifelse(dat$ccode==11, 1999, 0)
dat$post <- ifelse(dat$ep_election>=1999, 1, 0)
dat$y <- dat$rr_votes


# Table of means and differences

m <- lm(y ~ post*D, data=dat)
names(coef(m))

# lin. H test matrix 
# (incl. obvious coefficients for readability...)
K <- matrix(NA, nrow=9, ncol=4)
colnames(K) <- names(coef(m)) 
rownames(K) <- c("(1) EU, pre", 
                 "(2) EU, post", 
                 "(3) UK, pre", 
                 "(4) UK, post",
                 "(2)-(1)",
                 "(4)-(3)",
                 "(3)-(1)",
                 "(4)-(2)",
                 "Diff in Diff")
K[1,] <- c(1,0,0,0)
K[2,] <- c(1,1,0,0)
K[3,] <- c(1,0,1,0)
K[4,] <- c(1,1,1,1)
K[5,] <- K[2,] - K[1,]
K[6,] <- K[4,] - K[3,]
K[7,] <- K[3,] - K[1,]
K[8,] <- K[4,] - K[2,]
K[9,] <- K[6,] - K[5,]

m_est <- glht(m, linfct = K)
summary(m_est)
m_table <- data.frame(est = coef(m_est), 
                      se = sqrt(diag(vcov(m_est))))



# Parallel trends before reform?

# simple comparison if lin. and quad. pre-trends 
# (with two-way election and country FE)

dat <- dat[dat$post==0, ]

dat$t <- 0
j <- 1
for(i in sort(unique(dat$year))){
      dat$t[dat$year==i] <- j
      j <- j+1
}
dat$tsq <- dat$t^2


ptrend <- data.frame(
        p.rob = rep(NA, 4), 
        p.wild = rep(NA, 4))
rownames(ptrend) <- c("No cov, lin", "No cov, quad", 
"Cov, lin", "Cov, quad")



# NO covariates
# Lin t
m <- lm(y ~ D:t + factor(ccode) + factor(t), data=dat)
ptrend[1,1] <- lht(m, "D:t=0", vcov=vcovHC(m, type="HC3"))[4][2,1]
ptrend[1,2] <- lht(m, "D:t=0", vcov=vcovBS(m, type="wild", R=999L, cluster=dat$ccode))[4][2,1]


# Quad t
m <- lm(y ~ D:t + D:tsq + factor(ccode) + factor(t), data=dat)
ptrend[2,1] <- lht(m, "D:t + D:tsq = 0", vcov=vcovHC(m, type="HC3"))[4][2,1]
ptrend[2,2] <- lht(m, "D:t + D:tsq = 0", vcov=vcovBS(m, type="wild", R=999L, cluster=dat$ccode))[4][2,1]

# Cov. set used in DiD spec. M3d
# Lin t
m <- lm(y ~ D:t + factor(ccode) + factor(t) + 
        RAIIP  + E_eubadIP + eu_pos_meanIP + ch_imp_fns + opencIP + S_uegenIP, data=dat)
ptrend[3,1] <- lht(m, "D:t=0", vcov=vcovHC(m, type="HC3"))[4][2,1]
ptrend[3,2] <- lht(m, "D:t=0", vcov=vcovBS(m, type="wild", R=999L, cluster=dat$ccode))[4][2,1]
# Quad t
m <- lm(y ~ D:t + D:tsq + factor(ccode) + factor(t) + 
        RAIIP  + E_eubadIP + eu_pos_meanIP + ch_imp_fns + opencIP + S_uegenIP, data=dat)
ptrend[4,1] <- lht(m, "D:t + D:tsq = 0", vcov=vcovHC(m, type="HC3"))[4][2,1]
ptrend[4,2] <- lht(m, "D:t + D:tsq = 0", vcov=vcovBS(m, type="wild", R=999L, cluster=dat$ccode))[4][2,1]

res <- list(m_table, ptrend)
save(res, file="DiD_prelim.Rdata")

