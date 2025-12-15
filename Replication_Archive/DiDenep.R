
set.seed(54321)

library(readstata13)
library(estimatr)
library(sandwich)
library(lmtest)

source("functionDefs.R")
require(pbapply)


sessionInfo()



# Results list
res <- list()


######################## Panel DiD models ######################## 
dat <- read.dta13("./CountryEPData.dta", convert.factors=F)
dat <- subset(dat, subset=dat$ccode %nin% c(5,9,12))
dat <- subset(dat, subset=dat$ep_election!=2019)
datIP <- read.dta13("./CountryEPData_covIP.dta")
dat <- merge(dat, datIP, by=c("ccode", "year", "ep_election"))
dat <- as.data.frame(dat)
dat <- subset(dat, subset = dat$year %in% unique(dat$ep_election))
dat <- subset(dat, is.na(dat$enep)==FALSE)

dat$D <- ifelse(dat$ccode==11, 1, 0)
dat$fD <- ifelse(dat$ccode==11, 1999, 0)
dat$post <- ifelse(dat$ep_election>=1999, 1, 0)
dat$opencIP <- dat$opencIP/100

# Standardize X vars
cov <- c("unempIP", "S_uegenIP", "opencIP", "ch_imp_aec", "RAIIP", "E_eubadIP", "eu_pos_meanIP", "migr_inflIP")
dat[, cov] <- apply(dat[, cov], 2, zstd)


###################### Panel (multiperiod) DiD ##################

# M1 Panel DiD, unconditional parallel trends
f <- enep_ep ~ D
res$m1 <- est_pdid(f, dat, ~1)

# M2 Panel DiD, conditional parallel trends
res$m2a <- est_pdid(f, dat, ~1 + RAIIP, pDmethod="blogit")

res$m2b <- est_pdid(f, dat, ~1 + RAIIP + 
   ch_imp_fns + opencIP + S_uegenIP, pDmethod="blogit")

res$m2c <- est_pdid(f, dat[dat$ep_election>1979 & dat$ccode!=6,], ~1 + RAIIP + 
   migr_inflIP +ch_imp_fns + opencIP + 
   S_uegenIP, pDmethod="blogit")


save(res, file="DiDenep_est.Rdata")


