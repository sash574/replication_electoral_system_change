
# DiD model for main text
# LHS: RWP vote shares


set.seed(54321)

library(readstata13)
library(estimatr)
library(sandwich)
library(lmtest)
library(imputeTS)
library(pbapply)
library(arm)

source("functionDefs.R")


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

dat$D <- ifelse(dat$ccode==11, 1, 0)
dat$fD <- ifelse(dat$ccode==11, 1999, 0)
dat$post <- ifelse(dat$ep_election>=1999, 1, 0)
dat$opencIP <- dat$opencIP/100

# Standardize X vars
cov <- c("unempIP", "S_uegenIP", "opencIP", "ch_imp_aec", "RAIIP", "E_eubadIP", 
   "ch_imp_fns", "eu_pos_meanIP", "migr_inflIP")
dat[, cov] <- apply(dat[, cov], 2, zstd)



############################ 2x2 DiD ############################ 

dat2 <- aggregate(dat[, names(dat) %nin% c("country_name_short", "country")],
   list(dat$ccode, dat$post), 
   mean, na.rm=T)

res$m1 <- est_did(dat2, rr_votes ~ post*D)


###################### Panel (multiperiod) DiD ##################

# M2 Panel DiD, t-averaged ATT, unconditional parallel trends
f <- rr_votes ~ D
res$m2 <- est_pdid(f, dat, ~1)

# M3 Panel DiD, t-averaged ATT, conditional parallel trends

# M3a Devolution
res$m3a <- est_pdid(f, dat, ~1 + RAIIP, pDmethod = "blogit")

# M3b Economic Integration
res$m3b <- est_pdid(f, dat, ~1 + RAIIP + ch_imp_fns + opencIP, 
   pDmethod = "blogit")

# M3c Welfare generosity
res$m3c <- est_pdid(f, dat, ~1 + RAIIP  + ch_imp_fns + opencIP +
   S_uegenIP, pDmethod = "blogit")

# M3d Euroscepticism
res$m3d <- est_pdid(f, dat, ~1 + RAIIP  + E_eubadIP + eu_pos_meanIP + 
   ch_imp_fns + opencIP + S_uegenIP, pDmethod = "blogit")

# M3e Immigration inflows
res$m3e <- est_pdid(f, dat[dat$ep_election>1979 & dat$ccode!=6,], ~1 + RAIIP  + E_eubadIP + eu_pos_meanIP + 
   ch_imp_fns + opencIP + S_uegenIP + migr_inflIP, pDmethod = "blogit")


save(res, file="DiD_est.Rdata")
