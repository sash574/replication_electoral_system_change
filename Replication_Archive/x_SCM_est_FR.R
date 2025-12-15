# Synthetic control estimates
# 
# Outer function optimizer used:
# DEoptC, 50 random seeds
# best solution kept
# 


set.seed(54321)

library(MSCMT)
library(readstata13)
library(parallel)
cl <- makeCluster(8)

source("functionDefs.R")

sessionInfo()


dat <- read.dta13("./CountryEPData.dta", convert.factors=F)
dat <- subset(dat, subset=dat$ccode %nin% c(5,9,12))
dat <- subset(dat, subset=dat$ep_election!=2019)

datIP <- read.dta13("./CountryEPData_covIP.dta")
dat <- merge(dat, datIP, by=c("ccode", "year", "ep_election"))


dat <- as.data.frame(dat)
dat <- subset(dat, select=c("rr_votes", "E_eubadIP", "E_NatDem_satisfiedIP", 
   "unempIP", "opencIP", "S_uegenIP",  "rightgovIP", "ch_imp_fns", "eu_pos_meanIP", "ccode", "country", "ep_election"))


dat <- listFromLong(dat, unit.variable="ccode",
unit.names.variable="country",
   time.variable="ep_election")

treatment_id <- "United Kingdom"
controls_id  <- setdiff(colnames(dat[[1]]), treatment_id)
times_dep  <- cbind("rr_votes" = c(1979,1994))

seeds <- sample(4322:97834, 50, replace = FALSE) 



# Run w placebos, UK is list element [[1]]

# Specification:
# - covarites as time series 
# - lagged Y for 1994 (to capture pre-reform increase in RWP share)
times_pred <- cbind(
                    "E_eubadIP" = c(1979,1994),
                    "E_NatDem_satisfiedIP" = c(1979,1994),
                    "unempIP" = c(1979,1994),
                    "opencIP" = c(1979,1994),
                    "ch_imp_fns" = c(1979,1994),
                    "S_uegenIP" = c(1979,1994),
                    "rightgovIP" = c(1979,1994),
                    "eu_pos_meanIP" = c(1979,1994),
                    "rr_votes" = c(1994)
)
resFR <- mscmt(dat, treatment_id, controls_id, times_dep, times_pred, 
   agg.fns = rep("id", ncol(times_pred)),
   inner.optim = "wnnlsOpt",
   outer.optim=  "DEoptC",
   outer.par = list(lb=1e-9, opt.separate = TRUE),
   seed = seeds, check.global = TRUE, 
   return.ts = FALSE, cl = cl)


save(resFR, file="x_scm_est_FR.Rdata")

