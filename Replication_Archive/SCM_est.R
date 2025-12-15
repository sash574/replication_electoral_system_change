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
dat <- subset(dat, subset=dat$ccode %nin% c(5,6,9,12))
dat <- subset(dat, subset=dat$ep_election!=2019)

datIP <- read.dta13("./CountryEPData_covIP.dta")
dat <- merge(dat, datIP, by=c("ccode", "year", "ep_election"))


dat <- as.data.frame(dat)
dat <- subset(dat, select=c("rr_votes", "E_eubadIP", "E_NatDem_satisfiedIP", 
   "unempIP", "opencIP", "S_uegenIP",  "rightgovIP", "ch_imp_fns", "eu_pos_meanIP",
   "migr_inflIP", "ccode", "country", "ep_election"))


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
                    "migr_inflIP" = c(1984,1994),
                    "rr_votes" = c(1994)
)
res <- mscmt(dat, treatment_id, controls_id, times_dep, times_pred, 
   agg.fns = rep("id", ncol(times_pred)),
   inner.optim = "wnnlsOpt",
   outer.optim=  "DEoptC",
   outer.par = list(lb=1e-9, opt.separate = TRUE),
   seed = seeds, check.global = TRUE, 
   placebo = TRUE, placebo.with.treated = FALSE,
   return.ts = FALSE, cl = cl)


# In place p-value, two sided and one-sided [>]

# post-reform periods: 1999, 2004, 2009, 2014
post_periods <- 5:8

J <- 7    # note: excl. treated unit from placebo set as in Abadie et al
gaps <- matrix(NA, nrow=8, ncol=4)
for(i in 1:8){
   y1 <- res[[i]]$data.treat$rr_votes[post_periods]
   y0 <- res[[i]]$data.synth$rr_votes[post_periods] 
   gaps[i,1:4] <- y1-y0
}
post_p <- cbind(c(1999, 2004, 2009, 2014), gaps[1,], rep(NA,4), rep(NA,4))
for(i in 1:4){
   post_p[i,3] <- sum(abs(gaps[2:8,i])>abs(gaps[1,i]))/J
   post_p[i,4] <- sum(gaps[2:8,i]>gaps[1,i])/J
}
res$post_p <- post_p

# 1979-1994 average and p-val
avg <- apply(gaps, 1, mean)
avg_p <- sum(avg[2:8] > avg[1])/J
res$avg_p <- c(avg[1], avg_p)


save(res, file="scm_est.Rdata")



# print W, V, matrices
print(round(res[[1]]$w, 3))
print(round(res[[1]]$v, 3))

