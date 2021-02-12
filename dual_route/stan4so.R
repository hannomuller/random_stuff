# set up
Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# load data
my_df <- read.delim('~/GITHUB/BALDEY/bayesian-model/df4stan.txt', sep = "\t")
my_df$item <- factor(my_df$item)
my_df$subject <- factor(my_df$subject)

stanDat <- list(N=nrow(my_df), y=my_df$y, x1=my_df$x1, x2=my_df$x2, x3=my_df$x3, x4=my_df$x4, x5=my_df$x5, x6=my_df$x6, subj=as.integer(my_df$subject), J=nlevels(my_df$subject), item=as.integer(my_df$item), K=nlevels(my_df$item))

# fit mixed-effects model
# en_lmm <- stan(file="~/GITHUB/BALDEY/bayesian-model/en_lmm.stan", data=stanDat, iter=100000, chains=24, control=list(max_treedepth=20))
model <- stan(file="~/GITHUB/BALDEY/bayesian-model/lmm4so.stan", data=stanDat, iter=2000, chains=4, control=list(max_treedepth=10))
