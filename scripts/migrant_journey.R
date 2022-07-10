
# migrant journey data
# from the Home Office
# entry to UK by visa category and expiry year
# estimate survival curves

library(readr)
library(dplyr)
library(purrr)


migrant_data <- read_csv("data/migrant_journey_updatedSep21_e_data.csv")

migrant_data <-
  migrant_data |>
  mutate(valid_visa = c(migrant_data$value[-1], NA),
         total = value + valid_visa,
         prop = value/total) |>
  filter(status_year_end == "Expired")

# initial population entering UK
migrant_pop <-
  migrant_data |>
  group_by(nationality, year_issued, visa_cat_broad) |>
  summarise(pop = first(total))

save(migrant_pop, file = "data/migrant_pop.RData")

sample_dat <-
  migrant_data |>
  group_by(nationality, year_issued, year) |>
  summarise(value = sum(value),
            total = sum(total)) |>
  mutate(S = 1 - value/total)

########
# BUGS
########

jags_dat <-
  list(N = sample_dat$total,
       x = sample_dat$value,
       nat = as.numeric(as.factor(sample_dat$nationality)),  # Bangladesh, China, India, Pakistan, Sub_Saharan_Africa
       id = as.numeric(as.factor(sample_dat$year_issued)),
       t = sample_dat$year - min(sample_dat$year) + 1,
       n_id = length(unique(sample_dat$year_issued)),
       n_nat = length(unique(sample_dat$nationality)),
       n_dat = nrow(sample_dat))

# params <- c("rate", "pi")
# params <- c("mu", "sigma", "pi", "S_pred", "x_pred")
params <- c("mu", "sigma", "pi")

n_nat <- 5
n_yr <- 16

n.iter <- 10000
n.burnin <- 5000
n.thin <- floor((n.iter - n.burnin)/500)

library(R2jags)

out <-
  jags(data = jags_dat,
       parameters.to.save = params,
       model.file = "scripts/BUGS_script.txt",
       inits = list(list("mu" = matrix(0,17, n_nat),
                         "sigma" = matrix(1,17, n_nat),
                         "pi" = matrix(0.5,17, n_nat))),
       # inits = list(list("rate" = 0.5, "pi" = 0.5)),
       n.chains = 1,
       n.iter,
       n.burnin,
       n.thin,
       DIC = TRUE)

out$BUGSoutput$summary

R2WinBUGS::attach.bugs(out$BUGSoutput)

save(out, file = "data/BUGS_output.RData")

########
# plots
########

library(reshape2)
library(ggplot2)


props <- split(sample_dat$S, jags_dat$id)

mean_mu <- apply(out$BUGSoutput$sims.list$mu, FUN = mean, MARGIN = c(2,3))
mean_pi <- apply(out$BUGSoutput$sims.list$pi, FUN = mean, MARGIN = c(2,3))
mean_sigma <- apply(out$BUGSoutput$sims.list$sigma, FUN = mean, MARGIN = c(2,3))

plot(mean_pi[5,5] + (1-mean_pi[5,5])*plnorm(
  q = seq(0,17,1), meanlog = mean_mu[5,5], sdlog = mean_sigma[5,5], lower.tail = FALSE),
     ylim = c(0,1), type = "l")

res <- array(dim = c(n_yr, 18, n_nat),
             dimnames = list(year_issue = 2004:2019,
                             year_expire = 2003:2020,
                             nat = unique(sample_dat$nationality)))

for (j in seq_len(n_nat)) {
  for (i in seq_len(n_yr)) {
    cf <- mean_pi[i,j]
    S_mean <- cf + (1-cf)*plnorm(q = seq(0,17,1),
                                 meanlog = mean_mu[i,j],
                                 sdlog = mean_sigma[i,j],
                                 lower.tail = FALSE)
    res[i,,j] <- round(S_mean, 2)
    lines(S_mean, ylim = c(0,1), type = "l", col = j)
  }
}


# empirical
plot(NA, ylim = c(0,1), xlim = c(0,18))
map(1:(n_yr+1), ~lines(jags_dat$t[jags_dat$id == .] + 1, props[[.]], ylim = c(0,1), type = "o"))

for (i in 1:2000) {
  lines(pi[i] + (1-pi[i])*plnorm(
    q = seq(0, (n_yr+1), 1), meanlog = mu[i], sdlog = sigma[i], lower.tail = FALSE), col = "lightgrey")
}

## expire survival tidy data

res_long <-
  melt(res) |>
  mutate(grp = paste(year_issue, nat))

ggplot(res_long, aes(x = year_expire, y = value, group = grp, col = nat)) + geom_line() + ylim(0,1) +
  # facet_wrap(vars(nat))
  facet_wrap(vars(year_issue))





# save survival fits
save(res_long, file = "data/posterior_S.RData")




# # predictions
# plot(c(1, x_pred[1, ]/N), ylim = c(0,1), type = "l", col = "lightgrey")
# for (i in 1:200) {
#   lines(c(1, x_pred[i, ]/N), ylim = c(0,1), type = "l", col = "lightgrey")
# }


# library(survHE)
#
# survHE::fit.models()
# survHE::make.surv()

