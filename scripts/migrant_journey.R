
library(readr)
library(dplyr)

migrant_data <- read_csv("data/migrant_journey_updatedSep21_e_data.csv")
migrant_data$valid_visa <- c(migrant_data$value[-1],NA)
migrant_data$total <- migrant_data$value + migrant_data$valid_visa
migrant_data$prop <- migrant_data$value/migrant_data$total
migrant_data <- migrant_data[migrant_data$status_year_end == "Expired", ]

# initial population entering UK
migrant_pop <-
  migrant_data |>
  group_by(nationality, year_issued, visa_cat_broad) |>
  summarise(pop = first(total))

save(migrant_pop, file = "data/migrant_pop.RData")


sample_dat <-
  filter(
    migrant_data,
    # year_issued == 2005,
    visa_cat_broad == "Work",
    # visa_cat_broad == "Dep_J_A",
    nationality == "China")
    # nationality == "Sub_Saharan_Africa")


N <- sample_dat$total[1]

########
# BUGS
########

jags_dat <-
  list(N = sample_dat$total,
       x = sample_dat$value,
       nat = as.numeric(as.factor(sample_dat$nationality)),
       id = as.numeric(as.factor(sample_dat$year_issued)),
       t = sample_dat$year - min(sample_dat$year) + 1,
       n_id = length(unique(sample_dat$year_issued)),
       n_nat = length(unique(sample_dat$nationality)),
       n_dat = nrow(sample_dat))

# params <- c("rate", "pi")
# params <- c("mu", "sigma", "pi", "S_pred", "x_pred")
params <- c("mu", "sigma", "pi")

n.iter <- 10000
n.burnin <- 5000
n.thin <- floor((n.iter - n.burnin)/500)

library(R2jags)

out <-
  jags(data = jags_dat,
       parameters.to.save = params,
       model.file = "scripts/BUGS_script.txt",
       inits = list(list("mu" = matrix(rep(0,17)), "sigma" = matrix(rep(1,17)), "pi" = matrix(rep(0.5,17)))),
       # inits = list(list("rate" = 0.5, "pi" = 0.5)),
       n.chains = 1,
       n.iter,
       n.burnin,
       n.thin,
       DIC = TRUE)

R2WinBUGS::attach.bugs(out$BUGSoutput)

########
# plots
########

library(purrr)

props <- split(sample_dat$prop, jags_dat$id)

# empirical
plot(NA, ylim = c(0,1), xlim = c(0,18))
map(1:17, ~lines(jags_dat$t[jags_dat$id == .], 1 - props[[.]], ylim = c(0,1), type = "l"))

for (i in 1:2000) {
  lines(pi[i] + (1-pi[i])*plnorm(q = seq(0,17,1), meanlog = mu[i], sdlog = sigma[i], lower.tail = FALSE), col = "lightgrey")
}

lines(0.699 + (1-0.699)*pexp(q = seq(0,17,1), rate = 0.152, lower.tail = FALSE), col = "blue")
lines(0.741 + (1-0.741)*plnorm(q = seq(0,17,1), meanlog = 1.28, sdlog = 0.608, lower.tail = FALSE), col = "red")


lines(
  out$BUGSoutput$summary["pi[3,1]","mean"] +
    (1-out$BUGSoutput$summary["pi[3,1]","mean"])*plnorm(q = seq(0,17,1),
                                                   meanlog = out$BUGSoutput$summary["mu[3,1]","mean"],
                                                   sdlog = out$BUGSoutput$summary["sigma[3,1]","mean"],
                                                   lower.tail = FALSE), col = "red")

# # predictions
# plot(c(1, x_pred[1, ]/N), ylim = c(0,1), type = "l", col = "lightgrey")
# for (i in 1:200) {
#   lines(c(1, x_pred[i, ]/N), ylim = c(0,1), type = "l", col = "lightgrey")
# }


# library(survHE)
#
# survHE::fit.models()
# survHE::make.surv()

