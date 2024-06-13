# Load packages ----

library("dplyr")
library("ggplot2")
library("ggforce")
library("brms")

# Laod  data ----

data(pilot, package = "reppg2009")

# Plot p1 data ----

pilot |>
  dplyr::summarize(
    rt = median(rt)
    , dlpfc = mean(dlpfc_activity)
    , acc = mean(acc_activity)
    , .by = c("condition", "correct")
) |>
  dplyr::arrange(correct, condition)
j
## Correlation betwen RT and FMRI measures ----

ggplot(pilot) +
  aes(x = rt, y = dlpfc_activity) +
  geom_point() +
  facet_grid(correct ~ condition) 

ggplot(pilot) +
  aes(x = rt, y = acc_activity) +
  geom_point() +
  facet_grid(correct ~ condition) 

## DV distributions ----

ggplot(pilot) +
  aes(x = rt, y = condition, fill = correct) +
  geom_violin() +
  geom_sina()

ggplot(pilot) +
  aes(x = dlpfc_activity, y = condition, fill = correct) +
  geom_violin() +
  geom_sina() +
  stat_summary(
    fun.data = mean_cl_normal, shape = 21, position = position_dodge(width = 1), size = 2
  )

ggplot(pilot) +
  aes(x = acc_activity, y = condition, fill = correct) +
  geom_violin() +
  geom_sina() +
  stat_summary(
    fun.data = mean_cl_normal, shape = 21, position = position_dodge(width = 1), size = 2
  )


# Model fitting ----

options(contrasts = c("contr.sum", "contr.poly"))

lognorm_formula <- bf(
  rt ~ 1 + condition * correct
  # , ndt ~ 1 + condition * correct
)

mod_p1 <- brm(
  formula = lognorm_formula
  , data = pilot
  , family = brms::shifted_lognormal
  , chains = 4
  , cores = 2
  , iter = 2000
  , warmup = 1000
  , thin = 1
  # , control = list(adapt_delta = 0.99)
  , seed = 123
)

## Extract posterior means of distributional parameters ----

as.data.frame(mod_p1) |>
  posterior_summary()

pm_ndt <- as_draws(mod_p1, variable = "ndt") |>
  summary() |>
  dplyr::pull(mean)

ggplot(pilot) +
  aes(x = rt - pm_ndt, y = condition, fill = correct) +
  geom_violin() +
  geom_sina()



summary(mod_p1)

library("emmeans")

emmeans(mod_p1, ~ condition * correct) |>
  tibble::as_tibble() |>
  dplyr::mutate_if(is.numeric, ~ exp(.) + pm_ndt)

pilot |>
  dplyr::summarize(
    rt = mean(rt)
    , .by = c("condition", "correct")
) |>
  dplyr::arrange(correct, condition)

emmeans(mod_p1, ~ condition * correct) |>
  tibble::as_tibble()

normalizer <- pilot |>
  dplyr::summarize(
    logmean = mean(log(rt - pm_ndt))
    , logsd = sd(log(rt - pm_ndt))
    , .by = c("condition", "correct")
) |>
  dplyr::arrange(correct, condition) |>
  dplyr::mutate(
    logmean_offset = scale(logmean, scale = FALSE)[, 1]
    , logsd_offset = logsd / mean(logsd)
  ) |>
  dplyr::select(-logmean, -logsd)

blinded_pilot <- pilot |>
  dplyr::left_join(normalizer, by = c("correct", "condition")) |>
  dplyr::mutate(
    logrt = log(rt - pm_ndt)
    , blinded_logrt = logrt - logmean_offset
    , blinded_rt = exp(blinded_logrt) + pm_ndt
  ) # |>
  dplyr::select(-offset) |>
  dplyr::left_join(
    normalizer |> dplyr::mutate(condition = sample(condition), correct = sample(correct))
    , by = c("correct", "condition")
  ) |>
  dplyr::mutate(
    , blinded_logrt = logrt + offset
    , blinded_rt = exp(blinded_logrt) + pm_ndt
  )

# ggplot(blinded_pilot) +
#   aes(x = logrt, y = condition, fill = correct) +
#   geom_violin() +
#   geom_sina()

ggplot(blinded_pilot) +
  aes(y = condition, fill = correct) +
  geom_violin(aes(x = blinded_rt)) +
  geom_sina(aes(x = rt))


blinded_pilot |>
  dplyr::summarize(
    rt = median(blinded_rt)
    , .by = c("condition", "correct")
) |>
  dplyr::arrange(correct, condition)


lognorm_formula <- bf(
  blinded_rt ~ 1 + condition * correct
  # , ndt ~ 1 + condition * correct
)

mod_p1_blinded <- brm(
  formula = lognorm_formula
  , data = blinded_pilot
  , family = brms::shifted_lognormal
  , chains = 4
  , cores = 2
  , iter = 2000
  , warmup = 1000
  , thin = 1
  # , control = list(adapt_delta = 0.99)
  , seed = 123
)

summary(mod_p1_blinded)

emmeans(mod_p1_blinded, ~ condition * correct) |>
  tibble::as_tibble() |>
  dplyr::mutate_if(is.numeric, ~ exp(.) + pm_ndt)

blinded_pilot |>
  dplyr::summarize(
    rt = median(blinded_rt)
    , .by = c("condition", "correct")
) |>
  dplyr::arrange(correct, condition)


options(contrasts = c("contr.sum", "contr.poly"))

lognorm_formula <- bf(
  rt ~ 1 + condition * correct
  , sigma ~ 1 + condition * correct
  # , ndt ~ 1 + condition * correct
)

mod_p1s <- brm(
  formula = lognorm_formula
  , data = pilot
  , family = brms::shifted_lognormal
  , chains = 4
  , cores = 2
  , iter = 2000
  , warmup = 1000
  , thin = 1
  # , control = list(adapt_delta = 0.99)
  , seed = 123
)

as_draws(mod_p1s, variable = "sigma", regex = TRUE) |>
  summary() |>
  dplyr::select(variable, mean)

emmeans(mod_p1s, ~ condition * correct) |>
  tibble::as_tibble() |>
  dplyr::mutate_if(is.numeric, ~ exp(.) + pm_ndt)




# Hierachical data ----
summary(mod_p1)

n <- 35

## Generate synthetic data ----

### Dishonesty scores ----

curve(dbeta((x - 0.5) / 0.5, 0.75, 1.25), 0, 1)
dishonesty <- (rbeta(n, 0.75, 1.25) + 0.5) / 1.5

### Response times ----

curve(dshifted_lnorm(x, meanlog = log(0.200), sdlog = 1/2), 0, 0.5)
ndt <- log(0.2)
sd_ndt <- 1/2

curve(dshifted_lnorm(x, meanlog = -0.5, sdlog = 1/2), 0, 2)
sigma <- -0.5
sigma_sd <- 0.5

curve(dshifted_lnorm(x, meanlog = -0.75, sdlog = 1/3), 0, 1.5)
b_0 <- -0.75
sd_0 <- 1/3

b_condition <- -0.5
b_correct <- 0.33
b_cond_cor <- -0.3

sd_condition  <- 0.1
sd_correct <- 0.1
sd_cond_cor <- 0.1

ndt_i <- rshifted_lnorm(n, meanlog = ndt, sdlog = sd_ndt)
sigma_i <- rshifted_lnorm(n, meanlog = sigma, sdlog = sigma_sd)
b_0_i <- rnorm(n, mean = b_0, sd = sd_0)

b_condition_i <- rnorm(n, mean = b_condition, sd = sd_condition)
b_correct_i <- rnorm(n, mean = b_correct, sd = sd_correct)
b_cond_cor_i <- rnorm(n, mean = b_cond_cor, sd = sd_cond_cor)

### fMRI data ----

### Response times ----

b_0 <- 0
sd_0 <- 0.1

b_condition <- -0.5
b_correct <- 0.33
b_cond_cor <- -0.3

sd_condition  <- 0.1
sd_correct <- 0.1
sd_cond_cor <- 0.1

b_0_i <- rnorm(n, mean = b_0, sd = sd_0)
b_condition_i <- rnorm(n, mean = b_condition, sd = sd_condition)
b_correct_i <- rnorm(n, mean = b_correct, sd = sd_correct)
b_cond_cor_i <- rnorm(n, mean = b_cond_cor, sd = sd_cond_cor)

design <- expand.grid(
  condition = c("op", "noop")
  , correct = c("loss", "win")
) |>
  dplyr::mutate(
    n = c(10, 35, 60, 35)
    , condition_c = dplyr::if_else(condition == "noop", 1, -1)
    , correct_c = dplyr::if_else(correct == "loss", 1, -1)
    , cond_cor_c = condition_c * correct_c
  ) |>
  tidyr::crossing(id = 1:35)

coef <- data.frame(
  id = 1:35
  , dishonesty = dishonesty
  , ndt = ndt_i
  , sigma = sigma_i
  , b_0 = b_0_i
  , b_condition = b_condition_i
  , b_correct = b_correct_i
  , b_cond_cor = b_cond_cor_i
)

synth_dat <- dplyr::left_join(
  design
  , coef
  , by = "id"
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    rt = rshifted_lnorm(
      n
      , meanlog = b_0 + condition_c * b_condition + correct_c * b_correct +
        b_cond_cor * cond_cor_c
      , sdlog = sigma
      , shift = ndt
    ) |> list(rt = _)
    , dlpfc_activity = rnorm(
      n
      , mean = b_0 - !!b_0 + condition_c * b_condition - correct_c * b_correct -
        b_cond_cor * cond_cor_c
      , sd = 1
    ) |> list(dlpfc_activity = _)
    , acc_activity = rnorm(
      n
      , mean = b_0 - !!b_0 + condition_c * b_condition + correct_c * b_correct +
        b_cond_cor * cond_cor_c
      , sd = 1
    ) |> list(acc_activity = _)
  ) |>
  tidyr::unnest(cols = c("rt", "dlpfc_activity", "acc_activity"))

synth_dat |>
  dplyr::filter(id == 2) |>
  ggplot() +
    aes(x = dlpfc_activity, y = condition, fill = correct) +
    geom_violin() +
    geom_sina()

lognorm_formula <- bf(
  # rt ~ 1 + condition * correct + (condition * correct | id)
  rt ~ 1 + condition * correct + (1 | id)
  , sigma ~ 1 + (1 | id)
  , ndt ~ 1 + (1 | id)
)

mod_synth <- brm(
  formula = lognorm_formula
  , data = synth_dat
  , family = brms::shifted_lognormal
  , chains = 1
  , cores = 1
  , iter = 2000
  , warmup = 1000
  , init = list(list(Intercept_ndt = -5))
  , thin = 1
  , control = list(adapt_delta = 0.99, max_treedepth = 15)
  , seed = 123
)

post_dpar <- tidybayes::epred_draws(
  mod_synth
  , dpar = c(mu = "mu", sigma = "sigma", ndt = "ndt")
  , newdata = synth_dat[, c("id", "condition", "correct")] |> unique()
  , re_formula = NULL
) |>
  dplyr::summarize(
    mu = median(mu)
    , ndt = tidybayes::median_qi(ndt)
    , sigma = tidybayes::median_qi(sigma)
  )

dplyr::left_join(post_dpar, data.frame(id = 1:35, ndt_i = ndt_i), by = "id") |>
ggplot() +
  aes(x = ndt_i, y = ndt$y, ymin = ndt$ymin, ymax = ndt$ymax) +
  geom_abline() +
  geom_pointrange() +
  theme_minimal()

dplyr::left_join(post_dpar, data.frame(id = 1:35, sigma_i = sigma_i), by = "id") |>
ggplot() +
  aes(x = sigma_i, y = sigma$y, ymin = sigma$ymin, ymax = sigma$ymax) +
  geom_abline() +
  geom_pointrange() +
  theme_minimal()


post_rt <- tidybayes::predicted_draws(
  mod_synth
  , newdata = synth_dat[, c("id", "condition", "correct")] |> unique()
  , re_formula = NULL
) |>
  dplyr::summarize(.epred = tidybayes::median_qi(.prediction))

dplyr::left_join(
  post_rt
  , dplyr::summarize(
    synth_dat
    , rt = median(rt)
    , .by = c("id", "condition", "correct")
  )
  , by = c("id", "condition", "correct")
) |>
  ggplot() +
    aes(x = rt, y = .epred$y, ymin = .epred$ymin, ymax = .epred$ymax, color = id) +
    geom_abline() +
    geom_pointrange() +
    facet_grid(correct ~ condition, scales = "free") +
    theme_minimal()

post_rt <- tidybayes::predicted_draws(
  mod_synth
  , newdata = synth_dat[, c("id", "condition", "correct")] |> unique()
  , re_formula = NA
) |>
  dplyr::summarize(.epred = tidybayes::median_qi(.prediction))

post_dpar_group <- tidybayes::epred_draws(
  mod_synth
  , dpar = c(mu = "mu")
  , newdata = synth_dat[, c("id", "condition", "correct")] |> unique()
  , re_formula = NA
) |>
  dplyr::summarize(mu = median(mu))

synth_dat_blinded <- dplyr::left_join(
  post_dpar_group
  , dplyr::select(post_dpar, -sigma, -mu) |> 
    dplyr::mutate(ndt = ndt$y)
  , by = c("id", "condition", "correct", ".row")
  , relationship = "many-to-many"
) |>
  dplyr::group_by(id) |>
  dplyr::mutate(
    mu_offset = scale(mu, scale = FALSE)[, 1]
  ) |>
  dplyr::ungroup() |>
  dplyr::right_join(
    synth_dat
    , by = c("id", "condition", "correct")
    , suffix = c("_hat", "_true")
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    logrt = log(rt -  ndt_hat)
    , blinded_logrt = logrt - mu_offset
    , blinded_rt = exp(blinded_logrt) + ndt_hat
  ) |>
  dplyr::ungroup()

synth_dat_blinded |>
  dplyr::filter(id == 2) |>
  dplyr::summarize(rt = median(rt), .by = c("condition", "correct"))

synth_dat_blinded |>
  dplyr::filter(id == 2) |>
  dplyr::summarize(rt = median(blinded_rt), .by = c("condition", "correct"))

synth_dat_blinded |>
  dplyr::filter(id == 3) |>
  ggplot() +
    aes(y = condition, fill = correct) +
    geom_violin(aes(x = blinded_rt)) +
    geom_sina(aes(x = rt))

ggplot(synth_dat_blinded) +
  aes(x = blinded_rt, y = condition, color = correct) +
  stat_summary(fun.data = tidybayes::median_qi, position = position_dodge(width = 0.75))

ggplot(synth_dat_blinded) +
  aes(y = condition, fill = correct) +
  stat_summary(aes(x = rt), fun.data = mean_cl_normal, position = position_dodge(width = 0.75), shape = 21) +
  stat_summary(aes(x = blinded_rt), fun.data = mean_cl_normal, position = position_dodge(width = 0.75), shape = 22)


lognorm_formula_blinded <- bf(
  blinded_rt ~ 1 + condition * correct + (condition * correct | id)
  , sigma ~ 1 + (1 | id)
  , ndt ~ 1 + (1 | id)
)

mod_synth_blinded <- brm(
  formula = lognorm_formula_blinded
  , data = synth_dat_blinded
  , family = brms::shifted_lognormal
  , chains = 1
  , cores = 1
  , iter = 2000
  , warmup = 1000
  , init = list(list(Intercept_ndt = -5))
  , thin = 1
  , control = list(adapt_delta = 0.99, max_treedepth = 15)
  , seed = 123
)

summary(mod_synth_blinded)

# TODO ----

# Set seed for reproducibility

# Blind fMRI data

fmri_normalizer <- synth_dat |>
  dplyr::summarize(
    dlpfc_activity = mean(dlpfc_activity)
    , acc_activity = mean(acc_activity)
    , .by = c("condition", "correct")
  ) |>
  dplyr::mutate(
    dlpfc_offset = scale(dlpfc_activity, scale = FALSE)[, 1]
    , acc_offset = scale(acc_activity, scale = FALSE)[, 1]
  ) |>
  dplyr::select(-dlpfc_activity, -acc_activity)

synth_dat_blinded <- synth_dat_blinded |>
  dplyr::left_join(fmri_normalizer, by = c("correct", "condition")) |>
  dplyr::mutate(
    dlpfc_activity = dlpfc_activity - dlpfc_offset
    , acc_activity = acc_activity - acc_offset
  ) |>
  dplyr::select(-dlpfc_offset, -acc_offset)

ggplot(synth_dat_blinded) +
  aes(x = dlpfc_activity, y = condition, fill = correct) +
  stat_summary(
    fun.data = mean_cl_normal
    , shape = 21
    , position = position_dodge(width = 1)
    , size = 2
  )

# Shuffle dishonesty scores ----

shuffled_dishonesty <- synth_dat_blinded |>
  dplyr::select(id, dishonesty) |>
  unique() |>
  dplyr::mutate(dishonesty = sample(dishonesty))

synth_dat_blinded <- synth_dat_blinded |>
  dplyr::select(-dishonesty) |>
  dplyr::left_join(shuffled_dishonesty, by = "id")

# Shuffle fMRI ROI labels  ----
shuffled_fmri_labels <- sample(c(TRUE, FALSE), size = 1)

if(shuffled_fmri_labels) {
  synth_dat_blinded <- synth_dat_blinded |>
    dplyr::mutate(
      dlpfc_activity = synth_dat$acc_activity
      , acc_activity = synth_dat$dlpfc_activity
    )
}

# blinded_pilot <- pilot |>
#   dplyr::left_join(normalizer, by = c("correct", "condition")) |>
#   dplyr::mutate(
#     logrt = log(rt - pm_ndt)
#     , blinded_logrt = logrt - logmean_offset
#     , blinded_rt = exp(blinded_logrt) + pm_ndt
#   ) # |>
#   dplyr::select(-offset) |>
#   dplyr::left_join(
#     normalizer |> dplyr::mutate(condition = sample(condition), correct = sample(correct))
#     , by = c("correct", "condition")
#   ) |>
#   dplyr::mutate(
#     , blinded_logrt = logrt + offset
#     , blinded_rt = exp(blinded_logrt) + pm_ndt
#   )

