# ============================================================================================================================= #
# But:                                                                                                                          #
# Auteur: Francis Duval                                                                                                         #
# Date: Novembre 2020                                                                                                           #
# Input: train_ls.RDS                                                                                                           #
# Output:                                                                                                                       #
# ============================================================================================================================= #

source("1_code/00_source.R")


# Importer les jeux de données d'entrainement ===================================================================================
train_ls <- read_rds(here("2_pipeline", "01_create_data", "train_ls.RDS"))


# Créer les jeux d'entrainement =================================================================================================
train_classic <- train_ls[[1]] %>% select(starts_with("c_"), claim_ind, -c_expo)
train_ls %<>% map(select, starts_with("c_"), starts_with("t_"), claim_ind, -c_expo) 


# Créer les folds pour la validation croisée ====================================================================================
create_resamples <- function(data) {
  set.seed(2020)
  vfold_cv(data, v = 10, strata = claim_ind)
}

resamples_ls <- map(train_ls, create_resamples)
resamples_classic <- create_resamples(train_classic)


# Définir le tuning =============================================================================================================
tune_glmnet <- 
  logistic_reg(
    penalty = tune(),
    mixture = 1
  ) %>% 
  set_engine("glmnet")


# Définir le pré-traitement =====================================================================================================
rec <- 
  recipe(claim_ind ~ ., data = train_ls[[1]]) %>% 
  step_other(all_nominal(), -all_outcomes(), threshold = 0.05) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_YeoJohnson(all_predictors()) %>% 
  step_knnimpute(c_commute_distance)

rec_classic <- 
  recipe(claim_ind ~ ., data = train_classic) %>% 
  step_other(all_nominal(), -all_outcomes(), threshold = 0.05) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_YeoJohnson(all_predictors()) %>% 
  step_knnimpute(c_commute_distance)


# Définir les workflows =========================================================================================================
wf <- 
  workflow() %>% 
  add_model(tune_glmnet) %>% 
  add_recipe(rec)

wf_classic <- 
  workflow() %>% 
  add_model(tune_glmnet) %>% 
  add_recipe(rec_classic)


# Tuner les modèles =============================================================================================================
lambda_grid <- grid_regular(penalty(), levels = 50)

tune_models <- function(wf, resamples) {
  tune_grid(
    wf,
    resamples = resamples,
    grid = lambda_grid,
    metrics = metric_set(roc_auc)
  )
}

res <- map(resamples_ls, ~ tune_models(wf, resamples = .))
res_classic <- tune_models(wf_classic, resamples = resamples_classic)


# Sauvegarder les résultats =====================================================================================================
write_rds(res, path = here("2_pipeline", "03_calibrate", "res.RDS"))
write_rds(res_classic, path = here("2_pipeline", "03_calibrate", "res_classic.RDS"))


# Importer les résultats ========================================================================================================
res <- read_rds(here("2_pipeline", "03_calibrate", "res.RDS"))
res_classic <- read_rds(here("2_pipeline", "03_calibrate", "res_classic.RDS"))

res_total <- vector("list", length = 13)
res_total[2:13] <- res
res_total[1] <- list(res_classic)
names(res_total) <- glue("Variables classiques et {0:12} mois de télématique")

best_param_ls <- map_dbl(res_total, ~ select_best(., metric = "roc_auc")$penalty)
best_param_classic_ls <-  select_best(res_classic, metric = "roc_auc")


# Faire les graphiques du tuning ================================================================================================
plot_lasso_tuning <- function(res, subtitle) {
  res %>%
  collect_metrics() %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_linerange(aes(ymin = mean - std_err, ymax = mean + std_err), alpha = 0.5, col = "#481567FF") +
  geom_line(alpha = 0.3, col = "#481567FF") +
  geom_point( col = "#481567FF") +
  ggtitle("Calibration de la régression logistique LASSO") +
  xlab("Penalty") +
  ylab("AUC") +
  labs(caption = "10-folds cross-validation, grid search", subtitle = subtitle) +
  scale_x_log10(labels = trans_format("log10", math_format(10 ^ .x)))
}


cairo_pdf(here("2_pipeline", "03_calibrate", "tuning.pdf"), onefile = T)
  iwalk(res_total, ~ print(plot_lasso_tuning(.x, subtitle = .y)))
dev.off()


# ===============================================================================================================================
names(res_total) <- 0:12
res_df <- map2_dfc(res_total, best_param_ls, ~ collect_metrics(.x, summarize = F) %>% filter(penalty == .y) %>% pull(.estimate))
res_df %<>%
  mutate(fold = 1:10) %>% 
  pivot_longer(cols = -fold) %>% 
  mutate(name = as.numeric(name))

cairo_pdf(here("2_pipeline", "03_calibrate", "tuning_best_lambda.pdf"), onefile = T)
  res_df %>% 
    ggplot(aes(x = name, y = value, col = factor(fold))) +
    labs(col = "Fold") +
    ggtitle("Résultat de la calibration avec le meilleur lambda") +
    xlab("Nombre de mois de télématique utilisés") +
    ylab("AUC") +
    scale_x_continuous(breaks = 0:12) +
    geom_point(size = 1.5) +
    geom_line()
dev.off()
