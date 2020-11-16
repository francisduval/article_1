# ============================================================================================================================= #
# But:                                                                                                                          #
# Auteur: Francis Duval                                                                                                         #
# Date: Novembre 2020                                                                                                           #
# Input: train_01.RDS:train_12.RDS                                                                                              #
# Output:                                                                                                                       #
# ============================================================================================================================= #

source("1_code/00_source.R")


# Importer les jeux de données d'entrainement ===================================================================================
train_ls <- map(dir_ls(here("2_pipeline", "01_create_data"), regexp = "train"), read_rds)


# S'assurer que les 12 jeux d'entrainement ont le même nombre de lignes =========================================================
id_ls <- map(train_ls, ~ glue("{.$vin}_{.$contract_number}"))
ids <- reduce(id_ls, intersect)
train_ls %<>% map(~ filter(., glue("{vin}_{contract_number}") %in% ids)) 


# Ajout de la variable indicatrice d'une réclamation ou plus ====================================================================
train_ls %<>% map(~ mutate(., claim_ind = factor(as.numeric(nb_claims > 0), levels = c("0", "1")), nb_claims = NULL))


# Enlever la ligne avec exposition de zéro et les 2 lignes avec valeurs manquantes pour years_claim_free ========================
train_ls %<>% map(~ filter(., expo > 0.001, !is.na(years_claim_free)))


# Créer les folds pour la validation croisée ====================================================================================
resamples_ls <- map(train_ls, vfold_cv, v = 5, strata = claim_ind)


# Définir le tuning =============================================================================================================
tune_glmnet <- 
  logistic_reg(
    penalty = tune(),
    mixture = tune()
  ) %>% 
  set_engine("glmnet")


# Définir le pré-traitement =====================================================================================================
rec <- 
  recipe(claim_ind ~ ., data = train_ls[[1]]) %>% 
  step_rm(vin, contract_number, contract_start_date, contract_end_date) %>% 
  step_other(all_nominal(), -all_outcomes(), threshold = 0.05) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_YeoJohnson(all_predictors()) %>% 
  step_knnimpute(commute_distance)


# Définir le workflow ===========================================================================================================
wf <- 
  workflow() %>% 
  add_model(tune_glmnet) %>% 
  add_recipe(rec)


# Tuner les modèles =============================================================================================================
tune_models <- function(wf, resamples) {
  tune_bayes(
    wf,
    resamples = resamples,
    metrics = metric_set(roc_auc),
    iter = 50,
    initial = 10,
    control = control_bayes(save_pred = TRUE, verbose = T, no_improve = 20L)
  )
}

res <- map(resamples_ls, ~ tune_models(wf, resamples = .))
best_param_ls <- map(res, select_best, metric = "roc_auc")


# Sauvegarder les résultats =====================================================================================================
write_rds(best_param_ls, path = here("2_pipeline", "02_calibrate", "best_params.RDS"))


# Tuner le modèle sans données télématiques =====================================================================================
train_classic <- train_ls[[1]] %>% select(expo:years_licensed, claim_ind)
resamples_classic <- vfold_cv(train_classic, v = 5, strata = claim_ind)

rec_classic <- 
  recipe(claim_ind ~ ., data = train_classic) %>% 
  step_other(all_nominal(), -all_outcomes(), threshold = 0.05) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_YeoJohnson(all_predictors()) %>% 
  step_knnimpute(commute_distance)

wf_classic <- 
  workflow() %>% 
  add_model(tune_glmnet) %>% 
  add_recipe(rec_classic)

res_classic <- tune_models(wf_classic, resamples = resamples_classic)
best_param_classic <- select_best(res_classic, metric = "roc_auc")

write_rds(best_param_classic, path = here("2_pipeline", "02_calibrate", "best_params_classic.RDS"))


# Fonction pour faire le graphique du tuning des hyperparamètres pour un glmnet =================================================
plot_glmnet_tuning <- function(tune_res, subtitle) {
  dat <- 
    tune_res %>%
    collect_metrics() %>%
    mutate(best_param = mean == max(mean))
  
  dat_best_param <- 
    dat %>% 
    filter(best_param) %>% 
    filter(penalty == max(penalty)) %>% 
    filter(mixture == max(mixture))
  
  best_penalty <- signif(dat_best_param$penalty, 2)
  best_mixture <- signif(dat_best_param$mixture, 2)
  best_logscore <- signif(dat_best_param$mean, 5)
  
  lab <- glue("Penalty = {best_penalty}\nMixture = {best_mixture}\nAUC = {best_logscore}")
  
  ggplot(dat, aes(x = penalty, y = mixture, size = mean, col = best_param)) +
    geom_point(shape = 21) +
    annotate(
      "label", 
      x = dat_best_param$penalty, 
      y = dat_best_param$mixture + 0.12, 
      label = lab, 
      hjust = 0.5, 
      color = "red", 
      size = 2.1,
      alpha = 0.9,
      family = "Roboto"
    ) +
    scale_color_manual(values = c("black", "red"), guide = F) +
    scale_x_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
    xlab("Penalty") +
    ylab("Mixture") +
    labs(size = "AUC", caption = "Bayesian optimization, 5-folds cross-validation", subtitle = subtitle) +
    ggtitle("Tuning des hyperparamètres") +
    coord_cartesian(clip = "off")
}


# Sauvegarder les graphiques ====================================================================================================
cairo_pdf(here("2_pipeline", "02_calibrate", "tuning.pdf"), onefile = T)
  plot_glmnet_tuning(res_classic, subtitle = "Variables classiques seulement")
  walk2(res, seq_along(res), ~ print(plot_glmnet_tuning(.x, subtitle = glue("Variables classiques et {.y} mois de télématique"))))
dev.off()
