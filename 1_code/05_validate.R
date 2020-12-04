# ============================================================================================================================= #
# But:                                                                                                                          #
# Auteur: Francis Duval                                                                                                         #
# Date: Décembre 2020                                                                                                           #
# Input:                                                                                                                        #
# Output:                                                                                                                       #
# ============================================================================================================================= #

source("1_code/00_source.R")


# Importer les jeux de données d'entrainement et de validation ==================================================================
train_ls <- read_rds(here("2_pipeline", "01_create_data", "train_ls.RDS"))
test_ls <- read_rds(here("2_pipeline", "01_create_data", "test_ls.RDS"))


# Créer les jeux d'entrainement et de validation ================================================================================
train_classic <- train_ls[[1]] %>% select(starts_with("c_"), claim_ind, -c_expo)
test_classic <- test_ls[[1]] %>% select(starts_with("c_"), claim_ind, -c_expo)

train_ls %<>% map(select, starts_with("c_"), starts_with("t_"), claim_ind, -c_expo)
test_ls %<>% map(select, starts_with("c_"), starts_with("t_"), claim_ind, -c_expo)


# Importer les résultats de la calibration ======================================================================================
res_ls <- read_rds(here("2_pipeline", "03_calibrate", "res.RDS"))
res_classic <- read_rds(here("2_pipeline", "03_calibrate", "res_classic.RDS"))


# Lambda optimal pour chacun des modèles ========================================================================================
best_ls <- map_dbl(res_ls, ~ select_best(., metric = "roc_auc")$penalty)
best_classic <- select_best(res_classic, metric = "roc_auc")$penalty


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
define_wf <- function(recipe, penalty) {
  spec <- 
    logistic_reg(
      penalty = penalty,
      mixture = 1
    ) %>% 
    set_engine("glmnet")
  
  workflow() %>% 
    add_model(spec) %>% 
    add_recipe(recipe)
}

wf_ls <- map(best_ls, ~ define_wf(rec, penalty = .))
wf_classic <- define_wf(rec_classic, penalty = best_classic) 


# Entrainer les 13 modèles sur les ensembles d'entrainement correspondants ======================================================
fit_ls <- map2(wf_ls, train_ls, ~ fit(.x, data = .y))
fit_classic <- fit(wf_classic, data = train_classic)


# Créer les 500 échantillons bootstrap pour chacune des 13 bases de données test ================================================
n_boot <- 500
set.seed(1994)
boot <- bootstraps(test_classic, times = n_boot, strata = claim_ind)

boot_id_df <- 
  map_dfc(1:n_boot, ~ boot$splits[[.]]$in_id) %>% 
  mutate(original = seq(1, nrow(.)))

# write_rds(boot_id_df, path = here("2_pipeline", "05_validate", "boot_id_df.RDS"))


# Obtenir les prédictions pour chacun des 13 modèles et pour chacun des 500 échantillons bootstrap ==============================
get_pred_df <- function(fit, new_data) {
  map_dfc(boot_id_df, ~ predict(fit, new_data = slice(new_data, .), type = "prob") %>% pull(.pred_1))
}

pred_df_classic <- get_pred_df(fit_classic, new_data = test_classic)
pred_df_01 <- get_pred_df(fit_ls[[1]], new_data = test_ls[[1]])
pred_df_02 <- get_pred_df(fit_ls[[2]], new_data = test_ls[[2]])
pred_df_03 <- get_pred_df(fit_ls[[3]], new_data = test_ls[[3]])
pred_df_04 <- get_pred_df(fit_ls[[4]], new_data = test_ls[[4]])
pred_df_05 <- get_pred_df(fit_ls[[5]], new_data = test_ls[[5]])
pred_df_06 <- get_pred_df(fit_ls[[6]], new_data = test_ls[[6]])
pred_df_07 <- get_pred_df(fit_ls[[7]], new_data = test_ls[[7]])
pred_df_08 <- get_pred_df(fit_ls[[8]], new_data = test_ls[[8]])
pred_df_09 <- get_pred_df(fit_ls[[9]], new_data = test_ls[[9]])
pred_df_10 <- get_pred_df(fit_ls[[10]], new_data = test_ls[[10]])
pred_df_11 <- get_pred_df(fit_ls[[11]], new_data = test_ls[[11]])
pred_df_12 <- get_pred_df(fit_ls[[12]], new_data = test_ls[[12]])


# Sauvegarder ===================================================================================================================
# write_rds(pred_df_classic, path = here("2_pipeline", "05_validate", "pred_df_classic.RDS"))
# write_rds(pred_df_01, path = here("2_pipeline", "05_validate", "pred_df_01.RDS"))
# write_rds(pred_df_02, path = here("2_pipeline", "05_validate", "pred_df_02.RDS"))
# write_rds(pred_df_03, path = here("2_pipeline", "05_validate", "pred_df_03.RDS"))
# write_rds(pred_df_04, path = here("2_pipeline", "05_validate", "pred_df_04.RDS"))
# write_rds(pred_df_05, path = here("2_pipeline", "05_validate", "pred_df_05.RDS"))
# write_rds(pred_df_06, path = here("2_pipeline", "05_validate", "pred_df_06.RDS"))
# write_rds(pred_df_07, path = here("2_pipeline", "05_validate", "pred_df_07.RDS"))
# write_rds(pred_df_08, path = here("2_pipeline", "05_validate", "pred_df_08.RDS"))
# write_rds(pred_df_09, path = here("2_pipeline", "05_validate", "pred_df_09.RDS"))
# write_rds(pred_df_10, path = here("2_pipeline", "05_validate", "pred_df_10.RDS"))
# write_rds(pred_df_11, path = here("2_pipeline", "05_validate", "pred_df_11.RDS"))
# write_rds(pred_df_12, path = here("2_pipeline", "05_validate", "pred_df_12.RDS"))

# Calculer les AUC ==============================================================================================================
y_vec_df <- map_dfc(boot_id_df, ~ slice(test_classic, .) %>% pull(claim_ind) %>% as.character %>% parse_number)

df <- tibble(
  auc_vec_classic = map2_dbl(pred_df_classic, y_vec_df, ~ AUC(.x, labels = .y)),
  auc_vec_01 = map2_dbl(pred_df_01, y_vec_df, ~ AUC(.x, labels = .y)),
  auc_vec_02 = map2_dbl(pred_df_02, y_vec_df, ~ AUC(.x, labels = .y)),
  auc_vec_03 = map2_dbl(pred_df_03, y_vec_df, ~ AUC(.x, labels = .y)),
  auc_vec_04 = map2_dbl(pred_df_04, y_vec_df, ~ AUC(.x, labels = .y)),
  auc_vec_05 = map2_dbl(pred_df_05, y_vec_df, ~ AUC(.x, labels = .y)),
  auc_vec_06 = map2_dbl(pred_df_06, y_vec_df, ~ AUC(.x, labels = .y)),
  auc_vec_07 = map2_dbl(pred_df_07, y_vec_df, ~ AUC(.x, labels = .y)),
  auc_vec_08 = map2_dbl(pred_df_08, y_vec_df, ~ AUC(.x, labels = .y)),
  auc_vec_09 = map2_dbl(pred_df_09, y_vec_df, ~ AUC(.x, labels = .y)),
  auc_vec_10 = map2_dbl(pred_df_10, y_vec_df, ~ AUC(.x, labels = .y)),
  auc_vec_11 = map2_dbl(pred_df_11, y_vec_df, ~ AUC(.x, labels = .y)),
  auc_vec_12 = map2_dbl(pred_df_12, y_vec_df, ~ AUC(.x, labels = .y))
)

auc_original_df <- 
  df %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(nb_months = factor(rep(0:12, 501))) %>% 
  slice_tail(n = 13)
  

# Boxplots évolution de mois en mois ============================================================================================
df %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(nb_months = factor(rep(0:12, 501))) %>% 
  ggplot(aes(x = nb_months, y = value)) +
  geom_boxplot(color = "#00B32C", fill = "#1FD537", alpha = 0.3) +
  geom_point(aes(x = nb_months, y = value), data = auc_original_df, col = "#B3000C") +
  ggtitle("Distribution de l'AUC pour la régression logistique LASSO") +
  labs(subtitle = "Entrainement sur l'ensemble d'entrainement original, validation sur 500 échantillons bootstrap") +
  xlab("Nombre de mois de télématique utilisés") +
  ylab("AUC")
