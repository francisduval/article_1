# ============================================================================================================================= #
# But:                                                                                                                          #
# Auteur: Francis Duval                                                                                                         #
# Date: Novembre 2020                                                                                                           #
# Input:                                                                                                                        #
# Output:                                                                                                                       #
# ============================================================================================================================= #

source("1_code/00_source.R")
set.seed(2021)


# Importer les jeux de données d'entrainement ===================================================================================
train_ls <- read_rds(here("2_pipeline", "01_create_data", "train_ls.RDS"))


# Créer la base de données sans télématique à partir d'une des 12 bases avec télématique ========================================
train_classic <- train_ls[[1]] %>% select(-starts_with("t_"))


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


# Créer les 500 échantillons bootstrap pour chacune des 13 bases de données d'entrainement ======================================
n_boot <- 500
boot_id_df <- map_dfc(seq_len(n_boot), ~ sample.int(n = nrow(train_classic), replace = T)) 

create_bootstrap_samples <- function(data) {
  map(seq_len(n_boot), ~ slice(data, boot_id_df[[.]]))
}

train_bootstrap_ls <- map(train_ls, create_bootstrap_samples)
train_bootstrap_classic <- create_bootstrap_samples(train_classic)


# Entrainer les 13 LASSO optimaux sur chacun des 500 échantillons bootstrap =====================================================
get_glmnet_coefs <- function(wf, data) {
  fit(wf, data = data) %>% 
    pull_workflow_fit() %>%
    tidy()
}

res_boot_classic_ls <- map(train_bootstrap_classic, ~ get_glmnet_coefs(wf_classic, data = .))
res_boot_01_ls <- map(train_bootstrap_ls[[1]], ~ get_glmnet_coefs(wf_ls[[1]], data = .))
res_boot_02_ls <- map(train_bootstrap_ls[[2]], ~ get_glmnet_coefs(wf_ls[[2]], data = .))
res_boot_03_ls <- map(train_bootstrap_ls[[3]], ~ get_glmnet_coefs(wf_ls[[3]], data = .))
res_boot_04_ls <- map(train_bootstrap_ls[[4]], ~ get_glmnet_coefs(wf_ls[[4]], data = .))
res_boot_05_ls <- map(train_bootstrap_ls[[5]], ~ get_glmnet_coefs(wf_ls[[5]], data = .))
res_boot_06_ls <- map(train_bootstrap_ls[[6]], ~ get_glmnet_coefs(wf_ls[[6]], data = .))
res_boot_07_ls <- map(train_bootstrap_ls[[7]], ~ get_glmnet_coefs(wf_ls[[7]], data = .))
res_boot_08_ls <- map(train_bootstrap_ls[[8]], ~ get_glmnet_coefs(wf_ls[[8]], data = .))
res_boot_09_ls <- map(train_bootstrap_ls[[9]], ~ get_glmnet_coefs(wf_ls[[9]], data = .))
res_boot_10_ls <- map(train_bootstrap_ls[[10]], ~ get_glmnet_coefs(wf_ls[[10]], data = .))
res_boot_11_ls <- map(train_bootstrap_ls[[11]], ~ get_glmnet_coefs(wf_ls[[11]], data = .))
res_boot_12_ls <- map(train_bootstrap_ls[[12]], ~ get_glmnet_coefs(wf_ls[[12]], data = .))


# Sauvegarder les paramètres pour les 500 échantillons bootstrap et pour chacun des 13 modèles ==================================
write_rds(res_boot_classic_ls, path = here("2_pipeline", "04_train_bootstrap", "res_boot_classic_ls.RDS"))
write_rds(res_boot_01_ls, path = here("2_pipeline", "04_train_bootstrap", "res_boot_01_ls.RDS"))
write_rds(res_boot_02_ls, path = here("2_pipeline", "04_train_bootstrap", "res_boot_02_ls.RDS"))
write_rds(res_boot_03_ls, path = here("2_pipeline", "04_train_bootstrap", "res_boot_03_ls.RDS"))
write_rds(res_boot_04_ls, path = here("2_pipeline", "04_train_bootstrap", "res_boot_04_ls.RDS"))
write_rds(res_boot_05_ls, path = here("2_pipeline", "04_train_bootstrap", "res_boot_05_ls.RDS"))
write_rds(res_boot_06_ls, path = here("2_pipeline", "04_train_bootstrap", "res_boot_06_ls.RDS"))
write_rds(res_boot_07_ls, path = here("2_pipeline", "04_train_bootstrap", "res_boot_07_ls.RDS"))
write_rds(res_boot_08_ls, path = here("2_pipeline", "04_train_bootstrap", "res_boot_08_ls.RDS"))
write_rds(res_boot_09_ls, path = here("2_pipeline", "04_train_bootstrap", "res_boot_09_ls.RDS"))
write_rds(res_boot_10_ls, path = here("2_pipeline", "04_train_bootstrap", "res_boot_10_ls.RDS"))
write_rds(res_boot_11_ls, path = here("2_pipeline", "04_train_bootstrap", "res_boot_11_ls.RDS"))
write_rds(res_boot_12_ls, path = here("2_pipeline", "04_train_bootstrap", "res_boot_12_ls.RDS"))
