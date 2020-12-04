# ============================================================================================================================= #
# But:                                                                                                                          #
# Auteur: Francis Duval                                                                                                         #
# Date: Novembre 2020                                                                                                           #
# Input:                                                                                                                        #
# Output:                                                                                                                       #
# ============================================================================================================================= #

source("1_code/00_source.R")


# Importer les jeux de données d'entrainement ===================================================================================
train_ls <- read_rds(here("2_pipeline", "01_create_data", "train_ls.RDS"))


# Créer les jeux d'entrainement =================================================================================================
train_classic <- train_ls[[1]] %>% select(starts_with("c_"), claim_ind, -c_expo)
train_ls %<>% map(select, starts_with("c_"), starts_with("t_"), claim_ind, -c_expo) 


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
set.seed(1994)
boot <- bootstraps(train_classic, times = n_boot, strata = claim_ind)

boot_id_df <- 
  map_dfc(1:n_boot, ~ boot$splits[[.]]$in_id) %>% 
  mutate(original = seq(1, nrow(.)))


# Fonction pour obtenir les coefficients d'une régression logistique LASSO ======================================================
get_glmnet_coefs <- function(wf, data) {
  coefs <- 
    fit(wf, data = data) %>% 
    pull_workflow_fit() %>%
    tidy()
  
  i <<- i + 1
  setTxtProgressBar(pb, i)
  return(coefs)
}


# Entrainer les 13 LASSO optimaux sur chacun des 500 échantillons bootstrap =====================================================
pb <- txtProgressBar(min = 0, max = (n_boot + 1) * 13, style = 3)
i <- 0
res_boot_classic_ls <- map(boot_id_df, ~ get_glmnet_coefs(wf_classic, data = train_classic %>% slice(.)))
res_boot_01_ls <- map(boot_id_df, ~ get_glmnet_coefs(wf_ls[[1]], data = train_ls[[1]] %>% slice(.)))
res_boot_02_ls <- map(boot_id_df, ~ get_glmnet_coefs(wf_ls[[2]], data = train_ls[[2]] %>% slice(.)))
res_boot_03_ls <- map(boot_id_df, ~ get_glmnet_coefs(wf_ls[[3]], data = train_ls[[3]] %>% slice(.)))
res_boot_04_ls <- map(boot_id_df, ~ get_glmnet_coefs(wf_ls[[4]], data = train_ls[[4]] %>% slice(.)))
res_boot_05_ls <- map(boot_id_df, ~ get_glmnet_coefs(wf_ls[[5]], data = train_ls[[5]] %>% slice(.)))
res_boot_06_ls <- map(boot_id_df, ~ get_glmnet_coefs(wf_ls[[6]], data = train_ls[[6]] %>% slice(.)))
res_boot_07_ls <- map(boot_id_df, ~ get_glmnet_coefs(wf_ls[[7]], data = train_ls[[7]] %>% slice(.)))
res_boot_08_ls <- map(boot_id_df, ~ get_glmnet_coefs(wf_ls[[8]], data = train_ls[[8]] %>% slice(.)))
res_boot_09_ls <- map(boot_id_df, ~ get_glmnet_coefs(wf_ls[[9]], data = train_ls[[9]] %>% slice(.)))
res_boot_10_ls <- map(boot_id_df, ~ get_glmnet_coefs(wf_ls[[10]], data = train_ls[[10]] %>% slice(.)))
res_boot_11_ls <- map(boot_id_df, ~ get_glmnet_coefs(wf_ls[[11]], data = train_ls[[11]] %>% slice(.)))
res_boot_12_ls <- map(boot_id_df, ~ get_glmnet_coefs(wf_ls[[12]], data = train_ls[[12]] %>% slice(.)))
close(pb)


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
