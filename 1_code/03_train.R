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
train_ls <- map(dir_ls(here("2_pipeline", "01_create_data"), regexp = "train"), read_rds)


# S'assurer que les 12 jeux d'entrainement ont le même nombre de lignes =========================================================
id_ls <- map(train_ls, ~ glue("{.$vin}_{.$contract_number}"))
ids <- reduce(id_ls, intersect)
train_ls %<>% map(~ filter(., glue("{vin}_{contract_number}") %in% ids)) 


# Ajout de la variable indicatrice d'une réclamation ou plus ====================================================================
train_ls %<>% map(~ mutate(., claim_ind = factor(as.numeric(nb_claims > 0), levels = c("0", "1")), nb_claims = NULL))


# Enlever la ligne avec exposition de zéro et les 2 lignes avec valeurs manquantes pour years_claim_free ========================
train_ls %<>% map(~ filter(., expo > 0.001, !is.na(years_claim_free)))


# Créer la base de données sans télématique à partir d'une des 12 bases avec télématique ========================================
train_classic <- train_ls[[1]] %>% select(expo:years_licensed, claim_ind)


# Importer les résultats de la calibration ======================================================================================
best_param_ls <- read_rds(here("2_pipeline", "02_calibrate", "best_params.RDS"))
best_param_classic <- read_rds(here("2_pipeline", "02_calibrate", "best_params_classic.RDS"))


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

rec_prep <- prep(rec)

rec_classic <- 
  recipe(claim_ind ~ ., data = train_classic) %>% 
  step_other(all_nominal(), -all_outcomes(), threshold = 0.05) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_YeoJohnson(all_predictors()) %>% 
  step_knnimpute(commute_distance)

rec_classic_prep <- prep(rec_classic)

write_rds(rec, here("2_pipeline", "03_train", "rec.RDS"))
write_rds(rec_classic, here("2_pipeline", "03_train", "rec_classic.RDS"))


# Spécifier les modèles elastic net =============================================================================================
spec_glmnet_ls <- map(best_param_ls, ~ logistic_reg(penalty = .$penalty, mixture = .$mixture) %>% set_engine("glmnet"))
spec_glmnet_classic <- logistic_reg(penalty = best_param_classic$penalty, mixture = best_param_classic$mixture) %>% set_engine("glmnet")


# Définir les workflows =========================================================================================================
make_wf <- function(spec, recipe) {
  workflow() %>% 
    add_model(spec) %>% 
    add_recipe(recipe)
}

wf_ls <- map(spec_glmnet_ls, make_wf, recipe = rec)
wf_classic <- make_wf(spec_glmnet_classic, recipe = rec_classic)


# Créer les 500 échantillons bootstrap pour chacune des 13 bases de données d'entrainement ======================================
n_boot <- 500
boot_id_df <- map_dfc(seq_len(n_boot), ~ sample.int(n = nrow(train_classic), replace = T)) 

create_bootstrap_samples <- function(data) {
  map(seq_len(n_boot), ~ slice(data, boot_id_df[[.]]))
}

train_bootstrap_ls <- map(train_ls, create_bootstrap_samples)
train_bootstrap_classic <- create_bootstrap_samples(train_classic)


# Entrainer les 13 elastic net optimaux sur chacun des 500 échantillons bootstrap ===============================================
get_glmnet_coefs <- function(wf, data) {
  fit(wf, data = data) %>% 
    pull_workflow_fit() %>%
    tidy()
}

res_classic_ls <- map(train_bootstrap_classic, ~ get_glmnet_coefs(wf_classic, data = .))
res_01_ls <- map(train_bootstrap_ls[[1]], ~ get_glmnet_coefs(wf_ls[[1]], data = .))
res_02_ls <- map(train_bootstrap_ls[[2]], ~ get_glmnet_coefs(wf_ls[[2]], data = .))
res_03_ls <- map(train_bootstrap_ls[[3]], ~ get_glmnet_coefs(wf_ls[[3]], data = .))
res_04_ls <- map(train_bootstrap_ls[[4]], ~ get_glmnet_coefs(wf_ls[[4]], data = .))
res_05_ls <- map(train_bootstrap_ls[[5]], ~ get_glmnet_coefs(wf_ls[[5]], data = .))
res_06_ls <- map(train_bootstrap_ls[[6]], ~ get_glmnet_coefs(wf_ls[[6]], data = .))
res_07_ls <- map(train_bootstrap_ls[[7]], ~ get_glmnet_coefs(wf_ls[[7]], data = .))
res_08_ls <- map(train_bootstrap_ls[[8]], ~ get_glmnet_coefs(wf_ls[[8]], data = .))
res_09_ls <- map(train_bootstrap_ls[[9]], ~ get_glmnet_coefs(wf_ls[[9]], data = .))
res_10_ls <- map(train_bootstrap_ls[[10]], ~ get_glmnet_coefs(wf_ls[[10]], data = .))
res_11_ls <- map(train_bootstrap_ls[[11]], ~ get_glmnet_coefs(wf_ls[[11]], data = .))
res_12_ls <- map(train_bootstrap_ls[[12]], ~ get_glmnet_coefs(wf_ls[[12]], data = .))


# Sauvegarder les paramètres ====================================================================================================
write_rds(res_classic_ls, path = here("2_pipeline", "03_train", "res_classic_ls.RDS"))
write_rds(res_01_ls, path = here("2_pipeline", "03_train", "res_01_ls.RDS"))
write_rds(res_02_ls, path = here("2_pipeline", "03_train", "res_02_ls.RDS"))
write_rds(res_03_ls, path = here("2_pipeline", "03_train", "res_03_ls.RDS"))
write_rds(res_04_ls, path = here("2_pipeline", "03_train", "res_04_ls.RDS"))
write_rds(res_05_ls, path = here("2_pipeline", "03_train", "res_05_ls.RDS"))
write_rds(res_06_ls, path = here("2_pipeline", "03_train", "res_06_ls.RDS"))
write_rds(res_07_ls, path = here("2_pipeline", "03_train", "res_07_ls.RDS"))
write_rds(res_08_ls, path = here("2_pipeline", "03_train", "res_08_ls.RDS"))
write_rds(res_09_ls, path = here("2_pipeline", "03_train", "res_09_ls.RDS"))
write_rds(res_10_ls, path = here("2_pipeline", "03_train", "res_10_ls.RDS"))
write_rds(res_11_ls, path = here("2_pipeline", "03_train", "res_11_ls.RDS"))
write_rds(res_12_ls, path = here("2_pipeline", "03_train", "res_12_ls.RDS"))
