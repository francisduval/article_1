# ============================================================================================================================= #
# But:                                                                                                                          #
# Auteur: Francis Duval                                                                                                         #
# Date: Novembre 2020                                                                                                           #
# Input:                                                                                                                        #
# Output:                                                                                                                       #
# ============================================================================================================================= #

source("1_code/00_source.R")
set.seed(2021)


# Importer les glmnet entrainés =================================================================================================
files_train <- dir_ls(here("2_pipeline", "03_train"), regexp = "res")
fit_ls <- map(files_train, read_rds)
names(fit_ls) <- c(glue("res_{1:12}_ls"), "res_classic_ls")


# Importer les ensembles de validation ==========================================================================================
files_valid <- dir_ls(here("2_pipeline", "01_create_data"), regexp = "valid")
valid_ls <- map(files_valid, read_rds)
names(valid_ls) <- c(glue("valid_1_{1:12}"), glue("valid_2a_{1:12}"), glue("valid_2b_{1:12}"))


# Nettoyer les bases de validation ==============================================================================================
valid_ls %<>% map(~ mutate(., claim_ind = factor(as.numeric(nb_claims > 0), levels = c("0", "1")), nb_claims = NULL))
valid_ls %<>% map(~ filter(., !is.na(years_claim_free)))
valid_ls %<>% map(~ filter(., avg_daily_distance != Inf))
valid_ls %<>% map(~ filter(., gender != "Unknown"))


# S'assurer qu'on a les mêmes VINs dans tous les jeux de validation =============================================================
id_ls <- map(valid_ls, ~ .$vin)
ids <- reduce(id_ls, intersect)
valid_ls %<>% map(~ filter(., vin %in% ids)) 


# Définir le pré-traitement =====================================================================================================
rec <- 
  recipe(claim_ind ~ ., data = valid_ls[[1]]) %>% 
  step_rm(vin, contract_number, contract_start_date, contract_end_date) %>% 
  step_other(all_nominal(), -all_outcomes(), threshold = 0.05) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_YeoJohnson(all_predictors()) %>% 
  step_knnimpute(commute_distance)

rec_prep <- prep(rec)

tele_vars <- names(valid_ls[[1]] %>% select(avg_daily_distance:frac_expo_fri_sat))
rec_classic <- 
  recipe(claim_ind ~ ., data = valid_ls[[1]]) %>% 
  step_rm(vin, contract_number, contract_start_date, contract_end_date) %>% 
  step_rm(!!!syms(tele_vars)) %>% 
  step_other(all_nominal(), -all_outcomes(), threshold = 0.05) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_YeoJohnson(all_predictors()) %>% 
  step_knnimpute(commute_distance)

rec_classic_prep <- prep(rec_classic)


# Prétraiter les bases de validation ============================================================================================
pretraiter <- function(data, recipe_prepped) {
  bake(recipe_prepped, new_data = data) %>% 
    select(-claim_ind) %>% 
    mutate(x0 = 1) %>% 
    relocate(x0)
}

valid_ls_baked <- map(valid_ls, pretraiter, recipe_prepped = rec_prep)
valid_classic_1_baked <- pretraiter(valid_ls$valid_1_1, recipe_prepped = rec_classic_prep)
valid_classic_2a_baked <- pretraiter(valid_ls$valid_2a_1, recipe_prepped = rec_classic_prep)
valid_classic_2b_baked <- pretraiter(valid_ls$valid_2b_1, recipe_prepped = rec_classic_prep)


# Fonction pour faire la prédiction logistique ==================================================================================
predict_glmnet <- function(df_baked, df_coefs) {
  score <- as.numeric(as.matrix(df_baked) %*% df_coefs$estimate)
  1 / (1 + exp(-score))
}


# Scorer les modèles pour l'approche A ==========================================================================================
claim_ind_1_vec <- valid_ls$valid_1_1$claim_ind %>% as.character() %>% parse_number()
claim_ind_2a_vec <- valid_ls$valid_2a_1$claim_ind %>% as.character() %>% parse_number()
claim_ind_2b_vec <- valid_ls$valid_2b_1$claim_ind %>% as.character() %>% parse_number()

res_A_df <- tibble(
  # Approche 1
  res_a_1_classic = map_dbl(fit_ls$res_classic_ls, ~ AUC(predict_glmnet(valid_classic_1_baked, .), labels = claim_ind_1_vec)),
  res_a_1_01 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_1, .), labels = claim_ind_1_vec)),
  res_a_1_02 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_2, .), labels = claim_ind_1_vec)),
  res_a_1_03 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_3, .), labels = claim_ind_1_vec)),
  res_a_1_04 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_4, .), labels = claim_ind_1_vec)),
  res_a_1_05 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_5, .), labels = claim_ind_1_vec)),
  res_a_1_06 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_6, .), labels = claim_ind_1_vec)),
  res_a_1_07 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_7, .), labels = claim_ind_1_vec)),
  res_a_1_08 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_8, .), labels = claim_ind_1_vec)),
  res_a_1_09 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_9, .), labels = claim_ind_1_vec)),
  res_a_1_10 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_10, .), labels = claim_ind_1_vec)),
  res_a_1_11 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_11, .), labels = claim_ind_1_vec)),
  res_a_1_12 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_12, .), labels = claim_ind_1_vec)),
  
  # Approche 2a
  res_a_2a_classic = map_dbl(fit_ls$res_classic_ls, ~ AUC(predict_glmnet(valid_classic_2a_baked, .), labels = claim_ind_2a_vec)),
  res_a_2a_01 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_1, .), labels = claim_ind_2a_vec)),
  res_a_2a_02 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_2, .), labels = claim_ind_2a_vec)),
  res_a_2a_03 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_3, .), labels = claim_ind_2a_vec)),
  res_a_2a_04 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_4, .), labels = claim_ind_2a_vec)),
  res_a_2a_05 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_5, .), labels = claim_ind_2a_vec)),
  res_a_2a_06 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_6, .), labels = claim_ind_2a_vec)),
  res_a_2a_07 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_7, .), labels = claim_ind_2a_vec)),
  res_a_2a_08 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_8, .), labels = claim_ind_2a_vec)),
  res_a_2a_09 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_9, .), labels = claim_ind_2a_vec)),
  res_a_2a_10 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_10, .), labels = claim_ind_2a_vec)),
  res_a_2a_11 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_11, .), labels = claim_ind_2a_vec)),
  res_a_2a_12 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_12, .), labels = claim_ind_2a_vec)),
  
  # Approche 2b
  res_a_2b_classic = map_dbl(fit_ls$res_classic_ls, ~ AUC(predict_glmnet(valid_classic_2b_baked, .), labels = claim_ind_2b_vec)),
  res_a_2b_01 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_1, .), labels = claim_ind_2b_vec)),
  res_a_2b_02 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_2, .), labels = claim_ind_2b_vec)),
  res_a_2b_03 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_3, .), labels = claim_ind_2b_vec)),
  res_a_2b_04 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_4, .), labels = claim_ind_2b_vec)),
  res_a_2b_05 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_5, .), labels = claim_ind_2b_vec)),
  res_a_2b_06 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_6, .), labels = claim_ind_2b_vec)),
  res_a_2b_07 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_7, .), labels = claim_ind_2b_vec)),
  res_a_2b_08 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_8, .), labels = claim_ind_2b_vec)),
  res_a_2b_09 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_9, .), labels = claim_ind_2b_vec)),
  res_a_2b_10 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_10, .), labels = claim_ind_2b_vec)),
  res_a_2b_11 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_11, .), labels = claim_ind_2b_vec)),
  res_a_2b_12 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_12, .), labels = claim_ind_2b_vec))
)


# Graphiques pour l'approche A ==================================================================================================
res_A_df %>% 
select(res_a_1_classic:res_a_1_12) %>% 
pivot_longer(cols = everything()) %>% 
mutate(nb_months = factor(rep(0:12, 500))) %>% 
  ggplot(aes(x = nb_months, y = value)) +
  geom_boxplot(color = "blue", fill = "blue", alpha = 0.3) +
  ggtitle("Performance des régressions logistiques pénalisées") +
  labs(subtitle = glue("Approche entrainement: A\n Approche validation: 1 (télématique année 1 pour prédire année 2)")) +
  xlab("Nombre de mois de télématique utilisés pour l'ensemble de validation") +
  ylab("AUC sur l'ensemble de validation")

res_A_df %>% 
  select(res_a_2a_classic:res_a_2a_12) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(nb_months = factor(rep(0:12, 500))) %>% 
    ggplot(aes(x = nb_months, y = value)) +
    geom_boxplot(color = "blue", fill = "blue", alpha = 0.3) +
    ggtitle("Performance des régressions logistiques pénalisées") +
    labs(subtitle = glue("Approche entrainement: A\n Approche validation: 2a (télématique année 1 pour prédire année 1)")) +
    xlab("Nombre de mois de télématique utilisés pour l'ensemble de validation") +
    ylab("AUC sur l'ensemble de validation")

res_A_df %>% 
  select(res_a_2b_classic:res_a_2b_12) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(nb_months = factor(rep(0:12, 500))) %>% 
    ggplot(aes(x = nb_months, y = value)) +
    geom_boxplot(color = "blue", fill = "blue", alpha = 0.3) +
    ggtitle("Performance des régressions logistiques pénalisées") +
    labs(subtitle = glue("Approche entrainement: A\n Approche validation: 2b ((télématique année 2 pour prédire année 2))")) +
    xlab("Nombre de mois de télématique utilisés pour l'ensemble de validation") +
    ylab("AUC sur l'ensemble de validation")
    

# Scorer les modèles pour l'approche B ==========================================================================================
res_B_df <- tibble(
  # Approche 1
  res_a_1_classic = map_dbl(fit_ls$res_classic_ls, ~ AUC(predict_glmnet(valid_classic_1_baked, .), labels = claim_ind_1_vec)),
  res_a_1_01 = map_dbl(fit_ls$res_1_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_1, .), labels = claim_ind_1_vec)),
  res_a_1_02 = map_dbl(fit_ls$res_2_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_2, .), labels = claim_ind_1_vec)),
  res_a_1_03 = map_dbl(fit_ls$res_3_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_3, .), labels = claim_ind_1_vec)),
  res_a_1_04 = map_dbl(fit_ls$res_4_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_4, .), labels = claim_ind_1_vec)),
  res_a_1_05 = map_dbl(fit_ls$res_5_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_5, .), labels = claim_ind_1_vec)),
  res_a_1_06 = map_dbl(fit_ls$res_6_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_6, .), labels = claim_ind_1_vec)),
  res_a_1_07 = map_dbl(fit_ls$res_7_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_7, .), labels = claim_ind_1_vec)),
  res_a_1_08 = map_dbl(fit_ls$res_8_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_8, .), labels = claim_ind_1_vec)),
  res_a_1_09 = map_dbl(fit_ls$res_9_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_9, .), labels = claim_ind_1_vec)),
  res_a_1_10 = map_dbl(fit_ls$res_10_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_10, .), labels = claim_ind_1_vec)),
  res_a_1_11 = map_dbl(fit_ls$res_11_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_11, .), labels = claim_ind_1_vec)),
  res_a_1_12 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_1_12, .), labels = claim_ind_1_vec)),
  
  # Approche 2a
  res_a_2a_classic = map_dbl(fit_ls$res_classic_ls, ~ AUC(predict_glmnet(valid_classic_2a_baked, .), labels = claim_ind_2a_vec)),
  res_a_2a_01 = map_dbl(fit_ls$res_1_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_1, .), labels = claim_ind_2a_vec)),
  res_a_2a_02 = map_dbl(fit_ls$res_2_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_2, .), labels = claim_ind_2a_vec)),
  res_a_2a_03 = map_dbl(fit_ls$res_3_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_3, .), labels = claim_ind_2a_vec)),
  res_a_2a_04 = map_dbl(fit_ls$res_4_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_4, .), labels = claim_ind_2a_vec)),
  res_a_2a_05 = map_dbl(fit_ls$res_5_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_5, .), labels = claim_ind_2a_vec)),
  res_a_2a_06 = map_dbl(fit_ls$res_6_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_6, .), labels = claim_ind_2a_vec)),
  res_a_2a_07 = map_dbl(fit_ls$res_7_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_7, .), labels = claim_ind_2a_vec)),
  res_a_2a_08 = map_dbl(fit_ls$res_8_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_8, .), labels = claim_ind_2a_vec)),
  res_a_2a_09 = map_dbl(fit_ls$res_9_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_9, .), labels = claim_ind_2a_vec)),
  res_a_2a_10 = map_dbl(fit_ls$res_10_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_10, .), labels = claim_ind_2a_vec)),
  res_a_2a_11 = map_dbl(fit_ls$res_11_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_11, .), labels = claim_ind_2a_vec)),
  res_a_2a_12 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2a_12, .), labels = claim_ind_2a_vec)),
  
  # Approche 2b
  res_a_2b_classic = map_dbl(fit_ls$res_classic_ls, ~ AUC(predict_glmnet(valid_classic_2b_baked, .), labels = claim_ind_2b_vec)),
  res_a_2b_01 = map_dbl(fit_ls$res_1_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_1, .), labels = claim_ind_2b_vec)),
  res_a_2b_02 = map_dbl(fit_ls$res_2_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_2, .), labels = claim_ind_2b_vec)),
  res_a_2b_03 = map_dbl(fit_ls$res_3_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_3, .), labels = claim_ind_2b_vec)),
  res_a_2b_04 = map_dbl(fit_ls$res_4_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_4, .), labels = claim_ind_2b_vec)),
  res_a_2b_05 = map_dbl(fit_ls$res_5_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_5, .), labels = claim_ind_2b_vec)),
  res_a_2b_06 = map_dbl(fit_ls$res_6_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_6, .), labels = claim_ind_2b_vec)),
  res_a_2b_07 = map_dbl(fit_ls$res_7_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_7, .), labels = claim_ind_2b_vec)),
  res_a_2b_08 = map_dbl(fit_ls$res_8_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_8, .), labels = claim_ind_2b_vec)),
  res_a_2b_09 = map_dbl(fit_ls$res_9_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_9, .), labels = claim_ind_2b_vec)),
  res_a_2b_10 = map_dbl(fit_ls$res_10_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_10, .), labels = claim_ind_2b_vec)),
  res_a_2b_11 = map_dbl(fit_ls$res_11_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_11, .), labels = claim_ind_2b_vec)),
  res_a_2b_12 = map_dbl(fit_ls$res_12_ls, ~ AUC(predict_glmnet(valid_ls_baked$valid_2b_12, .), labels = claim_ind_2b_vec))
)


# Graphiques pour l'approche B ==================================================================================================
res_B_df %>% 
  select(res_a_1_classic:res_a_1_12) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(nb_months = factor(rep(0:12, 500))) %>% 
  ggplot(aes(x = nb_months, y = value)) +
  geom_boxplot(color = "blue", fill = "blue", alpha = 0.3) +
  ggtitle("Performance des régressions logistiques pénalisées") +
  labs(subtitle = glue("Approche entrainement: B\n Approche validation: 1 (télématique année 1 pour prédire année 2)")) +
  xlab("Nombre de mois de télématique utilisés pour l'ensemble de validation") +
  ylab("AUC sur l'ensemble de validation")

res_B_df %>% 
  select(res_a_2a_classic:res_a_2a_12) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(nb_months = factor(rep(0:12, 500))) %>% 
  ggplot(aes(x = nb_months, y = value)) +
  geom_boxplot(color = "blue", fill = "blue", alpha = 0.3) +
  ggtitle("Performance des régressions logistiques pénalisées") +
  labs(subtitle = glue("Approche entrainement: B\n Approche validation: 2a (télématique année 1 pour prédire année 1)")) +
  xlab("Nombre de mois de télématique utilisés pour l'ensemble de validation") +
  ylab("AUC sur l'ensemble de validation")

res_B_df %>% 
  select(res_a_2b_classic:res_a_2b_12) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(nb_months = factor(rep(0:12, 500))) %>% 
  ggplot(aes(x = nb_months, y = value)) +
  geom_boxplot(color = "blue", fill = "blue", alpha = 0.3) +
  ggtitle("Performance des régressions logistiques pénalisées") +
  labs(subtitle = glue("Approche entrainement: B\n Approche validation: 2b (télématique année 2 pour prédire année 2)")) +
  xlab("Nombre de mois de télématique utilisés pour l'ensemble de validation") +
  ylab("AUC sur l'ensemble de validation")
