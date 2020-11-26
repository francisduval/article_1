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


# Entrainer les 13 LASSO optimaux sur les 13 jeux d'entrainement ================================================================
get_glmnet_coefs <- function(wf, data) {
  fit(wf, data = data) %>% 
    pull_workflow_fit() %>%
    tidy()
}

wf_total_ls <- prepend(wf_ls, list(wf_classic))
train_total_ls <- prepend(train_ls, list(train_classic))

coefs_ls <- map2(wf_total_ls, train_total_ls, ~ get_glmnet_coefs(.x, data = .y))

coefs_df <- 
  coefs_ls %>% 
  map(select, -penalty) %>% 
  map(slice, -1) %>% 
  reduce(full_join, by = "term") %>% 
  set_names(c("variable", glue("model_{0:12}")))

coefs_df %>% 
  pivot_longer(cols = -variable) %>% 
  mutate(abs_value = abs(value), value = NULL, name = parse_number(name)) %>% 
  mutate(var_type = str_detect(variable, pattern = "^c_")) %>% 
  replace_na(list(abs_value = 0)) %>%
  filter(var_type == T) %>% 
  ggplot(aes(x = name, y = abs_value, col = variable)) +
  geom_point(size = 1.5) +
  geom_line() +
  labs(col = NULL) +
  ggtitle("LASSO optimaux entrainés sur l'ensemble d'entrainement") +
  xlab("Nombre de mois de télématique utilisés") +
  ylab("Valeur absolue des paramètres") +
  scale_x_continuous(breaks = 0:12)

coefs_df %>% 
  pivot_longer(cols = -variable) %>% 
  mutate(abs_value = abs(value), value = NULL, name = parse_number(name)) %>% 
  mutate(var_type = str_detect(variable, pattern = "^c_")) %>% 
  replace_na(list(abs_value = 0)) %>%
  filter(var_type == F) %>% 
  ggplot(aes(x = name, y = abs_value, col = variable)) +
  geom_point(size = 1.5) +
  geom_line() +
  labs(col = NULL) +
  ggtitle("LASSO optimaux entrainés sur l'ensemble d'entrainement") +
  xlab("Nombre de mois de télématique utilisés") +
  ylab("Valeur absolue des paramètres") +
  scale_x_continuous(breaks = 0:12)

coefs_df %>% 
  pivot_longer(cols = -variable) %>% 
  mutate(abs_value = abs(value), value = NULL, name = parse_number(name)) %>% 
  mutate(var_type = str_detect(variable, pattern = "^c_")) %>% 
  replace_na(list(abs_value = 0)) %>%
  filter(var_type == T) %>% 
  ggplot(aes(x = name, y = abs_value)) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free") +
  labs(col = NULL) +
  ggtitle("LASSO optimaux entrainés sur l'ensemble d'entrainement") +
  xlab("Nombre de mois de télématique utilisés") +
  ylab("Valeur absolue des paramètres") +
  scale_x_continuous(breaks = 0:12)

coefs_df %>% 
  pivot_longer(cols = -variable) %>% 
  mutate(abs_value = abs(value), value = NULL, name = parse_number(name)) %>% 
  mutate(var_type = str_detect(variable, pattern = "^c_")) %>% 
  replace_na(list(abs_value = 0)) %>%
  filter(var_type == F) %>% 
  ggplot(aes(x = name, y = abs_value)) +
  geom_line() +
  facet_wrap(vars(variable)) +
  labs(col = NULL) +
  ggtitle("LASSO optimaux entrainés sur l'ensemble d'entrainement") +
  xlab("Nombre de mois de télématique utilisés") +
  ylab("Valeur absolue des paramètres") +
  scale_x_continuous(breaks = 0:12)
