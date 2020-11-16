# ============================================================================================================================= #
# But:                                                                                                                          #
# Auteur: Francis Duval                                                                                                         #
# Date: Novembre 2020                                                                                                           #
# Input:                                                                                                                        #
# Output:                                                                                                                       #
# ============================================================================================================================= #

source("1_code/00_source.R")


# Importer les jeux de données ==================================================================================================
train <- read_rds(here("2_pipeline", "01_create_data", "train_12.RDS"))
valid <- read_rds(here("2_pipeline", "01_create_data", "valid_1_12.RDS"))


sum(train$nb_claims) / sum(train$expo, na.rm = T) # 0.1193823
sum(valid$nb_claims) / sum(valid$expo, na.rm = T) # 0.08832751

mean(train$nb_claims) # 0.09065785
mean(valid$nb_claims) # 0.08780037




