# ============================================================================================================================= #
# But: Créer les 12 jeux de données, les 12 baes d'entrainement et les 12 bases de validation                                   #
# Auteur: Francis Duval                                                                                                         #
# Date: Novembre 2020                                                                                                           #
# Input: ContractAuto.csv, TRIP_VIN1.csv:TRIP_VIN268.csv                                                                        #
# Output: jeu_ls.RDS, train_ls.RDS, test_ls.RDS                                                                                 #
# ============================================================================================================================= #

source("1_code/00_source.R")
options(readr.show_progress = F)


# Importer le jeu de données ContractAuto =======================================================================================
contracts_import <- read_csv(here("0_data", "Contrat_Nov2020.csv"), col_types = cols(.default = "c"))


# Nettoyer la base de données par contrat =======================================================================================
contracts <- 
  contracts_import %>% 
  transmute(
    vin                    = VIN,
    policy_id              = POLICY_ID_M,
    contract_start_date    = as.Date(parse_date_time2(POLEFFDATE_M, "%d%b%Y!")),
    contract_end_date      = as.Date(parse_date_time2(POLEXPDATE_M, "%d%b%Y!")),
    expo_2                 = as.numeric(expo_Col),
    expo_3                 = as.numeric(expo_Comp),
    expo_4                 = as.numeric(expo_DCPD),
    expo_5                 = as.numeric(expo_Liab),
    annual_distance        = as.integer(ANNUALMILEAGE),
    commute_distance       = as.integer(COMMUTEDISTANCE),
    conv_count_3_yrs_minor = as.integer(CONVCOUNTMINOR3YRS),
    gender                 = factor(DRIVER_GENDER),
    marital_status         = factor(DRIVER_MARITALSTATUS),
    pmt_plan               = factor(PAYMENTPLAN),
    veh_age                = as.integer(VEHICLEAGE),
    veh_use                = factor(VEHICLEUSE),
    years_claim_free       = as.integer(DRIVER_YEARSCLAIMFREE),
    years_licensed         = as.integer(DRIVER_YEARSLICENSED),
    first_claim_date       = as.Date(parse_date_time2(dateofloss1, "%d%b%Y:%H:%M:%OS")),
    second_claim_date      = as.Date(parse_date_time2(dateofloss2, "%d%b%Y:%H:%M:%OS")),
    third_claim_date       = as.Date(parse_date_time2(dateofloss3, "%d%b%Y:%H:%M:%OS")),
    fourth_claim_date      = as.Date(parse_date_time2(dateofloss4, "%d%b%Y:%H:%M:%OS")),
    first_claim_id         = claimid1,
    second_claim_id        = claimid2,
    third_claim_id         = claimid3,
    fourth_claim_id        = claimid4,
    first_claim_cov_1      = as.numeric(FirstClaim_cov1),
    first_claim_cov_2      = as.numeric(FirstClaim_cov2),
    first_claim_cov_3      = as.numeric(FirstClaim_cov3),
    first_claim_cov_4      = as.numeric(FirstClaim_cov4),
    second_claim_cov_1     = as.numeric(SecClaim_cov1),
    second_claim_cov_2     = as.numeric(SecClaim_cov2),
    second_claim_cov_3     = as.numeric(SecClaim_cov3),
    second_claim_cov_4     = as.numeric(SecClaim_cov4),
    third_claim_cov_1      = as.numeric(ThirdClaim_cov1),
    third_claim_cov_2      = as.numeric(ThirdClaim_cov2),
    third_claim_cov_3      = as.numeric(ThirdClaim_cov3),
    third_claim_cov_4      = as.numeric(ThirdClaim_cov4),
    fourth_claim_cov_1     = as.numeric(FourthClaim_cov1),
    fourth_claim_cov_2     = as.numeric(FourthClaim_cov2),
    fourth_claim_cov_3     = as.numeric(FourthClaim_cov3),
    fourth_claim_cov_4     = as.numeric(FourthClaim_cov4),
    first_claim_cost_1     = as.numeric(FirstClaim_loss1),
    first_claim_cost_2     = as.numeric(FirstClaim_loss2),
    first_claim_cost_3     = as.numeric(FirstClaim_loss3),
    first_claim_cost_4     = as.numeric(FirstClaim_loss4),
    second_claim_cost_1    = as.numeric(SecClaim_loss1),
    second_claim_cost_2    = as.numeric(SecClaim_loss2),
    second_claim_cost_3    = as.numeric(SecClaim_loss3),
    second_claim_cost_4    = as.numeric(SecClaim_loss4),
    third_claim_cost_1     = as.numeric(ThirdClaim_loss1),
    third_claim_cost_2     = as.numeric(ThirdClaim_loss2),
    third_claim_cost_3     = as.numeric(ThirdClaim_loss3),
    third_claim_cost_4     = as.numeric(ThirdClaim_loss4),
    fourth_claim_cost_1    = as.numeric(FourthClaim_loss1),
    fourth_claim_cost_2    = as.numeric(FourthClaim_loss2),
    fourth_claim_cost_3    = as.numeric(FourthClaim_loss3),
    fourth_claim_cost_4    = as.numeric(FourthClaim_loss4)
  ) %>% 
  arrange(vin, contract_start_date) %>% 
  filter_at(vars(starts_with("expo_")), all_vars(near(., expo_2, tol = 0.05))) %>%
  rename(expo = expo_2) %>% 
  select(-starts_with("expo_")) %>% 
  filter(expo > 0.95 & expo < 1.05) %>% 
  group_by(vin) %>% 
  slice(1) %>% 
  ungroup()
  

# Créer la base de données par réclamation ======================================================================================
# Garder seulement les contrats (lignes) avec au moins une réclamation
contracts_with_claim <- 
  contracts %>% 
  filter(!is.na(first_claim_id))


# Transformer la base de données par contrats de manière à avoir une ligne par réclamation
claims_first <- 
  contracts_with_claim %>% 
  select(
    vin, 
    policy_id,
    contract_start_date, 
    contract_end_date,
    contains("first")
  ) %>% 
  rename_all(~ str_replace_all(., "first_", ""))

# ----------

claims_second <- 
  contracts_with_claim %>% 
  select(
    vin, 
    policy_id,
    contract_start_date, 
    contract_end_date,
    contains("second")
  ) %>% 
  filter(!is.na(second_claim_id)) %>% 
  rename_all(~ str_replace_all(., "second_", ""))

# ----------

claims_third <- 
  contracts_with_claim %>% 
  select(
    vin, 
    policy_id,
    contract_start_date, 
    contract_end_date,
    contains("third")
  ) %>% 
  filter(!is.na(third_claim_id)) %>% 
  rename_all(~ str_replace_all(., "third_", ""))

# ----------

claims_fourth <- 
  contracts_with_claim %>% 
  select(
    vin, 
    policy_id,
    contract_start_date, 
    contract_end_date,
    contains("fourth")
  ) %>% 
  filter(!is.na(fourth_claim_id)) %>% 
  rename_all(~ str_replace_all(., "fourth_", ""))

# ----------

claims <- bind_rows(claims_first, claims_second, claims_third, claims_fourth)


# Créer une base de données ayant une ligne par couverture
cov_vec <- 
  claims %>%
  mutate_at(vars(claim_cov_1:claim_cov_4), as.character) %>% 
  pivot_longer(claim_cov_1:claim_cov_4, values_to = "cov", names_to = "name_cov") %>% 
  select(-starts_with("claim_cost"), -name_cov) %>% 
  filter(!is.na(cov)) %>% 
  pull(cov)

coverages <- 
  claims %>%
  mutate_at(vars(claim_cost_1:claim_cost_4), as.character) %>% 
  pivot_longer(claim_cost_1:claim_cost_4, values_to = "cost", names_to = "name_cost") %>% 
  select(-starts_with("claim_cov"), -name_cost) %>% 
  filter(!is.na(cost)) %>% 
  mutate(
    cov = as.numeric(cov_vec),
    cost = as.numeric(cost)
  )


# Créer des variables indicatrices et les coûts par couverture
coverages %<>%
  dummy_cols("cov") %>% 
  select(-cov) %>% 
  mutate_at(vars(contains("cov")), list(cost = ~. * coverages$cost)) %>% 
  select(-cost)


# Retour à une ligne par réclamation
claims_almost_final <- 
  coverages %>% 
  group_by(claim_id) %>% 
  summarise(
    vin = first(vin),
    policy_id = first(policy_id),
    contract_start_date = first(contract_start_date),
    contract_end_date = first(contract_end_date),
    claim_date = first(claim_date),
    cov_1_ind = max(cov_1),
    cov_2_ind = max(cov_2),
    cov_3_ind = max(cov_3),
    cov_4_ind = max(cov_4),
    cov_5_ind = max(cov_5),
    cov_6_ind = max(cov_6),
    cov_1_cost = max(cov_1_cost),
    cov_2_cost = max(cov_2_cost),
    cov_3_cost = max(cov_3_cost),
    cov_4_cost = max(cov_4_cost),
    cov_5_cost = max(cov_5_cost),
    cov_6_cost = max(cov_6_cost),
  ) %>% 
  ungroup()


# Dans la base claims, garder seulement les réclamations qui touchent aux couvertures 10 ou 12
claims_final <- 
  claims_almost_final %>% 
  filter(cov_2_ind + cov_4_ind >= 1) %>% 
  select(-contains(c("_1_", "_3_", "_5_", "_6_")))


# Base de données qui indique le nombre de réclamations pour chaque VIN =========================================================
df_nb_claims <- 
  claims_final %>% 
  group_by(vin) %>% 
  summarise(nb_claims = n())

rm(claims, claims_almost_final, claims_first, claims_second, claims_third, claims_fourth, coverages, contracts_with_claim, cov_vec)


# Fonction pour nettoyer une base de données par trajet =========================================================================
clean_trips <- function(trips_df) {
  trips_df %>% 
    lazy_dt() %>%
    transmute(
      trip_number = as.character(TRIP_NUMBER),
      vin = as.character(ENROLLED_VIN),
      datetime_start = parse_date_time2(LOCAL_TRIP_START_TIMESTAMP, "%d%b%Y:%H:%M:%OS"),
      datetime_end = parse_date_time2(LOCAL_TRIP_END_TIMESTAMP, "%d%b%Y:%H:%M:%OS"),
      duration = as.numeric(datetime_end - datetime_start),
      date_start = date(datetime_start),
      date_end = date(datetime_end),
      time_start = as_hms(datetime_start),
      time_end = as_hms(datetime_end),
      distance = as.numeric(VSS_DISTANCE),
      avg_speed = as.numeric(VSS_AVG_SPEED),
      max_speed = as.numeric(VSS_MAX_SPEED)
    ) %>%
    arrange(vin, datetime_start) %>% 
    as_tibble() %>% 
    na.omit()
}


# Fonction qui merge une base trajet nettoyée avec la base par contrat avec le VIN et les dates de début et fin du contrat ======
merge_trips_contracts <- function(trips_df) {
  trips_df %>% 
    left_join(contracts %>% select(vin:years_licensed), by = "vin") %>% 
    filter(date_start >= contract_start_date) %>% 
    filter(date_start < contract_end_date) 
}


# Fonction qui calcule les variables télématiques ===============================================================================
compute_tele_vars <- function(df) {
  df %>% 
    lazy_dt() %>% 
    mutate(
      weekday = weekdays(date_start, abbreviate = T),
      is_weekend = weekday %in% c("Sat", "Sun"),
      duration_night_trip = (time_start >= as_hms("00:00:00") & time_start < as_hms("06:00:00")) * duration,
      duration_noon_trip = (time_start >= as_hms("11:00:00") & time_start < as_hms("14:00:00")) * duration,
      duration_evening_trip = (time_start >= as_hms("20:00:00") & time_start <= as_hms("23:59:59")) * duration,
      duration_peak_morning_trip = ((time_start >= as_hms("07:00:00") & time_start < as_hms("09:00:00")) & !is_weekend) * duration,
      duration_peak_evening_trip = ((time_start >= as_hms("17:00:00") & time_start < as_hms("20:00:00")) & !is_weekend) * duration,
      duration_mon_to_thu = (weekday %in% c("Mon", "Tue", "Wed", "Thu")) * duration,
      duration_fri_sat = (weekday %in% c("Fri", "Sat")) * duration,
      duration_sun = (weekday == "Sun") * duration
    ) %>% 
    rename(
      trip_avg_speed = avg_speed,
      trip_distance = distance,
      trip_duration = duration,
      trip_max_speed = max_speed
    ) %>% 
    group_by(vin) %>% 
    summarise(
      contract_start_date      = first(contract_start_date),
      contract_end_date        = first(contract_end_date),
      c_expo                   = first(expo),
      c_annual_distance        = first(annual_distance),
      c_commute_distance       = first(commute_distance),
      c_conv_count_3_yrs_minor = first(conv_count_3_yrs_minor),
      c_gender                 = first(gender),
      c_marital_status         = first(marital_status),
      c_pmt_plan               = first(pmt_plan),
      c_veh_age                = first(veh_age),
      c_veh_use                = first(veh_use),
      c_years_claim_free       = first(years_claim_free),
      c_years_licensed         = first(years_licensed),
      t_avg_daily_distance     = sum(trip_distance),
      t_avg_daily_nb_trips     = n(),
      t_med_trip_avg_speed     = median(trip_avg_speed),
      t_med_trip_distance      = median(trip_distance),
      t_med_trip_max_speed     = median(trip_max_speed),
      t_max_trip_max_speed     = max(trip_max_speed),
      t_prop_long_trip         = sum(trip_distance > 100) / n(),
      t_frac_expo_night        = sum(duration_night_trip) / sum(trip_duration),
      t_frac_expo_noon         = sum(duration_noon_trip) / sum(trip_duration),
      t_frac_expo_evening      = sum(duration_evening_trip) / sum(trip_duration),
      t_frac_expo_peak_morning = sum(duration_peak_morning_trip) / sum(trip_duration),
      t_frac_expo_peak_evening = sum(duration_peak_evening_trip) / sum(trip_duration),
      t_frac_expo_mon_to_thu   = sum(duration_mon_to_thu) / sum(trip_duration),
      t_frac_expo_fri_sat      = sum(duration_fri_sat) / sum(trip_duration)
    ) %>% 
    ungroup() %>% 
    mutate(
      t_avg_daily_distance = t_avg_daily_distance / (365.25 * c_expo),
      t_avg_daily_nb_trips = t_avg_daily_nb_trips / (365.25 * c_expo)
    ) %>% 
    as_tibble()
}


# Fonction pour créer un jeu de données à partir d'un fichier TRIP_VIN*.csv =====================================================
create_data <- function(trips_df_filename, nb_months_tele) {
  trips_df <- read_csv(trips_df_filename, col_type = cols())
  
  res <- 
    trips_df %>% 
    clean_trips() %>% 
    merge_trips_contracts() %>% 
    filter(as.numeric(date_start) <= as.numeric(contract_start_date) + 30.4375 * nb_months_tele) %>% 
    group_by(trip_number) %>% 
    slice(1) %>% 
    ungroup() %>% 
    compute_tele_vars() %>%
    mutate(
      t_avg_daily_distance = t_avg_daily_distance * 365.25 / (30.4375 * nb_months_tele),
      t_avg_daily_nb_trips = t_avg_daily_nb_trips * 365.25 / (30.4375 * nb_months_tele)
    ) %>% 
    left_join(df_nb_claims, by = "vin") %>% 
    mutate(nb_claims = replace_na(nb_claims, 0)) %>% 
    mutate(claim_ind = factor(as.numeric(nb_claims > 0), levels = c("0", "1")))
  
  i <<- i + 1
  setTxtProgressBar(pb, i)
  return(res)
}


# Créer les 13 jeux de données ==================================================================================================
trip_files <- dir_ls(here("0_data"), regexp = "TRIP_VIN")

pb <- txtProgressBar(min = 0, max = 3216, style = 3)
i <- 0
jeux_ls <- vector("list", 12)
jeux_ls[[1]] <- map_dfr(trip_files, create_data, nb_months_tele = 1)
jeux_ls[[2]] <- map_dfr(trip_files, create_data, nb_months_tele = 2)
jeux_ls[[3]] <- map_dfr(trip_files, create_data, nb_months_tele = 3)
jeux_ls[[4]] <- map_dfr(trip_files, create_data, nb_months_tele = 4)
jeux_ls[[5]] <- map_dfr(trip_files, create_data, nb_months_tele = 5)
jeux_ls[[6]] <- map_dfr(trip_files, create_data, nb_months_tele = 6)
jeux_ls[[7]] <- map_dfr(trip_files, create_data, nb_months_tele = 7)
jeux_ls[[8]] <- map_dfr(trip_files, create_data, nb_months_tele = 8)
jeux_ls[[9]] <- map_dfr(trip_files, create_data, nb_months_tele = 9)
jeux_ls[[10]] <- map_dfr(trip_files, create_data, nb_months_tele = 10)
jeux_ls[[11]] <- map_dfr(trip_files, create_data, nb_months_tele = 11)
jeux_ls[[12]] <- map_dfr(trip_files, create_data, nb_months_tele = 12)
close(pb)

# Sauvegarder les 12 jeux de données ============================================================================================
write_rds(jeux_ls, file = here("2_pipeline", "01_create_data", "jeux_ls.RDS"))
jeux_ls <- read_rds(here("2_pipeline", "01_create_data", "jeux_ls.RDS"))


# Avoir exactement les mêmes VINs dans chacun des 12 jeux de données ============================================================
vins_to_keep <- map(jeux_ls, "vin") %>% reduce(intersect)
jeux_2_ls <- map(jeux_ls, filter, vin %in% vins_to_keep)


# Séparer en entrainement et test ===============================================================================================
set.seed(2020)
vins_to_keep_shuffled <- sample(vins_to_keep)

train_ls <- map(jeux_2_ls, filter, vin %in% vins_to_keep_shuffled[1:18880])
test_ls <- map(jeux_2_ls, filter, vin %in% vins_to_keep_shuffled[18881:26971])


# Enlever les variables inutiles pour le LASSO ==================================================================================
train_ls %<>% map(select, -vin, -contract_start_date, -contract_end_date, -nb_claims)
test_ls %<>% map(select, -vin, -contract_start_date, -contract_end_date, -nb_claims)


# Sauvegarder les bases train et test ===========================================================================================
write_rds(train_ls, file = here("2_pipeline", "01_create_data", "train_ls.RDS"))
write_rds(test_ls, file = here("2_pipeline", "01_create_data", "test_ls.RDS"))
