# ============================================================================================================================= #
# But: Créer les 12 bases de données d'entrainement ainsi que les 36 bases de données de validation                             #
# Auteur: Francis Duval                                                                                                         #
# Date: Octobre 2020                                                                                                            #
# Input: ContractAuto.csv, TRIP_VIN1.csv:TRIP_VIN268.csv                                                                        #
# Output: train_01:train_12, valid_1_01:valid_1_12, valid_2a_01:valid_2a_12, valid_2b_01:valid_2b_12                            #
# ============================================================================================================================= #

source("1_code/00_source.R")


# Importer le jeu de données ContractAuto =======================================================================================
contracts_import <- read_csv(here("0_data", "ContractAuto.csv"))


# Nettoyer la base de données par contrat =======================================================================================
contracts <- 
  contracts_import %>% 
  transmute(
    vin                    = ENROLLED_VIN,
    policy_id              = as.character(POLICY_ID_M),
    driver_id              = as.character(Princ_CGID1),
    contract_start_date    = as.Date(parse_date_time2(poleffdate_m, "%d%b%Y!")),
    contract_end_date      = as.Date(parse_date_time2(polexpdate_m, "%d%b%Y!")),
    expo                   = exposit1,
    expo_10                = exposit10,
    expo_11                = exposit11,
    expo_12                = exposit12,
    expo_26                = exposit26,
    expo_37                = exposit37,
    expo_45                = exposit45,
    annual_distance        = as.integer(ANNUALMILEAGE),
    commute_distance       = as.integer(COMMUTEDISTANCE),
    conv_count_3_yrs_minor = as.integer(CONVCOUNTMINOR3YRS),
    gender                 = factor(DRIVER_GENDER),
    marital_status         = factor(DRIVER_MARITALSTATUS),
    pmt_plan               = factor(PAYMENTPLAN),
    veh_age                = as.integer(VEHICLEAGE),
    veh_use                = factor(VEHICLEUSE),
    years_claim_free       = DRIVER_YEARSCLAIMFREE,
    years_licensed         = as.integer(DRIVER_YEARSLICENSED),
    first_claim_date       = as.Date(parse_date_time2(as.character(FirstClaim_date), "%d%b%y!:%H:%M:%OS")),
    second_claim_date      = as.Date(parse_date_time2(as.character(SecClaim_date), "%d%b%y!:%H:%M:%OS")),
    third_claim_date       = as.Date(parse_date_time2(as.character(ThirdClaim_date), "%d%b%y!:%H:%M:%OS")),
    first_claim_id         = as.character(claimid1),
    second_claim_id        = as.character(claimid2),
    third_claim_id         = as.character(claimid3),
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
    first_claim_cost_1     = as.numeric(FirstClaim_cost1),
    first_claim_cost_2     = as.numeric(FirstClaim_cost2),
    first_claim_cost_3     = as.numeric(FirstClaim_cost3),
    first_claim_cost_4     = as.numeric(FirstClaim_cost4),
    second_claim_cost_1    = as.numeric(SecClaim_cost1),
    second_claim_cost_2    = as.numeric(SecClaim_cost2),
    second_claim_cost_3    = as.numeric(SecClaim_cost3),
    second_claim_cost_4    = as.numeric(SecClaim_cost4),
    third_claim_cost_1     = as.numeric(ThirdClaim_cost1),
    third_claim_cost_2     = as.numeric(ThirdClaim_cost2),
    third_claim_cost_3     = as.numeric(ThirdClaim_cost3),
    third_claim_cost_4     = as.numeric(ThirdClaim_cost4),
    first_claim_faute_1    = as.numeric(FirstClaim_faute1),
    first_claim_faute_2    = as.numeric(FirstClaim_faute2),
    first_claim_faute_3    = as.numeric(FirstClaim_faute3),
    first_claim_faute_4    = as.numeric(FirstClaim_faute4),
    second_claim_faute_1   = as.numeric(SecClaim_faute1),
    second_claim_faute_2   = as.numeric(SecClaim_faute2),
    second_claim_faute_3   = as.numeric(SecClaim_faute3),
    second_claim_faute_4   = as.numeric(SecClaim_faute4),
    third_claim_faute_1    = as.numeric(ThirdClaim_faute1),
    third_claim_faute_2    = as.numeric(ThirdClaim_faute2),
    third_claim_faute_3    = as.numeric(ThirdClaim_faute3),
    third_claim_faute_4    = as.numeric(ThirdClaim_faute4)
  ) %>% 
  arrange(vin, contract_start_date) %>% 
  filter_at(vars(starts_with("expo_")), all_vars(near(., expo, tol = 0.05))) %>% 
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
    driver_id, 
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
    driver_id, 
    contract_start_date, 
    contract_end_date,
    contains("second")
  ) %>% 
  filter(!is.na(second_claim_id)) %>% 
  rename_all(~ str_replace_all(., "second_", ""))

# ----------

claims <- bind_rows(claims_first, claims_second)

# Créer une base de données ayant une ligne par couverture
coverages <- 
  claims %>%
  mutate_at(vars(claim_cov_1:claim_faute_4), as.character) %>% 
  pivot_longer(claim_cov_1:claim_faute_4) %>% 
  separate(name, c("delete", "info_type", "delete_2"), sep = "_") %>% 
  pivot_wider(names_from = info_type, values_from = value) %>% 
  select(-delete, -delete_2) %>% 
  filter(!is.na(cov)) %>% 
  mutate_at(vars(cov, cost, faute), as.numeric)

# Créer des variables indicatrices et les coûts par couverture
coverages %<>%
  dummy_cols("cov") %>% 
  select(-cov) %>% 
  mutate_at(vars(contains("cov")), list(cost = ~. * coverages$cost)) %>% 
  select(-cost)

# Retour à une ligne par réclamation
claims_final <- 
  coverages %>% 
  group_by(claim_id) %>% 
  summarise(
    vin = first(vin),
    policy_id = first(policy_id),
    driver_id = first(driver_id),
    contract_start_date = first(contract_start_date),
    contract_end_date = first(contract_end_date),
    claim_date = first(claim_date),
    faute = first(faute),
    cov_1_ind = max(cov_1),
    cov_10_ind = max(cov_10),
    cov_11_ind = max(cov_11),
    cov_12_ind = max(cov_12),
    cov_26_ind = max(cov_26),
    cov_37_ind = max(cov_37),
    cov_45_ind = max(cov_45),
    cov_1_cost = max(cov_1_cost),
    cov_10_cost = max(cov_10_cost),
    cov_11_cost = max(cov_11_cost),
    cov_12_cost = max(cov_12_cost),
    cov_26_cost = max(cov_26_cost),
    cov_37_cost = max(cov_37_cost),
    cov_45_cost = max(cov_45_cost)
  ) %>% 
  ungroup()


# Base de données qui indique le nombre de réclamations pour chaque VIN =========================================================
df_nb_claims <- 
  claims_final %>% 
  group_by(vin) %>% 
  summarise(nb_claims = n())

rm(claims, claims_first, claims_second, coverages, contracts_with_claim)


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
  trips_df <- read_csv(trips_df_filename)
  
  res <- 
    trips_df %>% 
    clean_trips() %>% 
    merge_trips_contracts() %>% 
    filter(as.numeric(date_start) <= as.numeric(contract_start_date) + 30.4375 * nb_months_tele) %>% 
    group_by(trip_number) %>% 
    slice(1) %>% 
    ungroup() %>% 
    compute_tele_vars() %>% 
    left_join(df_nb_claims, by = "vin") %>% 
    mutate(nb_claims = replace_na(nb_claims, 0)) %>% 
    mutate(claim_ind = factor(as.numeric(nb_claims > 0), levels = c("0", "1")))
  
  i <<- i + 1; print(i)
  return(res)
}


# Créer les 13 jeux de données ==================================================================================================
trip_files <- dir_ls(here("0_data"), regexp = "TRIP_VIN")

i <- 0
jeu_01 <- map_dfr(trip_files, create_data, nb_months_tele = 1)
write_rds(jeu_01, path = here("2_pipeline", "01_create_data", "jeu_01.RDS")); i <- 0
jeu_02 <- map_dfr(trip_files, create_data, nb_months_tele = 2)
write_rds(jeu_02, path = here("2_pipeline", "01_create_data", "jeu_02.RDS")); i <- 0
jeu_03 <- map_dfr(trip_files, create_data, nb_months_tele = 3)
write_rds(jeu_03, path = here("2_pipeline", "01_create_data", "jeu_03.RDS")); i <- 0
jeu_04 <- map_dfr(trip_files, create_data, nb_months_tele = 4)
write_rds(jeu_04, path = here("2_pipeline", "01_create_data", "jeu_04.RDS")); i <- 0
jeu_05 <- map_dfr(trip_files, create_data, nb_months_tele = 5)
write_rds(jeu_05, path = here("2_pipeline", "01_create_data", "jeu_05.RDS")); i <- 0
jeu_06 <- map_dfr(trip_files, create_data, nb_months_tele = 6)
write_rds(jeu_06, path = here("2_pipeline", "01_create_data", "jeu_06.RDS")); i <- 0
jeu_07 <- map_dfr(trip_files, create_data, nb_months_tele = 7)
write_rds(jeu_07, path = here("2_pipeline", "01_create_data", "jeu_07.RDS")); i <- 0
jeu_08 <- map_dfr(trip_files, create_data, nb_months_tele = 8)
write_rds(jeu_08, path = here("2_pipeline", "01_create_data", "jeu_08.RDS")); i <- 0
jeu_09 <- map_dfr(trip_files, create_data, nb_months_tele = 9)
write_rds(jeu_09, path = here("2_pipeline", "01_create_data", "jeu_09.RDS")); i <- 0
jeu_10 <- map_dfr(trip_files, create_data, nb_months_tele = 10)
write_rds(jeu_10, path = here("2_pipeline", "01_create_data", "jeu_10.RDS")); i <- 0
jeu_11 <- map_dfr(trip_files, create_data, nb_months_tele = 11)
write_rds(jeu_11, path = here("2_pipeline", "01_create_data", "jeu_11.RDS")); i <- 0
jeu_12 <- map_dfr(trip_files, create_data, nb_months_tele = 12)
write_rds(jeu_12, path = here("2_pipeline", "01_create_data", "jeu_12.RDS")); i <- 0
