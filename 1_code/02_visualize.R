# ============================================================================================================================= #
# But: Visualiser le jeu de données créé par 01_create_data                                                                     #
# Auteur: Francis Duval                                                                                                         #
# Date: Novembre 2020                                                                                                           #
# Input: jeux_ls.RDS                                                                                                            #
# Output:                                                                                                                       #
# ============================================================================================================================= #

source("1_code/00_source.R")


# Importer les jeux de données d'entrainement ===================================================================================
jeux_ls <- read_rds(here("2_pipeline", "01_create_data", "jeux_ls.RDS"))
claims <- read_rds(here("2_pipeline", "01_create_data", "claims.RDS"))


# Ajout d'un variable qui compte le nombre de réclamations comprehensive ========================================================
claims_comp <- 
  claims %>%
  filter(claim_cov_1 == 3 | claim_cov_2 == 3 | claim_cov_3 == 3 | claim_cov_4 == 3) %>% 
  group_by(vin) %>% 
  summarise(nb_claims_comp = n()) %>% 
  ungroup()

dat <- 
  jeux_ls[[12]] %>% 
  left_join(claims_comp, by = "vin") %>% 
  mutate(nb_claims_comp = replace_na(nb_claims_comp, 0))


# Jeux de données pour les graphiques ===========================================================================================
dat1 <- 
  dat %>% 
  select(t_avg_daily_distance, nb_claims) %>%
  mutate(km_group = floor(t_avg_daily_distance / 5) * 5) %>%
  group_by(km_group) %>%
  summarise(
    freq = mean(nb_claims),
    n_drivers = n()
  )

dat2 <- 
  dat %>% 
  select(t_avg_daily_distance, nb_claims_comp) %>%
  mutate(km_group = floor(t_avg_daily_distance / 5) * 5) %>%
  group_by(km_group) %>%
  summarise(
    freq = mean(nb_claims_comp),
    n_drivers = n()
  )

dat3 <- 
  dat %>% 
  select(t_max_trip_max_speed, nb_claims) %>%
  mutate(km_group = floor(t_max_trip_max_speed / 5) * 5) %>%
  group_by(km_group) %>%
  summarise(
    freq = mean(nb_claims),
    n_drivers = n()
  )

dat4 <- 
  dat %>% 
  select(t_max_trip_max_speed, nb_claims_comp) %>%
  mutate(km_group = floor(t_max_trip_max_speed / 5) * 5) %>%
  group_by(km_group) %>%
  summarise(
    freq = mean(nb_claims_comp),
    n_drivers = n()
  )

dat5 <- 
  dat %>% 
  select(t_frac_expo_noon, nb_claims) %>%
  mutate(km_group = floor(t_frac_expo_noon / 0.01) * 0.01) %>%
  group_by(km_group) %>%
  summarise(
    freq = mean(nb_claims),
    n_drivers = n()
  )

dat6 <- 
  dat %>% 
  select(t_frac_expo_noon, nb_claims_comp) %>%
  mutate(km_group = floor(t_frac_expo_noon / 0.01) * 0.01) %>%
  group_by(km_group) %>%
  summarise(
    freq = mean(nb_claims_comp),
    n_drivers = n()
  )

# Sauvegarder les graphiques ====================================================================================================
cairo_pdf(here("2_pipeline", "02_visualize", "dataviz.pdf"), onefile = T)
  ggplot(dat1, aes(x = km_group, y = freq, color = n_drivers)) + 
    geom_point() +
    xlab("Daily kilometers driven") +
    ylab("Claim frequency (collision + DCPD)") +
    labs(col = "Number of\ninsured drivers") +
    scale_colour_gradient(low = "#56B1F7", high = "#132B43") +
    ylim(0, 0.5)
  
  ggplot(dat1, aes(x = km_group, y = freq, col = n_drivers)) + 
    geom_point() +
    xlab("Daily kilometers driven") +
    ylab("Claim frequency (collision + DCPD)") +
    labs(col = "Number of\ninsured drivers") +
    scale_colour_gradient(low = "#56B1F7", high = "#132B43") +
    ylim(0, 0.1)
  
  ggplot(dat2, aes(x = km_group, y = freq, color = n_drivers)) + 
    geom_point() +
    xlab("Daily kilometers driven") +
    ylab("Claim frequency (comprehensive)") +
    labs(col = "Number of\ninsured drivers") +
    scale_colour_gradient(low = "#56B1F7", high = "#132B43") +
    ylim(0, 0.5)
  
  ggplot(dat2, aes(x = km_group, y = freq, col = n_drivers)) + 
    geom_point() +
    xlab("Daily kilometers driven") +
    ylab("Claim frequency (comprehensive)") +
    labs(col = "Number of\ninsured drivers") +
    scale_colour_gradient(low = "#56B1F7", high = "#132B43") +
    ylim(0, 0.1)
  
  ggplot(dat3, aes(x = km_group, y = freq, color = n_drivers)) + 
    geom_point() +
    xlab("Vitesse maximale atteinte dans le contrat") +
    ylab("Claim frequency (collision + DCPD)") +
    labs(col = "Number of\ninsured drivers") +
    scale_colour_gradient(low = "#56B1F7", high = "#132B43") +
    ylim(0, 0.5)
  
  ggplot(dat4, aes(x = km_group, y = freq, color = n_drivers)) + 
    geom_point() +
    xlab("Vitesse maximale atteinte dans le contrat") +
    ylab("Claim frequency (comprehensive)") +
    labs(col = "Number of\ninsured drivers") +
    scale_colour_gradient(low = "#56B1F7", high = "#132B43") +
    ylim(0, 0.5)
  
  ggplot(dat5, aes(x = km_group, y = freq, color = n_drivers)) + 
    geom_point() +
    xlab("Fraction de la conduite le midi") +
    ylab("Claim frequency (collision + DCPD)") +
    labs(col = "Number of\ninsured drivers") +
    scale_colour_gradient(low = "#56B1F7", high = "#132B43") +
    ylim(0, 0.075) +
    xlim(0, 0.5)
  
  ggplot(dat6, aes(x = km_group, y = freq, color = n_drivers)) + 
    geom_point() +
    xlab("Fraction de la conduite le midi") +
    ylab("Claim frequency (comprehensive)") +
    labs(col = "Number of\ninsured drivers") +
    scale_colour_gradient(low = "#56B1F7", high = "#132B43") +
    ylim(0, 0.075) +
    xlim(0, 0.5)
dev.off()




