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


# Jeux de données pour les graphiques ===========================================================================================
dat <- 
  jeux_ls[[12]] %>% 
  select(t_avg_daily_distance, nb_claims) %>%
  mutate(km_group = floor(t_avg_daily_distance / 5) * 5) %>%
  group_by(km_group) %>%
  summarise(
    freq = mean(nb_claims),
    n_drivers = n()
  )

dat2 <- 
  jeux_ls[[12]] %>% 
  select(c_expo, nb_claims) %>%
  mutate(expo_group = floor(c_expo / 0.005) * 0.005) %>%
  group_by(expo_group) %>%
  summarise(
    freq = mean(nb_claims),
    n_drivers = n()
  )


# Sauvegarder les graphiques ====================================================================================================
cairo_pdf(here("2_pipeline", "02_visualize", "dataviz.pdf"), onefile = T)
  ggplot(dat, aes(x = km_group, y = freq, color = n_drivers)) + 
    geom_point() +
    xlab("Daily kilometers driven") +
    ylab("Claim frequency") +
    labs(col = "Number of\ninsured drivers") +
    scale_colour_gradient(low = "#56B1F7", high = "#132B43")
  
  ggplot(dat, aes(x = km_group, y = freq, col = n_drivers)) + 
    geom_point() +
    xlab("Daily kilometers driven") +
    ylab("Claim frequency") +
    labs(col = "Number of\ninsured drivers") +
    scale_colour_gradient(low = "#56B1F7", high = "#132B43") +
    ylim(0, 0.11)
  
  ggplot(dat2, aes(x = expo_group, y = freq, color = n_drivers)) + 
    geom_point() +
    geom_text(aes(x = expo_group, y = freq, label = n_drivers), nudge_y = 0.01, size = 3) +
    xlab("Exposition (années)") +
    ylab("Claim frequency") +
    labs(col = "Number of\ninsured drivers") +
    scale_colour_gradient(low = "#56B1F7", high = "#132B43")
dev.off()
