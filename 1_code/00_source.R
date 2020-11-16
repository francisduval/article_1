library(tidyverse)
library(here)
library(dtplyr)
library(lubridate)
library(hms)
library(fastDummies)
library(glue)
library(magrittr)
library(readr)
library(tidymodels)
library(poissonreg)
library(silgelib)
library(Cairo)
library(doParallel)
library(tictoc)
library(hrbrthemes)
library(GGally)
library(viridis)
library(fs)
library(cvAUC)
theme_set(theme_roboto())


# Conflits ======================================================================================================================
here <- here::here
filter <- dplyr::filter
select <- dplyr::select
