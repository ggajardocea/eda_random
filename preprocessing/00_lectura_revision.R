# Se exploran los archivos ------------------------------------------------
library(dplyr)
library(lubridate)
library(data.table)
library(h2o)
library(purrr)
library(stringr)

# se leen los nombres
list.files("files/input")

# Se leen todos los experimentos
files <- list.files("files/input", full.names = TRUE, pattern = "experiment_[0-9]{2}.csv")
experiments <- map(files, ~fread(.))

# La dimensión de cada archivo
map(experiments, dim)

# Se le asigna el nombre
name <- list.files("files/input", pattern = "experiment_[0-9]{2}.csv") %>% 
  str_extract("[0-9]{2}")
names(experiments) <- name

# Se juntan todos
experiment_df <- bind_rows(experiments, .id = "experiment")
head(experiment_df)

# Se explora el experiment result
exp_t <- fread("files/input/train.csv")

# Se exploran las columnas con mayor cantidad de NA o ceros
nas_exp <- sapply(experiment_df, function(x){mean(x %in% c("", NaN, NA))})
nas_t <- sapply(exp_t, function(x){mean(x %in% c("", NaN, NA))})
table(exp_t$passed_visual_inspection, useNA = "always")

# Passed visual inspection tiene vacios cuando debería ser no
exp_t <- exp_t %>% 
  mutate(passed_visual_inspection = ifelse(passed_visual_inspection == "", "no", passed_visual_inspection))

# Se juntan ambos datasets
experiment_df <- experiment_df %>%  mutate(experiment = as.numeric(experiment))
base <- experiment_df %>% 
  left_join(exp_t, by = c("experiment" = "No"))
head(base)

# Se revisan las variables label para no tener algunas tan poco frecuentes
table(base$Machining_Process)

base <- base %>% 
  mutate(Machining_Process = case_when(Machining_Process == "end" ~ "End",
                                       Machining_Process == "Starting" ~ "Prep",
                                       TRUE ~ Machining_Process))
table(base$Machining_Process)

# Con esto ya es posible empezar a hacer gráficos
dir.create("files/intermedia")
saveRDS(base, "files/intermedia/00_base.rds")
