# Se modela ------------------------------------------------
library(dplyr)
library(lubridate)
library(data.table)
library(h2o)
library(purrr)
library(stringr)
library(ggplot2)
library(GGally)
h2o.init()

# Se lee la base y se usa automl 
base <- readRDS("files/intermedia/00_base.rds")

# Se tienen tres targets
no_usar <- c("clamp_pressure", "material", "feedrate", "Machining_Process", "experiment")
targets <- c("tool_condition", "machining_finalized", "passed_visual_inspection")
usar <- base %>% select(-c(no_usar, targets)) %>% colnames()

base <- base %>% 
  mutate_at(targets, as.factor)

base.h2o <- as.h2o(base, id="base")
base.split <- h2o.splitFrame(data = base.h2o, ratios=0.75)

y = "tool_condition"
x = usar

autoML= h2o.automl(x = x,
                   y = y, 
                   training_frame = base.split[[1]], 
                   leaderboard_frame = base.split[[2]], 
                   nfolds = 10,
                   max_runtime_secs = 60,
                   exclude_algos = c("StackedEnsemble", "DeepLearning"))

autoML.leader = autoML@leader
auc_valid <- autoML.leader@model[["cross_validation_metrics"]]@metrics[["AUC"]]

# Viendo como se desempeño en testeo
conf_mat <- h2o.confusionMatrix(autoML.leader, base.split[[2]])
pred <- h2o.predict(autoML.leader, base.split[[2]])

# Viendo además las principales variables
h2o.varimp_plot(autoML.leader)
