# Se explora gráficamente ------------------------------------------------
library(dplyr)
library(lubridate)
library(data.table)
library(h2o)
library(purrr)
library(stringr)
library(ggplot2)
library(GGally)

# Se hará un análisis exploratorio simple, que consiste en ver
# Series de tiempo
# Distribución de los outputs

# Se lee el archivo del preprocesamiento
base <- readRDS("files/intermedia/00_base.rds")
head(base)

# Distrubuciones de feedrate, clamp pressure y material count
base %>% 
  ggplot(aes(x = feedrate)) + 
  geom_density(fill = "red") +
  labs(title = "Densidad de feedrate")

base %>% 
  ggplot(aes(x = clamp_pressure)) + 
  geom_density(fill = "red") +
  labs(title = "Densidad de clamp pressure")

# Se revisa el output de la máquina
table(base$machining_finalized, base$passed_visual_inspection)

# Se comparan las variables de resultados de acuerdo a inspección de máquina y visual
base_t <- base %>% 
  select(passed_visual_inspection, machining_finalized, tool_condition) %>% 
  tidyr::pivot_longer(cols = c(passed_visual_inspection, machining_finalized), names_to = "examen")

table(base_t$tool_condition)  

base_t %>% 
  ggplot(aes(x = value, fill = tool_condition)) +
  geom_bar(stat = "count", position = "dodge") +
  facet_wrap(.~examen)

# Se estudia la distribución de los procesos
base %>% 
  ggplot(aes(x = reorder(Machining_Process, -table(Machining_Process)[Machining_Process]))) +
  geom_bar(stat="count", fill = "red") +
  theme(axis.text.x = element_text(angle = -45)) +
  labs(x = "Machine Process", y = "Frecuencia", 
       title = "Frecuencia de Machine Process")


# Se hace la gráfica de los experimentos
base_ts <- base %>% 
  group_by(experiment) %>% 
  mutate(time = 1:n()) %>% ungroup()

base_ts %>% 
  filter(experiment == 1) %>% 
  ggplot(aes(x = time, y = X1_ActualVelocity)) +
  geom_line()

base_ts %>% 
  ggplot(aes(x = time, y = X1_ActualVelocity, color = tool_condition)) +
  geom_line() +
  facet_wrap(.~experiment, nrow = 6, scales = "free_x") +
  labs(title = "X1_ActualVelocity [mm/s] vs t",
       subtitle = "Separados por experimento")


table(base_ts$experiment, base_ts$tool_condition)

# Se hace feedrate y  clamp_pressure, segun condición de la herramienta
base %>% 
  ggplot(aes(x = feedrate, fill = tool_condition)) + 
  geom_density() +
  labs(title = "Densidad de feedrate")

base_select <- base %>%
  select(feedrate, clamp_pressure, tool_condition, machining_finalized)

ggpairs(base_select, columns = 1:2, ggplot2::aes(colour=tool_condition), 
    title="Correlograma eedrate, clamp_pressure, por tool_condition") 

ggpairs(base_select, columns = 1:2, ggplot2::aes(colour=machining_finalized), 
        title="Correlograma eedrate, clamp_pressure, por machining_finalized") 

# Por ActualVelocity
base_select <- base %>%
  select(ends_with("ActualVelocity"), tool_condition)

ggpairs(base_select, columns = 1:4, ggplot2::aes(colour=tool_condition, alpha = 0.5), 
        title="Correlograma Actual Velocity, por tool_condition",
        diag = list(discrete="barDiag", 
                    continuous = wrap("densityDiag", alpha=0.5 ))) 
