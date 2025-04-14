library(tidyverse)
library(tidyterra)
library(terra)
library(glue)
library(lme4)

buffer <- 3000

pozos <- vect('data/processed/vectorial/pozo_filtrado.shp')
unidades <- vect(glue('data/processed/vectorial/unidades_{buffer}.shp'))

data <- read_rds(glue('data/processed/rds/dataset_{buffer}.rds'))

modelo <- read_rds(glue('data/processed/rds/lmm_model_dataset{buffer}.rds'))
coef_estandarizados <- read_rds(glue('data/processed/rds/std_coef_dataset{buffer}.rds'))

predictores_clave <- coef_estandarizados |> 
  filter(abs(Std_Coefficient) > 0.2) |> 
  pull(Parameter)

cor <- data |> 
  na.omit() |> 
  group_by(shac,codigo) |> 
  summarise(across(all_of(predictores_clave), ~cor(GWI, .x)))

pozos |> 
  left_join(cor) |> 
  writeVector('data/processed/vectorial/bp_correlation.shp')

cor_pozo <- cor |> 
  pivot_longer(-c(shac,codigo), names_to = "predictor", values_to = "cor")

cor_pozo |> 
  ggplot(aes(x = predictor, y = factor(codigo), fill = cor)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", limits = c(-1, 1)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  facet_grid(rows = vars(shac), space = 'free_y', scales = 'free_y', switch = 'y') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = 'white')) +
  labs(title = "Correlaciones GWI-Predictores por pozo",
       y = 'well code')




