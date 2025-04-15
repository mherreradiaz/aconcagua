library(tidyverse)
library(car)
library(lme4)
library(glue)
library(effectsize)

#general

for (buffer in c(3000,5000,8000)) {
  
  data <- read_rds(glue('data/processed/rds/dataset_{buffer}.rds'))
  
  hidro_var <- c('SWEI',paste0('SWEI',paste0('_lag_',1:4)),
                 with(expand.grid(prefix = c('EDDI','SPEI','SPI','SSI'), 
                                  suffix = c('_12','_36')),paste0(prefix, suffix)))
  
  vi_var <- with(expand.grid(prefix = c('zcNDVI_12', 'zcNDVI_1','SETI_1','SETI_36'), suffix = c('','_AG', '_VN')),
                 paste0(prefix, suffix))
  
  selected_indices <- c(hidro_var,vi_var)
  
  data_model <- data |> 
    mutate(well = as.factor(codigo),
           # season = as.factor(estacion),
           .before = COB_AG) |>
    select(GWI,well,contains('COB'),all_of(selected_indices)) |> 
    na.omit(GWI)
  
  predictors <- setdiff(names(data_model), c("GWI", "well"))
  formula <- as.formula(paste("GWI ~", paste(predictors, collapse = " + "), "+ (1 | well)"))
  
  modelo_lmm <- lmer(formula, data = data_model, REML = TRUE)
  summary(modelo_lmm)
  
  write_rds(modelo_lmm,glue('data/processed/rds/lmm_model_dataset{buffer}.rds'))
  
  coef_estandarizados <- standardize_parameters(modelo_lmm, method = "refit")
  print(coef_estandarizados)
  
  write_rds(coef_estandarizados,glue('data/processed/rds/std_coef_dataset{buffer}.rds'))
  
  coef_estandarizados |>
    rowwise() |> 
    mutate(sign = ifelse(between(0,CI_low,CI_high),0,1)) |> 
    ggplot(aes(x = Parameter, y = Std_Coefficient, ymin = CI_low, ymax = CI_high, color = as.factor(sign))) +
    geom_pointrange() +
    coord_flip() +
    scale_color_manual(values = c('0' = 'grey20', '1' = 'red2'), labels = c('0' = 'non-significative',
                                                                            '1' = 'significative')) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = glue('Well capture zone : {buffer} km'), x = 'predictors', y = 'Std. β (95% CI)',
         color = NULL) +
    theme_bw() +
    theme(legend.background = element_rect(fill = 'white', color = 'grey30',linewidth = 0.2),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10))
  
  ggsave(glue('output/fig/gwi_model/lmm_std_coef_dataset{buffer}.png'), width = 8,height = 7)
  
}

# según grupo trend

trend <- read_rds('data/processed/rds/pozo_trend.rds') |> 
  mutate(well = factor(codigo)) |> 
  select(well,class)

for (buffer in c(3000,5000,8000)) {
  
  data <- read_rds(glue('data/processed/rds/dataset_{buffer}.rds'))
  
  hidro_var <- c('SWEI',paste0('SWEI',paste0('_lag_',1:4)),
                 with(expand.grid(prefix = c('EDDI','SPEI','SPI','SSI'), 
                                  suffix = c('_12','_36')),paste0(prefix, suffix)))
  vi_var <- with(expand.grid(prefix = c('zcNDVI_12', 'zcNDVI_1','SETI_1','SETI_36'), suffix = c('','_AG', '_VN')),
                 paste0(prefix, suffix))
  selected_indices <- c(hidro_var,vi_var)
  
  data_model_class <- data |> 
    mutate(well = as.factor(codigo),
           # season = as.factor(estacion),
           .before = COB_AG) |>
    left_join(trend) |> 
    select(GWI,class,well,contains('COB'),all_of(selected_indices)) |> 
    na.omit(GWI)
  
  predictors <- setdiff(names(data_model), c("GWI", "well"))
  
  std_coef <- lapply(unique(data_model_class$class), \(x) {
    
    data_model <- data_model_class |> 
      filter(class == x) |> 
      select(GWI,everything(),-class)
    
    if (length(unique(data_model$well)) == 1) {
      formula <- as.formula(paste("GWI ~", paste(predictors, collapse = " + ")))
      modelo <- lm(formula, data = data_model)
    } else {
      formula <- as.formula(paste("GWI ~", paste(predictors, collapse = " + "), "+ (1 | well)"))
      modelo <- lmer(formula, data = data_model,REML =T)
    }
    
    # write_rds(modelo,glue('data/processed/rds/lmm_model_dataset{buffer}.rds'))
    # write_rds(coef_estandarizados,glue('data/processed/rds/std_coef_dataset{buffer}.rds'))
    
    standardize_parameters(modelo, method = "refit") |> 
      rowwise() |> 
      mutate(sign = ifelse(between(0,CI_low,CI_high),0,1),
             class = x)
    
  }) |> bind_rows()
  
  facet_labels <- setNames(c('-0.15 < S ≤ -0.1','-0.1 < S ≤ -0.05',
                             '-0.05 < S ≤ 0','0 < S ≤ 0.05'),
                           as.character(1:4))
  
  std_coef |> 
    ggplot(aes(x = Parameter, y = Std_Coefficient, ymin = CI_low, ymax = CI_high, color = as.factor(sign))) +
    geom_pointrange() +
    coord_flip() +
    scale_color_manual(values = c('0' = 'grey20', '1' = 'red2'), labels = c('0' = 'non-significative',
                                                                            '1' = 'significative')) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~class,ncol=2,nrow=2,labeller = labeller(class = facet_labels)) +
    labs(title = glue('Well capture zone : {buffer} km'), x = 'predictors', y = 'Std. β (95% CI)',
         color = NULL) +
    theme_bw() +
    theme(legend.background = element_rect(fill = 'white', color = 'grey30',size = 0.2),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12))
  
  ggsave(glue('output/fig/gwi_model/lmm_std_coef_dataset_class{buffer}.png'), width = 12,height = 10)
  
}
