library(tidyverse)
library(car)

compute_correlation <- function(df, predictors) {
  r <- df |> 
    group_by(shac, codigo) |> 
    reframe(across(contains(predictors), 
                   ~ {
                     test <- cor.test(GWI, .x, use = 'complete.obs')
                     r <- test$estimate
                     round(r, 2)
                   })) |> 
    pivot_longer(contains(predictors), names_to = 'predictor', values_to = 'r')
  
  sig <- df |> 
    group_by(shac, codigo) |> 
    reframe(across(contains(predictors), 
                   ~ {
                     test <- cor.test(GWI, .x, use = 'complete.obs')
                     p <- test$p.value
                     sig <- ifelse(p < 0.05, "*", "")
                   })) |> 
    pivot_longer(contains(predictors), names_to = 'predictor', values_to = 'sig')

  left_join(r,sig) |> 
    mutate(label = case_when(sig == '*' ~ r,
                             .default = NULL))

}

for (buffer in c(3000,5000,8000)) {
  
  data <- read_rds(glue('data/processed/rds/dataset_{buffer}.rds'))
  
  predictors <- c('SPEI_36','EDDI_36','SPI_36','SSI_36','SPEI_12','SPI_12','EDDI_12')
  
  data |> 
    select(shac, codigo, GWI, all_of(predictors)) |> 
    compute_correlation(predictors) |> 
    ggplot(aes(x = factor(predictor), y = factor(codigo), fill = r)) +
    geom_tile() + 
    geom_text(aes(label = label), size = 3, color = 'grey20') +
    scale_fill_distiller(palette = "RdBu", limits = c(-1, 1), direction = 1) +  
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    facet_grid(rows = vars(shac), space = 'free_y', scales = 'free_y', switch = 'y') +
    labs(x = NULL, y = 'well code', fill = 'r') +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'))
  
  ggsave(glue('output/fig/correlation_matrix/hidro_predictors_dataset{buffer}.png'),height=8.5,width = 7.5)
  
  data |> 
    select(shac, codigo, GWI, contains('NDVI')) |> 
    na.omit() |> 
    compute_correlation('NDVI') |> 
    rowwise() |> 
    mutate(veg_type = case_when(str_detect(predictor, 'AG') ~ 'AG',
                                str_detect(predictor, 'VN') ~ 'VN',
                                .default = 'ALL'),
           predictor = gsub('_AG|_VN','',predictor),
           predictor = factor(predictor,levels = paste0('zcNDVI_',c(1,3,6,12)))) |> 
    ggplot(aes(x = factor(predictor), y = factor(codigo), fill = r)) +
    geom_tile() + 
    geom_text(aes(label = label), size = 3, color = 'grey20') +
    scale_fill_distiller(palette = "RdBu", limits = c(-1, 1), direction = 1) +  
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    facet_grid(rows = vars(shac), cols = vars(veg_type), space = 'free_y', scales = 'free', switch = 'y') +
    labs(x = NULL, y = 'well code', fill = 'r') +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  ggsave(glue('output/fig/correlation_matrix/ndvi_predictors_dataset{buffer}.png'),height=8.5,width = 7.5)
  
  data |> 
    select(shac, codigo, GWI, contains('SETI')) |> 
    na.omit() |> 
    compute_correlation('SETI') |> 
    rowwise() |> 
    mutate(veg_type = case_when(str_detect(predictor, 'AG') ~ 'AG',
                                str_detect(predictor, 'VN') ~ 'VN',
                                .default = 'ALL'),
           predictor = gsub('_AG|_VN','',predictor),
           predictor = factor(predictor, levels = paste0('SETI_', c(1, 3, 6, 12, 24, 36)))) |>
    ggplot(aes(x = factor(predictor), y = factor(codigo), fill = r)) +  
    geom_tile() + 
    geom_text(aes(label = label), size = 3, color = 'grey20') +  
    scale_fill_distiller(palette = "RdBu", limits = c(-1, 1), direction = 1) +  
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    facet_grid(rows = vars(shac), cols = vars(veg_type), space = 'free_y', scales = 'free', switch = 'y') +
    labs(x = NULL, y = 'well code', fill = 'r') +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  ggsave(glue('output/fig/correlation_matrix/seti_predictors_dataset{buffer}.png'),height=8.5,width = 8.5)
  
  data |> 
    select(shac, codigo, GWI, contains('COB')) |> 
    compute_correlation('COB') |> 
    ggplot(aes(x = factor(predictor), y = factor(codigo), fill = r)) +  
    geom_tile() + 
    geom_text(aes(label = label), size = 3, color = 'grey20') +  
    scale_fill_distiller(palette = "RdBu", limits = c(-1, 1), direction = 1) +  
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    facet_grid(rows = vars(shac), space = 'free_y', scales = 'free', switch = 'y') +
    labs(x = NULL, y = 'well code', fill = 'r') +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'))
  
  ggsave(glue('output/fig/correlation_matrix/cob_predictors_dataset{buffer}.png'),height=8.5,width = 6)

}
