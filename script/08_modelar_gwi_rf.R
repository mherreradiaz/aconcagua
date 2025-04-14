library(tidyverse)
library(car)

data <- read_rds('data/processed/rds/dataset.rds')

indices <- c('EDDI','SETI','SPEI','SPI','SSI','ZCNDVI')

#VIF por índice (no se hizo)

selected_indices <- lapply(indices,\(index) {
  
  data_index <- data |> 
    select(GWI,contains(index))
  
  index_vif <- data_index |> 
    lm(GWI ~ .,data = _) |> 
    vif() |> 
    as.data.frame() |> 
    rename(vif = 1) |> 
    rownames_to_column(var = 'index_scale') |> 
    mutate(index_scale = gsub('`','',index_scale)) |> 
    arrange(vif)
  
  second_scale <- index_vif |> 
    reframe(select_scale = ifelse(grepl('_12',first(index_scale)),
                                  nth(index_scale, 2),
                                  first(index_scale))) |> 
    pull()
  
  
  select_scales <- c(paste0(index,'_12'),second_scale)
  
}) |> unlist()

selected_indices <- lapply(indices,\(index) {paste0(index,c('_12','_36'))}) |> 
  unlist()

selected_indices <- gsub('NDVI_36','NDVI_1',selected_indices)

#rf

data <- read_rds('data/processed/rds/dataset_3000.rds')

hidro_var <- c('SWEI',paste0('SWEI',paste0('_lag_',1:4)),with(expand.grid(prefix = c('EDDI','SPEI','SPI','SSI'), suffix = c('_12','_36')),
                  paste0(prefix, suffix)))
  
vi_var <- with(expand.grid(prefix = c('zcNDVI_12', 'zcNDVI_1','SETI_1','SETI_36'), suffix = c('_AG', '_VN')),
               paste0(prefix, suffix))

selected_indices <- c(hidro_var,vi_var)

data_model <- data |> 
  mutate(group = as.factor(paste(shac,codigo,sep='_')),
         shac = as.factor(shac),
         well = as.factor(codigo),
         season = as.factor(estacion),
         .before = COB_AG) |>
  select(GWI,group,shac,well,season,contains('COB'),all_of(selected_indices)) |> 
  na.omit()

library(randomForest)
library(caret)
library(ranger)
library(progress)
library(patchwork)

set.seed(123)

train_data <- data_model |> 
  group_by(group) |> 
  slice_sample(prop = 0.8) |> 
  ungroup()

test_data <- anti_join(data_model, train_data)

num_predictores <- ncol(train_data) - 1

mtry_values <- 3:num_predictores
min_node_values <- 1:30
sample_fractions <- c(.7,.9,.8, 1)

best_r2 <- -Inf
best_model <- NULL
results <- tibble(mtry = integer(), min.node.size = integer(), sample.fraction = double(), R2 = double(), RMSE = double(), MAE = double())

total_combinations <- length(mtry_values) * length(min_node_values) * length(sample_fractions)
pb <- progress_bar$new(total = total_combinations, format = 'Tuning [:bar] :percent eta: :eta')

for (mtry in mtry_values) {
  for (min_node in min_node_values) {
    for (sf in sample_fractions) {
      
      rf_model <- ranger(GWI ~ ., 
                         data = train_data,
                         num.trees = 500,
                         mtry = mtry,
                         min.node.size = min_node,
                         sample.fraction = sf,
                         importance = 'permutation')
      
      predicciones <- predict(rf_model, test_data)$predictions
      
      r2 <- cor(predicciones, test_data$GWI)^2
      rmse <- sqrt(mean((predicciones - test_data$GWI)^2))
      mae <- mean(abs(predicciones - test_data$GWI))
      
      results <- bind_rows(results, tibble(mtry = mtry, min.node.size = min_node, sample.fraction = sf, R2 = r2, RMSE = rmse, MAE = mae))
      
      if (r2 > best_r2) {
        best_r2 <- r2
        best_model <- rf_model
      }
      
      pb$tick()
    }
  }
}

write_rds(best_model,'data/processed/rds/modelo_gwi_3000.rds')

best_params <- results |> arrange(desc(R2)) |> slice(1)
print(best_params)

best_predictions <- predict(best_model, test_data)$predictions

plot_data <- test_data |> 
  mutate(Predicho = best_predictions)

p1 <- ggplot(plot_data, aes(x = GWI, y = Predicho)) +
  geom_hline(yintercept = 0, linetype = 'dashed',color = 'grey30', alpha = .7) +
  geom_vline(xintercept = 0, linetype = 'dashed',color = 'grey30',alpha = .7) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed',color = 'grey30',alpha = .7) +
  geom_point(alpha = .7) +
  theme_bw() +
  xlim(-2.2,1.5) +
  ylim(-2.2,1.5) +
  labs(x = 'observed GWI',
       y = 'predicted GWI',
       color = 'vegetation type') +
  coord_fixed(ratio = 1) +
  annotate('text', x = -2.2, y = 1.32, 
           label = paste0('R²: ', round(best_params$R2, 3), 
                          '\nRMSE: ', round(best_params$RMSE, 3), 
                          '\nMAE: ', round(best_params$MAE, 3)),
           hjust = 0, size = 4) +
  theme(legend.position = c(.86, .105),
        legend.background = element_rect(fill = 'white', color = 'grey30',
                                         , size = 0.2),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10))

print(p1)

ggsave('output/fig/gwi_model/pred_obs.png',width = 11.78*.7,height=7.42*.7)

var_importance <- as.data.frame(best_model$variable.importance)
var_importance$Variable <- rownames(var_importance)

p2 <- ggplot(var_importance, aes(x = reorder(Variable, best_model$variable.importance), y = best_model$variable.importance)) +
  geom_col(fill = 'steelblue') +
  coord_flip() +
  theme_bw() +
  labs(x = 'predictors',
       y = 'importance (increase in MSE)')

print(p2)

ggsave('output/fig/gwi_model/importance.png',width = 11.78*.7,height=7.42*.7)

p1+p2

ggsave('output/fig/gwi_model/gwi_model.png',width = 11.78,height=7.42)

