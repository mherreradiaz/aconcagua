source('script/00_setup.R')
library(car)
library(ggcorrplot)
library(RColorBrewer)

data <- read_rds('data/processed/rds/dataset.rds') |> 
  select(-contains('ncGWDI_'),-ends_with(c('-1','-3','-6'))) |> 
  select(-(año:cob))

data_vif <- data |> 
  lm(ncGWDI ~ .,data = _) |> 
  vif() |> 
  as.data.frame() |> 
  rename(vif = 1) |> 
  rownames_to_column(var = "indicator") |> 
  mutate(indicator = gsub('`','',indicator)) |> 
  arrange(vif)
  
data_vif |> 
  ggplot(aes(x = reorder(indicator, vif), y = vif)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "firebrick3", linewidth = .6) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "dodgerblue3", linewidth = .6) +
  labs(title = "Variance Inflation Factor (VIF)",
       x = "Variables",
       y = "VIF") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_cor <- cor(data, use = 'complete.obs')

data_cor |> 
ggcorrplot(method = "square", 
           type = "upper",
           lab = TRUE,
           lab_size = 3,
           lab_col = 'black',
           ggtheme = theme_bw()) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")),
                       limits = c(-1, 1),
                       breaks = seq(-1, 1, by = 0.5),
                       name = "r") +
  theme(legend.position = 'none')

# sacar variables

vif_promedio <- function(data, var) {
  data_select <- data |> 
    select(-matches('EDDI-|SPEI-|SPI-|zcSM-'), all_of(variables_incluidas))
  modelo <- lm(ncGWDI ~ ., data = data_select) 
  vif_valores <- vif(modelo)
  mean(as.numeric(vif_valores))
}

combinaciones <- list(EDDI = c("EDDI-12", "EDDI-24", "EDDI-36"),
                  SPEI = c("SPEI-12", "SPEI-24", "SPEI-36"),
                  SPI  = c("SPI-12", "SPI-24", "SPI-36"),
                  zcSM = c("zcSM-12", "zcSM-24", "zcSM-36")) |> 
  expand.grid()

mejor_combinacion <- combinaciones |> 
  rowwise() |> 
  mutate(vif_promedio = calcular_vif_promedio(data, c(EDDI, SPEI, SPI, zcSM))) |> 
  ungroup() |> 
  filter(vif_promedio == min(vif_promedio)) |> 
  pivot_longer(cols=c(EDDI:zcSM), names_to = 'indicador',values_to = 'nombre') |> 
  pull(nombre) |> 
  as.character()

data_vif <- data |> 
  select(-matches('EDDI-|SPEI-|SPI-|zcSM-'), all_of(mejor_combinacion)) |>
  lm(ncGWDI ~ .,data = _) |> 
  vif() |> 
  as.data.frame() |> 
  rename(vif = 1) |> 
  rownames_to_column(var = "indicator") |> 
  mutate(indicator = gsub('`','',indicator)) |> 
  arrange(vif)

data_vif |> 
  ggplot(aes(x = reorder(indicator, vif), y = vif)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "firebrick3", linewidth = .6) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "dodgerblue3", linewidth = .6) +
  labs(title = "Variance Inflation Factor (VIF)",
       x = "Variables",
       y = "VIF") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_cor <- cor(data_ex, use = 'complete.obs')

data_cor |> 
  ggcorrplot(method = "square", 
             type = "upper",
             lab = TRUE,
             lab_size = 3,
             lab_col = 'black',
             ggtheme = theme_bw()) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")),
                       limits = c(-1, 1),
                       breaks = seq(-1, 1, by = 0.5),
                       name = "r") +
  theme(legend.position = 'none')

# guardar dataset filtrado

data_filtrada <- data |> 
  select(-matches('EDDI-|SPEI-|SPI-|zcSM-'),all_of(mejor_combinacion),-`SPEI-24`)

write_rds(data_filtrada,'data/processed/rds/dataset_limpio.rds')
