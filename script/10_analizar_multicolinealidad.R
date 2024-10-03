source('script/00_setup.R')
library(car)

data <- read_rds('data/processed/rds/dataset.rds') |> 
  select(-contains('ncGWDI_'),-ends_with(c('-1','-3','-6')))

data_vif <- data |> 
  select(-(año:cob)) |> 
  lm(ncGWDI ~ .,data = _) |> 
  vif()

data <- tibble(indicator = names(data_vif), vif = as.numeric(data_vif)) |> 
  mutate(indicator = gsub('`','',indicator))
  
data |> 
  ggplot(aes(x = reorder(indicator, vif), y = vif)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "firebrick3", linewidth = .6) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "dodgerblue3", linewidth = .6) +
  labs(title = "Variance Inflation Factor (VIF)",
       x = "Variables",
       y = "VIF") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
