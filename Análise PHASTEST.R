# Importação de dataframe PHASTEST ----
library(openxlsx)
fagos <- read.xlsx("C:/Users/khayl/Downloads/Tabela_para_o_X-Meeting_2 (USAR ESSA).xlsx",
                   sheet = "PHASTEST",
                   rowNames = F, # Os rownames precisam ser falsos para que os NA da coluna 1 não interfiram
                   colNames = T)
