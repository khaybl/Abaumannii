# Importação de dataframe PHASTEST ----
library(openxlsx); library(tidyverse)
fagos <- read.xlsx("C:/Users/khayl/Downloads/Tabela_para_o_X-Meeting_2 (USAR ESSA).xlsx",
                   sheet = "PHASTEST",
                   rowNames = F, # Os rownames precisam ser falsos para que os NA da coluna 1 não interfiram
                   colNames = T)

# Estatística descritiva ----

## Média de proteínas por fago identificado
mean(na.omit(fagos$Total.Proteins)) 

## Distribuição de completude dos fagos
Completude<- as.matrix(sort(table(na.omit(fagos$Completeness)), decreasing = T))
rownames(Completude) <- c("Intacto", "Questionável", "Incompleto", "Sem fago")
phago_porcent <- prop.table(Completude)*100

cores <- c("olivedrab1","yellow","red","skyblue")
pie3D(phago_porcent,
      labels = rownames(phago_porcent),
      col = cores,
      family = "serif",
      theta = pi/4,
      family = "serif",
      )

## Distribuição dos fagos
tipos <- as.matrix(sort(table(fagos$Most.Common.Phage), decreasing = T)) %>% print()
