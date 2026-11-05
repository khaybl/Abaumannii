# IMPORTAÇÃO DA TABELA ----
library(openxlsx); library(readxl);library(fdth); library(tidyverse)

GRAs <- read.xlsx("C:/Users/khayl/Downloads/Tabela_para_o_X-Meeting_2 (USAR ESSA).xlsx",
                  rowNames = T,
                  colNames = T,
                  sheet = "Resistoma - Quantitativo - 535")

GRAs

# TRATAMENTO DE DADOS ----
## Soma de colunas ----
GRAs_sum <- t(as.matrix(colSums(GRAs)))
GRAs_sum <- t(as.matrix(GRAs_sum[,1:226]))
rownames(GRAs_sum) <- c("Frequência absoluta")

# Soma de OXA's
vari <- "OXA" # Usar vari <- "OXA" permite que  o objeto retenha o character
OXAs <- t(select(as.data.frame(GRAs_sum), starts_with(vari))) # O Vari é reconhecido por start_with ; Texto em string também ; dois textos podem ser reconhecidos se usado c()
OXAs.sum <- as.matrix(colSums(OXAs)); rm(OXAs) 

# Soma de ADC's
vari <- "ADC" # Usar vari <- "OXA" permite que  o objeto retenha o character
ADCs <- t(select(as.data.frame(GRAs_sum), starts_with(vari))) # O Vari é reconhecido por start_with ; Texto em string também ; dois textos podem ser reconhecidos se usado c()
ADCs.sum <- as.matrix(colSums(ADCs)); rm(ADCs)

# Soma aph(3
vari <- "aph(3'" # Usar vari <- "OXA" permite que  o objeto retenha o character
APHs <- t(select(as.data.frame(GRAs_sum), starts_with(vari))) # O Vari é reconhecido por start_with ; Texto em string também ; dois textos podem ser reconhecidos se usado c()
APHs.sum <- as.matrix(colSums(APHs)); rm(APHs)

# Remoção de genes de produtos de limpeza exclusivos
GRAs_sum <- as.data.frame(GRAs_sum)
GRAs_sum$qacE <- NULL; GRAs_sum$qacEdelta1 <- NULL; GRAs_sum$qacG <- NULL; GRAs_sum$qacJ <- NULL
GRAs_sum <- as.matrix(GRAs_sum)

# Nomes dos genes
Cromo <- colnames(GRAs_sum)

# Remoção de OXA, ADC e APH(3'
GRAs_sum <- select(as.data.frame(GRAs_sum), -starts_with(c("OXA", "ADC", vari)))

# Fixação de OXA's, ADC's, e APH's
colnames(ADCs.sum) <- c("ADCs")
colnames(OXAs.sum) <- c("OXAs")
colnames(APHs.sum) <- c("aph(3's")

rownames(ADCs.sum) <- c("")
rownames(OXAs.sum) <- c("")
rownames(APHs.sum) <- c("")

GRAs_sum <- cbind(GRAs_sum, OXAs.sum, APHs.sum, ADCs.sum)
GRAs_sum <- GRAs_sum[, order(unlist(GRAs_sum[1, ]), decreasing = TRUE)]

barplot(as.matrix(GRAs_sum[,1:30]),ylim = c(0,1000) ,las=2, family="serif", col="green4")

# Diagrama de Venn
library(VennDiagram)
library(grid)

futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

listas <- list(
  Carbapenem    = carbapenem,
  Cephalosporin = cephalosporin,
  Penam         = Penam
)

venn <- venn.diagram(
  x = listas,
  filename = NULL,
  category.names = c("", "", ""),        # sem nomes dos conjuntos
  fill = c("royalblue1", "firebrick2", "olivedrab1"),
  alpha = 0.85,                          # menos transparente
  col = "white", lwd = 2,
  cex = 1.4, fontface = "bold",
  margin = 0.08
)

grid.newpage()
grid.draw(venn)

png("venn_classes.png", width = 2000, height = 2000, res = 300)
grid.draw(venn)
dev.off()

Plasmídeos <- read.xlsx("C:/Users/khayl/Downloads/Tabela_para_o_X-Meeting_2 (USAR ESSA).xlsx",
                        rowNames = T,
                        colNames = T,
                        sheet = "Plasmídeos  - Quantitativo 2")
Plasm_sum <- as.matrix(colSums(Plasmídeos))
colSums(Plasm_sum)