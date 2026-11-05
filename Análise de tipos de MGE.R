# IMPORTAÇÃO DE TABELA ----
library(openxlsx); library(readxl); library(tidyverse)

lista_MGEs <- read.xlsx("C:/Users/khayl/Downloads/Tabela_para_o_X-Meeting_2 (USAR ESSA).xlsx",
                        sheet = "Elementos - 535",
                        colNames = T)
lista_MGEs$X5 <- NULL
#View(lista_MGEs)
head(lista_MGEs)

colnames(lista_MGEs) <- c("IS", "Cn", "Tn", "ICE")
length(na.omit(lista_MGEs$IS)); length(na.omit(lista_MGEs$Tn)); length(na.omit(lista_MGEs$Cn)); length(na.omit(lista_MGEs$ICE))


# EXTRAÇÃO DE DADOS ----

## Extração de IS ----

### Tabelamento e recorte ----
IS <- lista_MGEs$IS # Isolamento da coluna
tab_IS <- as.matrix(sort(table(IS), decreasing = T)); tab_IS # Tabelamento para descrição

nrow(tab_IS)

# 1º recorte para tipos explicitos
IS_maiorq80 <- as.matrix(sort(tab_IS[tab_IS>=80], decreasing = T));IS_maiorq80 # Corte para N maior que 80 (ponto neutro entre 120 e 77) de freq
#Nomeação das linhas de IS_maiorq80
names_maiorq <- rownames(tab_IS); names_maiorq <- names_maiorq[1:15]
#                 ↑ Vetorização dos nomes              ↑ Recorte do vetor para os nomes que quero
rownames(IS_maiorq80) <- c(names_maiorq)

ISmnrq80 <- sum(tab_IS[tab_IS<80]);ISmnrq80 # Vetor contendo menores que 80 de freq em uma soma
#description_mnrq80 <- tab_IS[tab_IS<=80];description_mnrq80
is.vector(ISmnrq80) # Verificação

ISmnrq80_matrix <- as.matrix(ISmnrq80);rm(ISmnrq80);ISmnrq80_matrix
colnames(ISmnrq80_matrix) <- c(" "); ISmnrq80_matrix
rownames(ISmnrq80_matrix) <- c("IS com freq <80"); ISmnrq80_matrix

IS_compressed <- rbind(IS_maiorq80, ISmnrq80_matrix); IS_compressed
colnames(IS_compressed) <- c("Freq Absoluta")
IS_compressed <- t(IS_compressed);print(IS_compressed)

porcent_IS_comp <- prop.table(IS_compressed, margin = NULL)*100; rownames(porcent_IS_comp) <- c("Freq em %"); print(porcent_IS_comp)

### Plot de frequência absoluta ----
par(mar = c(10, 6, 4, 1))
barplot(IS_compressed,
        col = "orangered",
        las = 2,
        ylab = "Frequência absoluta",
        main = "Distribuição de IS",
        cex.main = 1.5,
        family="serif")

### Transformação com arcoseno da raiz quadrada ----
prop_IS <- IS_compressed / sum(IS_compressed) # Calculo de proporção
asin_IS <- asin(sqrt(prop_IS)) #Arcoseno da tabela

# Temporário
barplot(prop_IS)

# Plot com IS transformadas
#barplot(asin_IS,
#        col = "orangered",
#       ylim = c(0, 1),
#      las = 2,
#     ylab = "asin(sqrt(proporção))",
#    main = "Valores transformados com o arcoseno  da raiz quadrada",
#   cex.main = 1.5)

# Tópico para limpeza de environment de elementos de IS
# A definir


## Extração de Tn ----

### Tabelamento e recorte ----
Tn <- as.matrix(na.omit(lista_MGEs$Tn))
Tn
tab_Tn <- as.matrix(sort(table(Tn), decreasing = T)); tab_Tn

nrow(tab_Tn)

porcent_Tn <- prop.table(tab_Tn)*100; colnames(porcent_Tn) <- c("Freq em %")

sum(tab_Tn)
### Plot de frequência absoluta ----
par(mar = c(8, 6, 4, 1))
barplot(t(Tn_compressed),
        ylim = c(0, 300),
        col = "springgreen4",
        main = "Distribuição de Transposons em A. baumannii",
        ylab = "Frequência absoluta",
        las =2)

# Maiores Tn por frequência
Tn_compressed[1:5,]


## Extração de Cn ----

### Tabelamento e recorte ----
Cn <- as.matrix(na.omit(lista_MGEs$Cn))
Cn
tab_Cn <- as.matrix(sort(table(Cn), decreasing = T)); tab_Cn

nrow(tab_Cn)

Cnmaiorq20 <- as.matrix(sort(tab_Cn[tab_Cn > 20], decreasing = T)); print(Cnmaiorq20)
Cn_names <- rownames(tab_Cn);Cn_names <- Cn_names[1:21]
rownames(Cnmaiorq20) <- c(Cn_names); colnames(Cnmaiorq20) <- c("Frequência absoluta")

Cnmnrq20 <- as.matrix(sum(tab_Cn[tab_Cn <= 20]))
Cnmnrq20_expanded <- as.matrix(tab_Cn[tab_Cn <= 20])
rownames(Cnmnrq20) <- "Cns de frequência ≤ 20"
Cnmnrq20

#Cnmnrq20_desc <- as.matrix(tab_Cn[tab_Cn <= 20]);nrow(Cnmnrq20_desc)

tab_Cn_compressed <- rbind(Cnmaiorq20, Cnmnrq20)
colnames(tab_Cn_compressed) <- "Frequência Absoluta"
print(tab_Cn_compressed)

porcent_Cn <- prop.table(tab_Cn_compressed)*100; colnames(porcent_Cn) <- c("Freq em %")
### Plot de frequência absoluta ----
par(mar = c(6,4,2,2))
barplot(t(tab_Cn_compressed),
        ylim = c(0, 4500),
        las = 2,
        cex.names = 0.5,
        col = "gold1",
        main = "Distribuição por frequência dos Transposons Compostos")
tibble(tab_Cn_compressed)

### Skewness ----
moments::skewness(tab_Cn)
moments::kurtosis(tab_Cn)

# Top 5
tab_Cn_compressed[1:5,]

### Tipos de IS mais frequentes nos Cn ----
ISs_Cn <- as.data.frame(Cn) # Intermédiario
colnames(ISs_Cn) <- c("Cns")
ISs_Cn <- mutate(ISs_Cn, sub(".*_", "", Cns))
tab_Is_Cn <- t(as.matrix(sort(table(ISs_Cn$`sub(".*_", "", Cns)`), decreasing = T)))

# Recorte para as IS com frequência >= 30
tab_Is_Cn_top <- tab_Is_Cn[, tab_Is_Cn >= 30]

# Soma do restante (frequência < 30)
Is_Cn_resto <- sum(tab_Is_Cn[tab_Is_Cn < 30])

# Junta top + resto agrupado
tab_Is_Cn_compressed <- c(tab_Is_Cn_top, Is_Cn_resto)
names(tab_Is_Cn_compressed) <- c(names(tab_Is_Cn_top), "ISs com <30")
tab_Is_Cn_compressed <- t(as.matrix(tab_Is_Cn_compressed))

barplot(tab_Is_Cn_compressed,
        las = 2,
        ylim = c(0, 3000),
        col = "navyblue",
        cex.main = 1.5,
        main = "Principal IS flanqueadora dos Transposons Compostos",
        ylab = "Frequência absoluta",
        family = "TNR")

#pie(tab_Is_Cn[1,1:5],
#   labels = c(colnames(tab_Is_Cn)),
#  clockwise = T,
# col = rainbow(30),
#family = "serif")


# CONTROLE DE GRÁFICOS ----

# Só usar após rodar o código integralmente

## Parâmetros gráficos ----
png("figura_banner.png", width = 1584 * 3, height = 1018 * 3, res = 300)
#layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE))
par(mfrow = c(2,2))
windowsFonts(TNR=windowsFont("Arial"))

## IS frequência absoluta ----
par(mar = c(8, 6, 4,  2))
barplot(IS_compressed,
        col = "orangered",
        las = 2,
        ylab = "Frequência absoluta",
        main = "Distribuição de Sequências de Inserção (IS)",
        cex.main = 1.5,
        cex.names = 1,
        cex.axis = 1,
        family = "TNR")

## IS frequência transformada ----
#barplot(asin_IS,
#        col = "orangered",
#        ylim = c(0, 1),
#        las = 2,
#        ylab = "asin(sqrt(proporção))",
#        main = "Valores transformados com o arcoseno  da raiz quadrada",
#        cex.main = 1.5,
#        family = "TNR")

## Tn frequência absoluta ----
par(mar = c(8, 4, 4,  2))
barplot(t(Tn_compressed),
        ylim = c(0, 300),
        col = "springgreen4",
        main = "Distribuição de Transposons (Tn) em A. baumannii",
        ylab = "Frequência absoluta",
        cex.main = 1.5,
        las =2,
        family = "TNR")

## Cn frequência absoluta ----
par(mar = c(10, 6, 6,  4))
barplot(t(log(tab_Cn_compressed, 10)),
        ylim = c(0, 4),
        las = 2,
        cex.names = 1,
        ylab = "Frequência em logaritmo de 10",
        col = "gold1",
        cex.main = 1.5,
        main = "Distribuição por frequência dos Transposons Compostos (Cn)",
        family = "TNR",
        yaxt = "n")   # desliga o eixo Y automático

axis(2, at = seq(0, 4, by = 0.5), las = 2, family = "TNR")   # marcações de 0.5 em 0.5

## Tipos de IS mais frequentes nos Cn ----
par(mar = c(12, 4, 4,  2))
barplot(tab_Is_Cn_compressed,
        las = 2,
        ylim = c(0, 3000),
        col = "navyblue",
        cex.main = 1.5,
        main = "Principal IS flanqueadora dos Transposons Compostos",
        ylab = "Frequência absoluta",
        family = "TNR")
dev.off()

## Tabela descritiva -----
library(flextable);library(officer)

# Montagem da tabela
tabela_resumo <- data.frame(
  Elemento = c("Cn", "Tn", "IS"),
  Frequência = c(length(Cn), length(Tn), length(IS))
)

tabela_resumo$Percentual <- round(100 * tabela_resumo$Frequência / sum(tabela_resumo$Frequência), 1)

# Linha de total
tabela_resumo <- rbind(tabela_resumo,
                       data.frame(Elemento = "Total",
                                  Frequência = sum(tabela_resumo$Frequência),
                                  Percentual = 100))

# Formatação estilo artigo
tabela_flex <- flextable(tabela_resumo) %>%
  set_header_labels(Elemento = "Elemento", Frequência = "Frequência absoluta", Percentual = "Percentual (%)") %>%
  add_header_lines("Distribuição de elementos genéticos móveis identificados em A. baumannii") %>%
  add_footer_lines("Fonte: elaboração própria") %>%
  theme_booktabs() %>%
  align(align = "center", part = "all") %>%
  bold(i = nrow(tabela_resumo), part = "body") %>%   # destaca a linha de Total em negrito
  autofit()

tabela_flex
write.xlsx(tabela_resumo, "tabela_resumo.csv", rowNames = FALSE)

# RASCUNHO ----

# Estatística descritiva dos GRAs dos MGEs
GRAMGE <- read.xlsx("C:/Users/khayl/Downloads/Tabela_para_o_X-Meeting_2 (USAR ESSA).xlsx",
                    sheet = "MGE - ARGs",
                    colNames = T)
GRAs <- GRAMGE$ARGs
GRAs_tab <- as.matrix(sort(table(GRAs), decreasing = T))
porcent_gras <- prop.table(GRAs_tab)*100; colnames(porcent_gras) <- c("Freq em %")
