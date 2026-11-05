# IMPORTAÇÃO DE TABELAS ----

## Pacotes ----
library(openxlsx); library(VennDiagram); library(tidyverse); library(plotrix); library(lessR)

## Caminho do arquivo ----
ficheiro <- "C:/Users/khayl/Downloads/Tabela_para_o_X-Meeting_2 (USAR ESSA).xlsx"

## Tabela de caracterização ----
caracter <- read.xlsx(ficheiro,
                      sheet = "Caracterização",
                      colNames = T,
                      rowNames = T)

## Tabela de resistoma (genomas) ----
genomas <- read.xlsx(ficheiro,
                     colNames = T,
                     rowNames = T,
                     sheet = "Resistoma - Quantitativo - 535")

## Tabela de elementos ----
elementos <- read.xlsx(ficheiro,
                       colNames = T,
                       rowNames = F,
                       sheet = "Elementos - 535"); elementos$X5 <- NULL; colnames(elementos) <- c("IS", "Cn", "Tn", "ICE")

## Tabela de prófagos (PHASTEST) ----
fagos <- read.xlsx(ficheiro,
                   colNames = T,
                   rowNames = F,
                   sheet = "PHASTEST")

# ESTAT DE CARACTERIZAÇÃO ----

## Tamanho amostral ----
length(caracter$`GEN.BANK(CROMO)`)

## Local de isolamento ----
tab_carac <- t(as.matrix(sort(table(caracter$HOSPEDEIRO), decreasing = T))) %>% print()
sum(tab_carac)

Human <- (sum(tab_carac != "Humano", na.rm = T)-sum(tab_carac)) * -1
#                          ↑ Parametro: Tudo que está relacionado a humano na catalogação; Contagem feita com a remoção do inverso e subtração do total

Non.human <- sum(tab_carac != "Humano", na.rm = T)
tab_carac <- round(prop.table(rbind(Human, Non.human))*100) ; colnames(tab_carac) <- c("Freq em %") ; print(tab_carac)
#            ↑ Transformação em uma tabela  de porcentagem

### Limpeza de intermediários ----
rm(Human, Non.human)

## Genomas com ao menos 1 plasmídeo ----
length(caracter$NÚMERO.DE.P[caracter$NÚMERO.DE.P>0])

# ESTAT DE ELEMENTOS ----

## Extração de IS ----

### Tabelamento e recorte ----
IS <- elementos$IS # Isolamento da coluna
tab_IS <- as.matrix(sort(table(IS), decreasing = T)); tab_IS # Tabelamento para descrição

nrow(tab_IS) # N de tipos

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
colnames(ISmnrq80_matrix) <- c(" "); rownames(ISmnrq80_matrix) <- c("IS com freq <80"); ISmnrq80_matrix

IS_compressed <- rbind(IS_maiorq80, ISmnrq80_matrix); IS_compressed
colnames(IS_compressed) <- c("Freq Absoluta")
IS_compressed <- t(IS_compressed);print(IS_compressed)

porcent_IS_comp <- prop.table(IS_compressed, margin = NULL)*100; rownames(porcent_IS_comp) <- c("Freq em %"); print(porcent_IS_comp)

### Plot de frequência absoluta ----
par(mar = c(10, 6, 4, 1))
barplot(IS_compressed,
        col = "orangered",
        las = 2,
        ylim = c(0,((max(IS_compressed)+500))),
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

### Limpeza de intermediários ----
# Tópico para limpeza de environment de elementos de IS
# A definir
rm(IS_maiorq80, ISmnrq80_matrix, names_maiorq)

## Extração de Tn ----

### Tabelamento e recorte ----
Tn <- as.matrix(na.omit(elementos$Tn))
Tn
tab_Tn <- as.matrix(sort(table(Tn), decreasing = T)); tab_Tn
colnames(tab_Tn) <- c("Freq absoluta")
nrow(tab_Tn)

porcent_Tn <- prop.table(tab_Tn)*100; colnames(porcent_Tn) <- c("Freq em %")

sum(tab_Tn)

### Plot de frequência absoluta ----
par(mar = c(8, 6, 4, 1))
barplot(t(tab_Tn),
        ylim = c(0, 160),
        col = "springgreen4",
        main = "Distribuição de Transposons em A. baumannii",
        ylab = "Frequência absoluta",
        las =2,
        family = "serif")

barplot(t(porcent_Tn),
        ylim = c(0, 100),
        col = "springgreen4",
        main = "Distribuição de Transposons em A. baumannii",
        ylab = "Frequência em %",
        las =2,
        family = "serif")

## Extração de Cn ----

### Tabelamento e recorte ----
Cn <- as.matrix(na.omit(elementos$Cn))
Cn
tab_Cn <- as.matrix(sort(table(Cn), decreasing = T)); tab_Cn

nrow(tab_Cn)

Cnmaiorq20 <- as.matrix(sort(tab_Cn[tab_Cn > 20], decreasing = T)); print(Cnmaiorq20)
Cn_names <- rownames(tab_Cn);Cn_names <- Cn_names[1:21]
rownames(Cnmaiorq20) <- c(Cn_names); colnames(Cnmaiorq20) <- c("Frequência absoluta")

Cnmnrq20 <- as.matrix(sum(tab_Cn[tab_Cn <= 20]))
# Cnmnrq20_expanded <- as.matrix(tab_Cn[tab_Cn <= 20]) # Reativar se eu precisar fazer um gráfico de classes raras
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
barplot(t(porcent_Cn),
        ylim = c(0, 100),
        las = 2,
        cex.names = 0.5,
        col = "gold1",
        ylab = "Freq em %",
        main = "Distribuição por frequência dos Transposons Compostos")

### Skewness e curtose ----
moments::skewness(tab_Cn);moments::kurtosis(tab_Cn)

### Cn mais frequentes ----
# Top 5
tab_Cn_compressed[1:3,]

### Limpeza de intermediários ----
rm(Cnmaiorq20, Cnmnrq20, Cn_names)

## IS flanqueadoras dos Cn (inferência) ----

### Tabelamento e recorte ----
# Tipos de IS mais frequentes nos Cn por inferência
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

### Plot de frequência absoluta ----
par(mar=c(6,6,4,2))
barplot(tab_Is_Cn_compressed,
        las = 2,
        ylim = c(0, 3000),
        col = "navyblue",
        cex.main = 1.5,
        main = "Principal IS flanqueadora dos 
Transposons Compostos inferidos",
        ylab = "Frequência absoluta",
        family = "serif")
