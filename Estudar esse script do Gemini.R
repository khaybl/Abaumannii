library(readxl)
library(tidyverse)

# 1. Leitura da aba PHASTEST
ficheiro <- "C:/Users/khayl/Downloads/Tabela_para_o_X-Meeting_2 (USAR ESSA).xlsx"
phastest_raw <- read_excel(ficheiro, sheet = "PHASTEST")

# 2. Tratamento e estruturação dos dados
phastest_clean <- phastest_raw %>%
  # Remover colunas vazias
  select(-starts_with("Unnamed")) %>%
  # Preencher o ID do genoma para as linhas das regiões
  fill(`Genoma/Acesso`, .direction = "down") %>%
  # Filtrar linhas que não contêm regiões válidas
  filter(!is.na(Region)) %>%
  # Separar a coluna de coordenadas genómicas (ex: "1186509-1241060") em Start e Stop
  separate(`Region Position`, into = c("Start", "Stop"), sep = "-", convert = TRUE) %>%
  # Limpar o nome do fago removendo o número de hits entre parênteses no final
  mutate(
    Phage_Clean = str_remove(`Most Common Phage`, "\\(\\d+\\)$"),
    Completeness = factor(Completeness, levels = c("intact", "questionable", "incomplete", "Sem fago"))
  )

# Visualizar a estrutura tratada
glimpse(phastest_clean)

# Importar a aba de ARGs
mge_args_raw <- read_excel(ficheiro, sheet = "MGE - ARGs")

args_clean <- mge_args_raw %>%
  separate(`Position Contig`, into = c("ARG_Start", "ARG_Stop"), sep = "-", convert = TRUE) %>%
  filter(!is.na(ARG_Start) & !is.na(ARG_Stop))

# Identificar ARGs localizados dentro de regiões profágicas
phage_args_overlap <- phastest_clean %>%
  inner_join(args_clean, by = c("Genoma/Acesso" = "Acession"), relationship = "many-to-many") %>%
  filter(ARG_Start >= Start & ARG_Stop <= Stop) %>%
  select(`Genoma/Acesso`, Region, Completeness, Phage_Clean, ARGs, ARG_Start, ARG_Stop, Start, Stop)

# Resumo dos genes de resistência encontrados em profagos
phage_args_summary <- phage_args_overlap %>%
  count(ARGs, Completeness, sort = TRUE)

print(phage_args_summary)

library(pheatmap)

# Selecionar os 15 profagos mais frequentes
top_phages <- phastest_clean %>%
  count(Phage_Clean, sort = TRUE) %>%
  slice_head(n = 15) %>%
  pull(Phage_Clean)

# Criar matriz binária (1 = Presente, 0 = Ausente)
matriz_fagos <- phastest_clean %>%
  filter(Phage_Clean %in% top_phages) %>%
  distinct(`Genoma/Acesso`, Phage_Clean) %>%
  mutate(presente = 1) %>%
  pivot_wider(names_from = Phage_Clean, values_from = presente, values_fill = 0) %>%
  column_to_rownames("Genoma/Acesso")

# Gerar o Heatmap
pheatmap(
  as.matrix(matriz_fagos),
  clustering_distance_rows = "binary",
  clustering_distance_cols = "binary",
  color = c("#f7f7f7", "red"),
  legend_breaks = c(0, 1),
  legend_labels = c("Ausente", "Presente"),
  fontsize_col = 8,
  show_rownames = FALSE,
  main = "Perfil de Distribuição dos Principais Profagos em Acinetobacter baumannii"
)

resumo_genomas <- phastest_clean %>%
  group_by(`Genoma/Acesso`) %>%
  summarise(
    Total_Profagos = n(),
    Profagos_Intactos = sum(Completeness == "intact"),
    Tamanho_Total_Kb = sum(`Region Length (Kb)`),
    GC_Medio_Fagos = mean(`GC (%)`)
  )

head(resumo_genomas)

ggplot(phastest_clean, aes(x = Completeness, fill = Completeness)) +
  geom_bar(width = 0.6, show.legend = F) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("intact" = "#2ca02c", "questionable" = "#ff7f0e", "incomplete" = "#d62728", "Sem fago" = "#7f7f7f")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Integridade das Regiões Profágicas Identificadas",
    x = "Classificação de Integridade (PHASTEST)",
    y = "Número de Regiões Fágicas"
  ) +
  ylim(0, max(table(phastest_clean$Completeness)) * 1.1)

ggplot(phastest_clean, aes(x = `Region Length (Kb)`, y = `GC (%)`, color = Completeness)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("intact" = "#2ca02c", "questionable" = "#ff7f0e", "incomplete" = "#d62728", "Sem fago" = "#7f7f7f")) +
  theme_bw(base_size = 12) +
  labs(
    title = "Distribuição do Tamanho e Conteúdo GC% das Regiões Fágicas",
    x = "Tamanho da Região (Kb)",
    y = "Conteúdo GC (%)",
    color = "Integridade"
  )

library(readxl)
library(tidyverse)

# 1. Leitura e Limpeza
phastest_clean <- read_excel(ficheiro, sheet = "PHASTEST") %>%
  select(-starts_with("Unnamed")) %>%
  fill(`Genoma/Acesso`, .direction = "down") %>%
  filter(!is.na(Region)) %>%
  separate(`Region Position`, into = c("Start", "Stop"), sep = "-", convert = TRUE) %>%
  # Converte explicitamente as colunas numéricas para evitar o erro de 'character'
  mutate(
    `Region Length (Kb)` = as.numeric(`Region Length (Kb)`),
    `GC (%)`             = as.numeric(`GC (%)`),
    Score                = as.numeric(Score),
    `Total Proteins`     = as.numeric(`Total Proteins`),
    Phage_Clean          = str_remove(`Most Common Phage`, "\\(\\d+\\)$"),
    Completeness         = factor(Completeness, levels = c("intact", "questionable", "incomplete", "Sem fago"))
  )

# 2. Resumo por Genoma
resumo_genomas <- phastest_clean %>%
  group_by(`Genoma/Acesso`) %>%
  summarise(
    Total_Profagos    = n(),
    Profagos_Intactos = sum(Completeness == "intact", na.rm = TRUE),
    Tamanho_Total_Kb  = sum(`Region Length (Kb)`, na.rm = TRUE),
    GC_Medio_Fagos    = mean(`GC (%)`, na.rm = TRUE)
  )

# Visualizar o resultado
head(resumo_genomas)

library(tidyverse)
library(readxl)

phastest_clean <- read_excel(ficheiro, sheet = "PHASTEST") %>%
  select(-starts_with("Unnamed")) %>%
  fill(`Genoma/Acesso`, .direction = "down") %>%
  filter(!is.na(Region)) %>%
  # Forçar a conversão de texto para número
  mutate(
    `Region Length (Kb)` = as.numeric(`Region Length (Kb)`),
    `GC (%)`             = as.numeric(`GC (%)`),
    Completeness         = factor(Completeness, levels = c("intact", "questionable", "incomplete", "Sem fago"))
  )

# Gráfico corrigido com eixos contínuos e legíveis
ggplot(phastest_clean, aes(x = `Region Length (Kb)`, y = `GC (%)`, color = Completeness)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("intact" = "#2ca02c", "questionable" = "#ff7f0e", "incomplete" = "#d62728", "Sem fago" = "#7f7f7f")) +
  theme_bw(base_size = 12) +
  labs(
    title = "Distribuição do Tamanho e Conteúdo GC% das Regiões Fágicas",
    x = "Tamanho da Região (Kb)",
    y = "Conteúdo GC (%)",
    color = "Integridade"
  )
