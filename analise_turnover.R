# ==============================================================================
# PROJETO: People Analytics na Prática - Capítulo 11
# SCRIPT: analise_turnover.R
# OBJETIVO: Pipeline completo de People Analytics para predição de Turnover
#           Voluntário (Data Wrangling, EDA, Regressão Logística e Avaliação)
# AUTOR: Ilton Luiz Assi (Orgdata Consulting / Gestão de Pessoas Brasil)
# DATA: 2026
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CONFIGURAÇÃO DO AMBIENTE E CARREGAMENTO DE PACOTES
# ------------------------------------------------------------------------------
# Limpar ambiente de trabalho para garantir reprodutibilidade
rm(list = ls())
options(scipen = 999) # Evitar notação científica nas saídas estatísticas

# Lista de pacotes necessários para o pipeline
pacotes_necessarios <- c(
  "tidyverse",  # Manipulação de dados (dplyr, tidyr) e visualização (ggplot2)
  "readxl",     # Leitura de arquivos Excel (.xlsx)
  "janitor",    # Padronização de nomes de colunas e tabelas de frequência
  "skimr",      # Sumário estatístico detalhado
  "caret",      # Particionamento de dados, matriz de confusão e métricas de ML
  "pROC",       # Curva ROC e cálculo de AUC
  "broom",      # Organização de saídas de modelos estatísticos em data frames
  "car"         # Diagnóstico de multicolinearidade (VIF)
)

# Instalar automaticamente pacotes ausentes e carregá-los
pacotes_ausentes <- pacotes_necessarios[!(pacotes_necessarios %in% installed.packages()[, "Package"])]
if (length(pacotes_ausentes) > 0) {
  install.packages(pacotes_ausentes, dependencies = TRUE)
}

invisible(lapply(pacotes_necessarios, library, character.only = TRUE))

cat("Ambiente configurado e pacotes carregados com sucesso.\n\n")

# ------------------------------------------------------------------------------
# 2. INGESTÃO E INTEGRAÇÃO DAS BASES DE DADOS (HRIS + PESQUISA DE CLIMA)
# ------------------------------------------------------------------------------
# Nota: Caso os arquivos estejam na pasta '02-datasets/', ajuste o caminho abaixo.
caminho_master <- "Dataset_1_Employee_Master_nomes.xlsx"
caminho_survey <- "Dataset_2_Engagement_Survey.xlsx"

# Verificação de segurança da existência dos arquivos na raiz ou subpasta
if (!file.exists(caminho_master)) {
  caminho_master <- "02-datasets/Dataset_1_Employee_Master_nomes.xlsx"
}
if (!file.exists(caminho_survey)) {
  caminho_survey <- "02-datasets/Dataset_2_Engagement_Survey.xlsx"
}

# Importação das planilhas
df_master_raw <- read_excel(caminho_master) %>% clean_names()
df_survey_raw <- read_excel(caminho_survey) %>% clean_names()

cat("Dimensões Dataset Master:", nrow(df_master_raw), "linhas x", ncol(df_master_raw), "colunas\n")
cat("Dimensões Dataset Engagement:", nrow(df_survey_raw), "linhas x", ncol(df_survey_raw), "colunas\n\n")

# Inspeção inicial da estrutura (descomente se desejar visualizar no console)
# glimpse(df_master_raw)
# glimpse(df_survey_raw)

# Identificar dinamicamente a chave primária (ex: id_funcionario, employee_id, matricula)
chave_join <- intersect(names(df_master_raw), names(df_survey_raw))[1]
cat("Chave de integração identificada entre as bases:", chave_join, "\n\n")

# Integração (Left Join) mantendo toda a base cadastral como referência
df_integrado <- df_master_raw %>%
  left_join(df_survey_raw, by = chave_join)

# ------------------------------------------------------------------------------
# 3. LIMPEZA, TRATAMENTO DE DADOS E ENGENHARIA DE ATRIBUTOS (FEATURE ENGINEERING)
# ------------------------------------------------------------------------------
cat("Iniciando limpeza e tratamento de dados...\n")

# 3.1. Remoção de registros duplicados com base na chave do colaborador
df_limpo <- df_integrado %>%
  distinct(across(all_of(chave_join)), .keep_all = TRUE)

# 3.2. Padronização da variável-alvo (Target: Turnover / Desligamento)
# Nota: Adapte o nome da coluna abaixo caso sua planilha utilize 'status', 'attrition' ou 'desligamento'
nome_coluna_target <- names(df_limpo)[str_detect(names(df_limpo), "turnover|attrition|deslig|status")][1]
cat("Variável-alvo detectada:", nome_coluna_target, "\n")

df_limpo <- df_limpo %>%
  rename(target_raw = all_of(nome_coluna_target)) %>%
  mutate(
    # Converte respostas textuais ("Sim", "Yes", "Desligado", "1") em fator binário (0 = Ativo, 1 = Turnover)
    turnover = case_when(
      str_to_lower(as.character(target_raw)) %in% c("sim", "yes", "1", "true", "desligado", "saiu") ~ 1,
      str_to_lower(as.character(target_raw)) %in% c("não", "nao", "no", "0", "false", "ativo", "ficou") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(turnover)) # Remove registros sem definição de status

# 3.3. Tratamento de Valores Ausentes (Missing Values / NAs)
# Estratégia: Imputação pela mediana para variáveis numéricas e moda/"Não Informado" para categóricas
calcular_moda <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

df_limpo <- df_limpo %>%
  mutate(
    across(where(is.numeric) & !all_of("turnover"), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)),
    across(where(is.character), ~ ifelse(is.na(.), "Nao_Informado", str_trim(.)))
  )

# 3.4. Remoção de colunas identificadoras ou com variância zero (sem poder preditivo)
# Exclui nomes pessoais (LGPD / Privacy by Design) e IDs antes da modelagem
colunas_remover <- names(df_limpo)[str_detect(names(df_limpo), "nome|name|cpf|email|target_raw")]
colunas_remover <- unique(c(colunas_remover, chave_join))

df_modelagem <- df_limpo %>%
  select(-any_of(colunas_remover)) %>%
  mutate(
    across(where(is.character), as.factor),
    turnover = factor(turnover, levels = c(0, 1), labels = c("Retido", "Turnover"))
  )

# Remove colunas fatoriais que possuam apenas 1 nível único (evita erro na regressão)
df_modelagem <- df_modelagem %>%
  select(where(~ !(is.factor(.) && nlevels(.) < 2)))

cat("Base final pronta para modelagem:", nrow(df_modelagem), "observações e", ncol(df_modelagem), "variáveis.\n\n")

# ------------------------------------------------------------------------------
# 4. ANÁLISE EXPLORATÓRIA DE DADOS (EDA) E DIAGNÓSTICO DE DESBALANCEAMENTO
# ------------------------------------------------------------------------------
cat("Distribuição da variável-alvo (Turnover):\n")
tabela_target <- df_modelagem %>%
  tabyl(turnover) %>%
  adorn_pct_formatting(digits = 2)
print(tabela_target)
cat("\n")

# Visualização 1: Taxa de Turnover na base
grafico_target <- ggplot(df_modelagem, aes(x = turnover, fill = turnover)) +
  geom_bar(width = 0.5, show.legend = FALSE) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("Retido" = "#2C3E50", "Turnover" = "#E74C3C")) +
  labs(
    title = "Distribuição de Retenção vs. Turnover Voluntário",
    subtitle = "Diagnóstico inicial de balanceamento de classes",
    x = "Status do Colaborador",
    y = "Contagem de Colaboradores"
  ) +
  theme_minimal(base_size = 12)

print(grafico_target)

# ------------------------------------------------------------------------------
# 5. PARTICIONAMENTO DOS DADOS (TREINO E TESTE ESTRATIFICADOS)
# ------------------------------------------------------------------------------
# Semente aleatória fixa para garantir que os resultados sejam replicáveis em sala/empresa
set.seed(2026)

# Divisão 70% Treino / 30% Teste mantendo a proporção exata de Turnover em ambos
indice_treino <- createDataPartition(df_modelagem$turnover, p = 0.70, list = FALSE)

dados_treino <- df_modelagem[indice_treino, ]
dados_teste  <- df_modelagem[-indice_treino, ]

cat("Amostra de Treino:", nrow(dados_treino), "registros\n")
cat("Amostra de Teste: ", nrow(dados_teste), "registros\n\n")

# ------------------------------------------------------------------------------
# 6. MODELAGEM PREDITIVA: REGRESSÃO LOGÍSTICA BINÁRIA
# ------------------------------------------------------------------------------
cat("Ajustando modelo de Regressão Logística...\n")

# Ajuste do modelo completo com todas as variáveis preditoras disponíveis
modelo_logistico <- glm(
  formula = turnover ~ .,
  data = dados_treino,
  family = binomial(link = "logit")
)

# Sumário estatístico clássico (Coeficientes Log-Odds, Erro Padrão, Valor-z e p-valor)
summary(modelo_logistico)

# 6.1. Seleção de variáveis parcimoniosa via Stepwise (critério AIC)
# Remove ruído e mantém apenas os preditores que realmente explicam o turnover
modelo_refinado <- step(modelo_logistico, direction = "both", trace = FALSE)

cat("\n--- SUMÁRIO DO MODELO REFINADO (STEPWISE AIC) ---\n")
print(summary(modelo_refinado))

# 6.2. Diagnóstico de Multicolinearidade (VIF - Variance Inflation Factor)
# Valores de VIF > 5 indicam colinearidade problemática entre preditoras
if (length(coef(modelo_refinado)) > 2) {
  cat("\n--- DIAGNÓSTICO DE MULTICOLINEARIDADE (VIF) ---\n")
  print(vif(modelo_refinado))
}

# ------------------------------------------------------------------------------
# 7. INTERPRETABILIDADE PARA RH: CÁLCULO DE ODDS RATIOS (RAZÃO DE CHANCES)
# ------------------------------------------------------------------------------
# Em People Analytics, gestores não tomam decisão com "Log-Odds".
# Exponenciamos os coeficientes (exp(beta)) para obter a Razão de Chances (Odds Ratio).

tabela_odds_ratio <- tidy(modelo_refinado, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    impacto_percentual = (estimate - 1) * 100,
    significancia = case_when(
      p.value < 0.001 ~ "*** (Altamente Significativo)",
      p.value < 0.01  ~ "**  (Muito Significativo)",
      p.value < 0.05  ~ "*   (Significativo)",
      p.value < 0.10  ~ ".   (Marginal)",
      TRUE            ~ "ns  (Não Significativo)"
    )
  ) %>%
  arrange(p.value)

cat("\n--- FATORES DE RISCO E PROTEÇÃO (ODDS RATIOS) ---\n")
print(tabela_odds_ratio %>% select(term, estimate, conf.low, conf.high, p.value, significancia))

# Visualização 2: Forest Plot dos Odds Ratios (Direcionadores do Turnover)
grafico_or <- tabela_odds_ratio %>%
  filter(p.value < 0.10) %>% # Exibe fatores relevantes estatisticamente
  ggplot(aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3.5, color = "#1F618D") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "#1F618D") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#C0392B", linewidth = 0.8) +
  coord_flip() +
  labs(
    title = "Direcionadores de Turnover (Odds Ratios com IC 95%)",
    subtitle = "Valores > 1 aumentam o risco de saída | Valores < 1 atuam como fator de retenção",
    x = "Variável Preditora",
    y = "Odds Ratio (Razão de Chances)"
  ) +
  theme_minimal(base_size = 11)

print(grafico_or)

# ------------------------------------------------------------------------------
# 8. PREDIÇÃO NA BASE DE TESTE E AVALIAÇÃO VIA MATRIZ DE CONFUSÃO
# ------------------------------------------------------------------------------
# Previsão das probabilidades contínuas de turnover (entre 0 e 1) nos dados inéditos
probabilidades_teste <- predict(modelo_refinado, newdata = dados_teste, type = "response")

# 8.1. Avaliação com Ponto de Corte Padrão (Threshold = 0.50)
ponto_corte_padrao <- 0.50
predicao_classe_50 <- factor(
  ifelse(probabilidades_teste >= ponto_corte_padrao, "Turnover", "Retido"),
  levels = c("Retido", "Turnover")
)

cat("\n====================================================================\n")
cat("MATRIZ DE CONFUSÃO - PONTO DE CORTE PADRÃO (0.50)\n")
cat("====================================================================\n")
matriz_confusao_50 <- confusionMatrix(
  data = predicao_classe_50,
  reference = dados_teste$turnover,
  positive = "Turnover"
)
print(matriz_confusao_50)

# 8.2. Curva ROC e Cálculo da Área Sob a Curva (AUC)
curva_roc <- roc(
  response = dados_teste$turnover,
  predictor = probabilidades_teste,
  levels = c("Retido", "Turnover"),
  direction = "<"
)

auc_valor <- auc(curva_roc)
cat("\nÁrea Sob a Curva ROC (AUC):", round(auc_valor, 4), "\n")

# Plot da Curva ROC
plot(
  curva_roc,
  col = "#2980B9",
  lwd = 3,
  main = paste0("Curva ROC - Predição de Turnover (AUC = ", round(auc_valor, 3), ")")
)
abline(a = 0, b = 1, lty = 2, col = "gray50")

# ------------------------------------------------------------------------------
# 9. OTIMIZAÇÃO DE DECISÃO PARA RH (AJUSTE DE THRESHOLD VIA ÍNDICE DE YOUDEN)
# ------------------------------------------------------------------------------
# Em RH, o custo de um Falso Negativo (perder um talento que o modelo não alertou)
# costuma ser maior do que o custo de uma ação preventiva de retenção.
# Buscamos o ponto de corte ótimo que equilibra Sensibilidade e Especificidade.

melhor_corte <- coords(
  curva_roc,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity")
)

threshold_otimo <- as.numeric(melhor_corte["threshold"])
cat("\nPonto de Corte Otimizado (Critério de Youden):", round(threshold_otimo, 4), "\n")

predicao_classe_otima <- factor(
  ifelse(probabilidades_teste >= threshold_otimo, "Turnover", "Retido"),
  levels = c("Retido", "Turnover")
)

cat("\n====================================================================\n")
cat("MATRIZ DE CONFUSÃO - PONTO DE CORTE OTIMIZADO PARA RH (", round(threshold_otimo, 2), ")\n")
cat("====================================================================\n")
matriz_confusao_otima <- confusionMatrix(
  data = predicao_classe_otima,
  reference = dados_teste$turnover,
  positive = "Turnover"
)
print(matriz_confusao_otima)

# ------------------------------------------------------------------------------
# 10. GERAÇÃO DE LISTA ACIONÁVEL DE RISCO (SCORING DE COLABORADORES ATIVOS)
# ------------------------------------------------------------------------------
# Aplica o modelo nos colaboradores atualmente ativos para priorizar ações de RH
colaboradores_ativos <- df_limpo %>%
  filter(turnover == 0)

if (nrow(colaboradores_ativos) > 0) {
  ativos_para_score <- colaboradores_ativos %>%
    select(any_of(names(dados_treino))) %>%
    mutate(across(where(is.character), as.factor))

  colaboradores_ativos$probabilidade_turnover <- predict(
    modelo_refinado,
    newdata = ativos_para_score,
    type = "response"
  )

  ranking_risco <- colaboradores_ativos %>%
    mutate(
      faixa_risco = case_when(
        probabilidade_turnover >= threshold_otimo ~ "Alto Risco (Ação Imediata)",
        probabilidade_turnover >= (threshold_otimo * 0.65) ~ "Médio Risco (Monitorar)",
        TRUE ~ "Baixo Risco"
      )
    ) %>%
    arrange(desc(probabilidade_turnover))

  cat("\nResumo da Classificação de Risco entre Colaboradores Ativos:\n")
  print(table(ranking_risco$faixa_risco))
}

cat("\nPipeline de People Analytics executado com sucesso!\n")
