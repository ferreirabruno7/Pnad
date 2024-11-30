# Carregar pacotes necessários
library(PNADcIBGE)
library(dplyr)
library(ggplot2)
library(scales)

# Função para processar dados da PNAD Contínua e calcular métricas
processar_pnad <- function(year, quarter) {
  # Baixar os microdados
  dados <- get_pnadc(
    year = year,
    quarter = quarter,
    vars = c(
      "V1028", "VD4016", "VD4017", "VD4031", "V4010", "V4013", "V4012", "V4041",
      "V4044", "V4043", "V2007", "V2009", "V3009A", "VD4020", "UF"
    ),
    labels = FALSE,
    design = FALSE
  )
  
  # Converter variáveis para numéricas
  dados <- dados %>%
    mutate(
      V4010 = as.numeric(V4010),
      V4013 = as.numeric(V4013),
      V4012 = as.numeric(V4012),
      V4041 = as.numeric(V4041),
      V4044 = as.numeric(V4044),
      V4043 = as.numeric(V4043),
      V1028 = as.numeric(V1028),  # Peso amostral
      VD4016 = as.numeric(VD4016), # Rendimento habitual
      VD4017 = as.numeric(VD4017), # Rendimento efetivo
      VD4031 = as.numeric(VD4031), # Horas trabalhadas
      V2007 = as.numeric(V2007),  # Sexo
      V2009 = as.numeric(V2009),  # Idade
      V3009A = as.numeric(V3009A), # Escolaridade
      VD4020 = as.numeric(VD4020),  # Rendimento mensal efetivo
      UF = as.numeric(UF)  # Unidade da Federação
    )
  
  # Filtrar trabalhadores por conta própria
  conta_propria <- dados %>%
    filter(V4012 == 6 | V4043 == 6)
  
  # Calcular resumo das variáveis
  resumo <- conta_propria %>%
    summarise(
      total_trabalhadores = sum(V1028, na.rm = TRUE),
      passageiros = sum((V4010 %in% c(8321, 8322)) * V1028, na.rm = TRUE),
      mercadorias = sum((V4010 %in% c(9331)) * V1028, na.rm = TRUE),
      renda_habitual_passageiros = sum(VD4016 * (V4010 %in% c(8321, 8322)) * V1028, na.rm = TRUE) /
        sum((V4010 %in% c(8321, 8322)) * V1028, na.rm = TRUE),
      renda_habitual_mercadorias = sum(VD4016 * (V4010 %in% c(9331)) * V1028, na.rm = TRUE) /
        sum((V4010 %in% c(9331)) * V1028, na.rm = TRUE),
      renda_efetiva_passageiros = sum(VD4017 * (V4010 %in% c(8321, 8322)) * V1028, na.rm = TRUE) /
        sum((V4010 %in% c(8321, 8322)) * V1028, na.rm = TRUE),
      renda_efetiva_mercadorias = sum(VD4017 * (V4010 %in% c(9331)) * V1028, na.rm = TRUE) /
        sum((V4010 %in% c(9331)) * V1028, na.rm = TRUE),
      horas_trabalhadas_passageiros = sum(VD4031 * (V4010 %in% c(8321, 8322)) * V1028, na.rm = TRUE) /
        sum((V4010 %in% c(8321, 8322)) * V1028, na.rm = TRUE),
      horas_trabalhadas_mercadorias = sum(VD4031 * (V4010 %in% c(9331)) * V1028, na.rm = TRUE) /
        sum((V4010 %in% c(9331)) * V1028, na.rm = TRUE),
      proporcao_mulheres = sum((V2007 == 2) * V1028, na.rm = TRUE) / sum(V1028, na.rm = TRUE),
      proporcao_homens = sum((V2007 == 1) * V1028, na.rm = TRUE) / sum(V1028, na.rm = TRUE),
      media_idade = sum(V2009 * V1028, na.rm = TRUE) / sum(V1028, na.rm = TRUE),
      rendimento_medio_efetivo = sum(VD4020 * V1028, na.rm = TRUE) / sum(V1028, na.rm = TRUE),
      proporcao_superior = sum((V3009A == 12) * V1028, na.rm = TRUE) / sum(V1028, na.rm = TRUE)
    ) %>%
    mutate(ano = year, trimestre = quarter)
  
  return(resumo)
}

# Definir opções para evitar avisos sobre métodos de download depreciados
options(download.file.method = "libcurl")

# Loop para processar dados de 2015 a 2021
anos <- 2015:2021
trimestres <- 1:4
resultados <- list()

for (ano in anos) {
  for (trimestre in trimestres) {
    cat("Processando ano:", ano, "trimestre:", trimestre, "\n")
    resultados[[paste(ano, trimestre, sep = "_")]] <- processar_pnad(ano, trimestre)
  }
}

# Consolidar os resultados em um único data frame
dados_totais <- bind_rows(resultados)

# Transformar ano e trimestre em um formato amigável para gráficos
dados_totais <- dados_totais %>%
  mutate(periodo = paste(ano, "T", trimestre, sep = ""))

# Visualizar tabela resumida
print(dados_totais)

# Gráficos
## Rendimento Habitual e Efetivo
ggplot(dados_totais, aes(x = periodo)) +
  geom_line(aes(y = renda_habitual_passageiros, color = "Habitual Passageiros", group = 1), linewidth = 1.2) +
  geom_line(aes(y = renda_habitual_mercadorias, color = "Habitual Mercadorias", group = 1), linewidth = 1.2) +
  geom_line(aes(y = renda_efetiva_passageiros, color = "Efetiva Passageiros", group = 1), linetype = "dashed", linewidth = 1.2) +
  geom_line(aes(y = renda_efetiva_mercadorias, color = "Efetiva Mercadorias", group = 1), linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Evolução dos Rendimentos no Transporte (Conta Própria)",
    subtitle = "Brasil, 2015 a 2021",
    x = "Período (Ano e Trimestre)",
    y = "Rendimento Médio (R$)",
    color = "Tipo de Rendimento"
  ) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Horas Trabalhadas
ggplot(dados_totais, aes(x = periodo)) +
  geom_line(aes(y = horas_trabalhadas_passageiros, color = "Passageiros", group = 1), linewidth = 1.2) +
  geom_line(aes(y = horas_trabalhadas_mercadorias, color = "Mercadorias", group = 1), linewidth = 1.2) +
  labs(
    title = "Evolução das Horas Trabalhadas no Transporte (Conta Própria)",
    subtitle = "Brasil, 2015 a 2021",
    x = "Período (Ano e Trimestre)",
    y = "Horas Trabalhadas por Semana",
    color = "Setor"
  ) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Proporção de Homens e Mulheres
ggplot(dados_totais, aes(x = periodo)) +
  geom_line(aes(y = proporcao_mulheres, color = "Mulheres", group = 1), linewidth = 1.2) +
  geom_line(aes(y = proporcao_homens, color = "Homens", group = 1), linewidth = 1.2) +
  labs(
    title = "Proporção de Homens e Mulheres no Transporte (Conta Própria)",
    subtitle = "Brasil, 2015 a 2021",
    x = "Período (Ano e Trimestre)",
    y = "Proporção",
    color = "Sexo"
  ) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Proporção de Nível Superior
ggplot(dados_totais, aes(x = periodo)) +
  geom_line(aes(y = proporcao_superior, color = "Nível Superior", group = 1), linewidth = 1.2) +
  labs(
    title = "Proporção de Trabalhadores com Nível Superior",
    subtitle = "Brasil, 2015 a 2021",
    x = "Período (Ano e Trimestre)",
    y = "Proporção",
    color = "Indicador"
  ) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Idade Média dos Trabalhadores
ggplot(dados_totais, aes(x = periodo)) +
  geom_line(aes(y = media_idade, color = "Idade Média", group = 1), linewidth = 1.2) +
  labs(
    title = "Idade Média dos Trabalhadores no Transporte (Conta Própria)",
    subtitle = "Brasil, 2015 a 2021",
    x = "Período (Ano e Trimestre)",
    y = "Idade Média (anos)",
    color = "Indicador"
  ) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

