# Carregar e instalar pacotes necessários
required_packages <- c("PNADcIBGE", "dplyr", "ggplot2", "scales", 
                       "tidyr", "zoo", "survey", "MatchIt", "cobalt", "knitr")
installed_packages <- rownames(installed.packages())

for(p in required_packages){
  if(!(p %in% installed_packages)){
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

# Função para processar dados individuais da PNAD Contínua
processar_pnad_individual <- function(year, quarter) {
  # Baixar os microdados
  dados <- get_pnadc(
    year = year,
    quarter = quarter,
    vars = c(
      "V1028",      # Peso amostral
      "VD4016",     # Rendimento habitual
      "VD4017",     # Rendimento efetivo
      "VD4031",     # Horas trabalhadas
      "V4010",      # Código da ocupação
      "V4013",      # Código da ocupação secundária
      "V4012",      # Tipo de ocupação principal
      "V4041",      # Código da ocupação secundária
      "V4044",      # Código da ocupação secundária
      "V4043",      # Tipo de ocupação secundária
      "V2007",      # Sexo
      "V2009",      # Idade
      "V2010",      # Cor ou raça
      "V3009A",     # Escolaridade
      "VD4020",     # Rendimento mensal efetivo
      "UF"          # Unidade da Federação
    ),
    labels = FALSE,
    design = FALSE
  )
  
  # Converter variáveis para numéricas onde aplicável
  dados <- dados %>%
    mutate(across(c(
      V4010, V4013, V4012, V4041, V4044, V4043,
      V1028, VD4016, VD4017, VD4031,
      V2007, V2009, V2010,
      V3009A,
      VD4020, UF
    ), as.numeric))
  
  # Filtrar trabalhadores por conta própria
  conta_propria <- dados %>%
    filter(V4012 == 6 | V4043 == 6) %>%
    mutate(
      transporte_passageiros = ifelse(
        (V4010 %in% c(8321, 8322) & V4013 == 49030) |
          (V4041 %in% c(8321, 8322) & V4044 == 49030),
        1, 0
      ),
      transporte_mercadorias = ifelse(
        (V4010 %in% c(9331) & V4013 %in% c(49040, 53002)) |
          (V4041 %in% c(9331) & V4044 %in% c(49040, 53002)),
        1, 0
      )
    )
  
  # Retornar os dados individuais filtrados
  return(conta_propria)
}

# Definir opções para evitar avisos sobre métodos de download depreciados
options(download.file.method = "libcurl")

# Definir intervalos de anos e trimestres
anos <- 2015:2023
trimestres <- 1:4
resultados_individual <- list()

# Loop para processar dados individuais
for (ano in anos) {
  for (trimestre in trimestres) {
    cat("Processando ano:", ano, "trimestre:", trimestre, "\n")
    tryCatch({
      dados_individual <- processar_pnad_individual(ano, trimestre)
      dados_individual$ano <- ano
      dados_individual$trimestre <- trimestre
      resultados_individual[[paste(ano, trimestre, sep = "_")]] <- dados_individual
    }, error = function(e){
      cat("Erro no ano:", ano, "trimestre:", trimestre, "\n")
      # Opcional: registrar erros para posterior análise
    })
  }
}

# Consolidar os dados individuais em um único data frame
dados_completos <- bind_rows(resultados_individual)

# Verificar a estrutura dos dados
str(dados_completos)

# Definir a variável de tratamento
dados_completos$tratamento <- ifelse(dados_completos$transporte_passageiros == 1, 1, 0)

# Selecionar as covariáveis para o Propensity Score Model
covariaveis_psm <- c(
  "V2007",      # Sexo
  "V2009",      # Idade
  "V2010",      # Cor/Raça
  "V3009A",     # Escolaridade
  "UF",         # Unidade da Federação
  "VD4016",     # Rendimento Habitual
  "VD4031"      # Horas Trabalhadas
)

# Converter 'UF' para fator (se ainda não estiver)
dados_completos$UF <- as.factor(dados_completos$UF)

# Criar dummies para 'UF' (necessário para modelos de regressão)
dados_completos <- dados_completos %>%
  mutate(UF = as.factor(UF)) %>%
  # Utilizar a função model.matrix para criar dummies, removendo a interceptação
  cbind(model.matrix(~ UF - 1, data = dados_completos))

# Tratar valores ausentes nas covariáveis
dados_completos_clean <- dados_completos %>%
  filter(!is.na(V3009A) & !is.na(VD4016))

# Verificar novamente se há valores ausentes
sum(is.na(dados_completos_clean$V3009A))  # Deve ser 0
sum(is.na(dados_completos_clean$VD4016))  # Deve ser 0

# Estimar o Propensity Score usando MatchIt
set.seed(123)  # Para reprodutibilidade

matchit_psm <- matchit(
  tratamento ~ V2007 + V2009 + V2010 + V3009A + UF + VD4016 + VD4031,
  data = dados_completos_clean,
  method = "nearest",       # Método de matching por vizinho mais próximo
  ratio = 1,                # 1 controle para cada tratado
  replace = FALSE           # Sem reposição
)

# Avaliar o balanceamento das covariáveis
balance_table <- bal.tab(matchit_psm, un = TRUE, m.threshold = 0.25)
print(balance_table)

# Extrair os dados matchados com os pesos
dados_matched <- match.data(matchit_psm)

# Criar um novo peso final como o produto dos pesos de matching e os pesos amostrais originais (V1028)
dados_matched$final_weight <- dados_matched$weights * dados_matched$V1028

# Criar o objeto de design amostral com os pesos finais
design_matched <- svydesign(
  ids = ~1,                        # Sem hierarquia de cluster
  weights = ~final_weight,         # Pesos finais combinados
  data = dados_matched             # Dados matchados
)

# Estimar a média da renda efetiva (VD4017) e habitual (VD4016) para tratados e controles
media_renda_efetiva_tratado <- svymean(~VD4017, subset(design_matched, tratamento == 1))
media_renda_efetiva_controle <- svymean(~VD4017, subset(design_matched, tratamento == 0))

media_renda_habitual_tratado <- svymean(~VD4016, subset(design_matched, tratamento == 1))
media_renda_habitual_controle <- svymean(~VD4016, subset(design_matched, tratamento == 0))

# Calcular o ATT para renda efetiva e habitual
ATT_efetiva <- coef(media_renda_efetiva_tratado) - coef(media_renda_efetiva_controle)
ATT_habitual <- coef(media_renda_habitual_tratado) - coef(media_renda_habitual_controle)

# Exibir os resultados
cat("Efeito Médio do Tratamento (ATT) na Renda Efetiva:", round(ATT_efetiva, 2), "R$\n")
cat("Efeito Médio do Tratamento (ATT) na Renda Habitual:", round(ATT_habitual, 2), "R$\n")

# Regressão linear ponderada para estimar o ATT
modelo_efetiva <- svyglm(VD4017 ~ tratamento, design = design_matched)
modelo_habitual <- svyglm(VD4016 ~ tratamento, design = design_matched)

summary(modelo_efetiva)
summary(modelo_habitual)

# O coeficiente da variável 'tratamento' representa o ATT
ATT_regressao_efetiva <- coef(modelo_efetiva)['tratamento']
ATT_regressao_habitual <- coef(modelo_habitual)['tratamento']

cat("Efeito Médio do Tratamento (ATT) na Renda Efetiva (Regressão):", round(ATT_regressao_efetiva, 2), "R$\n")
cat("Efeito Médio do Tratamento (ATT) na Renda Habitual (Regressão):", round(ATT_regressao_habitual, 2), "R$\n")
