setwd('C:/Users/Danilo/OneDrive/FIA/Pós Data Science/Aulas/Analytics/04 - fev22/20220209')
getwd()
install.packages("readxl")
install.packages("tidyverse")
install.packages("Information")
install.packages("InformationValue")
install.packages("smbinning")
install.packages("HH")
install.packages("scorecard")
library(dynam)
library(readxl)
library(tidyverse)
library(Information)
library(InformationValue)
library(smbinning)
library(HH)
library(scorecard)
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%


options(scipen = 999)

base_2021_06 <- read_excel('Aluguel_Imoveis.xlsx', sheet = 'Base_Dados_Jun_21')
base_2021_07 <- read_excel('Aluguel_Imoveis.xlsx', sheet = 'Base_Dados_Jul_21')
base_2021_08 <- read_excel('Aluguel_Imoveis.xlsx', sheet = 'Base_Dados_Ago_21')


# Tabela de frequencias absolutas e relativas da efetivacao de contrato versus qualidade de conservacao

table(base_2021_06$estado_conservacao, base_2021_06$alugou_30d_pos_visita)
prop.table(table(base_2021_06$estado_conservacao, base_2021_06$alugou_30d_pos_visita), 1)

table(base_2021_07$estado_conservacao, base_2021_07$alugou_30d_pos_visita)
prop.table(table(base_2021_07$estado_conservacao, base_2021_07$alugou_30d_pos_visita), 1)

table(base_2021_08$estado_conservacao, base_2021_08$alugou_30d_pos_visita)
prop.table(table(base_2021_08$estado_conservacao, base_2021_08$alugou_30d_pos_visita), 1)

##### MULTICOLINEARIDADE

modelo_inicial <- glm(alugou_30d_pos_visita ~
                        qtde_visitas_cliente_ult_semana +
                        qtde_visitas_cliente_ult_mes +
                        qtde_visitas_cliente_ult_3m +
                        qtde_visitas_imovel_ult_semana +
                        qtde_visitas_imovel_ult_mes +
                        qtde_visitas_imovel_ult_3m +
                        distancia_imovel +
                        qtde_comercios_proximos +
                        metragem +
                        estado_conservacao,
                      data = base_2021_06,
                      family = binomial (link = 'logit'))

HH::vif(modelo_inicial)

##### PODER PREDITIVO DAS VARIAVEIS (IV)


# Base de 06/2021

IV <- create_infotables(data = base_2021_06[,c(2:12)],
                        y = "alugou_30d_pos_visita")
IV$Summary

# Base de 07/2021

IV <- create_infotables(data = base_2021_07[,c(2:12)],
                        y = "alugou_30d_pos_visita")
IV$Summary

# Base de 08/2021

IV <- create_infotables(data = base_2021_08[,c(2:12)],
                        y = "alugou_30d_pos_visita")
IV$Summary

##### DESENVOLVIMENTO DO MODELO GERAL

# Modelo geral

modelo_geral <- glm(alugou_30d_pos_visita ~
                      qtde_visitas_cliente_ult_semana +
                      qtde_visitas_imovel_ult_mes +
                      distancia_imovel +
                      qtde_comercios_proximos +
                      metragem +
                      estado_conservacao,
                    data = base_2021_06,
                    family = binomial (link = 'logit'))

summary(modelo_geral)


##### DESENVOLVIMENTO DE MODELOS SEGMENTADOS

# Modelo segmentado: Conservacao razoavel (com reducao subsequente de variaveis nao significativas)

modelo_cons_razoavel <- glm(alugou_30d_pos_visita ~
                              qtde_visitas_cliente_ult_semana +
                              qtde_visitas_imovel_ult_mes +
                              distancia_imovel +
                              qtde_comercios_proximos +
                              metragem,
                            data = base_2021_06 %>% filter(estado_conservacao == 'Razoavel'),
                            family = binomial (link = 'logit'))

summary(modelo_cons_razoavel)

modelo_cons_razoavel <- glm(alugou_30d_pos_visita ~
                              qtde_visitas_cliente_ult_semana +
                              qtde_visitas_imovel_ult_mes +
                              qtde_comercios_proximos +
                              metragem,
                            data = base_2021_06 %>% filter(estado_conservacao == 'Razoavel'),
                            family = binomial (link = 'logit'))

summary(modelo_cons_razoavel)

modelo_cons_razoavel <- glm(alugou_30d_pos_visita ~
                              qtde_visitas_cliente_ult_semana +
                              qtde_visitas_imovel_ult_mes +
                              qtde_comercios_proximos,
                            data = base_2021_06 %>% filter(estado_conservacao == 'Razoavel'),
                            family = binomial (link = 'logit'))

summary(modelo_cons_razoavel)

modelo_cons_razoavel <- glm(alugou_30d_pos_visita ~
                              qtde_visitas_imovel_ult_mes +
                              qtde_comercios_proximos,
                            data = base_2021_06 %>% filter(estado_conservacao == 'Razoavel'),
                            family = binomial (link = 'logit'))

summary(modelo_cons_razoavel)

modelo_cons_razoavel <- glm(alugou_30d_pos_visita ~
                              qtde_visitas_imovel_ult_mes,
                            data = base_2021_06 %>% filter(estado_conservacao == 'Razoavel'),
                            family = binomial (link = 'logit'))

summary(modelo_cons_razoavel)

# Modelo segmentado: Conservacao boa (com reducao subsequente de variaveis nao significativas)

modelo_cons_boa <- glm(alugou_30d_pos_visita ~
                         qtde_visitas_cliente_ult_semana +
                         qtde_visitas_imovel_ult_mes +
                         distancia_imovel +
                         qtde_comercios_proximos +
                         metragem,
                       data = base_2021_06 %>% filter(estado_conservacao == 'Bom'),
                       family = binomial (link = 'logit'))

summary(modelo_cons_boa)

modelo_cons_boa <- glm(alugou_30d_pos_visita ~
                         qtde_visitas_cliente_ult_semana +
                         qtde_visitas_imovel_ult_mes +
                         distancia_imovel +
                         qtde_comercios_proximos,
                       data = base_2021_06 %>% filter(estado_conservacao == 'Bom'),
                       family = binomial (link = 'logit'))

summary(modelo_cons_boa)

modelo_cons_boa <- glm(alugou_30d_pos_visita ~
                         qtde_visitas_cliente_ult_semana +
                         qtde_visitas_imovel_ult_mes +
                         distancia_imovel,
                       data = base_2021_06 %>% filter(estado_conservacao == 'Bom'),
                       family = binomial (link = 'logit'))

summary(modelo_cons_boa)

# Modelo segmentado: Conservacao excelente (com reducao subsequente de variaveis nao significativas)

modelo_cons_excelente <- glm(alugou_30d_pos_visita ~
                               qtde_visitas_cliente_ult_semana +
                               qtde_visitas_imovel_ult_mes +
                               distancia_imovel +
                               qtde_comercios_proximos +
                               metragem,
                             data = base_2021_06 %>% filter(estado_conservacao == 'Excelente'),
                             family = binomial (link = 'logit'))

summary(modelo_cons_excelente)

modelo_cons_excelente <- glm(alugou_30d_pos_visita ~
                               qtde_visitas_cliente_ult_semana +
                               qtde_visitas_imovel_ult_mes +
                               distancia_imovel +
                               qtde_comercios_proximos,
                             data = base_2021_06 %>% filter(estado_conservacao == 'Excelente'),
                             family = binomial (link = 'logit'))

summary(modelo_cons_excelente)

##### APLICACAO DE TODOS OS MODELOS NA BASE DE DADOS

# Aplicacao para obtencao dos scores preditos (probabilidades)

base_2021_06$score_geral <- predict(modelo_geral, newdata = base_2021_06, type = 'response')
base_2021_06$score_razoavel <- predict(modelo_cons_razoavel, newdata = base_2021_06, type = 'response')
base_2021_06$score_bom <- predict(modelo_cons_boa, newdata = base_2021_06, type = 'response')
base_2021_06$score_excelente <- predict(modelo_cons_excelente, newdata = base_2021_06, type = 'response')
base_2021_06$score_segmentado <- ifelse(base_2021_06$estado_conservacao == 'Razoavel', base_2021_06$score_razoavel,
                                        ifelse(base_2021_06$estado_conservacao == 'Bom', base_2021_06$score_bom,
                                               base_2021_06$score_excelente))

# Calculo do ponto de corte, baseado na proporcao geral de 1's

p_corte_geral <- mean(base_2021_06$alugou_30d_pos_visita)
p_corte_razoavel  <- mean(base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Razoavel'])
p_corte_bom <- mean(base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Bom'])
p_corte_excelente <- mean(base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Excelente'])

# Calculo da resposta predita: 0/1

base_2021_06$predito_geral <- as.factor(ifelse(base_2021_06$score_geral > p_corte_geral, 1, 0))
base_2021_06$predito_razoavel <- as.factor(ifelse(base_2021_06$score_razoavel > p_corte_razoavel, 1, 0))
base_2021_06$predito_bom <- as.factor(ifelse(base_2021_06$score_bom > p_corte_bom, 1, 0))
base_2021_06$predito_excelente <- as.factor(ifelse(base_2021_06$score_excelente > p_corte_excelente, 1, 0))
base_2021_06$predito_segmentado <- as.factor(ifelse(base_2021_06$estado_conservacao == 'Razoavel', ifelse(base_2021_06$score_razoavel > p_corte_razoavel, 1, 0),
                                                    ifelse(base_2021_06$estado_conservacao == 'Bom', ifelse(base_2021_06$score_bom > p_corte_bom, 1, 0),
                                                           ifelse(base_2021_06$score_excelente > p_corte_excelente, 1, 0))))

##### AVALIACAO DE DESEMPENHO

# Modelo unico: Todas as visitas

tabela <- table(base_2021_06$alugou_30d_pos_visita,
                base_2021_06$predito_geral)

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_06))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

ks_stat(actuals = base_2021_06$alugou_30d_pos_visita, predictedScores = base_2021_06$score_geral)
plotROC(actuals = base_2021_06$alugou_30d_pos_visita, predictedScores = base_2021_06$score_geral)

# Modelo unico: Visitas a imoveis com conservacao razoavel

tabela <- table(base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Razoavel'],
                base_2021_06$predito_geral[base_2021_06$estado_conservacao == 'Razoavel'])

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_06))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

ks_stat(actuals = base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Razoavel'],
        predictedScores = base_2021_06$score_geral[base_2021_06$estado_conservacao == 'Razoavel'])
plotROC(actuals = base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Razoavel'],
        predictedScores = base_2021_06$score_geral[base_2021_06$estado_conservacao == 'Razoavel'])

# Modelo unico: Visitas a imoveis com conservacao boa

tabela <- table(base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Bom'],
                base_2021_06$predito_geral[base_2021_06$estado_conservacao == 'Bom'])

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_06))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

ks_stat(actuals = base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Bom'],
        predictedScores = base_2021_06$score_geral[base_2021_06$estado_conservacao == 'Bom'])
plotROC(actuals = base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Bom'],
        predictedScores = base_2021_06$score_geral[base_2021_06$estado_conservacao == 'Bom'])

# Modelo unico: Visitas a imoveis com conservacao excelente

tabela <- table(base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Excelente'],
                base_2021_06$predito_geral[base_2021_06$estado_conservacao == 'Excelente'])

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_06))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

ks_stat(actuals = base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Excelente'],
        predictedScores = base_2021_06$score_geral[base_2021_06$estado_conservacao == 'Excelente'])
plotROC(actuals = base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Excelente'],
        predictedScores = base_2021_06$score_geral[base_2021_06$estado_conservacao == 'Excelente'])

# Modelo segmentado: Visitas a imoveis com conservacao razoavel

tabela <- table(base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Razoavel'],
                base_2021_06$predito_razoavel[base_2021_06$estado_conservacao == 'Razoavel'])

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_06[base_2021_06$estado_conservacao == 'Razoavel',]))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

ks_stat(actuals = base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Razoavel'],
        predictedScores = base_2021_06$score_razoavel[base_2021_06$estado_conservacao == 'Razoavel'])
plotROC(actuals = base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Razoavel'],
        predictedScores = base_2021_06$score_razoavel[base_2021_06$estado_conservacao == 'Razoavel'])

# Modelo segmentado: Visitas a imoveis com conservacao boa

tabela <- table(base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Bom'],
                base_2021_06$predito_bom[base_2021_06$estado_conservacao == 'Bom'])

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_06[base_2021_06$estado_conservacao == 'Bom',]))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

ks_stat(actuals = base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Bom'],
        predictedScores = base_2021_06$score_bom[base_2021_06$estado_conservacao == 'Bom'])
plotROC(actuals = base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Bom'],
        predictedScores = base_2021_06$score_bom[base_2021_06$estado_conservacao == 'Bom'])

# Modelo segmentado: Visitas a imoveis com conservacao excelente

tabela <- table(base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Excelente'],
                base_2021_06$predito_excelente[base_2021_06$estado_conservacao == 'Excelente'])

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_06[base_2021_06$estado_conservacao == 'Excelente',]))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

ks_stat(actuals = base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Excelente'],
        predictedScores = base_2021_06$score_excelente[base_2021_06$estado_conservacao == 'Excelente'])
plotROC(actuals = base_2021_06$alugou_30d_pos_visita[base_2021_06$estado_conservacao == 'Excelente'],
        predictedScores = base_2021_06$score_excelente[base_2021_06$estado_conservacao == 'Excelente'])

# Modelo segmentado: Empilhando os 3 segmentos

tabela <- table(base_2021_06$alugou_30d_pos_visita,
                base_2021_06$predito_segmentado)

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_06))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

ks_stat(actuals = base_2021_06$alugou_30d_pos_visita,
        predictedScores = base_2021_06$score_segmentado)
plotROC(actuals = base_2021_06$alugou_30d_pos_visita,
        predictedScores = base_2021_06$score_segmentado)

##### CATEGORIZACAO DE VARIAVEIS

base_2021_06_cat = as.data.frame(base_2021_06)

# Categorizacao da variavel 'alugou_30d_pos_visita': Visitas a imoveis com conservacao boa

agrup <- smbinning(df = base_2021_06_cat[base_2021_06_cat$estado_conservacao == 'Bom',],
                   y = "alugou_30d_pos_visita",
                   x = "distancia_imovel",
                   p = 0.05)
agrup$ivtable

par(mfrow = c(2,2))
boxplot(base_2021_06_cat$distancia_imovel[base_2021_06_cat$estado_conservacao == 'Bom'] ~
          base_2021_06_cat$alugou_30d_pos_visita[base_2021_06_cat$estado_conservacao == 'Bom'],
        horizontal = T, frame = F, col = "lightgray",main = "Distribution", ylab = "alugou", xlab = "x")
smbinning.plot(agrup, option = "dist") 
smbinning.plot(agrup, option = "goodrate") 
smbinning.plot(agrup, option = "WoE")

base_2021_06_cat <- smbinning.gen(base_2021_06_cat,
                                  agrup,
                                  chrname = "distancia_imovel_cat_bom")

# Retreino do modelo: Visitas a imoveis com conservacao boa

modelo_cons_boa_cat <- glm(alugou_30d_pos_visita ~
                             qtde_visitas_cliente_ult_semana +
                             qtde_visitas_imovel_ult_3m +
                             distancia_imovel_cat_bom,
                           data = base_2021_06_cat %>% filter(estado_conservacao == 'Bom'),
                           family = binomial (link = 'logit'))

summary(modelo_cons_boa_cat)

# Aplicando o modelo retreinado a base de dados: Visitas a imoveis com conservacao boa

base_2021_06_cat$score_bom <- predict(modelo_cons_boa_cat, newdata = base_2021_06_cat, type = 'response')

p_corte_bom <- mean(base_2021_06_cat$alugou_30d_pos_visita[base_2021_06_cat$estado_conservacao == 'Bom'])

base_2021_06_cat$predito_bom <- as.factor(ifelse(base_2021_06_cat$score_bom > p_corte_bom, 1, 0))

# Avaliacao de desempenho do modelo retreinado: Visitas a imoveis com conservacao boa

tabela <- table(base_2021_06_cat$alugou_30d_pos_visita[base_2021_06_cat$estado_conservacao == 'Bom'],
                base_2021_06_cat$predito_bom[base_2021_06_cat$estado_conservacao == 'Bom'])

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_06_cat[base_2021_06_cat$estado_conservacao == 'Bom',]))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

ks_stat(actuals = base_2021_06_cat$alugou_30d_pos_visita[base_2021_06_cat$estado_conservacao == 'Bom'],
        predictedScores = base_2021_06_cat$score_bom[base_2021_06_cat$estado_conservacao == 'Bom'])
plotROC(actuals = base_2021_06_cat$alugou_30d_pos_visita[base_2021_06_cat$estado_conservacao == 'Bom'],
        predictedScores = base_2021_06_cat$score_bom[base_2021_06_cat$estado_conservacao == 'Bom'])

##### VALIDACAO DE MODELOS: JUL/21

# Aplicando os modelos a base de JUL-21

base_2021_07$score_razoavel = predict(modelo_cons_razoavel, base_2021_07, type = "response")
base_2021_07$score_bom = predict(modelo_cons_boa, base_2021_07, type = "response")
base_2021_07$score_excelente = predict(modelo_cons_excelente, base_2021_07, type = "response")
base_2021_07$score_segmentado  <- ifelse(base_2021_07$estado_conservacao == 'Razoavel', base_2021_07$score_razoavel,
                                         ifelse(base_2021_07$estado_conservacao == 'Bom', base_2021_07$score_bom,
                                                base_2021_07$score_excelente))

base_2021_07$predito_razoavel  <- as.factor(ifelse(base_2021_07$score_razoavel > p_corte_razoavel, 1, 0))
base_2021_07$predito_bom <- as.factor(ifelse(base_2021_07$score_bom > p_corte_bom, 1, 0))
base_2021_07$predito_excelente   <- as.factor(ifelse(base_2021_07$score_excelente > p_corte_excelente, 1, 0))
base_2021_07$predito_segmentado  <- as.factor(ifelse(base_2021_07$estado_conservacao == 'Razoavel', ifelse(base_2021_07$score_razoavel > p_corte_razoavel, 1, 0),
                                                     ifelse(base_2021_07$estado_conservacao == 'Bom', ifelse(base_2021_07$score_bom > p_corte_bom, 1, 0),
                                                            ifelse(base_2021_07$score_excelente > p_corte_excelente, 1, 0))))

# Avaliacao de desempenho do modelo: Visitas a imoveis com conservacao razoavel

tabela <- table(base_2021_07$alugou_30d_pos_visita[base_2021_07$estado_conservacao == 'Razoavel'],
                base_2021_07$predito_razoavel[base_2021_07$estado_conservacao == 'Razoavel'])

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_07[base_2021_07$estado_conservacao == 'Razoavel',]))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

ks_stat(actuals = base_2021_07$alugou_30d_pos_visita[base_2021_07$estado_conservacao == 'Razoavel'],
        predictedScores = base_2021_07$score_razoavel[base_2021_07$estado_conservacao == 'Razoavel'])
plotROC(actuals = base_2021_07$alugou_30d_pos_visita[base_2021_07$estado_conservacao == 'Razoavel'],
        predictedScores = base_2021_07$score_razoavel[base_2021_07$estado_conservacao == 'Razoavel'])

# Avaliacao de desempenho do modelo: Visitas a imoveis com conservacao boa

tabela <- table(base_2021_07$alugou_30d_pos_visita[base_2021_07$estado_conservacao == 'Bom'],
                base_2021_07$predito_bom[base_2021_07$estado_conservacao == 'Bom'])

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_07[base_2021_07$estado_conservacao == 'Bom',]))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

ks_stat(actuals = base_2021_07$alugou_30d_pos_visita[base_2021_07$estado_conservacao == 'Bom'],
        predictedScores = base_2021_07$score_bom[base_2021_07$estado_conservacao == 'Bom'])
plotROC(actuals = base_2021_07$alugou_30d_pos_visita[base_2021_07$estado_conservacao == 'Bom'],
        predictedScores = base_2021_07$score_bom[base_2021_07$estado_conservacao == 'Bom'])

# Avaliacao de desempenho do modelo: Visitas a imoveis com conservacao excelente

tabela <- table(base_2021_07$alugou_30d_pos_visita[base_2021_07$estado_conservacao == 'Excelente'],
                base_2021_07$predito_excelente[base_2021_07$estado_conservacao == 'Excelente'])

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_07[base_2021_07$estado_conservacao == 'Excelente',]))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

ks_stat(actuals = base_2021_07$alugou_30d_pos_visita[base_2021_07$estado_conservacao == 'Excelente'],
        predictedScores = base_2021_07$score_excelente[base_2021_07$estado_conservacao == 'Excelente'])
plotROC(actuals = base_2021_07$alugou_30d_pos_visita[base_2021_07$estado_conservacao == 'Excelente'],
        predictedScores = base_2021_07$score_excelente[base_2021_07$estado_conservacao == 'Excelente'])

# Avaliacao de desempenho do modelo: Empilhando os 3 segmentos

tabela <- table(base_2021_07$alugou_30d_pos_visita,
                base_2021_07$predito_segmentado)

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_07))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

ks_stat(actuals = base_2021_07$alugou_30d_pos_visita,
        predictedScores = base_2021_07$score_segmentado)
plotROC(actuals = base_2021_07$alugou_30d_pos_visita,
        predictedScores = base_2021_07$score_segmentado)

##### VALIDACAO DE MODELOS: AGO/21

# Aplicando os modelos a base de AGO-21

base_2021_08$score_razoavel = predict(modelo_cons_razoavel, base_2021_08, type = "response")
base_2021_08$score_bom = predict(modelo_cons_boa, base_2021_08, type = "response")
base_2021_08$score_excelente = predict(modelo_cons_excelente, base_2021_08, type = "response")
base_2021_08$score_segmentado  <- ifelse(base_2021_08$estado_conservacao == 'Razoavel', base_2021_08$score_razoavel,
                                         ifelse(base_2021_08$estado_conservacao == 'Bom', base_2021_08$score_bom,
                                                base_2021_08$score_excelente))

base_2021_08$predito_razoavel  <- as.factor(ifelse(base_2021_08$score_razoavel > p_corte_razoavel, 1, 0))
base_2021_08$predito_bom <- as.factor(ifelse(base_2021_08$score_bom > p_corte_bom, 1, 0))
base_2021_08$predito_excelente   <- as.factor(ifelse(base_2021_08$score_excelente > p_corte_excelente, 1, 0))
base_2021_08$predito_segmentado  <- as.factor(ifelse(base_2021_08$estado_conservacao == 'Razoavel', ifelse(base_2021_08$score_razoavel > p_corte_razoavel, 1, 0),
                                                     ifelse(base_2021_08$estado_conservacao == 'Bom', ifelse(base_2021_08$score_bom > p_corte_bom, 1, 0),
                                                            ifelse(base_2021_08$score_excelente > p_corte_excelente, 1, 0))))

# Avaliacao de desempenho do modelo: Visitas a imoveis com conservacao razoavel

tabela <- table(base_2021_08$alugou_30d_pos_visita[base_2021_08$estado_conservacao == 'Razoavel'],
                base_2021_08$predito_razoavel[base_2021_08$estado_conservacao == 'Razoavel'])

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_08[base_2021_08$estado_conservacao == 'Razoavel',]))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

# Avaliacao de desempenho do modelo: Visitas a imoveis com conservacao boa

tabela <- table(base_2021_08$alugou_30d_pos_visita[base_2021_08$estado_conservacao == 'Bom'],
                base_2021_08$predito_bom[base_2021_08$estado_conservacao == 'Bom'])

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_08[base_2021_08$estado_conservacao == 'Bom',]))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

# Avaliacao de desempenho do modelo: Visitas a imoveis com conservacao excelente

tabela <- table(base_2021_08$alugou_30d_pos_visita[base_2021_08$estado_conservacao == 'Excelente'],
                base_2021_08$predito_excelente[base_2021_08$estado_conservacao == 'Excelente'])

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_08[base_2021_08$estado_conservacao == 'Excelente',]))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

# Avaliacao de desempenho do modelo: Empilhando os 3 segmentos

tabela <- table(base_2021_08$alugou_30d_pos_visita,
                base_2021_08$predito_segmentado)

(acuracia <- (tabela[1,1] + tabela[2,2]) / nrow(base_2021_08))
(sensibilidade <- tabela[2,2] / (tabela[2,1] + tabela[2,2]))
(especificidade <- tabela[1,1] / (tabela[1,1] + tabela[1,2]))

##### MONITORAMENTO DE VARIAVEIS

# PSI para safra de jul/21

PSI_score <- perf_psi(score = list(train = base_2021_06$score_segmentado,
                                   test = base_2021_07$score_segmentado))

print(PSI_score)

# PSI para safra de ago/21

PSI_score <- perf_psi(score = list(train = base_2021_06$score_segmentado,
                                   test = base_2021_08$score_segmentado))

print(PSI_score)
