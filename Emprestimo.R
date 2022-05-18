setwd("C:/Users/Danilo/OneDrive/FIA/Pós Data Science/Aulas/Analytics/03 - jan22/20220131")
getwd()
library(readxl)
Emprestimo <-read_excel("Emprestimo_bancario.xlsx", sheet = "Base de Dados")
names(Emprestimo)
summary(Emprestimo)
library(GGally)
install.packages('psych')
ggpairs(Emprestimo, title = 'correlogram with ggpairs')
par(mfrow = c(1,3))
boxplot(Emprestimo$idade ~ Emprestimo$classif, col = "darkturquoise", main = "idade")
boxplot(Emprestimo$experiencia ~ Emprestimo$classif, col = "darkturquoise", main = "experiencia")
boxplot(Emprestimo$tempo_endereco ~ Emprestimo$classif, col = "darkturquoise", main = "tempo_endereco")
boxplot(Emprestimo$renda ~ Emprestimo$classif, col = "darkturquoise", main = "renda")
boxplot(Emprestimo$debito_renda ~ Emprestimo$classif, col = "darkturquoise", main = "debito_renda")
boxplot(Emprestimo$cred_deb ~ Emprestimo$classif, col = "darkturquoise", main = "cred_deb")
boxplot(Emprestimo$outros_debitos ~ Emprestimo$classif, col = "darkturquoise", main = "outros_debitos")

#Retirar outlier linha 8
View(Emprestimo)
Emprestimo = Emprestimo[Emprestimo$renda < 2461,]

Verificar se existem NA
sum(is.na(Emprestimo))

round(prop.table(table(Emprestimo$classif)), 3)
options(scipen=30, digits=20)

reglog <- glm(classif ~	
                        idade +	
                        experiencia +	
                        tempo_endereco +	
                        renda +	
                        debito_renda +	
                        cred_deb +	
                        outros_debitos, 	
                      family = binomial(link = "logit"), 	
                      data = Emprestimo)	
summary(reglog)	

#Retirar a variavel renda
reglog1 <- glm(classif ~	
                idade +	
                experiencia +	
                tempo_endereco +	
                debito_renda +	
                cred_deb +	
                outros_debitos, 	
              family = binomial(link = "logit"), 	
              data = Emprestimo)	
summary(reglog1)	

#retirar a variavel idade
reglog2<- glm(classif ~	
                 experiencia +	
                 tempo_endereco +	
                 debito_renda +	
                 cred_deb +	
                 outros_debitos, 	
               family = binomial(link = "logit"), 	
               data = Emprestimo)	
summary(reglog2)	

#retirar a variavel outros_debitos
reglog3 <- glm(classif ~	
                experiencia +	
                tempo_endereco +	
                debito_renda +	
                cred_deb,	
              family = binomial(link = "logit"), 	
              data = Emprestimo)	
summary(reglog3)	

Emprestimo$score <- predict(reglog3, newdata = Emprestimo, type = "response")
View(Emprestimo)

# (i) Considerando essa sugestao, construa a matriz de confusao. Qual a taxa de classificacao correta?

Emprestimo$predito <- as.factor(ifelse(Emprestimo$score > 0.5, 1, 0)) # exemplo: considerando 0,5 como ponto de corte

(tabela <- table(Emprestimo$classif, Emprestimo$predito))
(acuracia <- (tabela[1,1] + tabela[2,2]) / sum(tabela))
(especificidade <- tabela[1,1] / sum(tabela[1,]))
(sensibilidade <- tabela[2,2] / sum(tabela[2,]))















