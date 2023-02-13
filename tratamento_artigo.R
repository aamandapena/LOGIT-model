###ARTIGO###
##AMANDA PENA##
##SCRIPT PARA ESTIMAR UM MODELO LOGIT PARA O MERCADO DE TRABALHO CAPIXABA##


# ABRINDO PACOTES E IMPORTANDO A BASE DE DADOS ----------------------------


##ABRINDO PACOTES DA BIBLIOTECA##
library(readxl)
library(dplyr)
library(lmtest)
library(stargazer)
library(mfx)
library(car)
library(lmtest)
library(AER)
if(!require(pacman)) 
  library(pacman)
library(sandwich)
library(xtable)
library(magrittr)


pacman::p_load(dplyr, psych, car, MASS, DescTools, QuantPsyc, ggplot2)

##IMPORTANDO A BASE DE DADOS##
setwd("C:/Users/aaman/OneDrive/Documentos/MESTRADO/ARTIGO/Trabalho de Econometria I")
dados <- read_delim("~/MESTRADO/ARTIGO/dados_cripto.csv", 
                           delim = ";", escape_double = FALSE, col_types = cols(`Tipo Vínculo` = col_character()), 
                           trim_ws = TRUE)
dados <- dados_cripto

# TRATAMENTO DA BASE DE DADOS ---------------------------------------------

##CHECAGEM DA BASE DE DADOS##

class(dados)
glimpse(dados)
table(dados$ufes)
table(dados$demissao) 
summary(dados)
colnames(dados)

##RENOMEANDO AS COLUNAS##
colnames(dados) <- c("CPF_criptografado", "formacao_ufes","município", "mes_desligamento",
                     "motivo_desligamento", "idade", "tipo_vínculo","genero", "raça", "ufes", 
                     "data_admissao", "tipo_admissao", "CNAE_subclasse", "CNAE_95_classe", "CBO_94", "CBO_2002", "CNAE_20_classe")


##REMOVENDO OBSERVAÇÕES QUE NÃO PERTENCEM A CATEGORIA DE TRABALHO FORMAL REGIDO PELA CLT
dados <- dados[!dados$tipo_vínculo %in% c("30","31","35","40","50","80","90","95","96","97"), ]

##VERIFICANDO OS CPF'S DUPLICADOS NA BASE DE DADOS##
dados$duplicado <- duplicated(dados$CPF_criptografado) #cria uma nova coluna chamada "Duplicado" 
#que será verdadeira para todas as linhas em que o CPF já foi visto anteriormente e falsa
#para todas as linhas em que é o primeiro aparição do CPF.

CPF_unicos <- unique(dados$CPF_criptografado)
dados$duplicado <- !(dados$CPF_criptografado %in% CPF_unicos) #criar uma lista de CPFs únicos e comparar 
#essa lista com a coluna de CPFs originais.

sum(dados$duplicado == "TRUE")

##CRIANDO A COLUNA DEMISSÕES COM 1 PARA EMPREGADO E 0 PARA DESEMPREGO##
dados$demissao <- ifelse(dados$motivo_desligamento %in% c(10, 11) & dados$tipo_admissao == 0, 0, 1)

##AGRUPAR OS DEMITIDOS POR MÊS##

# Lê os dados de demissões
demissao <- !(dados$demissao %in% dados$mes_desligamento)
demissao <- as.data.frame(demissao)

as.character(dados$mes_desligamento)

# Agrupa os dados por mês
demissao_por_mes <- demissao %>%
  group_by(mes = dados$mes_desligamento) %>%
  summarise(total_demissões = n())

# Calcula a porcentagem de demissões por mês
demissao_por_mes$percentual_demissões <- demissao_por_mes$total_demissões / sum(demissao_por_mes$total_demissões)

# Ordena os dados por percentual de demissões
demissao_por_mes <- demissao_por_mes %>%
  arrange(desc(percentual_demissões))

# Plota gráfico de barras dos dados agrupados e ordenados 
barplot(demissao_por_mes$percentual_demissões,
        names.arg = demissao_por_mes$mes,
        ylab = "Percentual de Demissões",
        xlab = "Mês",
        main = "Percentual de Demissões por Mês")
# O gráfico mostra que a maioria dos registros de desligamento não informaram em qual mês ele foi feito

##CRIANDO A VARIÁVEL DUMMY PARA GRANDE VITÓRIA
dados$gv <- ifelse(dados$município %in% 
                     c("3205309", "3205200", "3205101", "3205002", "3202405", "3202207", "3201308"), 1, 0)

##CRIANDO A VARIÁVEL DUMMY PARA RAÇA
library(dplyr)
dados <- dados %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.factor, as.numeric) %>% 
  mutate(INDIGENA = ifelse(raça == 1, 1, 0),
         BRANCA = ifelse(raça == 2, 1, 0),
         PRETA = ifelse(raça == 4, 1, 0),
         AMARELA = ifelse(raça == 6, 1, 0),
         PARDA = ifelse(raça == 8, 1, 0),
         NAO_IDENT = ifelse(raça == 9, 1, 0),
         IGNORADO = ifelse(is.na(raça) | raça == -1, 1, 0))

##MUDANDO A VARIÁVEL UFES PARA VARIÁVEL DUMMY
dados$ufes[dados$ufes == 0] <- NA
dados$ufes <- as.numeric(dados$ufes)
dados$ufes[is.na(dados$ufes)] <- 0

##MUDANDO A VARIÁVEL GÊNERO PARA VARIÁVEL DUMMY
dados$genero[dados$genero == 2] <- 0

# RODANDO MODELO LOGIT ----------------------------------------------------

##CONSTRU?AO DO MODELO##
logit <- glm(demissao ~ ufes+ gv + idade + idade^2 + genero + INDIGENA + PARDA + PRETA + AMARELA +NAO_IDENT +IGNORADO,
             family=binomial(link="logit"), data=dados)
summary(logit)


##ESTIMANDO COM ERRO PADRÃO ROBUSTO

robust <- coeftest(logit, vcov. = vcovHC(logit))
robust
plot(logit)
stargazer(logit, robust, type="latex", out="logit.htm")

## AUSENCIA DE OUTLIERS##
plot(logit, which = 5)
summary(stdres(logit))


##AUSENCIA DE MULTICOLINEARIDADE##

vif(logit) ### Multicolinearidade: VIF > 10.

##ANALISANDO OS RESÍDUOS##

#GERANDO OS RESÍUDOS DO LOGIT ESTIMADO
res <- logit$residuals
res
dotchart(res)

#RESÍDUOS ^2

res2 <- res^2
res
plot(res2)

#TESTE BP PARA HETEROCEDASTICIDADE
bp.het <- bptest(logit, studentize = TRUE)
bp.het


#TESTE PARA AUTOCORRELAÇÃO DOS RESÍDUOS#
lmtest::dwtest(logit)

##ANÁLISE DO MODELO##

## Overall effects

Anova(logit, type = 'II', test = "Wald") ##vari?vel sexo e ind?gina nao ? estat?sticamente significantes

## OBTENDO A OR COM IC 95% (usando log-likelihood)

OR <- exp(cbind(OR = coef(logit), confint(logit)))
print(OR)
stargazer(OR, type="latex", out="OR.html")


##CALCULANDO OS EFEITOS MARGINAIS ##

## Marginal effects show the change in probability when the predictor or independent variable 
## increases by one unit. For continuous variables this represents the instantaneous change given 
## that the 'unit' may be very small. For binary variables, the change is from 0 to 1, so one 'unit' 
## as it is usually thought.

ef_mg <-logitmfx(demissao ~ ufes+ gv + idade + idade^2 +sexo + indigena + preto + parda 
                 + amarela +n_identificado, data=dados)
print(ef_mg, type="latex")
xtable(ef_mg)
class(ef_mg)
stargazer(ef_mg, type="text", out="Efeitos Marginais.html")



##Calculamos as probabilidades para cada valor das variáveis independentes
probabilidades <- predict(logit, type = "response")

# Agora, calculamos os efeitos marginais para cada variável independente
efeitos_marginais <- data.frame(variável = names(logit$coefficients),
                                coeficiente = logit$coefficients,
                                efeito_marginal = probabilidades[1] - probabilidades[0],
                                p.value = logit$coefficients[10],
                                stringsAsFactors = FALSE)

# Finalmente, exportamos os resultados para o formato LaTeX
library(xtable)
print(xtable(ef_mg), type = "latex", include.rownames = FALSE)

