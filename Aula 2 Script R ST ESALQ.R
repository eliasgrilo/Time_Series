################################################################
####   AULA 2 - SÉRIES TEMPORAIS - Fabiano - MBA USP ESALQ
####                   DATA SCIENCE e ANALYTICS
################################################################

# Instalação e Carregamento de Todos os Pacotes ---------------------------
# Rotina prof. Rafael Souza e Prof Fávero

pacotes <- c("readr","readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS",
             "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
             "quantmod","dgof","seasonal")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
	instalador <- pacotes[!pacotes %in% installed.packages()]
	for(i in 1:length(instalador)) {
		install.packages(instalador, dependencies = T)
		break()}
	sapply(pacotes, require, character = T)
} else {
	sapply(pacotes, require, character = T)
}

#-------------------------------------------------------------------------

###############################################################################
###############################################################################
## Modelos ARIMA - (Box - Jenkins)
###############################################################################
###############################################################################

#######################################################################
### MODELOS ARIMA - Simulados
#######################################################################

pontos = 500
set.seed(1)


## Simulando um modelo AR(1)

modelo_ar = list(ar=0.8)
dp=1

serie_ar=arima.sim(n=pontos,model=modelo_ar,sd=dp)
plot(serie_ar, ylab=" Modelo AR(1)", main="Modelo AR(1) X(t)=0.8.X(t-1) + erro(t)")


## Simulando um modelo MA(1)

modelo_ma=list(ma=-0.3)
dp=1

serie_ma=arima.sim(n=pontos,model=modelo_ma,sd=dp)
plot(serie_ma, ylab=" Modelo MA(1)", main="Modelo MA(1) X(t)=-0.3erro(t-1) + erro(t)")

## Simulando um modelo ARMA(1,1)

modelo_arma=list(ar=0.8,ma=-0.3)
dp=1

serie_arma=arima.sim(n=pontos,model=modelo_arma,sd=dp)
plot(serie_arma, ylab=" Modelo ARMA(1,1)", main="Modelo ARMA(1,1)")


## Simulando um modelo ARIMA(1,1,1)

modelo_arima=list(order=c(1,1,1),ar=0.8,ma=-0.3)
dp=1
serie_arima=arima.sim(n=pontos,model=modelo_arima, sd=dp)
plot(serie_arima, ylab=" Modelo ARIMA(1,1,1)", main="Modelo ARIMA(1,1,1)")


##################################################
## Analisando as séries autoregressivas
##################################################
## Realizar os testes de Estacionariedade
## Precisamos do pacote URCA - Unit Root and Cointegration Test
###################################################################

# Teste de Dickey-Fuller
# Ho: A série Não é Estacionária
# H1: A série é Estacionária

testeDF_ar=ur.df(serie_ar)
testeDF_ar
summary(testeDF_ar)

# Conclusão: p-value 4.28e-13*** < 0.01 (99% confiança) - REJEITO Ho,
# portanto a série é estacionária

testeDF_ma=ur.df(serie_ma)
summary(testeDF_ma)

# Conclusão: p-value 2e-16*** < 0.01 (99% confiança) - REJEITO Ho,
# portanto a série é estacionária


testeDF_arma=ur.df(serie_arma)
summary(testeDF_arma)

# Conclusão: p-value 7.52e-12*** < 0.01 (99% confiança) - REJEITO Ho,
# portanto a série é estacion?ria

# ATENÇÃO: vamos rodar para a série ARIMA, tem o I = 1

testeDF_arima=ur.df(serie_arima)
summary(testeDF_arima)

# Conclusão: p-value 0.537 > 0.01 (99% confiança) - ACEITO Ho,
# portanto a série NÃO é estacionária

# Teste de KPSS
# Ho: A série é Estacionária
# H1: A série NÃO é Estacionária


testeKPSS_ar=ur.kpss(serie_ar)
summary(testeKPSS_ar)

# Conclusão: t-test= 0.2297 < 0.739 (ponto crítico para 99% confiança) - ACEITO Ho,
# portanto a série é estacionária

testeKPSS_ma=ur.kpss(serie_ma)
summary(testeKPSS_ma)

# Conclusão: t-test= 0.0685 < 0.739 (ponto crítico para 99% confiança) - ACEITO Ho,
# portanto a série é estacionária

testeKPSS_arma=ur.kpss(serie_arma)
summary(testeKPSS_arma)

# Conclusão: t-test= 0.2429 < 0.739 (ponto crítico para 99% confiança) - ACEITO Ho,
# portanto a série é estacionária

testeKPSS_arima=ur.kpss(serie_arima)
summary(testeKPSS_arima)

# Conclusão: t-test= 4.7212 > 0.739 (ponto crético para 99% confiança) - REJEITA Ho,
# portanto a série NÃO é estacionária

###################################################################################
### Estimação de um modelo ARIMA - Escolher, p, q e d
###################################################################################

ggtsdisplay(serie_ar)

acf(serie_ar)
pacf(serie_ar, lag.max = 5)

# Série AR(1) não precisa de ser diferenciada

# Quantas diferenciações seriam necess?rias para tornar a série estacionária

ndiffs(serie_ar)

# Resposta = 0

ndiffs(serie_arima)

# Resposta = 1 (a série precisa de 1 dierenciação para se tornar estacionária)

ndiffs(serie_ma)
ndiffs(serie_arma)

# Comando Geral para se estimar um modelo ARIMA, quando eu não sei a ordem da série
# não conheço os valores de p, d e q

# Lembrando: simulanos um AR(1) de coeficiente 0.8
estima = auto.arima(serie_ar)
estima

# Lembrando: simulamos um MA(1) de coeficiente -0.3
estima1=auto.arima(serie_ma)
estima1

# Simulando agora modelos de ordens maiores 
# por exemplo, simulando um AR(2)

modelo1=list(ar=c(0.8,0.1))  # AR(2): ar1=0.8, ar2=0.1
serie1=arima.sim(n=pontos, model=modelo1, sd=dp)
plot(serie1, ylab="modelo AR(2)")

# Teste de Estacionariedade - Teste Dickey-Fuller
teste1=ur.df(serie1)
summary(teste1)

# Conclusão: z.lag1 tem p-valor < 0.01 - rejeita-se H0
# portanto a ST é estacionária

# Olhando as funções de ACF e a PACF
acf(serie1)
pacf(serie1)

modelo2=list(ar=c(0.5,0.1,0.3))  # AR(3)
serie2=arima.sim(n=pontos, model=modelo2, sd=dp)
plot(serie2)
acf(serie2)
pacf(serie2)

modelo3=list(ar=c(0.5,-0.1,-0.3))  # AR(3)
serie3=arima.sim(n=pontos, model=modelo3, sd=dp)
plot(serie3)
acf(serie3)
pacf(serie3, lag.max=5)

# partindo para a identificação do modelo

estima1=arima(serie1,order=c(2,0,0))
estima1

estima2=arima(serie2, order=c(3,0,0))
estima2

estima3=arima(serie3,order=c(3,0,0))
estima3

# partindo para a identificação do modelo SEM conhecer a ordem

estima31=auto.arima(serie3, trace=T)
estima31

# Estimar ARMA(2,2)

modelo4=list(ar=c(0.8,0.1), ma=c(0.4, -0.3))
serie4=arima.sim(n=pontos, model=modelo4, sd=dp)
plot(serie4)

# estacionariedade
teste4=ur.df(serie4)
summary(teste4)

# a série4 é estacionária com p-valor<0.01

acf(serie4)
pacf(serie4)

estima4=arima(serie4, order=c(2,0,2))
estima4

### Caso encontre ARIMA(0,0,0) - não foi possível encontrar memória
## autoregressiva significativa

#####################################################################################
# modelos ARIMA com Sazonalidade - SARIMA, possui os parâmetros P, D e Q Sazonais.
# Fica SARIMA(p,d,q)(P,D,Q)
#####################################################################################

# Buscando a série do Índice de Volume de Vendas de SP
# Pelo pacote BETS - Brazilian Economic Time Series ou GetBCBData
#varejosp=BETSget(1475)
#varejo=ts(varejosp[2], start = c(2000,1), end = c(2021,8), frequency = 12)
#plot(varejo)

# Alternativa: pacote: GetBCBData
varejo2=gbcbd_get_series(1475,first.date='2000-01-01')
varejo2=ts(varejo2[2], start = c(2000,1), end = c(2021,8), frequency = 12)
plot(varejo2)

# divisão de janelas
varejotreino=window(varejo2, start=c(2000,1), end=c(2019,8))
varejoteste=window(varejo2,start=c(2019,9), end=c(2021,8))
length(varejoteste)

dygraph(varejo2)

# plotando as duas séries juntas para checagem

autoplot(varejo2) +
  autolayer(varejotreino, series="Treino") +
  autolayer(varejoteste, series="Teste") +
  scale_color_viridis_d() +
  theme_bw()

## Análise da Série
ggtsdisplay(varejotreino)

# tenho uma prossível sazonalidade

# Teste de Estacionariedade
testevarejo=ur.df(varejotreino)
summary(testevarejo)

# A série não é estacionária - precisa ser diferenciada

ndiffs(varejotreino)

difvarejotreino=diff(varejotreino)
ggtsdisplay(difvarejotreino)

testevarejodif=ur.df(difvarejotreino)
summary(testevarejodif)

arimavarejo=auto.arima(varejotreino, trace=T)

#### validação e diagnóstico

checkresiduals(arimavarejo)

# 1. teste de Ljung-Box p-value = 0.5589>0.01, aceitamos H0, resíduos não são
# correlacionados

# 2. Normalidade dos resíduos
ks.test(arimavarejo$residuals, "pnorm", mean(arimavarejo$residuals),
				sd(arimavarejo$residuals))
# p-valor = 0.1237 > 0,01 - Aceita H0, ou seja, resíduos normais

# confirmada a não existência de autocorrelação serial e normalidade dos resíduos
# Podemos verificar a estacionariedade de variãncia
# verificar se existe efeitos ARCH

ArchTest(arimavarejo$residuals)

# p-valor 0.1784 > 0,01, aceita-se H0, não se rejeita a H0, garante não existência
# de efeitos ARCH

## Previsao para a série de varejo SP

prevvarejo=forecast::forecast(arimavarejo, h=24)

autoplot(prevvarejo) +
  theme_bw()

accuracy(prevvarejo, varejoteste)

ggplotly(
  autoplot(varejotreino)+
    autolayer(varejoteste,serie="Valores Reais")+
    autolayer(prevvarejo$mean, serie="Forecast")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

# -------------------------------------------------------------------------------
# Testando um modelo ETS

ETS=forecast(ets(varejotreino),h=24)

summary(ETS)

ggplotly(
  autoplot(varejotreino)+
    autolayer(varejoteste,serie="Valores Reais")+
    autolayer(ETS$mean, serie="Previstos")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)
autoplot(ETS) +
  theme_bw()

accuracy(ETS$mean, varejoteste)

# Teste de Dickey-Fuller
# Ho: A série Não é Estacionária
# H1: A série é Estacionária

testeDF_ar=ur.df(varejotreino)
testeDF_ar
summary(testeDF_ar)

# Fazendo uma transformaçãoo de Box-Cox
l = BoxCox.lambda(varejo2)

ETS=forecast(ets(varejotreino, lambda = l),h=24)
summary(ETS)

autoplot(forecast(ETS,h=24)) +
  xlab("Ano") +
  ylab("Índice Base 100") +
  ggtitle("Índice de volume de vendas no varejo total de SP BCB ") +
  theme_bw()


accuracy(ETS,varejoteste)["Test set","MAPE"]



##############################################################################
### Prevendo a Inflação - IPCA - BACEN
##############################################################################

ipca1=gbcbd_get_series(433,first.date='2007-01-01')
ipca=ts(ipca1[2], start = c(2007,1), end=c(2021,10), frequency = 12)

autoplot(ipca) +
  theme_bw()

# Analisando mês a mês os dados

monthplot(ipca, col.base=1,lty.base = 2)

# Separando as janelas
sipca=window(ipca, start=c(2007,1), end=c(2020,10))
plot(sipca)
teste=window(ipca, start=c(2020,11), end=c(2021,10))
plot(teste)
length(teste)

# Analisando a série para estimação
ggtsdisplay(sipca)
acf(sipca)
pacf(sipca)

# Estimando um modelo inicial
mod=Arima(sipca,order = c(1,0,0), seasonal=c(0,0,1))
mod

# pelo autoarima
modelo=auto.arima(sipca,trace = T)

# Fazendo a previsão do modelo com sazonalidade

pipca<-forecast::forecast(mod,h=12)

autoplot(pipca) +
  theme_bw()

accuracy(pipca,teste)

# Fazendo a previsão do modelo sem sazonalidade

psipca<-forecast::forecast(modelo,h=12)

autoplot(psipca) +
  theme_bw()

accuracy(psipca,teste)


## agora que temos um modelo definido precisamos saber se o modelo capturou
## toda a estrutura do processo
## Significa que devemos checar se os resíduos do modelo estão limpos
## quer dizer, devemos ter resíduos não autocorrelacionados e normalmente
## distribuídos

# 1. Teste se os resíduos são não autocorrelacionados
# Teste de Ljung-Box
# H0: independência da ST, isto é, resíduos não correlacionados no tempo
# H1: dependência da ST, isto é, resíduos correlacionados, indicando que o 
# modelo não capturou alguma estrutura que indica um erro sistemático

checkresiduals(pipca)
# 1. Teste de Ljung-Box, o p-valor < 0,05 - Rejeição de H0

# 2. Teste de Normalidade dos Resíduos
# Teste de Kolmogorv-Smirnov
# H0: Resíduos com comportamento normal
# H1: Resíduos sem normalidade

# Teste de Shapiro-Wilk (n<30), KS>30
ks.test(pipca$residuals, "pnorm", mean(pipca$residuals), 
        sd(pipca$residuals))
# p-value >0,01 - Aceita H0, são normais

## Testar a estacionariedade da variância
## testar se existe efeitos ARCH
# H0: Não Existe Efeitos ARCH
# H1: Existe Efeitos ARCH


ArchTest(pipca$residuals)

#o p-value < 0,05 Rejeita-se a H0, a série apresenta efeitos ARCH

#################################################################################
#################################################################################
###  Previsão com FABLE
#################################################################################
#################################################################################

# lendo um arquivo csv direto de um endereço eletrônico e convertendo para tsibble
# vamos usar série do covid19
# Script: github.com/paulamacaira

covid = read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv")

covid %>% View()

covid = read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv") %>%
	select(date, state, newDeaths, newCases)%>%
	as_tsibble(
		index = date,
		key = state
	) %>%
	filter(date < lubridate::today()) %>%
	group_by(state) %>%
	mutate(MM_mortes = zoo::rollmean(newDeaths, k = 7, fill = NA, align = "right"),
				 MM_casos = zoo::rollmean(newCases, k = 7, fill = NA, align = "right"))
covid

# fazendo um gráfico com o ggplot2

covid %>%
	filter(state == "TOTAL") %>%
	autoplot(newDeaths, color = "#DCE319FF") +
	geom_line(aes(y = MM_mortes), color = "#440154FF") +
	labs(x="Dia",
	     y="Mortes", title="Média Móvel (7 dias) do número de mortes por COVID-19 no Brasil") +
  theme_bw()

covid %>%
	filter(state != "TOTAL") %>%
	autoplot(MM_mortes) +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
	labs(x="Dia",y="Mortes (MM 7 dias)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 10),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 7),
        legend.position = "bottom")

covid %>%
	filter(state != "TOTAL") %>%
	autoplot(MM_mortes) +
  scale_color_viridis_d() +
	facet_wrap(~state, scales = "free") +
	labs(x="Dia",y="Mortes (MM 7 dias)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")

covid %>%
	filter(state == "TOTAL") %>%
	gg_season(MM_mortes) +
	labs(x="Dia",y="Mortes (MM 7 dias)") +
  scale_color_viridis_c() +
  theme_bw()

covid %>%
	filter(state == "TOTAL") %>%
	gg_season(MM_mortes, period = "month")  +
	labs(x="Dia",y="Mortes (MM 7 dias)") +
  scale_color_viridis_c() +
  theme_bw()

# Vamos rodar uma série de modelos padrões de séries temporais
# MEAN, NAIVE, SNAIVE e RW

# Usamos a função model() gerar o modelo
# está configurado para gerar somente o modelo para a base total = Brasil

covid_fit = covid %>%
  filter(state == "TOTAL") %>%
  model(
    Seasonal_naive = SNAIVE(newCases),
    Naive = NAIVE(newCases),
    Drift = RW(newCases ~ drift()),
    Mean = MEAN(newCases)
  )
covid_fit

# se desejarmos mais estados além do total:

covid_fit3 = covid %>%
  filter(state %in% c("TOTAL", "RJ", "SP")) %>%
  model(
    Seasonal_naive = SNAIVE(newCases),
    Naive = NAIVE(newCases),
    Drift = RW(newCases ~ drift()),
    Mean = MEAN(newCases)
  )
covid_fit3


# para produzir as previsões use a função forecast()

covid_fc = covid_fit %>%
  forecast(h = 12)
covid_fc %>% View()

covid_fc3 = covid_fit3 %>%
  forecast(h = 12)
covid_fc3 %>% View()

# fable é uma tabela de previsão com previsões pontuais e distribuições

# se quiser ver somente as previsões
covid_fc %>%
	autoplot() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

covid_fc3 %>%
  autoplot() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# se quiser ver somente as previsões
covid_fc %>%
	autoplot(covid) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

covid_fc3 %>%
  autoplot(covid) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

covid_fc %>%
	autoplot(covid, level = NULL) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

covid_fc3 %>%
  autoplot(covid, level = NULL) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# visualização a partir de 01/07/2021
covid_fc %>%
	autoplot(covid %>% filter(date >= "2021-07-01"), level = NULL) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

covid_fc3 %>%
  autoplot(covid %>% filter(date >= "2021-07-01"), level = NULL) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()
# Análise dos resíduos: Não devem ser correlacionados
# caso sejam correlacionados, dignifica que ficaram informações nos resíduos
# que deveriam estar no modelo
# os resíduos devem possuir média zero, caso não seja então as previsões são viesadas

augment(covid_fit)

augment(covid_fit3)

# olhando apenas os resíduos do modelo Seasonal Naive
augment(covid_fit) %>%
	filter(.model == "Seasonal_naive") %>%
	autoplot(.resid) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

augment(covid_fit3) %>%
  filter(.model == "Seasonal_naive") %>%
  autoplot(.resid) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# se quiser ver todos os resíduos de todos os modelos
augment(covid_fit3) %>%
	autoplot(.resid) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# Olhando a função de autocorrelação
augment(covid_fit) %>%
	filter(.model == "Seasonal_naive") %>%
	ACF(.resid) %>%
	autoplot() +
  theme_bw()

augment(covid_fit3) %>%
  filter(.model == "Seasonal_naive") %>%
  ACF(.resid) %>%
  autoplot() +
  theme_bw()
# fazendo uma análise mais técnica dos resíduos

# Teste de Ljung-box
# H0: os resíduos são iid
# H1: os resíduos não são iid
# não quero rejeitar H0 (quero um pvalor grande)

augment(covid_fit) %>%
	features(.resid, ljung_box)

augment(covid_fit3) %>%
  features(.resid, ljung_box)

# Medidas de acurácia

# Separando uma parte dos dados para gerar o modelo
covid_fit = covid %>%
	filter(state == "TOTAL",
				 date <= "2021-10-10") %>%
	model(Seasonal_naive = SNAIVE(newCases))

covid_fit3 = covid %>%
  filter(state == "TOTAL",
         date <= "2021-10-10") %>%
  model(Seasonal_naive = SNAIVE(newCases))

# prevendo 1 mês adiante
covid_fit %>%
	forecast(h = "1 month") %>%
	autoplot(covid %>%
					 	filter(state == "TOTAL", date >= "2021-07-01"),
					 level = NULL) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

covid_fc = covid_fit %>%
	forecast(h = "1 month")

accuracy(covid_fc, covid %>%
				 	filter(state == "TOTAL"))

covid_fc3 = covid_fit %>%
  forecast(h = "1 month")

accuracy(covid_fc3, covid %>%
           filter(state == "TOTAL"))

# ------------------------------------------------------------------------------
# EXPONENTIAL SMOOTHING

# E (Error) T (Trend) S (Season)
# Erro: aditivo (A) ou multiplicativo (M)
# Tendência: nenhuma (N), aditiva (A), multiplicativa (M) ou amortecida (Ad ou Md)
# Sazonalidade: nenhuma (N), aditiva (A) ou multiplicativa (M)

covid_RJ = covid %>%
	filter(state == "RJ") %>%
	select(newDeaths)  


covid_RJ %>%
	autoplot() +
  theme_bw()

fit = covid_RJ %>%
	model(ets = ETS(newDeaths))

fit
components(fit)

components(fit) %>%
	autoplot() +
  theme_bw()

fit %>%
	forecast(h = 14) %>%
	autoplot(covid_RJ %>% filter(date >= "2021-08-01")) +
  theme_bw()

# posso forçar uma tendência

covid_RJ %>%
	model(ets = ETS(newDeaths ~ trend("Ad"))) %>%
	forecast(h = 14) %>%
	autoplot(covid_RJ %>% filter(date >= "2021-08-01")) +
  theme_bw()

# prevendo para 3 regiões
covid_3 = covid %>%
  filter(state %in% c("TOTAL", "RJ", "SP")) %>%
  select(newDeaths)  


covid_3 %>%
  autoplot() +
  theme_bw()

fit = covid_3 %>%
  model(ets = ETS(newDeaths))

fit
components(fit)

components(fit) %>%
  autoplot() +
  theme_bw()

fit %>%
  forecast(h = 14) %>%
  autoplot(covid_3 %>% filter(date >= "2021-08-01")) +
  theme_bw()


# Bootstrap

# posso traçar cenários de distribuição

sim = fit %>%
	generate(h = 14, times = 5, bootstrap = TRUE)
sim
View(sim)

covid_RJ %>%
	filter(date >= "2021-08-01") %>%
	ggplot(aes(x = date)) +
	geom_line(aes(y = newDeaths)) +
	geom_line(aes(y = .sim, colour = as.factor(.rep)), data = sim) +
	guides(col = "none") +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# ARIMA

# AR: autoregressivo (observações defasadas como input)
#  I: integrado (diferenciação para tornar a série estacionária)
# MA: média móvel (erros defasados como input)


fit_ARIMA = covid_RJ %>%
  model(arima = fable::ARIMA(newDeaths))

fit_ARIMA
fabletools::report(fit_ARIMA)

fit_ARIMA %>%
  forecast(h = 14) %>%
  autoplot(covid_RJ %>% filter(date >= "2021-08-01")) +
  theme_bw()

covid_RJ %>%
  model(arima = fable::ARIMA(newDeaths ~ pdq(1,1,1)+PDQ(1,1,1))) %>%
  forecast(h = 14) %>%
  autoplot(covid_RJ %>% filter(date >= "2021-08-01")) +
  theme_bw()

covid_RJ %>%
  filter(date >= "2021-08-01") %>%
  model(ets = fable::ETS(newDeaths),
        arima = fable::ARIMA(newDeaths)) %>%
  forecast(h = 14) %>%
  autoplot(covid_RJ %>%
             filter(date >= "2021-08-01")) +
  theme_bw()


# prevendo para 3 regiões - SP, RJ e BRASIL (TOTAL)

fit_ARIMA3 = covid_3 %>%
  model(arima = fable::ARIMA(newDeaths))

fit_ARIMA3

fit_ARIMA3 %>%
  forecast(h = 14) %>%
  autoplot(covid_3 %>% filter(date >= "2021-08-01")) +
  theme_bw()

covid_3 %>%
  model(arima = fable::ARIMA(newDeaths ~ pdq(1,1,1)+PDQ(1,1,1))) %>%
  forecast(h = 14) %>%
  autoplot(covid_3 %>% filter(date >= "2021-08-01")) +
  theme_bw()

covid_3 %>%
  filter(date >= "2021-08-01") %>%
  model(ets = fable::ETS(newDeaths),
        arima = fable::ARIMA(newDeaths)) %>%
  forecast(h = 14) %>%
  autoplot(covid_3 %>%
             filter(date >= "2021-08-01")) +
  theme_bw()

# ----------------------------------------------------------------------------
# Previsão para a série de Consumo de Energia Elétrica GWh
# Ipeadata para Regiões S, SE, CO, NE e N
# Período de jan/1979 a set/21
###############################################################################

load("consumo_brasil.RData")

View(consumo_brasil)

# Para gerar um gráfico estático:
consumo_brasil %>% 
  ggplot(aes(x = Data, y = Consumo, color = regiao, group = regiao)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  scale_color_viridis_d("Região:") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 8),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey95"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")

# Para gerar um gráfico interativo:
ggplotly(
  consumo_brasil %>% 
    ggplot(aes(x = Data, y = Consumo, color = regiao, group = regiao)) +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +
    scale_color_viridis_d("Região") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 8),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey95"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "bottom")
) %>% layout(showlegend = TRUE,
             legend = list(orientation = "h"))

# Convertendo para TSIBBLE

consumo=as_tsibble(consumo_brasil, index = Data, key=regiao)

# Observando os dados
glimpse(consumo)
consumo

#------------------------------------------------------------------------------
# separando somente a série do consumo da região SE
cons_SE = consumo %>%
  filter(regiao == "SE") %>%
  select(Consumo)  

consumo %>%
  filter(regiao == "SE") %>%
  autoplot(Consumo, color = "#DCE319FF") +
  geom_line(aes(y = Consumo), color = "#440154FF") +
  labs(x="Meses",
       y="Consumo em GWh", title="Consumo em GWh - Região Sudeste") +
  theme_bw()

# Usamos a função model() gerar o modelo
# está configurado para gerar somente o modelo para a base SE

fit_SE = cons_SE %>% 
  model(ets = ETS(Consumo))

fit_SE

# -- separando somente SE de um segundo modo
consumo_fit_SE=consumo %>%
  filter(regiao == "SE") %>%
  model(
    energia.ets <- ETS(Consumo)
  )

consumo_fit_SE

# -- previsão somente para SE
consumo_fc_SE = consumo_fit_SE %>%
  forecast(h = 12)

# Observando as previsões

consumo_fc_SE %>% View()

# se quiser ver somente as previsões somente para SE
consumo_fc_SE %>%
  autoplot() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# se quiser ver somente as previsões para SE em conjunto com os dados
consumo_fc_SE %>%
  autoplot(consumo) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# Análise dos resíduos: Não devem ser correlacionados
# caso sejam correlacionados, dignifica que ficaram informações nos resíduos
# que deveriam estar no modelo
# os resíduos devem possuir média zero, caso não seja então as previsões são viesadas

augment(consumo_fit_SE)

# olhando apenas os resíduos do modelo ETS
augment(consumo_fit_SE) %>%
  filter(.model == "energia.ets <- ETS(Consumo)") %>%
  autoplot(.resid) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# Olhando a função de autocorrelação
augment(consumo_fit_SE) %>%
  filter(.model == "energia.ets <- ETS(Consumo)") %>%
  ACF(.resid) %>%
  autoplot() +
  theme_bw()

# fazendo uma análise mais técnica dos resíduos

# Teste de Ljung-box
# H0: os resíduos são iid
# H1: os resíduos não são iid
# não quero rejeitar H0 (quero um pvalor grande)

augment(consumo_fit_SE) %>%
  features(.resid, ljung_box)

# Medidas de acurácia --------------------------

# Separando uma parte dos dados para gerar o modelo
# Vamos separar a série de treino para os dados antes de nov/2020
# e fazer uma previsão para 12 meses até nov/2021

consumo_fit_SE2 = consumo %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao == "SE" & Data <= "2020-09-01") %>% 
  mutate(Data = yearmonth(Data)) %>% 
  model(energia.ets <- ETS(Consumo))

# prevendo 1 ano adiante
consumo_fit_SE2 %>%
  forecast(h = "1 year") %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao == "SE") %>% 
  autoplot() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

consumo_fc_SE2 = consumo_fit_SE2 %>%
  forecast(h = "1 year")

consumo_fc_SE2

accuracy(consumo_fc_SE2, consumo %>%
           filter(regiao == "SE"))

 
#### ------------------------------------------------------------------------
## Fazendo agora as previsões em conjunto para mais de uma região
#### ------------------------------------------------------------------------

# pegando mais de uma região
# Escolhendo as regiões SE, E e CO

consumo_fit2=consumo %>%
  filter(regiao %in% c("SE", "S", "CO")) %>%
  model(
    energia.ets <- ETS(Consumo)
  )

consumo_fit2

# previsao para mais regiões
# previsão para as regiões SE, S e CO

consumo_fc2 = consumo_fit2 %>%
  forecast(h = 12)
consumo_fc2 %>% View()

autoplot(consumo_fc2)

# se quiser ver as previsões para mais estados
consumo_fc2 %>%
  autoplot() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# se quiser ver somente as previsões para mais regiões em conjunto com os dados
consumo_fc2 %>%
  autoplot(consumo) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# Visualizando sem os intervalos de confiança para as regiões e os dados juntos
consumo_fc2 %>%
  autoplot(consumo, level = NULL) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()


# Análise dos resíduos: Não devem ser correlacionados
# caso sejam correlacionados, dignifica que ficaram informações nos resíduos
# que deveriam estar no modelo
# os resíduos devem possuir média zero, caso não seja então as previsões são viesadas


# se quiser ver todos os resíduos de todos os modelos
augment(consumo_fit2) %>%
  autoplot(.resid) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()



# fazendo uma análise mais técnica dos resíduos

# Teste de Ljung-box
# H0: os resíduos são iid
# H1: os resíduos não são iid
# não quero rejeitar H0 (quero um pvalor grande)

augment(consumo_fit2) %>%
  features(.resid, ljung_box)

# Medidas de acurácia -------------------------------------------------
# Vamos separar a série de treino para os dados antes de nov/2020
# e fazer uma previsão para 12 meses até nov/2021

 # para mais de uma série juntas
# Separando uma parte dos dados para gerar o modelo

consumo_fit4 = consumo %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao %in% c("SE", "S", "CO") & Data <= "2020-09-01") %>% 
  mutate(Data = yearmonth(Data)) %>% 
  model(energia.ets <- ETS(Consumo))

# prevendo 1 mês adiante
consumo_fit4 %>%
  forecast(h = "1 year") %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao %in% c("SE", "S", "CO")) %>% 
  autoplot() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

consumo_fc4 = consumo_fit4 %>%
  forecast(h = "1 year")

View(consumo_fc4)

accuracy(consumo_fc4, consumo %>%
           filter(regiao %in% c("SE", "S", "CO"))
)


######### Usando ARIMA e comparando com ETS

# ----- prevendo somente para SE

consumo_fit_energ = consumo %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao == "SE" & Data <= "2020-09-01") %>% 
  mutate(Data = yearmonth(Data)) %>% 
  model(arima = fable::ARIMA(Consumo),
        ets <- ETS(Consumo))

consumo_fit_energ

# prevendo 1 ano adiante
consumo_fit_energ %>%
  forecast(h = "1 year") %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao == "SE") %>% 
  autoplot() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

consumo_fit_energ_fc= consumo_fit_energ %>%
  forecast(h = "1 year")

consumo_fit_energ_fc

consumo_fit_energ_fc %>%
  autoplot(consumo) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

accuracy(consumo_fit_energ_fc, consumo %>%
           filter(regiao == "SE"))



# ----- prevendo somente para mais de uma região 

consumo_fit_energ2 = consumo %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao %in% c("SE", "S", "CO") & Data <= "2020-09-01") %>% 
  mutate(Data = yearmonth(Data)) %>% 
  model(arima = fable::ARIMA(Consumo),
        ets <- ETS(Consumo))

consumo_fit_energ2

# prevendo 1 ano adiante
consumo_fit_energ2 %>%
  forecast(h = "1 year") %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao %in% c("SE", "S", "CO")) %>% 
  autoplot() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

consumo_fit_energ_fc2= consumo_fit_energ2 %>%
  forecast(h = "1 year")

consumo_fit_energ_fc2

consumo_fit_energ_fc2 %>%
  autoplot(consumo) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

accuracy(consumo_fit_energ_fc2, consumo %>%
           filter(regiao %in% c("SE", "S", "CO"))
)                  

