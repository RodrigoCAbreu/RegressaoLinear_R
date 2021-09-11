######### INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS

#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
  
  
####################### REGRESSÃO LINEAR SIMPLES ##############################
  
# Carregando a base de dados
load(file = "tempodist.RData")
  

# OBSERVANDO OS DADOS CARREGADOS DO DATASET tempodist
  
tempodist %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                  full_width = F,
                  font_size = 22)

#Estatísticas univariadas
summary(tempodist) 

# GRÁFICO DE DISPERSÃO COM GGPLOT

ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", se = F, size = 2) +
    xlab("Distância") +
    ylab("Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

#Estimando o modelo
modelo_tempodist <- lm(formula = tempo ~ distancia,
                       data = tempodist)

#Observando os parâmetros do modelo_tempodist
summary(modelo_tempodist)

###Outras maneiras de apresentar os outputs do modelo

#função summ do pacote jtools
summ(modelo_tempodist, confint = T, digits = 4, ci.width = .95)

export_summs(modelo_tempodist, scale = F, digits = 4)

#Inserindo fitted values (variável yhat) e residuals (variável erro) no dataset
tempodist$yhat <- modelo_tempodist$fitted.values

tempodist$erro <- modelo_tempodist$residuals


#Gráfico didático para visualizar o conceito de R²
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", se = F, size = 2) +
    geom_hline(yintercept = 30, color = "grey50", size = .5) +
    geom_segment(aes(color = "Ychapéu - Ymédio", x = distancia, xend = distancia,
                     y = yhat, yend = mean(tempo)), size = 0.7, linetype = 2) +
    geom_segment(aes(color = "Erro = Y - Ychapéu", x = distancia, xend = distancia,
                     y = tempo, yend = yhat), size = 0.7, linetype = 3) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = c("#55C667FF", "grey50", "#440154FF")) +
    theme_classic()
)


#Plotando o Intervalo de Confiança de 90%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.90,) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)


#Plotando o Intervalo de Confiança de 95%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.95) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

#Plotando o Intervalo de Confiança de 99%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.99) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

#Calculando os intervalos de confiança (função confint)

confint(modelo_tempodist, level = 0.90) # siginificância 10%

confint(modelo_tempodist, level = 0.95) # siginificância 5%

confint(modelo_tempodist, level = 0.99) # siginificância 1%

#Fazendo predições em modelos OLS

# Ex: Qual seria o tempo gasto, em média, para percorrer a distância de 25km?
predict(object = modelo_tempodist,
        data.frame(distancia = 25))

# Predições com os Intervalos de confiança
predict(object = modelo_tempodist,
        data.frame(distancia = 25),
        interval = "confidence", level = 0.95)


####################### REGRESSÃO LINEAR MÚLTIPLA ##############################

# Carregando a base de dados
load(file = "paises.RData")

#Estatísticas univariadas
summary(paises)

#Gráfico 3D com scatter
scatter3d(cpi ~ idade + horas,
          data = paises,
          surface = F,
          point.col = "#440154FF",
          axis.col = rep(x = "black",
                         times = 3))

#Visualizando a base de dados
paises %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

#Análise das correlações entre as variáveis

# função correlation

paises %>%
  correlation(method = "pearson") %>%
  plot()

# função chart.Correlation()

chart.Correlation((paises[2:4]), histogram = TRUE)

#Estimando o modelo de regressão
modelo_paises <- lm(formula = cpi ~ . - pais,
                    data = paises)

#Parâmetros do modelo
summary(modelo_paises)

#Calculando intervalo de confiança
confint(modelo_paises, level = 0.95) # siginificância de 5%

#Inserindo os fitted values na base de dados
paises$cpifit <- modelo_paises$fitted.values

#Gráfico 3D com scatter e fitted values
scatter3d(cpi ~ idade + horas,
          data = paises,
          surface = T, fit = "linear",
          point.col = "#440154FF",
          axis.col = rep(x = "black",
                         times = 3))


############ REGRESSÃO COM UMA VARIÁVEL EXPLICATIVA QUALITATIVA ###############

# Carregando a base de dados
load(file = "corrupcao.RData")

#Visualização das observações
glimpse(corrupcao)

#Observando os rótulos da variável regiao
levels(glimpse(corrupcao$regiao)) 

#Tabela de frequências da variável regiao
table(corrupcao$regiao) 

#Estatísticas univariadas
summary(corrupcao)

#Dummizando a variável regiao, estabelecendo como categoria de referência a 
#dummy mais frequente (PROCEDIMENTO N-1 DUMMIES)

corrupcao_dummies <- dummy_columns(.data = corrupcao,
                                   select_columns = "regiao",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = T)

#Visualizando a base de dados dummizada
corrupcao_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

#Estimando o modelo de regressão
modelo_corrupcao_dummies <- lm(cpi ~ . - pais, corrupcao_dummies)

#Parâmetros do modelo_corrupcao_dummies
summary(modelo_corrupcao_dummies)

#Plotando o modelo_corrupcao_dummies de forma interpolada
my_plot3 <- 
  corrupcao %>%
  mutate(rotulo = paste(pais, cpi)) %>%
  ggplot(aes(x = as.numeric(regiao), y = cpi, label = rotulo)) +
  geom_point(color = "#FDE725FF") +
  stat_smooth(aes(color = "Fitted Values"),
              method = "lm", 
              formula = y ~ bs(x, df = 4)) +
  labs(x = "Região",
       y = "Corruption Perception Index") +
  scale_x_discrete(labels = c("1" = "América do Sul", 
                              "2" = "Oceania", 
                              "3" = "Europa", 
                              "4" = "EUA e Canadá", 
                              "5" = "Ásia")) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  geom_text_repel() +
  theme_bw()
my_plot3
