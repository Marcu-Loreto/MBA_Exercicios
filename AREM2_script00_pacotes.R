########################
# Instalação de pacotes
pacotes <- c(
             'tidyverse',  # Pacote básico de datawrangling
             'rpart',      # Biblioteca de árvores
             'rpart.plot', # Conjunto com Rpart, plota a parvore
             'gtools',     # funções auxiliares como quantcut,
             'Rmisc',      # carrega a função sumarySE para a descritiva
             'scales',     # importa paletas de cores
             'viridis',    # Escalas 'viridis' para o ggplot2
             'caret',       # Funções úteis para machine learning
             'AMR',
             'randomForest',
             'fastDummies',
             'rattle',
             'xgboost',
             'ggpubr'
             
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

########################## Funções de apoio ####################################
descritiva <- function(var){
  # Sumariza a taxa de sobreviventes por categoria da variável em análise
  tgc <- Rmisc::summarySE(tmp, measurevar="survived", groupvars=c(var))
  
  ggplot(tgc) + 
    # Plota o gráfico de barras com as frequências
    geom_bar(aes(x=tgc[,var], weight=N/891, fill=as.factor(tgc[,var]))) + 
    # Plota as barras de erro
    geom_errorbar(aes(x=tgc[,var], y=survived, ymin=survived-se, ymax=survived+se, colour='1'), width=.1) +
    # Plota as médias de cada grupo
    geom_point(aes(x=tgc[,var], y=survived, colour='1', group='1')) +
    # Plota as linhas que conectam as médias
    geom_line(aes(x=tgc[,var], y=survived, colour='1', group='1')) +
    # Escala de cores do gráfico de médias
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    # Escala de cores do gráfico de barras
    scale_fill_viridis_d(direction = -1, begin=.85, end=.95) +
    # Estética mais 'leve' do gráfico
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    # Remove a legenda
    theme(legend.position = "none") +
    # Rótulo dos eixos
    xlab(var) + ylab("Taxa de sobreviventes") + 
    # Marcas do eixo secundário
    scale_y_continuous(sec.axis = sec_axis(~.*891, name = "Frequencia"), labels = scales::percent)
}


descritiva2 <- function(var, resp, df) {
  # Sumariza a taxa de sobreviventes por categoria da variável em análise
  tgc <- Rmisc::summarySE(df, measurevar = resp, groupvars = c(var))
  maxN <- max(tgc$N)
  
  # Gráfico de barras
  p <- ggplot(tgc) +
    geom_bar(aes(x = tgc[,var], 
                 y = max(tgc[,resp])*N/maxN, 
                 fill = as.factor(tgc[,var])), 
             position = "identity", stat = "identity", 
             alpha = 0.5) +
    scale_fill_viridis_d(direction = -1, begin = .85, end = .95)
  
  # Gráfico de linhas
  p <- p +
    geom_line(aes(x = tgc[,var], y = tgc[,resp]), colour = '1', group = '1') +
    geom_point(aes(x = tgc[,var], y = tgc[,resp] ), colour = '1', group = '1') +
    geom_errorbar(aes(x = tgc[,var], 
                      y = tgc[,resp], 
                      ymin = tgc[,resp] + qnorm(.025)*se, 
                      ymax = tgc[,resp] + qnorm(.975)*se, colour = '1'), width = .5) +
    
    #geom_point(aes(x = tgc[,var], y = tgc[,resp] - tgc[,ep]*qnorm(.975)), colour = '1', group = '1') +
    scale_color_viridis_d(direction = -1, begin = 0, end = .25)
  
  # Ajuste dos eixos
  p <- p +
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey"),
          axis.text = element_text(size = 14),  # Tamanho da fonte dos números dos eixos
          axis.title = element_text(size = 16),  # Tamanho da fonte dos títulos dos eixos
          legend.position = "none") +
    xlab(var) + ylab("Barras")
  
  p <- p +
    scale_y_continuous(sec.axis = sec_axis(~ . *maxN/max(tgc[,resp]), name = "Frequencia", labels = scales::number)) +
    ylab(resp) +
    # Limite do eixo vertical esquerdo
    coord_cartesian(ylim = c(min(tgc[,resp]) - 0.02, max(tgc[,resp]) + 0.02))
  
  return(p)
}
