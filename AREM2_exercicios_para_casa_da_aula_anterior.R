set.seed(42)  # Define uma semente aleatória para reprodução dos resultados

# Gerar as variáveis simuladas com correlação
idade <- sample(18:70, 10000, replace = TRUE)

# Gerar variáveis correlacionadas usando a função rmvnorm() do pacote mvtnorm
library(mvtnorm)

mean_values <- c(5000, 2000, 0.5, 5)  # Médias das variáveis
correlation_matrix <- matrix(c(1, 0.3, 0.2, -0.1, 0.3, 1, -0.1, 0.2, 0.2, -0.1, 1, 0.4, -0.1, 0.2, 0.4, 1), nrow = 4)  # Matriz de correlação

simulated_data <- rmvnorm(10000, mean = mean_values, sigma = correlation_matrix)

renda <- simulated_data[, 1]
divida <- simulated_data[, 2]
utilizacao_credito <- pmin(pmax(simulated_data[, 3], 0), 1)  # Limita a utilização de crédito entre 0 e 1
consultas_recentes <- pmax(simulated_data[, 4], 0)  # Garante que o número de consultas recentes seja não negativo

# Gerar função linear das variáveis explicativas
preditor_linear <- -7 - 0.01*idade - 0.0002*renda + 0.003*divida - 3*utilizacao_credito + 0.5*consultas_recentes

# Calcular probabilidade de default (PD) usando a função de link logit
prob_default <- plogis(preditor_linear)
#OBS: problogis(x) é equivalente ao 1/(1+exp(-x))

# Gerar inadimplência como variável Bernoulli com base na probabilidade de default
inadimplencia <- rbinom(10000, size = 1, prob = prob_default)

# Criar dataframe
dados <- data.frame(idade, renda, divida, utilizacao_credito, consultas_recentes, inadimplencia)


head(dados)
tmp = dados

tmp$idade_cat <- quantcut(tmp$idade, 5)
descritiva2("idade_cat", "inadimplencia", tmp)


head(tmp)
tmp %>% colnames

#####################################################################################
# Agora é a sua vez: Ajuste uma árvore de decisão, e explore os recursos que fizemos 
# ao longo da aula nesta base de dados ;)

tmp$inadimplencia <- factor(tmp$inadimplencia, levels = c(0, 1), labels = c("N", "Y"))

###############################################
# Vamos separar a base em treinamento e teste #
set.seed(123)
bool_treino <- stats::runif(dim(tmp)[1])>.25

treino <- tmp[bool_treino,]
teste  <- tmp[!bool_treino,]

titanic %>% str
tmp %>% colnames

##################################################
# treinar a árvore na base de TREINO  livremente #

set.seed(123)
arvore <- rpart::rpart(inadimplencia ~ idade 
                                      + renda 
                                      + divida 
                                      + utilizacao_credito 
                                      + consultas_recentes,
                       data=treino,
                       method='class',
                       xval=5,
                       control = rpart.control(cp = 0, 
                                               minsplit = 1, 
                                               maxdepth = 30)
)


############################################
# Avaliar a árvore na base de treino
p_treino = stats::predict(arvore, treino)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "Y", "N"))
p_teste = stats::predict(arvore, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

c_teste
c_treino




###############################
# Curva ROC                   #
###############################

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_treino <- data.frame(obs=treino$inadimplencia, 
                          pred=c_treino,
                          Y = p_treino[,2],
                          N = 1-p_treino[,2]
)

aval_treino %>% head

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")

CurvaROC


############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$inadimplencia, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC


##########################
# pós-poda (Grid Search) #
##########################

tab_cp <- rpart::printcp(arvore)

plotcp(arvore)


tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda <- rpart::rpart(inadimplencia ~ idade 
                            + renda 
                            + divida 
                            + utilizacao_credito 
                            + consultas_recentes,
                            data=treino,
                            method='class',
                            xval=0,
                            control = rpart.control(cp = cp_min, 
                                                    minsplit = 1, 
                                                    maxdepth = 30)
)

p_treino = stats::predict(arvore_poda, treino)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "Y", "N"))
p_teste = stats::predict(arvore_poda, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

#####
aval_treino <- data.frame(obs=treino$inadimplencia, 
                          pred=c_treino,
                          Y = p_treino[,2],
                          N = 1-p_treino[,2]
)

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")

CurvaROC

############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$inadimplencia, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC
