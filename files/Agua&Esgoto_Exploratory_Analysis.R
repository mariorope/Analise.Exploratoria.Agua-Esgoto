# Carregando os pacotes
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(corrplot)

# Carregando os dados
df <- fread('../data/planilha_resumo.csv', skip = 8)

# Visualizando os dados
View(df)

# Fazendo o update do dataframe com as linhas 4, 1 e todas as outras partir da 5
df <- rbind(rbind(df[4,],df[1,]), df[5:length(df$V1),])

# Nomeando a variável que contem os estados, pois a linha 1 será utilizada como nome das variáveis (colunas)
df[1,1] <- "State"

# Renomeando as colunas, utilizando a linha 1 como character
colnames(df) <- sapply(df[1,], as.character)

# Removendo a primeira linha dos dados, pois já não precisamos mais dela
df <- df[-1,]

# Visualizando as mudanças
View(df)

# Selecionando as variáveis de interesse para esta análise exploratória
df <- df %>%
  select(State, GE05a, GE05b, FN002, FN003, AG001, AG007, AG010, ES001, ES005, ES006)

# Renomeando a primeira linha dos dados para facilitar o entendimento das variáveis bem como para a criação de um dicionário de dados para consulta
df[1,4] <- "Receitas operacionais diretas com agua"
df[1,5] <- "Receitas operacionais diretas com esgoto"
df[1,7] <- "Volume de agua tratada em ETAs"
df[1,8] <- "Volume de agua consumido"
df[1,10] <- "Volume de esgoto coletado"
df[1,11] <- "Volume de esgoto tratado"

# Criando um dataframe com duas colunas (symbol e description)
df_dict <- data.frame(symbol = colnames(df),
                      description = t(df[1,])[,1])

# Removendo a primeira linha dos dados, pois não contém informação
df_dict <- df_dict[-1,]

# Removendo a coluna Symbol, pois já temos os símbolos das variáveis como index
df_dict$symbol <- NULL

# Visualizando o dicionário de dados
df_dict

# Agora podemos remover a primeira linha do conjunto de dados df, pois já temos o dicionário de dados para consulta
df <- df[-1,]

# Visualizando os tipos de dados
glimpse(df)

# Substituindo vírgula por ponto com decimal e removendo pontos como separador de milhares para todas as variáveis que serão transformadas em numéricas (exceto State)
df$GE05a <- sapply(df$GE05a, function(x) { ifelse(is.na(x), NA ,gsub(",", ".", gsub("\\.", "", x))) } )
df$GE05b <- sapply(df$GE05b, function(x) { ifelse(is.na(x), NA ,gsub(",", ".", gsub("\\.", "", x))) } )
df$FN002 <- sapply(df$FN002, function(x) { ifelse(is.na(x), NA ,gsub(",", ".", gsub("\\.", "", x))) } )
df$FN003 <- sapply(df$FN003, function(x) { ifelse(is.na(x), NA ,gsub(",", ".", gsub("\\.", "", x))) } )
df$AG001 <- sapply(df$AG001, function(x) { ifelse(is.na(x), NA ,gsub(",", ".", gsub("\\.", "", x))) } )
df$AG007 <- sapply(df$AG007, function(x) { ifelse(is.na(x), NA ,gsub(",", ".", gsub("\\.", "", x))) } )
df$AG010 <- sapply(df$AG010, function(x) { ifelse(is.na(x), NA ,gsub(",", ".", gsub("\\.", "", x))) } )
df$ES001 <- sapply(df$ES001, function(x) { ifelse(is.na(x), NA ,gsub(",", ".", gsub("\\.", "", x))) } )
df$ES005 <- sapply(df$ES005, function(x) { ifelse(is.na(x), NA ,gsub(",", ".", gsub("\\.", "", x))) } )
df$ES006 <- sapply(df$ES006, function(x) { ifelse(is.na(x), NA ,gsub(",", ".", gsub("\\.", "", x))) } )

# Tranformando as variáveis numéricas para dados do tipo numérico
df$GE05a <- sapply(df$GE05a, function(x) { as.numeric(x) } )
df$GE05b <- sapply(df$GE05b, function(x) { as.numeric(x) } )
df$FN002 <- sapply(df$FN002, function(x) { as.numeric(x) } )
df$FN003 <- sapply(df$FN003, function(x) { as.numeric(x) } )
df$AG001 <- sapply(df$AG001, function(x) { as.numeric(x) } )
df$AG007 <- sapply(df$AG007, function(x) { as.numeric(x) } )
df$AG010 <- sapply(df$AG010, function(x) { as.numeric(x) } )
df$ES001 <- sapply(df$ES001, function(x) { as.numeric(x) } )
df$ES005 <- sapply(df$ES005, function(x) { as.numeric(x) } )
df$ES006 <- sapply(df$ES006, function(x) { as.numeric(x) } )

# Criando a variável Region na tabela de dados df
df$Region <- c(rep('Norte',9), rep('Nordeste',11), rep('Sudeste',6), rep('Sul',5), rep('Centro_Oeste',6), 'Brazil')

# Criando uma nova tabela de dados com os dados das regiões
df_per_Region <- df[c(9,20,26,31,37),]

# Renomeando a primeira coluna (regiões)
colnames(df_per_Region)[1] <- 'Region'

# Criando uma nova variável Region na tabela de dados df_per_Region
df_per_Region$Region <- c('Norte', 'Nordeste', 'Sudeste', 'Sul', 'Centro_Oeste')

# Visualizando a tabela de dados df_per_Region
View(df_per_Region)

# Visualizando a tabela de dados df
View(df)

# Removendo as linhas que não contém informação estadual
df <- df[-c(1,9,10,20,21,26,27,31,32,37,38),]

# Criando duas novas colunas contendo o número de municípios e a população
df <- df %>%
  mutate(
    Num_Mun = c(22,16,62,144,52,15,139,102,417,184,217,223,185,224,
                167,75,78,853,92,645,399,497,295,1,246,79,141),
    Population = c(894470,861773,4207714,8690745,1796460,631181,1590248,
                   3351543,14930634,9187103,7114598,4039277,9616621,3281480,
                   3534165,2318822,4064052,21292666,17366189,46289333,11516840,
                   11422973,7252502,3055149,7113540,2809394,3526220)
  )

# Ajustando o dado que estava estranho (row 7)
for (i in 1:length(df$GE05a)) {
  if (df$GE05a[i] > df$Num_Mun[i]) {
    df$GE05a[i] <- df$Num_Mun[i]
  }
}

# Visualizando as transformações na tabela df
View(df)

# Apresentando estatística descritiva dos dados
summary(df)

# Calculando a correlação para as variáveis numéricas
df_cor <- cor(df[,-c('State','Region')], method = "spearman")

# Resultados da análise de correlação
df_cor

# Resultados da análise de correlação de forma gráfica
corrplot(df_cor)

ggplot(df) +
  geom_bar(aes(x=State, y=(GE05a / Num_Mun)*100),
           stat = 'identity') +
  theme_bw() +
  ylab('Municípios atendidos (%)') +
  xlab('Estado') +
  ggtitle('Proporção de municipios atendido com abastecimento por Estado') +
  theme(
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(hjust = 0.5)
  )

ggplot(df) +
  geom_bar(aes(x=State, y=(GE05b / Num_Mun)*100),
           stat = 'identity') +
  theme_bw() +
  ylab('Municípios atendidos (%)') +
  xlab('Estado') +
  ggtitle('Proporção de municipios atendido com esgotamento por Estado') +
  theme(
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(hjust = 0.5)
  )

ggplot(df) +
  geom_bar(aes(x=State, y=AG001 / Population * 100),
           stat = 'identity') +
  theme_bw() +
  ylab('População atendida (%)') +
  xlab('Estado') +
  ggtitle('Proporção da população atendida com abastecimento por Estado') +
  theme(
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(hjust = 0.5)
  )

ggplot(df) +
  geom_bar(aes(x=State, y=ES001 / Population * 100),
           stat = 'identity') +
  theme_bw() +
  ylab('População atendida (%)') +
  xlab('Estado') +
  ggtitle('Proporção da população atendida com esgotamento por Estado') +
  theme(
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(hjust = 0.5)
  )

ggplot(df) +
  geom_bar(aes(x=State, y=FN002 / Population),
           stat = 'identity') +
  theme_bw() +
  ylab('Receitas operacionais (R$)') +
  xlab('Estado') +
  ggtitle('Receitas operacionais direta com água per capta por Estado') +
  theme(
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(hjust = 0.5)
  )

ggplot(df) +
  geom_bar(aes(x=State, y=FN003 / Population),
           stat = 'identity') +
  theme_bw() +
  ylab('Receitas operacionais (R$)') +
  xlab('Estado') +
  ggtitle('Receitas operacionais direta com esgoto per capta por Estado') +
  theme(
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(hjust = 0.5)
  )

ggplot(df) +
  geom_point(aes(x=FN002 / Population, y=AG001 / Population * 100),
             color='blue', alpha=0.5) +
  theme_bw() +
  ylab('População atendida com abastecimento (%)') +
  xlab('Receitas operacionais per capta com abastecimento (R$)') +
  ggtitle('População atendida X Receitas operacionais per capta - abastecimento') +
  ylim(0,100) +
  xlim(0,NA) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggplot(df) +
  geom_point(aes(x=FN003 / Population, y=ES001 / Population * 100),
             color='brown', alpha=0.5) +
  theme_bw() +
  ylab('População atendida com esgotamento (%)') +
  xlab('Receitas operacionais per capta com esgotamento (R$)') +
  ggtitle('População atendida X operacionais per capta - esgotamento') +
  ylim(0,100) +
  xlim(0,NA) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggplot(df) +
  geom_bar(aes(x=State, y=AG007 / Population * 1000),
           stat = 'identity', fill='lightblue', alpha=0.5) +
  geom_bar(aes(x=State, y=AG010 / Population * 1000),
           stat = 'identity', fill='blue', alpha=0.5) +
  geom_bar(aes(x=State, y=ES005 / Population * 1000),
           stat = 'identity', fill='brown', alpha=0.5) +
  geom_bar(aes(x=State, y=ES006 / Population * 1000),
           stat = 'identity', fill='red', alpha=0.5) +
  theme_bw() +
  ylab('Volume per capta (L)') +
  xlab('Estado') +
  ggtitle('Volume de água tratada e consumida, esgoto coletado e tratado') +
  theme(
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(hjust = 0.5)
  )

# Criando a variável pop_water_prop que representa a proporção da população com abastecimento
df$pop_water_prop <- round(df$AG001/df$Population*100,1)

# Criando a variável pop_sewage_prop que representa a proporção da população com esgotamento
df$pop_sewage_prop <- round(df$ES001/df$Population*100,1)

gg <- ggplot(df) +
  geom_point(aes(x=pop_water_prop,
                 y=pop_sewage_prop,
                 color=Region,
                 size=Population,
                 group=State)) +
  theme_bw() +
  xlab("% da população atendida com abastecimento") +
  ylab("% da população atendida com esgotamento") +
  ggtitle("% da população atentida com água e esgoto") +
  ylim(0,100) +
  xlim(0,100) +
  guides(color= guide_legend(), size=guide_legend()) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggplotly(gg) %>%
  highlight("plotly_hover")

