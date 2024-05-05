knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(corrplot)
library(gridExtra)
library(jpeg)
library(patchwork)

df <- fread('../data/planilha_organizada.csv', skip = 8)
head(df)

df <- rbind(rbind(df[4,],df[1,]), df[5:length(df$V1),])
df[1,1] <- "estado"
colnames(df) <- sapply(df[1,], as.character)
df <- df[-c(1,2),]

df <- df %>%
  select(estado, GE05a, GE05b, FN002, FN003, AG001, AG007, AG010, ES001, ES005, ES006)

df[1,4] <- "Receitas operacionais diretas com agua"
df[1,5] <- "Receitas operacionais diretas com esgoto"
df[1,7] <- "Volume de agua tratada em ETAs"
df[1,8] <- "Volume de agua consumida"
df[1,10] <- "Volume de esgoto coletado"
df[1,11] <- "Volume de esgoto tratado"
head(df)

dim(df)

glimpse(df)


df$GE05a <- as.integer(df$GE05a)
df$GE05b <- as.integer(df$GE05b)
df$FN002 <- as.double(sub("\\,", ".", sub("\\.", "", sub("\\.", "", sub("\\.", "", df$FN002)))))
df$FN003 <- as.double(sub("\\,", ".", sub("\\.", "", sub("\\.", "", sub("\\.", "", df$FN003)))))
df$AG001 <- as.integer(sub("\\,", ".", sub("\\.", "", sub("\\.", "", sub("\\.", "", df$AG001)))))
df$AG007 <- as.double(sub("\\,", ".", sub("\\.", "", sub("\\.", "", sub("\\.", "", df$AG007)))))
df$AG010 <- as.double(sub("\\,", ".", sub("\\.", "", sub("\\.", "", sub("\\.", "", df$AG010)))))
df$ES001 <- as.integer(sub("\\,", ".", sub("\\.", "", sub("\\.", "", sub("\\.", "", df$ES001)))))
df$ES005 <- as.double(sub("\\,", ".", sub("\\.", "", sub("\\.", "", sub("\\.", "", df$ES005)))))
df$ES006 <- as.double(sub("\\,", ".", sub("\\.", "", sub("\\.", "", sub("\\.", "", df$ES006)))))

df$regiao <- c(rep('Norte',9), rep('Nordeste',11), rep('Sudeste',6), rep('Sul',5), rep('Centro_Oeste',6), 'Brazil')

df_estadual <- df[-c(1,9,10,20,21,26,27,31,32,37,38),]
df_estadual

df_estadual <- df_estadual %>%
  mutate(
    num_mun = c(22,16,62,144,52,15,139,102,417,184,217,223,185,224,
                167,75,78,853,92,645,399,497,295,1,246,79,141),
    populacao = c(894470,861773,4207714,8690745,1796460,631181,1590248,
                   3351543,14930634,9187103,7114598,4039277,9616621,3281480,
                   3534165,2318822,4064052,21292666,17366189,46289333,11516840,
                   11422973,7252502,3055149,7113540,2809394,3526220)
  )

for (i in 1:length(df_estadual$GE05a)) {
  if (df_estadual$GE05a[i] > df_estadual$num_mun[i]) {
    print('Problem with GE05a')
    print(df_estadual$estado[i])
  }
  if (df_estadual$GE05b[i] > df_estadual$num_mun[i]) {
    print('Problem with GE05b')
    print(df_estadual$estado[i])
  }
}

df_estadual[df_estadual$estado == "Tocantins (TO)"]

for (i in 1:length(df_estadual$GE05a)) {
  if (df_estadual$estado[i] == 'Tocantins (TO)') {
    df_estadual$GE05a[i] <- 134
  }
}

for (i in 1:length(df_estadual$AG001)) {
  
  if (df_estadual$AG001[i] > df_estadual$populacao[i]) {
    print('Problem with AG001')
    print(i)
  }
  
  if (df_estadual$ES001[i] > df_estadual$populacao[i]) {
    print('Problem with ES001')
    print(i)
  }
}

AG007_plot <- ggplot(data=df_estadual) +
  geom_boxplot(mapping = aes(y=AG007/populacao)) +
  ggtitle('AG007') +
  ylim(0,0.1) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text('none'),
    axis.text.x = element_text('none'),
    axis.text.y = element_text(size=9),
    axis.ticks.x = element_blank(),
    legend.position = 'none',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

AG010_plot <- ggplot(data=df_estadual) +
  geom_boxplot(mapping = aes(y=AG010/populacao)) +
  ggtitle('AG010') +
  ylim(0,0.1) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text('none'),
    axis.text.x = element_text('none'),
    axis.text.y = element_text(size=9),
    axis.ticks.x = element_blank(),
    legend.position = 'none',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

ES005_plot <- ggplot(data=df_estadual) +
  geom_boxplot(mapping = aes(y=ES005/populacao)) +
  ggtitle('ES005') +
  ylim(0,0.1) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text('none'),
    axis.text.x = element_text('none'),
    axis.text.y = element_text(size=9),
    axis.ticks.x = element_blank(),
    legend.position = 'none',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

ES006_plot <- ggplot(data=df_estadual) +
  geom_boxplot(mapping = aes(y=ES006/populacao)) +
  ggtitle('ES006') +
  ylim(0,0.1) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text('none'),
    axis.text.x = element_text('none'),
    axis.text.y = element_text(size=9),
    axis.ticks.x = element_blank(),
    legend.position = 'none',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )
  
ggarrange(AG007_plot,
          AG010_plot,
          ES005_plot,
          ES006_plot,
          ncol = 2, nrow = 2)

for (i in 1:length(df_estadual$AG010)) {
  
  if (df_estadual$AG010[i] > df_estadual$AG007[i]) {
    print('Problem with AG010-AG007')
    print(i)
  }
  
  if (df_estadual$ES005[i] > df_estadual$AG010[i]) {
    print('Problem with ES005-AG010')
    print(i)
  }
  
  if (df_estadual$ES006[i] > df_estadual$ES005[i]) {
    print('Problem with ES006-ES005')
    print(i)
  }
}

df_estadual %>%
  select(estado, AG007, AG010) %>%
  slice(4, 7, 8, 11, 15, 26)

for (i in 1:length(df_estadual$AG010)) {
  if (df_estadual$AG010[i] > df_estadual$AG007[i]) {
    temp <- df_estadual$AG010[i]
    df_estadual$AG010[i] <- df_estadual$AG007[i]
    df_estadual$AG007[i] <- temp
  }
}

df_nacional <- df_estadual %>%
  summarise(country = 'Brasil',
            populacao = sum(populacao),
            num_mun = sum(num_mun),
            GE05a = sum(GE05a),
            GE05b = sum(GE05b),
            FN002 = sum(FN002),
            FN003 = sum(FN003),
            AG001 = sum(AG001),
            AG007 = sum(AG007),
            AG010 = sum(AG010),
            ES001 = sum(ES001),
            ES005 = sum(ES005),
            ES006 = sum(ES006))

summary(df_estadual)

df_estadual <- df_estadual %>%
  mutate(prop_mun_aten_abas_agua = GE05a / num_mun * 100,
         prop_mun_aten_esgo_sani = GE05b / num_mun * 100,
         rec_oper_dir_agua_mun_aten = FN002 / GE05a,
         rec_oper_dir_esgo_mun_aten = FN003 / GE05b,
         rec_oper_dir_agua_pop_aten = FN002 / AG001,
         rec_oper_dir_esgo_pop_aten = FN003 / ES001,
         prop_pop_aten_abas_agua = AG001 / populacao * 100,
         prop_vol_agua_trat_pop_aten = (AG007 * 1000) / AG001,
         prop_vol_agua_cons_pop_aten = (AG010 * 1000) / AG001,
         prop_pop_aten_esgo_sani = ES001 / populacao * 100,
         prop_vol_esgo_cole_pop_aten = (ES005 * 1000) / AG001,
         prop_vol_esgo_trat_pop_aten = (ES006 * 1000) / AG001,
         prop_vol_agua_cons_agua_trat = AG010 / AG007 * 100,
         prop_vol_esgo_cole_agua_cons = ES005 / AG010 * 100,
         prop_vol_esgo_cole_agua_cons = ES005 / (AG010) * 100,
         prop_vol_esgo_trat_esgo_cole = ES006 / ES005  * 100,
         prop_vol_esgo_trat_agua_cons = ES006 / AG010 * 100,
         prop_vol_esgo_trat_agua_cons = ES006 / (AG010)  * 100)

df_nacional <- df_nacional %>%
  mutate(prop_mun_aten_abas_agua = GE05a / num_mun * 100,
         prop_mun_aten_esgo_sani = GE05b / num_mun * 100,
         rec_oper_dir_agua_mun_aten = FN002 / GE05a,
         rec_oper_dir_esgo_mun_aten = FN003 / GE05b,
         rec_oper_dir_agua_pop_aten = FN002 / AG001,
         rec_oper_dir_esgo_pop_aten = FN003 / ES001,
         prop_pop_aten_abas_agua = AG001 / populacao * 100,
         prop_vol_agua_trat_pop_aten = (AG007 * 1000) / AG001,
         prop_vol_agua_cons_pop_aten = (AG010 * 1000) / AG001,
         prop_pop_aten_esgo_sani = ES001 / populacao * 100,
         prop_vol_esgo_cole_pop_aten = (ES005 * 1000) / AG001,
         prop_vol_esgo_trat_pop_aten = (ES006 * 1000) / AG001,
         prop_vol_agua_cons_agua_trat = AG010 / AG007 * 100,
         prop_vol_esgo_cole_agua_cons = ES005 / AG010 * 100,
         prop_vol_esgo_cole_agua_cons = ES005 / (AG010) * 100,
         prop_vol_esgo_trat_esgo_cole = ES006 / ES005  * 100,
         prop_vol_esgo_trat_agua_cons = ES006 / AG010 * 100,
         prop_vol_esgo_trat_agua_cons = ES006 / (AG010)  * 100)

df_estadual$estado_sigla = ''
df_estadual$estado_sem_sigla = ''
for (i in 1:length(df_estadual$estado)) {
  position = unlist(gregexpr('\\(', df_estadual$estado[i]))
  df_estadual$estado_sigla[i] <- substr(df_estadual$estado[i], position+1, position+2)
  df_estadual$estado_sem_sigla[i] <- substr(df_estadual$estado[i], 1, position-1)
}

# Criando o plot para a proporção de municípios atendidos com abastecimento de água
prop_mun_aten_abas_agua_plot <-
  ggplot() +
  geom_col(data=df_estadual, mapping=aes(x=estado_sigla, y=prop_mun_aten_abas_agua,  color=regiao, fill=regiao), alpha=0.5) +
  geom_hline(data=df_nacional, mapping=aes(yintercept = prop_mun_aten_abas_agua), color='red', size=0.2) + ylim(0,100) +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  labs(x = 'Estado', y='Proporção de Municípios (%)') +
   ggtitle('Proporção de Municípios Atendidos com Abastecimento Água') +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text(size=11,face='bold'),
    axis.text = element_text(size=10),
    legend.title = element_text(size =11, face = 'bold'),
    legend.text = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

# Criando o plot para a proporção de municípios atendidos com esgotamento sanitário
prop_mun_aten_esgo_sani_plot <-
  ggplot() +
  geom_col(data=df_estadual, mapping=aes(x=estado_sigla, y=prop_mun_aten_esgo_sani, color=regiao, fill=regiao), alpha=0.5) +
  geom_hline(data=df_nacional, mapping=aes(yintercept = prop_mun_aten_esgo_sani), color='red', size=0.2) + ylim(0,100) +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  labs(x = 'Estado', y='Proporção de Municípios (%)') +
  ggtitle('Proporção de Municípios Atendidos com Esgotamento Sanitário') +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text(size=11,face='bold'),
    axis.text = element_text(size=10),
    legend.title = element_text(size =11, face = 'bold'),
    legend.text = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

ggarrange(prop_mun_aten_abas_agua_plot,
          prop_mun_aten_esgo_sani_plot,
          ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")

prop_pop_aten_abas_agua_plot <-
  ggplot() +
  geom_col(data=df_estadual, mapping=aes(x=estado_sigla, y=prop_pop_aten_abas_agua, color=regiao, fill=regiao), alpha=0.5) +
  geom_hline(data=df_nacional, mapping=aes(yintercept = prop_pop_aten_abas_agua), color='red', size=0.2) + ylim(0,100) +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  labs(x = 'Estado', y='Proporção da População (%)') +
  ggtitle('Proporção da Popopulação Atendida com Abastecimento Água') +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text(size=11,face='bold'),
    axis.text = element_text(size=10),
    legend.title = element_text(size =11, face = 'bold'),
    legend.text = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )


prop_pop_aten_esgo_sani_plot <-
  ggplot() +
  geom_col(data=df_estadual, mapping=aes(x=estado_sigla, y=prop_pop_aten_esgo_sani, color=regiao, fill=regiao), alpha=0.5) +
  geom_hline(data=df_nacional, mapping=aes(yintercept = prop_pop_aten_esgo_sani), color='red', size=0.2) + ylim(0,100) +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  labs(x = 'Estado', y='Proporção da População (%)') +
  ggtitle('Proporção da Popopulação Atendida com Esgotamento Sanitátio') +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text(size=11,face='bold'),
    axis.text = element_text(size=10),
    legend.title = element_text(size =11, face = 'bold'),
    legend.text = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

ggarrange(prop_pop_aten_abas_agua_plot,
          prop_pop_aten_esgo_sani_plot,
          ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")

rec_oper_dir_agua_mun_aten_plot <-
  ggplot() +
  geom_col(data=df_estadual[df_estadual$estado != 'Distrito Federal (DF)' & df_estadual$estado != 'Rio de Janeiro (RJ)',], mapping=aes(x=estado_sigla, y=rec_oper_dir_agua_mun_aten, color=regiao, fill=regiao), alpha=0.5) +
  geom_hline(data=df_nacional, mapping=aes(yintercept = rec_oper_dir_agua_mun_aten), color='red', size=0.2) +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  labs(x = 'Estado', y='Receita (R$/município/ano)') +
  ggtitle('Receitas Operacionais Diretas com Água por Município (somente municípios atendidos)') +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text(size=11, face='bold'),
    axis.text = element_text(size=10),
    legend.title = element_text(size =11, face = 'bold'),
    legend.text = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

rec_oper_dir_esgo_muni_aten_plot <-
  ggplot() +
  geom_col(data=df_estadual[df_estadual$estado != 'Distrito Federal (DF)' & df_estadual$estado != 'Rio de Janeiro (RJ)',], mapping=aes(x=estado_sigla, y=rec_oper_dir_esgo_mun_aten, color=regiao, fill=regiao), alpha=0.5) +
  geom_hline(data=df_nacional, mapping=aes(yintercept = rec_oper_dir_esgo_mun_aten), color='red', size=0.2) +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  labs(x = 'Estado', y='Receita (R$/município/ano)') +
  ggtitle('Receitas Operacionais Diretas com Esgoto por Município (somente municípios atendidos)') +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text(size=11,face='bold'),
    axis.text = element_text(size=10),
    legend.title = element_text(size =11, face = 'bold'),
    legend.text = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

ggarrange(rec_oper_dir_agua_mun_aten_plot,
          rec_oper_dir_esgo_muni_aten_plot,
          ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")

rec_oper_dir_agua_pop_aten_plot <-
  ggplot() +
  geom_col(data=df_estadual, mapping=aes(x=estado_sigla, y=rec_oper_dir_agua_pop_aten, color=regiao, fill=regiao), alpha=0.5) +
  geom_hline(data=df_nacional, mapping=aes(yintercept = rec_oper_dir_agua_pop_aten), color='red', size=0.2) +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  labs(x = 'Estado', y='Receita (R$ per capita/ano)') +
  ggtitle('Receitas Operacionais Diretas per Capita com Água (somente população atendida)') +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text(size=11,face='bold'),
    axis.text = element_text(size=10),
    legend.title = element_text(size =11, face = 'bold'),
    legend.text = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

rec_oper_dir_esgo_pop_aten_plot <-
  ggplot() +
  geom_col(data=df_estadual, mapping=aes(x=estado_sigla, y=rec_oper_dir_esgo_pop_aten, color=regiao, fill=regiao), alpha=0.5) +
  geom_hline(data=df_nacional, mapping=aes(yintercept = rec_oper_dir_esgo_pop_aten), color='red', size=0.2) +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  labs(x = 'Estado', y='Receita (R$ per capita/ano)') +
  ggtitle('Receitas Operacionais Diretas per Capita com Esgoto (somente municípios atendidos)') +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text(size=11,face='bold'),
    axis.text = element_text(size=10),
    legend.title = element_text(size =11, face = 'bold'),
    legend.text = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

ggarrange(rec_oper_dir_agua_pop_aten_plot,
          rec_oper_dir_esgo_pop_aten_plot,
          ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")

# Criando o plot nacional para os volumes
plot_nacional <- ggplot(data=df_nacional) +
  geom_col(aes(x=country, y=prop_vol_agua_trat_pop_aten), fill='lightblue', alpha=0.5) +
  geom_col(aes(x=country, y=prop_vol_agua_cons_pop_aten), fill='blue', alpha=0.5) +
  geom_col(aes(x=country, y=prop_vol_esgo_cole_pop_aten), fill='brown', alpha=0.5) +
  geom_col(aes(x=country, y=prop_vol_esgo_trat_pop_aten), fill='red', alpha=0.5) +
  stat_count(geom = "text", colour = "black", size = 3.5, aes(x=country, label = round(prop_vol_agua_trat_pop_aten, 0),), position=position_stack(vjust=69)) +
  stat_count(geom = "text", colour = "black", size = 3.5, aes(x=country, label = round(prop_vol_agua_cons_pop_aten, 0),), position=position_stack(vjust=46)) +
  stat_count(geom = "text", colour = "black", size = 3.5, aes(x=country, label = round(prop_vol_esgo_cole_pop_aten, 0),), position=position_stack(vjust=30)) +
  stat_count(geom = "text", colour = "black", size = 3.5, aes(x=country, label = round(prop_vol_esgo_trat_pop_aten, 0),), position=position_stack(vjust=15)) +
  ylab(bquote(bold('Volume per capita ('~m^3*' )'))) +
  xlab('') +
  ylim(0,100) +
  theme(
    axis.title = element_text(size = 11,  hjust = 0.5, face = 'bold'),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8, face = 'bold'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
    )

# Criando o plot regional para os volumes
plot_regional <- ggplot(data=df_estadual) +
  geom_col(aes(x=estado_sigla, y=prop_vol_agua_trat_pop_aten), fill='lightblue', alpha=0.5) +
  geom_col(aes(x=estado_sigla, y=prop_vol_agua_cons_pop_aten), fill='blue', alpha=0.5) +
  geom_col(aes(x=estado_sigla, y=prop_vol_esgo_cole_pop_aten), fill='brown', alpha=0.5) +
  geom_col(aes(x=estado_sigla, y=prop_vol_esgo_trat_pop_aten), fill='red', alpha=0.5) +
  xlab('Estado') +
  ylim(0,100) +
  theme(
    axis.title.x = element_text(size=11, face='bold'),
    axis.title.y = element_text('none'),
    axis.text = element_text(size=10),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8, face = 'bold'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
    )

h1 <- geom_hline(yintercept = df_nacional$prop_vol_agua_trat_pop_aten, size=0.2, color='black')
h2 <- geom_hline(yintercept = df_nacional$prop_vol_agua_cons_pop_aten, size=0.2, color='black')
h3 <- geom_hline(yintercept = df_nacional$prop_vol_esgo_cole_pop_aten, size=0.2, color='black')
h4 <- geom_hline(yintercept = df_nacional$prop_vol_esgo_trat_pop_aten, size=0.2, color='black')

my_legend <- readJPEG("../images/legend_custom.jpeg", native = TRUE)

annotate_figure(grid.arrange(plot_nacional, plot_regional+h1+h2+h3+h4, ncol=2, widths = c(1,6)),
                top = text_grob("Volume per Capita de Água Tratada e Consumida, Esgoto Coletado e Tratado (somente população atendida)", 
               face = "bold", size = 12)) + inset_element(p = my_legend, 
                              left = 0.19, 
                              bottom = 0.86, 
                              right = 0.66, 
                              top = 0.93)

prop_vol_agua_cons_agua_trat_plot <- ggplot(data=df_estadual) +
  geom_col(mapping = aes(x=estado_sigla, y=prop_vol_agua_cons_agua_trat, color=regiao, fill=regiao), alpha=0.5) +
  geom_hline(data=df_nacional, mapping=aes(yintercept = prop_vol_agua_cons_agua_trat), color='red', size=0.2) +
  ylim(0, 100) +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  labs(x = 'Estado', y='Proporção (%)') +
  ggtitle('Proporção entre o Volume de Água Consumida e o Volume de Água Tratada') +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text(size=11,face='bold'),
    axis.text = element_text(size=10),
    legend.title = element_text(size =11, face = 'bold'),
    legend.text = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

prop_vol_esgo_cole_agua_cons_plot <- ggplot(data=df_estadual) +
  geom_col(mapping = aes(x=estado_sigla, y=prop_vol_esgo_cole_agua_cons, color=regiao, fill=regiao), alpha=0.5) +
  geom_hline(data=df_nacional, mapping=aes(yintercept = prop_vol_esgo_cole_agua_cons), color='red', size=0.2) +
  ylim(0, 100) +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  labs(x = 'Estado', y='Proporção (%)') +
  ggtitle('Proporção entre o Volume de Esgoto Coletado e o Volume de Água Consumida') +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text(size=11,face='bold'),
    axis.text = element_text(size=10),
    legend.title = element_text(size =11, face = 'bold'),
    legend.text = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

prop_vol_esgo_trat_esgo_cole_plot <- ggplot(data=df_estadual) +
  geom_col(mapping = aes(x=estado_sigla, y=prop_vol_esgo_trat_esgo_cole, color=regiao, fill=regiao), alpha=0.5) +
  geom_hline(data=df_nacional, mapping=aes(yintercept = prop_vol_esgo_trat_esgo_cole), color='red', size=0.2) +
  ylim(0, 100) +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  labs(x = 'Estado', y='Proporção (%)') +
  ggtitle('Proporção entre o Volume de Esgoto Tratado e o Volume de Esgoto Coletado') +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text(size=11,face='bold'),
    axis.text = element_text(size=10),
    legend.title = element_text(size =11),
    legend.text = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

ggarrange(prop_vol_agua_cons_agua_trat_plot,
          prop_vol_esgo_cole_agua_cons_plot,
          prop_vol_esgo_trat_esgo_cole_plot,
          ncol = 1, nrow = 3, common.legend = TRUE, legend = "bottom")

prop_vol_esgo_trat_agua_cons_plot <- ggplot(data=df_estadual) +
  geom_col(mapping = aes(x=estado_sigla, y=prop_vol_esgo_trat_agua_cons, color=regiao, fill=regiao), alpha=0.5) +
  geom_hline(data=df_nacional, mapping=aes(yintercept = prop_vol_esgo_trat_agua_cons), color='red', size=0.2) +
  ylim(0, 100) +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  labs(x = 'Estado', y='Proporção (%)') +
  ggtitle('Proporção entre o Volume de Esgoto Tratado e o Volume de Água Consumida') +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    axis.title = element_text(size=11,face='bold'),
    axis.text = element_text(size=10),
    legend.title = element_text(size =11),
    legend.text = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

ggarrange(prop_vol_esgo_trat_agua_cons_plot,
          ncol = 1, nrow = 1, common.legend = TRUE, legend = "bottom")

df_estadual_cor <- cor(df_estadual[,-c('estado','regiao', 'estado_sigla','estado_sem_sigla')], method = "spearman")
corrplot(df_estadual_cor)

list_of_R = c()
list_row = c()
list_col = c()
list_side = c()
for (i in colnames(df_estadual_cor)) {
  for (j in rownames(df_estadual_cor)) {
    if (i != j) {
      if (df_estadual_cor[j,i] >= 0.8) {
        list_of_R <- append(list_of_R, values = df_estadual_cor[j,i])
        list_row <- append(list_row, values = j)
        list_col <- append(list_col, values = i)
        list_side <- append(list_side, values = 'positiva')
      }
      else if (df_estadual_cor[j,i] <= -0.6) {
        list_of_R <- append(list_of_R, values = df_estadual_cor[j,i])
        list_row <- append(list_row, values = j)
        list_col <- append(list_col, values = i)
        list_side <- append(list_side, values = 'negativa')
      }
    }
  }
}

higher_correlations = data.frame(list_of_R, list_row, list_col, list_side) %>% arrange(desc(list_of_R))
list_index_remove <- c()
for (i in 2:length(higher_correlations$list_of_R)) {
  if (higher_correlations$list_of_R[i-1] == higher_correlations$list_of_R[i]) {
    if (higher_correlations$list_row[i-1] == higher_correlations$list_col[i] & 
        higher_correlations$list_col[i-1] == higher_correlations$list_row[i]) {
      list_index_remove <- append(list_index_remove, i-1)
    }
  }
}

higher_correlations <- higher_correlations[-list_index_remove,]
list_index_remove <- c()
for (i in 3:length(higher_correlations$list_of_R)) {
  if (higher_correlations$list_of_R[i-2] == higher_correlations$list_of_R[i]) {
    if (higher_correlations$list_row[i-2] == higher_correlations$list_col[i] & 
        higher_correlations$list_col[i-2] == higher_correlations$list_row[i]) {
      list_index_remove <- append(list_index_remove, i-2)
    }
  }
}

higher_correlations <- higher_correlations[-list_index_remove,]
length(higher_correlations$list_of_R)

#for (i in 1:length(higher_correlations$list_row)) {
#  plot <- ggscatter(df_estadual, x = higher_correlations$list_row[i], y = higher_correlations$list_col[i], 
#          add = "reg.line", conf.int = TRUE, 
#          cor.coef = TRUE, cor.method = "pearson",
#          xlab = higher_correlations$list_row[i], ylab = higher_correlations$list_col[i])
#  print(plot)
#}

plot1 <- ggplot(df_estadual, aes(x = num_mun, y = GE05a)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x,
              se = TRUE,
              color = "blue", fill = "lightblue") +
  geom_label(aes(x=0, y=840, label='1'),
            color="blue", size=5 , fontface="bold" ) +
  labs(x = 'NM', y='NMAAA') +
  theme(
    axis.title = element_text(size=8,face='bold'),
    axis.text = element_text(size=7),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

plot2 <- ggplot(df_estadual, aes(x = num_mun, y = GE05b)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x,
              se = TRUE,
              color = "blue", fill = "lightblue") +
  geom_label(aes(x=0, y=680, label='2'),
            color="blue", size=5 , fontface="bold" ) +
  labs(x = 'NM', y='NMAES') +
  theme(
    axis.title = element_text(size=8,face='bold'),
    axis.text = element_text(size=7),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

plot3 <- ggplot(df_estadual, aes(x = populacao, y = prop_pop_aten_abas_agua)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x,
              se = TRUE,
              color = "blue", fill = "lightblue") +
  geom_label(aes(x=0, y=130, label='3'),
            color="blue", size=5 , fontface="bold" ) +
  labs(x = 'Pop', y='PPAAA') +
  theme(
    axis.title = element_text(size=8,face='bold'),
    axis.text = element_text(size=7),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

plot4 <- ggplot(df_estadual, aes(x = populacao, y = prop_pop_aten_esgo_sani)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x,
              se = TRUE,
              color = "blue", fill = "lightblue") +
  geom_label(aes(x=0, y=130, label='4'),
            color="blue", size=5 , fontface="bold" ) +
  labs(x = 'Pop', y='PPAES') +
  theme(
    axis.title = element_text(size=8,face='bold'),
    axis.text = element_text(size=7),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

annotate_figure(grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2),
                top = text_grob("Análise de Correlação entre alguns Pares de Variáveis", 
               face = "bold", size = 12))

cor.test(df_estadual$num_mun, df_estadual$GE05a)

cor.test(df_estadual$num_mun, df_estadual$GE05b)

cor.test(df_estadual$populacao, df_estadual$prop_pop_aten_abas_agua)

cor.test(df_estadual$populacao, df_estadual$prop_pop_aten_esgo_sani)

ggplot(df_estadual, aes(x = prop_vol_agua_trat_pop_aten, y = prop_vol_agua_cons_agua_trat)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x,
              se = TRUE,
              color = "blue", fill = "lightblue") +
  labs(x = 'VATPC', y='PVACVAT') +
  theme(
    axis.title = element_text(size=8,face='bold'),
    axis.text = element_text(size=7),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = 'black'),
    panel.border = element_rect(size = 1, fill = NA)
  )

cor.test(df_estadual$prop_vol_agua_trat_pop_aten, df_estadual$prop_vol_agua_cons_agua_trat)

anova_prop_pop_aten_abas_agua <- aov(prop_pop_aten_abas_agua ~ regiao, data = df_estadual)
summary(anova_prop_pop_aten_abas_agua)
TukeyHSD(anova_prop_pop_aten_abas_agua)

anova_prop_pop_aten_esgo_sani <- aov(prop_pop_aten_esgo_sani ~ regiao, data = df_estadual)
summary(anova_prop_pop_aten_esgo_sani)
TukeyHSD(anova_prop_pop_aten_esgo_sani)

fig <- plot_ly(df_estadual, x = ~prop_pop_aten_abas_agua, y = ~prop_pop_aten_esgo_sani, text = ~estado, type = 'scatter', mode = 'markers', size = ~populacao, color = ~regiao, colors = 'Dark2',
        #Choosing the range of the bubbles' sizes:
        sizes = c(10, 50),
        marker = list(opacity = 0.5, sizemode = 'diameter'))

fig <- fig %>%
  layout(title = '<b>Proporção da População Atentida com Abastecimento de Água e Esgotamento Sanitário</b>',
         plot_bgcolor = '#DEDEDE',
         xaxis = list(title = '<b>População Atendida com Abastecimento de Água (%)</b>',
                      range = c(0, 100),
                      showgrid = FALSE,
                      mirror=TRUE,
                      ticks='outside',
                      showline=TRUE),
         yaxis = list(title = '<b>População Atendida com Esgotamento Sanitário (%)</b>',
                      range = c(0, 100),
                      showgrid = FALSE,
                      mirror=TRUE,
                      ticks='outside',
                      showline=TRUE),
         showlegend = TRUE,
         legend = list(title = list(text = '<b>Região</b>'),
                       font = list(size = 7),
                       titlefont = list(size = 8),
                       x = 0.03, y = 0.95),
         margin = list(l = 50, r = 50, b = 40, t = 80),
         paper_bgcolor = 'white',
         plot_bgcolor = 'white',
         hovermode = 'closest')

fig
