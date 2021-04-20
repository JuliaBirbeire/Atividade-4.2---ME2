library(infer)
library(EnvStats)
library(tidyverse)

amostra_190090090$LOCALIZACAO[amostra_190090090$LOCALIZACAO == '1'] <- 'Urbana'
amostra_190090090$LOCALIZACAO[amostra_190090090$LOCALIZACAO == '2'] <-  'Rural'
amostra_190090090$DEPENDENCIA_ADM[amostra_190090090$DEPENDENCIA_ADM == '3'] <- 'Municipal'
amostra_190090090$DEPENDENCIA_ADM[amostra_190090090$DEPENDENCIA_ADM == '2'] <-  'Estadual + Federal'
amostra_190090090$DEPENDENCIA_ADM[amostra_190090090$DEPENDENCIA_ADM == '1'] <-  'Estadual + Federal'

a30 = rep_sample_n(amostra_190090090 ,size = 30, reps = 1, replace = TRUE)
a100 = rep_sample_n(amostra_190090090 ,size = 100, reps = 1, replace = TRUE)

# NOTA MT E LOCALIZAÇÃO
# amostra de n = 30
# grafico 
a30 %>%
  ggplot(aes(x = factor(""), y = NOTA_MT)) +
  geom_boxplot(fill = c('#008080'), width = 0.5) +
  stat_summary(fun.y = 'mean', geom = 'point', shape = 23, size = 3,  fill = 'white') + 
  labs(x = '', y ='Nota Matemática') + 
  theme_bw() +
  theme(axis.title.y = element_text(colour = 'black', size = 12),
        axis.title.x = element_text(colour = 'black', size = 12),
        axis.text = element_text(colour = 'black', size = 9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = 'black')) + 
  facet_wrap(~LOCALIZACAO) 

ggsave("mat30.png", width = 160, height = 80, units = "mm")

# medidas de posição e variabilidade segundo as categorias da localização
a30 %>%
  group_by(LOCALIZACAO)%>%
  summarize(media = mean(NOTA_MT),
            variância = var(NOTA_MT),
            desvio_padrão = sd(NOTA_MT),
            min = min(NOTA_MT),
            q25 = quantile(NOTA_MT, probs = .25),
            mediana  = quantile(NOTA_MT, probs = .5),
            q75 = quantile(NOTA_MT, probs = .75),
            max = max(NOTA_MT))
# teste de normalidade MT - a30 
shapiro.test(a30$NOTA_MT) # proximo de 1 -> normal

# teste de homocedasticidade

# TESTE
# Ho a proficiencia em matemática é maior em escola urbanas
urbanas30 <- a30 %>% 
  filter(LOCALIZACAO == "Urbana")

rural30 <- a30 %>%
  filter(LOCALIZACAO == "Rural")

var.test(urbanas30$NOTA_MT,rural30$NOTA_MT) # variancias iguais

# t student 
t.test(urbanas30$NOTA_MT,rural30$NOTA_MT,alternative = 'less') # aceita Ho

# amostra de n = 100
# grafico 
a100 %>%
  ggplot(aes(x = factor(""), y = NOTA_MT)) +
  geom_boxplot(fill = c('#008080'), width = 0.5) +
  stat_summary(fun.y = 'mean', geom = 'point', shape = 23, size = 3,  fill = 'white') + 
  labs(x = '', y ='Nota Matemática') + 
  theme_bw() +
  theme(axis.title.y = element_text(colour = 'black', size = 12),
        axis.title.x = element_text(colour = 'black', size = 12),
        axis.text = element_text(colour = 'black', size = 9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = 'black')) + 
  facet_wrap(~LOCALIZACAO) 

ggsave("mat100.png", width = 160, height = 80, units = "mm")

# medidas de posição e variabilidade segundo as categorias da localização
a100 %>%
  group_by(LOCALIZACAO)%>%
  summarize(media = mean(NOTA_MT),
            variância = var(NOTA_MT),
            desvio_padrão = sd(NOTA_MT),
            min = min(NOTA_MT),
            q25 = quantile(NOTA_MT, probs = .25),
            mediana  = quantile(NOTA_MT, probs = .5),
            q75 = quantile(NOTA_MT, probs = .75),
            max = max(NOTA_MT))

# teste de normalidade MT - a100 
shapiro.test(a100$NOTA_MT) # proximo de 1 - normal

# teste de homocedasticidade


# TESTE
# Ho a proficiencia em matemática é maior em escola urbanas
urbanas100 <- a100 %>% 
  filter(LOCALIZACAO == "Urbana")

rural100 <- a100 %>%
  filter(LOCALIZACAO == "Rural")

var.test(urbanas100$NOTA_MT,rural100$NOTA_MT) # variancias iguais

# t student 
t.test(urbanas100$NOTA_MT,rural100$NOTA_MT, alternative = 'less') # aceita Ho

# NOTA LP E DEPENDENCIA ADMINISTRATIVA
# amostra de n = 30
# grafico 
a30 %>%
  ggplot(aes(x = factor(""), y = NOTA_LP)) +
  geom_boxplot(fill = c('#008080'), width = 0.5) +
  stat_summary(fun.y = 'mean', geom = 'point', shape = 23, size = 3,  fill = 'white') + 
  labs(x = '', y ='Nota Língua Portuguesa') + 
  theme_bw() +
  theme(axis.title.y = element_text(colour = 'black', size = 12),
        axis.title.x = element_text(colour = 'black', size = 12),
        axis.text = element_text(colour = 'black', size = 9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = 'black')) + 
  facet_wrap(~DEPENDENCIA_ADM) 

ggsave("port30.png", width = 160, height = 80, units = "mm")

# medidas de posição e variabilidade segundo as categorias da localização
a30 %>%
  group_by(DEPENDENCIA_ADM)%>%
  summarize(media = mean(NOTA_LP),
            variância = var(NOTA_LP),
            desvio_padrão = sd(NOTA_LP),
            min = min(NOTA_LP),
            q25 = quantile(NOTA_LP, probs = .25),
            mediana  = quantile(NOTA_LP, probs = .5),
            q75 = quantile(NOTA_LP, probs = .75),
            max = max(NOTA_LP))

# teste de normalidade LP - a30 
shapiro.test(a30$NOTA_LP) # proximo de 1 - normal

# teste de homocedasticidade


# TESTE
# existe diferena entre proficiencia em lp segundo categoria adm 
municipal30 <- a30 %>%
  filter(DEPENDENCIA_ADM == "Municipal") 

est_fed30 <- a30 %>%
  filter(DEPENDENCIA_ADM == "Estadual + Federal") 

# teste de variancia
var.test(municipal30$NOTA_LP,est_fed30$NOTA_LP) # variancias diferentes (p < 0,05)

# t student 
t.test(municipal30$NOTA_LP,est_fed30$NOTA_LP, var.equal = F) # aceita Ho

# amostra de n = 100
# grafico 
a100  %>%
  ggplot(aes(x = factor(""), y = NOTA_LP)) +
  geom_boxplot(fill = c('#008080'), width = 0.5) +
  stat_summary(fun.y = 'mean', geom = 'point', shape = 23, size = 3,  fill = 'white') + 
  labs(x = '', y ='Nota Língua Portuguesa') + 
  theme_bw() +
  theme(axis.title.y = element_text(colour = 'black', size = 12),
        axis.title.x = element_text(colour = 'black', size = 12),
        axis.text = element_text(colour = 'black', size = 9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = 'black')) + 
  facet_wrap(~DEPENDENCIA_ADM) 

ggsave("port100.png", width = 160, height = 80, units = "mm")

# medidas de posição e variabilidade segundo as categorias da localização
a100 %>%
  group_by(DEPENDENCIA_ADM)%>%
  summarize(media = mean(NOTA_LP),
            variância = var(NOTA_LP),
            desvio_padrão = sd(NOTA_LP),
            min = min(NOTA_LP),
            q25 = quantile(NOTA_LP, probs = .25),
            mediana  = quantile(NOTA_LP, probs = .5),
            q75 = quantile(NOTA_LP, probs = .75),
            max = max(NOTA_LP))

# teste de normalidade LP - a100 
shapiro.test(a100$NOTA_LP) # proximo de 1 - normal

# teste de homocedasticidade

# TESTE
# existe diferena entre proficiencia em lp segundo categoria adm 
municipal100 <- a100 %>%
  filter(DEPENDENCIA_ADM == "Municipal") 

est_fed100 <- a100 %>%
  filter(DEPENDENCIA_ADM == "Estadual + Federal") 

# teste de variancia
var.test(municipal100$NOTA_LP,est_fed100$NOTA_LP) # variancias iguai

# t student 
t.test(municipal100$NOTA_LP,est_fed100$NOTA_LP) # rejeito Ho

# NOTA LP E NOTA MAT
banco_lp <- a30 %>%
  mutate(Materia = "Nota Lingua Portuguesa",Nota = NOTA_LP)%>%
  select(Materia,Nota)
banco_mt <- a30 %>%
  mutate(Materia = "Nota Matematica",Nota = NOTA_MT)%>%
  select(Materia,Nota)
banco_notas <- rbind(banco_mt, banco_lp)

banco_notas %>%
  ggplot(aes(x = factor(""), y = Nota)) +
  geom_boxplot(fill = c('#008080'), width = 0.5) +
  stat_summary(fun.y = 'mean', geom = 'point', shape = 23, size = 3,  fill = 'white') + 
  labs(x = '', y ='Nota') + 
  theme_bw() +
  theme(axis.title.y = element_text(colour = 'black', size = 12),
        axis.title.x = element_text(colour = 'black', size = 12),
        axis.text = element_text(colour = 'black', size = 9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = 'black')) + 
  facet_wrap(~Materia) 

ggsave("pm.png", width = 160, height = 80, units = "mm")

# teste de normalidade
shapiro.test(banco_notas$Nota) # proximo de 1 - normal

# teste de variancia 
var.test(a30$NOTA_LP,a30$NOTA_MT) # var iguais

# t student
t.test(a30$NOTA_LP,a30$NOTA_MT) # aceita Ho

banco_notas %>%
  group_by(Materia)%>%
  summarize(media = mean(Nota),
            variância = var(Nota),
            desvio_padrão = sd(Nota),
            min = min(Nota),
            q25 = quantile(Nota, probs = .25),
            mediana  = quantile(Nota, probs = .5),
            q75 = quantile(Nota, probs = .75),
            max = max(Nota))
