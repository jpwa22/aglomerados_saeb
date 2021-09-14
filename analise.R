# Teste de Hipótese Para nota do Saeb nas esocolas do município de Recife

# Testando diferenças na média entre dois grupos

# Hipótese Nula (H0) – As notas das escolas localizadas em aglomerados subnormais não diferem das demais escolas.
# Hipótese Alternativa (H1) – As notas das escolas localizadas em aglomerados subnormais diferem das demais escolas.

# Carregando Bibliotecas
library(plyr)
library(tidyverse)
library(ggplot2)



# Carregando a Base de Dados
escolas <- read_rds("~/Projetos/Censo Escolar/Geo/escolas_join.rds")
class(escolas)

# Transformar variáveis em fatores com níveis descritivos
escolas$NM_AGSN %>% replace_na(0)
escolas <-escolas %>% filter(MEDIA_5EF_LP > 0) %>%
                     transmute(escola = as.factor(Escola),
                     latitude = Latitude,
                     longitude = Longitude,
                     nome_aglomerado = as.factor(NM_AGSN),
                     ME5F_LP = MEDIA_5EF_LP,
                     ME5F_MT = MEDIA_5EF_MT)


escolas$aglomerado = is.na(escolas$nome_aglomerado)
escolas <- escolas %>% mutate(aglomerado = as_factor(aglomerado),
                              aglomerado = str_replace(aglomerado,"TRUE","NÃO"),
                              aglomerado = str_replace(aglomerado,"FALSE","SIM"),
                              aglomerado = as_factor(aglomerado))

# Criando boxplot mostrando como o ME5F_LP varia entre status de fumante
# Boxplot Português:
qplot(x = aglomerado, y = ME5F_LP,
      geom = "boxplot", data = escolas,
      xlab = "Aglomerado", 
      ylab = "Nota Português 5EF 2019",
      fill = I("orchid4")) 
# Boxplot Matemática:
qplot(x = aglomerado, y = ME5F_MT,
         geom = "boxplot", data = escolas,
         xlab = "Aglomerado", 
         ylab = "Nota Matemática 5EF 2019",
         fill = I("orchid4"))


# O gráfico acima sugere que a média do SAEB no 5º ano final do Ensino Médio nas escolas localizadas em Aglomerados Subnormais é menor do que nas demais escolas.

# Como podemos avaliar se essa diferença é estatisticamente significativa?

# Vamos calcular uma tabela de resumo
ddply(escolas, ~ aglomerado, summarize,
      mean.escolas = mean(ME5F_LP),
      sd.escolas = sd(ME5F_LP)
)


# É bom ter o desvio padrão, mas, para avaliar a significância estatística, 
# queremos realmente ter o erro padrão (que o desvio padrão foi ajustado pelo tamanho do grupo).
ddply(escolas, ~ aglomerado, summarize,
      group.size = length(ME5F_LP),
      medi_lp = mean(ME5F_LP),
      desvio_padrao_lp = sd(ME5F_LP),
      erro_padrao_lp = desvio_padrao_lp/ sqrt(group.size)
)

# Essa diferença parece bastante significativa. 
# Para executar um teste t de duas amostras, podemos simplesmente usar a função t.test().

# Um teste t é um teste analítico usado para determinar se há uma diferença significativa 
# entre dois conjuntos de dados ou se a média de um conjunto de dados difere 
# significativamente de um valor previsto.

# Português
escolas_lp_.t.test <- t.test(ME5F_LP ~ aglomerado, data = escolas)


# A função t.test () também gera um intervalo de confiança.

names(escolas_lp_.t.test)
escolas_lp_.t.test$p.value 
escolas_lp_.t.test$estimate  
escolas_lp_.t.test$conf.int  
attr(escolas_lp_.t.test$conf.int, "conf.level") 

# Calculando a diferença entre os grupos
escolas.smoke.diff <- round(escolas_lp_.t.test$estimate[1] - escolas_lp_.t.test$estimate[2], 1)
escolas.smoke.diff

# Verificando o nível de confiança usado no teste
conf.level_lp <- attr(escolas_lp_.t.test$conf.int, "conf.level") * 100
conf.level_lp




# Matemática

escolas_mt_.t.test <- t.test(ME5F_MT ~ aglomerado, data = escolas)

names(escolas_mt_.t.test)
escolas_mt_.t.test$p.value 
escolas_mt_.t.test$estimate  
escolas_mt_.t.test$conf.int  
attr(escolas_mt_.t.test$conf.int, "conf.level") 

# Calculando a diferença entre os grupos
escolas.smoke.diff <- round(escolas_mt_.t.test$estimate[1] - escolas_mt_.t.test$estimate[2], 1)
escolas.smoke.diff

# Verificando o nível de confiança usado no teste
conf.level_lp <- attr(escolas_mt_.t.test$conf.int, "conf.level") * 100
conf.level_lp

# Vemos a partir destes resultado que a diferença é altamente significativa. 




# Nosso estudo constata que as médias de português e mate
# no grupo de não fumantes do que no grupo de fumantes 
# (estatística t 2,73, valor-p = 0,007, IC 95% [78,6, 489] g)

# Um pequeno valor-p (normalmente ≤ 0,05) indica forte evidência contra a hipótese nula; 
# portanto, você rejeita a hipótese nula. 

# Um valor-p grande (> 0,05) indica evidência fraca contra a hipótese nula; 
# portanto, você falha em rejeitar a hipótese nula.

# Para este exercício, rejeitamos a H0 pois o valor-p é menor que 0.05.








