
polaridade2 <-read.csv("data_polaridade0909.csv", sep =";", header = T)


#GRÁFICO EVOLUCAO POLARIDADE NOVO

polaridade2$created <- as.Date(polaridade2$created)

gg <- ggplot(polaridade2, aes(created, positive)) +
  geom_line(aes(colour = "#366392"), size = 1) +
  geom_line(aes(y = negative, colour = "#A92626"), size = 1) +
  geom_line(aes(y = neutral, colour = "#5B882C"), size = 1) +
  labs(color = "Polaridade", x = "Data", y = "Total de tweets por polaridade") +
  scale_x_date(date_labels = "%d/%b/%Y", date_breaks = "3 days") +
  scale_y_continuous(n.breaks = 10) +
  scale_color_identity(
    name = "Polaridade",
    breaks = c("#366392", "#A92626", "#5B882C"),
    labels = c("Neutro", "Negativo", "Positivo"),
    guide = "legend"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 16)
  )  

gg  


  

# se precisar ler sem as stopwords
tweets_textos2 <- read.csv("tweets_semdupli_limpos.csv", sep=",")
tweets_textos2 <- tweets_textos2[,c(2,3)]

#tweets evaluation function
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}
pos <- scan('termos_pos.csv', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('termos_neg.csv', what='character', comment.char=';') #folder with negative dictionary
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')
Dataset <- tweets_textos2
scores <- score.sentiment(Dataset$texto_limpo, pos.words, neg.words, .progress='text')

stat <- scores

#salvando o stat
library(tidyverse)
write_csv(stat, "stat_0109.csv")

stat <- read.csv("stat_0109.csv", sep = ',', header = T)
stat$X1 <- NULL

stat <- mutate(stat, polaridade=ifelse(stat$score > 0, 'Positivo', ifelse(stat$score < 0, 'Negativo', 'Neutro')))
texto_limpo <-  Dataset[,c(2)]

##
pol_sum <- stat %>% select(polaridade) %>% count(polaridade)

##GRÁFICO DE POLARIDADE NOVA
ggplot(pol_sum,aes(x = polaridade, y = n, fill = polaridade)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,500000,100000), limits = c(0,500000), labels = label_number(), name = "Total de Tweets") +
  scale_x_discrete(name = "Polaridade") +
  scale_fill_manual(values = c("#A92626", "#366392", "#5B882C")) +
  theme_bw() +
  theme(legend.title = element_blank(),  
    legend.position = "bottom",
    text = element_text(size = 16)
  ) 

require(scales)  



#######NuVEM DE PALAVRA

# Visualiza os tweets  por polaridade  
stat <- stat%>%
  group_by(polaridade) %>%
  summarise(pasted=paste(text, collapse=" "))


# Criando o corpus
corpus = Corpus(VectorSource(stat$pasted))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = stat$polaridade

tdm_df <- as.data.frame(tdm)
tdm_df <- tdm_df %>% mutate(name = row.names(.)) 

tdm_mx <- as.matrix(tdm_df[-4])
row.names(tdm_mx) <- tdm_df$name



comparison.cloud(tdm_mx, colors = c("#A92626", "#5B882C", "#366392"), max.words = 200, 
                 scale = c(4,1), random.order = FALSE, title.size = 1.5)

# GRÁFICO TWEETS POR HORA
tweets_textos3 <-read.csv("tweets_porhora.csv", sep =",", header = T)

#tweets_textos3 é a base só com o texto limpo e created_at (com hora e data)
library(dplyr)


install.packages("lubridate")
library(lubridate)

tweets_textos3$date <- day(tweets_textos3$created_at)
tweets_textos3$hour <- hour(tweets_textos3$created_at)
library(ggplot2)

dev.off()
gg <- ggplot(tweets_textos3, aes(x = hour)) +
  geom_density() +
  theme_bw() +
  scale_x_continuous(n.breaks = 13) +
  scale_y_continuous(n.breaks = 8) +
  labs(x = "Hora", y = "Tweets")

gg
ggsave("tweets_porhora_1109.png", gg)

