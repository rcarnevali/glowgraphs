#############################################################################################################
## Packages ##
# List of packages for session
.packages = c("rtweet", "tidytext", "wordcloud", "RColorBrewer", "grid", "ggthemes", "hrbrthemes", 
              "data.table", "showtext", "extrafont", "forcats", "png", "janitor",
              "colorspace", "tidyverse", "stringr", "gtrendsR", "lubridate")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0)
  install.packages(.packages[!.inst],dependencies = T)

# Load packages into session
lapply(.packages, require, character.only = T)

#############################################################################################################

# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = token,
  access_secret = access_secret)

rstats_tweets <- search_tweets(q = "#skincare",
                               n = 1000,
                               lang ="pt",
                               #geocode = lookup_coords("brazil"),
                               include_rts = FALSE)

# Tratando as strings
hashtag_pat <- "#[a-zA-Z0-9_-ー\\.]+"
hashtag <- str_extract_all(rstats_tweets$text, hashtag_pat)
hashtag_word <- unlist(hashtag)
hashtag_word <- tolower(hashtag_word)
hashtag_word <- gsub("[[:punct:]ー]", "", hashtag_word)
hash <- as.data.frame(hashtag_word)


# Função para normalizar texto
Normaliza <- function(texto){
  
# Normaliza texto
texto %>% 
  chartr(
    old = "áéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ´`^~¨:.!?&$@#0123456789",
    new = "aeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC                       ",
    x = .) %>% # Elimina acentos e caracteres desnecessarios
  str_squish() %>% # Elimina espacos excedentes 
  tolower() %>% # Converte para minusculo
  return() 
}

# Lista de palavras para remover
palavrasRemover <- c(stopwords(kind = "pt"), letters) %>%
  as.tibble() %>% 
  rename(Palavra = value) %>% 
  mutate(Palavra = Normaliza(Palavra))

wordstoremove <- c("https", "weightloss", "loseweightnow", "weight", "weightlossmotivation", "weightlossmotivation", "topbrands", 
                   "bestdeals", "weightwatchers", "loseweight", "weightlossjourney", "acspshop", "https", "tudohmakesrecife", 
                   "bigbrands", "araruamarj", "	eubiaveiga", "biaveiga", "bargain", "homeopathy", "lifequotes", "sale", "medestec",
                   "bbnaija2020", "blacklivesmatter", "bipolaridade", "diatermia", "lalalive", "gustavosaber", "trioserumsou",
                   "tratamientosfaciales", "crevolutioncosmetics", "teacherlife", "plantas", "piles", "crueltyf", "cute", "creative", "colima",
                   "caudaliebrasil", "caudalie", "photography", "partiu", "cuidado", "corneoterapia", "california", "photooftheday")

twitter <- hash %>%
  mutate(outro = hashtag_word,
         outro = str_replace_all(outro, regex("^\\s+|\\s+$"), ""),# elimina espaÃ§o antes e depois das palavras
         outro = str_remove_all(outro, regex(str_c("\\b", wordstoremove, "\\b", collapse = '|'), ignore_case = T)),
         outro = str_replace_all(outro, regex("^\\bdica\\b", ignore_case = T), "dicas"),
         outro = str_replace_all(outro, regex("^\\bcolÃ¡geno\\b", ignore_case = T), "colageno"),
         outro = str_replace_all(outro, regex("^\\bcabelos\\b", ignore_case = T), "cabelo"),
         outro = str_replace_all(outro, regex("^\\bcicatriz\\b", ignore_case = T), "cicatrizes"),
         outro = str_replace_all(outro, regex("^\\bcosmetics\\b", ignore_case = T), "cosmeticos"),
         outro = str_replace_all(outro, regex("^\\bcosmÃ©ticos\\b", ignore_case = T), "cosmeticos"),
         outro = str_replace_all(outro, regex("^\\bcomesticos\\b", ignore_case = T), "cosmeticos"),
         outro = str_replace_all(outro, regex("^\\bprepreparaÃ§Ã£odepele\\b", ignore_case = T), "preparaÃ§Ã£odepele"),
         outro = str_replace_all(outro, regex("^\\baulas\\b", ignore_case = T), "aula")) %>% #para substituir uma palavra que termina com: \w*letra\b
  filter(!str_detect(outro, '^glam\\s*'),
         !str_detect(outro, "^girl\\s*"),
         !str_detect(outro, "^phy\\s*"),
         !str_detect(outro, "^tati\\s*"),
         !str_detect(outro, "\\s*foz$"),
         #!str_detect(outro,"[\r\n]"),
         !str_detect(outro,"^[^:punct:]|[:punct:]$")) %>% #remove aqueles que comeÃ§am com pontuaÃ§Ã£o
  select(outro) %>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit() %>%
  count(outro) %>%
  top_n(40, n) %>%
  mutate(outro = reorder(outro, n)) %>%
  arrange(desc(n)) 
# twitter$outro[24]
# twitter <-  twitter[!(is.na(twitter$outro) | df$outro==""), ]

# Definindo a paleta de cores
sequential_hcl(5, "Purple-Blue")
brewer.pal(6, "PuRd")
my_palette <- c("#980043", "#7665A4", "#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0","#BA4B8E", "#7D1D67","#DD1C77")

# Fazendo a nuvem
set.seed(1234)
wordcloud(words = twitter$outro, freq = twitter$n, min.freq = 1,
          max.words = 40, random.order = FALSE, 
          colors = rev(my_palette), family = "Pumkinpie")

# Adicionando Titulo
layout(matrix(c(1, 2), nrow = 2), heights = c(1, 3))
par(mar = rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Quando as pessoas falam de skincare \n no Twitter quais # elas usam?", 
     family = "Pumkinpie", col = "grey20", cex = 1.8)
wordcloud(words = twitter$outro, freq = twitter$n, min.freq = 1,
          max.words = 40, random.order = FALSE, 
          colors = rev(my_palette), family = "Pumkinpie")



ll <- list.files(patt='*.png')
imgs <- lapply(ll,function(x){
  img <- as.raster(readPNG(x))
  ## get the file name
  x.name <- gsub('(.*).png','\\1',x)
  ## new device for new image version
  png(file =paste(x.name,'_modified','.png',sep=''))
  grid.raster(img)
  ## here I add title
  grid.text(label = x.name,x=0.5,y=0.9,gp=gpar(cex=2))
  dev.off()
  
})
