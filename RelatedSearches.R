#############################################################################################################
## Packages ##
# List of packages for session
.packages = c("ggthemes", "hrbrthemes", "data.table", "showtext", "extrafont", "forcats",
              "colorspace", "tidyverse", "stringr", "gtrendsR", "lubridate", "janitor")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0)
  install.packages(.packages[!.inst],dependencies = T)

# Load packages into session
lapply(.packages, require, character.only = T)

install_github("timriffe/DemoTools", force = TRUE)
library(DemoTools) 

#############################################################################################################

topico <- "Primer"
 
# Importando os dados
relacionado <- gtrends(topico, geo = "BR", time = 'all')

# View(as.data.frame(relacionado$related_queries))

# Selecionando os tópicos do topo
related <- relacionado$related_queries %>%
  filter(related_queries == "top") %>%
  mutate(value = factor(value,
                        levels = rev(as.character(value))),
         subject = as.numeric(subject))

related$outro <- related$value

# Corrigindo problemas com o encoding
related$outro <- gsub("Ã©", "é", related$outro)
related$outro <- gsub("Ã§", "ç", related$outro)
related$outro <- gsub("Ã£", "ã", related$outro)

# Tratando as strings
# https://www.regular-expressions.info/completelines.html
# https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf
# https://stringr.tidyverse.org/articles/regular-expressions.html

wordstoremove <- c("tinta primer", "primer rh")

related <- related %>%
  mutate(outro = str_remove_all(outro, regex(str_c("\\b", wordstoremove, "\\b", collapse = '|'), ignore_case = T)),
         outro = str_replace_all(outro, regex("^\\bmaquiagem\\b|\\bmaquiagem\\s*\\bprimer\\b", ignore_case = T), "maquiagem"),
         outro = str_replace_all(outro, regex("^\\bprimer\\b\\s+\\bde\\b\\s+\\bmaquiagem\\s*|^\\bprimer\\b\\s+\\bpara\\b\\s+\\bmaquiagem\\s*", 
                                              ignore_case = T), "maquiagem"),
         outro = str_replace_all(outro, regex("\\bprimer\\s*\\bbeyoung\\b", ignore_case = T), "beyoung"),
         outro = str_replace_all(outro, regex("\\bruby\\s*\\brose\\s*\\bprimer\\s*", ignore_case = T), "ruby rose"),
         outro = str_replace_all(outro, regex("\\bprimer\\s*\\bmary\\s*\\bkay\\s*", ignore_case = T), "mary kay"),
         outro = str_replace_all(outro, regex("\\bprimer\\s*\\bvult\\s*", ignore_case = T), "vult"),
         outro = str_replace_all(outro, regex("\\bprimer\\s*\\bavon\\s*", ignore_case = T), "avon"),
         outro = str_replace_all(outro, regex("\\bprimer\\s*\\bpreço\\s*", ignore_case = T), "preço"),
         outro = str_replace_all(outro, regex("\\bprimer\\s*\\bpara\\s*\\bque\\s*\\bserve", ignore_case = T), "para que serve"),
         outro = str_replace_all(outro, regex("\\bprime\\b", ignore_case = T), "primer"),
         outro = str_replace_all(outro, regex("\\bcomo\\s*\\busar\\s*\\bprimer\\s*", ignore_case = T), "como usar"),
         outro = str_replace_all(outro, regex("^\\s+|\\s+$"), "")) # elimina espaço antes e depois das palavras

related$outro <- gsub("maquiagem primer", "maquiagem", related$outro)

related$outro <- trimws(gsub("\\s+", " ", related$outro))

related <- related %>%
  mutate(outro = str_replace_all(outro, regex("^\\bhidratante\\b\\s+\\bpele\\b\\s+\\bseca", ignore_case = T), "hidratante"),
         outro = str_replace_all(outro, regex("^\\bhidratante\\b\\s+\\brosto\\b\\s*\\bpele\\b\\s+\\bseca", ignore_case = T), "hidratante rosto"),
         outro = str_replace_all(outro, regex("^\\bcetaphil\\s*\\bpele\\s*\\bseca", ignore_case = T), "cetaphil"),
         outro = str_remove_all(outro, regex("\\bpele\\s*\\bseca")),
         outro = str_replace_all(outro, regex("^\\s+|\\s+$"), ""))


related <- related %>%
  #mutate(outro = sub("(.)", "\\U\\1", outro, perl = TRUE)) #com regex
  #mutate(outro = str_to_sentence(outro)) # 1º termo com primeira letra maiuscula
  mutate(outro = str_to_title(outro)) # todos os termos com primeira letra maiuscula

related$tentativa <- factor(related$outro, levels = unique(related$outro[order(related$value)])) # não sei se está certo

# final <- related %>%
#   group_by(outro) %>%
#   summarise(subject = sum(subject))

# Carregar as fontes
showtext_auto()
font_import(paths = NULL, recursive = TRUE, prompt = TRUE, pattern = NULL)
loadfonts(device = "win")

# Para escolher cores
hcl_palettes(plot = TRUE)
sequential_hcl(5, "Purp")
diverging_hcl(5, "Berlin")
qualitative_hcl(5, "Warm")


# Gráfico de Barras
related %>%
  # mutate(tentativa = fct_reorder(subject, outro, fun = sum)) %>%
  filter(!tentativa == "") %>%
  ggplot(aes(x = reorder(tentativa, subject, sum), y = subject)) + 
  geom_bar(stat = 'identity', fill = "#7FBFF5") + 
  coord_flip() +
  labs(title = "O que as pessoas pesquisam \n no Google sobre 'primer' ?", #Mudar o título
       caption = "@Glow Graphs Brasil \n Dados extraídos do Google Trends - Ago/2020",
       x = (""),
       y = "Hits") +
  theme_classic(base_family = "Palatino Linotype") +
  theme(axis.text.x = element_text(hjust = 1, size = 10, family = "Palatino")) + 
  theme(plot.title = element_text(family = "Pumkinpie", color = "grey20", size = 24, face = "bold")) +
  theme(panel.grid.minor = element_blank())
