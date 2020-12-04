#############################################################################################################
## Packages ##
# List of packages for session
.packages = c("ggthemes", "hrbrthemes", "data.table", "extrafont", "colorspace", "forcats",
              "tidyverse", "stringr", "gtrendsR", "lubridate", "janitor")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0)
  install.packages(.packages[!.inst],dependencies = T)

# Load packages into session
lapply(.packages, require, character.only = T)

#############################################################################################################
## Extracting the data ##
tema1 <- "cleansing oil"
tema2 <- "cleansing balm "

# Importando os dados
data <- gtrends(keyword = c(tema1, tema2),
                       geo = "BR", time='all', onlyInterest = TRUE)

primeiro <- data$interest_over_time %>%
  filter(keyword == tema1) %>%
  mutate(mes = floor_date(date, "month")) %>%
  #group_by(mes) %>%
  #summarize(interesse = sum(hits)) %>%
  mutate(date = as.Date(mes)) %>%
  select(date, mes, hits, keyword) %>%
  as.data.frame

segundo <- data$interest_over_time %>%
  filter(keyword == tema2) %>%
  mutate(mes = floor_date(date, "month")) %>%
  #group_by(mes) %>%
  #summarize(interesse = sum(hits)) %>%
  mutate(date = as.Date(mes)) %>%
  select(date, mes, hits, keyword) %>%
  as.data.frame

## Juntando as bases ##
juntos <- inner_join(primeiro, segundo, by = "date") %>%
  select(date, keyword.x, hits.x, keyword.y, hits.y)


juntos <- juntos %>% 
  rename(tema_1 = hits.x, 
         tema_2 = hits.y) %>%
  mutate(month = zoo::as.yearmon(date))

 
juntos$tema_1 <- as.numeric(juntos$tema_1)
juntos$tema_2 <- as.numeric(juntos$tema_2)
juntos[is.na(juntos)] <- 0

## Carregar as fontes ##
showtext_auto()
font_import(paths = NULL, recursive = TRUE, prompt = TRUE, pattern = NULL)
loadfonts(device = "win")

# Para escolher cores
hcl_palettes(plot = TRUE)
sequential_hcl(5, "PuRd")
diverging_hcl(5, "Blue-Red 2")
qualitative_hcl(5, "Cold")

## Making the Plot ##
juntos %>%
  filter(date >= "2017-01-01" 
         #& date <= "2020-08-01"
         ) %>%
  ggplot(aes(date)) + 
  geom_line(aes(y = tema_1,  color = "#9FA2FF"), size = 1.5) + 
  geom_line(aes(y = tema_2, color = "#D33F6A"), size = 1.5) +
  #ggtitle("Esse ou Aquele?") + 
  labs(title = "Esse ou Aquele?",
       caption = "@Glow Graphs Brasil \n Dados extraídos do Google Trends - Jan/2017 até Agosto/2020",
       x = (""),
       y = "Hits Mensais Médios") +
  scale_color_manual(name = "", labels = c("Cleansing Oil", "Cleansing Balm"), values = c("#D33F6A", "#9FA2FF")) + #MUDAR OS LABELS
  theme_classic(base_family = "Palatino Linotype") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.spacing.x = grid::unit(0.60, "cm")
        ) +
  theme(plot.title = element_text(family = "Pumkinpie", color = "grey20", size = 32, face = "bold")) +
  theme(panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") #https://www.r-bloggers.com/customizing-time-and-date-scales-in-ggplot2/
  # + annotate(geom = "text", x = Inf, y = -Inf, label = "Glow Graphs Brasil",
  #          hjust = 4.2, vjust = -0.5, col = "skyblue", cex = 4,
  #          fontface = "bold", family = "Pumkinpie", alpha = 0.7)

showtext_auto(FALSE) 
# Outras cores: "#999999", "#E69F00", "#899DA4", "#E58601", "lightblue", "plum", "maroon1", "mediumpurple1", "blueviolet", "lavender", "turquoise1",
# "skyblue", "skyblue1", "slategray2", "deepskyblue", "cornflowerblue", "salmon", "deeppink", "lightcoral", "palevioletred1", "lightpink1",
# "tan1", "purple1"
# https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf

# Marca D'Agua
watermarkGrob <- function(lab = "Glow Graphs Brasil"){
  grob(lab = lab, cl = "watermark")
}

## custom draw method to
## calculate expansion factor on-the-fly
drawDetails.watermark <- function(x, rot = 45, ...){
  
  cex <- convertUnit(unit(1, "npc"), "mm", val = TRUE) /
    convertUnit(unit(1, "grobwidth", textGrob(x$val)), "mm", val = TRUE)
  grid.text(x$lab, rot = rot, gp = gpar(cex = cex, col = "lightgrey",
                                    fontface = "bold", family = "Pumkinpie", alpha = 0.5))
  
}
