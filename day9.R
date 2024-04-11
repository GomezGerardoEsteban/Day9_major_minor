
rm(list = ls())

library(tidyverse)
library(ggtext)
library(readxl)

bases_macro <- list()

for(i in 1:2){
  bases_macro[[i]] <- read_excel(path = "rmd/bases/maticesIP/agregados_macro_25_sectores.xlsx", sheet = i)
}

# Convertimos en formato largo

base_macro_sectores <- bases_macro[[1]] %>% 
  gather(key = year, value = valor, 5:length(bases_macro[[1]]))

# Función para calcular tasas de crecimiento

calculoTasas <- function(base){
  
  bases_macro_sec1 <- base %>% 
    filter(year == "2005")
  
  PIB <- base %>% filter(year == "2021" & Nombre == "PIB") %>% select(valor)
  
  bases_macro_sec2 <- base %>% 
    filter(year == "2021") %>% 
    mutate(prop = valor/PIB$valor[1])
  
  
  bases_macro_sec <- bases_macro_sec1 %>% 
    left_join(bases_macro_sec2 %>% select(4,6:7), 
              by = c("Nombre" = "Nombre"), 
              suffix = c(".05", ".21"))
  
  bases_macro_sec <- bases_macro_sec %>% 
    mutate(tasa_crecimiento = ((valor.21/valor.05)-1),
           tasa_equiv_anual = (1 + tasa_crecimiento)^(1/(2021-2005))-1)
  
  
  return(bases_macro_sec)
  
} 

# Aplicamos sobre base en formato largo

bases_macro_sec <- calculoTasas(base = base_macro_sectores)

# Generamos una base que solo incluya los sectores

bases_solo_sec <- bases_macro_sec[bases_macro_sec$Nombre != "VAB" & bases_macro_sec$Nombre != "Hogares", ]

etiquetas <- bases_solo_sec

etiquetas$win_lose <- ifelse(etiquetas$tasa_equiv_anual == max(etiquetas$tasa_equiv_anual) |
                               etiquetas$Nombre == "PIB" |
                               etiquetas$tasa_equiv_anual == min(etiquetas$tasa_equiv_anual), 1, 0)

nudge <- ifelse(nchar(etiquetas$Nombre[etiquetas$win_lose != 1]) < 8, 0.009,
                ifelse(nchar(etiquetas$Nombre[etiquetas$win_lose != 1]) >= 8 & 
                         nchar(etiquetas$Nombre[etiquetas$win_lose != 1]) < 11, 0.01,
                ifelse(nchar(etiquetas$Nombre[etiquetas$win_lose != 1]) >= 11 & 
                         nchar(etiquetas$Nombre[etiquetas$win_lose != 1]) < 15, 0.012, 
                       ifelse(nchar(etiquetas$Nombre[etiquetas$win_lose != 1]) == 26, 0.022, 0.015))))

nudge <- nudge*1.5

bases_solo_sec$colores <- ifelse(bases_solo_sec$tasa_equiv_anual == max(bases_solo_sec$tasa_equiv_anual), "maximo",
                                 ifelse(bases_solo_sec$tasa_equiv_anual == min(bases_solo_sec$tasa_equiv_anual), "minimo",
                                        ifelse(bases_solo_sec$Nombre == "PIB", "PIB", "otro")))
                                    
                              
bases_solo_sec %>% 
  ggplot(mapping = aes(x = reorder(Nombre, desc(tasa_equiv_anual)), y = tasa_equiv_anual)) +
  geom_col(mapping = aes(fill = colores), show.legend = F) +
  geom_text(data = etiquetas %>% 
              filter(win_lose != 1), 
            mapping = aes(y = tasa_equiv_anual, 
                          label = Nombre), 
            angle = 90, 
            nudge_y = nudge,
            size = 3.5) +
  geom_text(data = etiquetas %>% 
              filter(win_lose == 1),
            mapping = aes(y = tasa_equiv_anual+0.05, 
                          label = Nombre,
                          color = Nombre),
            size = 3.8,
            show.legend = F) +
  geom_segment(data = etiquetas %>% 
                 filter(win_lose == 1),
               mapping = aes(x = Nombre, xend = Nombre,
                             y = tasa_equiv_anual+0.008, yend = tasa_equiv_anual + 0.047,
                             color = Nombre),
               linetype = 2,
               show.legend = F) +
  geom_text(mapping = aes(label = paste(round(tasa_equiv_anual*100,1),"%",sep=""),
                          y = tasa_equiv_anual+0.003),
            size = 3) +
  scale_y_continuous(limits = c(0, 0.12),
                     breaks = seq(0.00, 0.07, 0.005),
                     labels = seq(0.00, 0.07, 0.005)*100) +
  scale_x_discrete(expand = c(0.1,0.1)) +
  scale_fill_manual(values = c("#3E134F", "#C53270", "#F8B83C", "#F36E35")) +
  scale_color_manual(values = c("#C53270", "#3E134F", "#F36E35")) +
  labs(title = "Tasa de crecimiento anual equivalente en los sectores de la economía colombiana (2005 - 2021)",
       x = NULL,
       y = NULL,
       caption = "Fuente: elaboración propia en base al DANE  **#30DayChartChallenge #Day9**<br>@GEstebanGomez") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.caption = element_markdown(size=10, hjust=0.0, color="black", face = "italic"))
  


ggsave(filename = "../rmd/resultados/graficos/30DayChartChallenge/9Day_major_minor.png",
       dpi = 500,
       width = 9.89,
       height = 5.43
)

