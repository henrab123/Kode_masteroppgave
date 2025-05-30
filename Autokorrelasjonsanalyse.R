library(dplyr)
library(ggplot2)
library(forecast) 
RTMB_V22

V22_TS <- RTMB_V22 %>%
  #filter(kommisjon == 3) %>%
  mutate(Y_total = Y_m[,19] + Y_m[,20]) %>%  
  mutate(index = row_number()) %>%
  arrange(kandidatnummer) %>%
  select(index, kandidatnummer, Y_total)  

V23_TS <- RTMB_V23 %>%
  #filter(kommisjon == 3) %>%
  mutate(Y_total = Y_m[,16] + Y_m[,17]) %>%  
  mutate(index = row_number()) %>%
  arrange(kandidatnummer) %>%
  select(index, kandidatnummer, Y_total)  


V22_TS_ts <- ts(V22_TS$Y_total, start = 1, frequency = 1)
V23_TS_ts <- ts(V23_TS$Y_total, start = 1, frequency = 1)

# Lag ACF-plot med flere tilpasninger
acf_V22 <- ggAcf(V22_TS_ts, lag.max = 682) +
  ggtitle("Autokorrelasjonsanalyse av dataen fra v\u00e5ren 2022") +
  theme_light(base_size = 20) +                      # St??rre font for bedre lesbarhet
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#2C3E50"),  # Sentralisert og fet tittel med fin farge
    axis.title = element_text(face = "bold", color = "#34495E"),               # Fet akseltittel
    axis.text = element_text(color = "#34495E"),                              # Akselteks i fin gr??bl?? farge
    panel.grid.major = element_line(color = "#BDC3C7", linetype = "dashed"),   # Diskret grid med stiplet linje
    panel.grid.minor = element_blank()                                         # Fjern mindre grids for ryddigere plot
  ) +
  labs(x = "Lag (forsinkelse)", y = "Autokorrelasjon")       +
  geom_bar(stat = "identity", fill = "#3498DB", color = "#2980B9", width = 0.7, size = 0.9, alpha = 0.95)
# Tydelige aksetitler


# Lag ACF-plot med flere tilpasninger
acf_V23 <- ggAcf(V23_TS_ts, lag.max = 682) +
  ggtitle("Autokorrelasjonsanalyse av dataen fra v\u00e5ren 2023") +
  theme_light(base_size = 20) +                      # St??rre font for bedre lesbarhet
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#2C3E50"),  # Sentralisert og fet tittel med fin farge
    axis.title = element_text(face = "bold", color = "#34495E"),               # Fet akseltittel
    axis.text = element_text(color = "#34495E"),                              # Akselteks i fin gr??bl?? farge
    panel.grid.major = element_line(color = "#BDC3C7", linetype = "dashed"),   # Diskret grid med stiplet linje
    panel.grid.minor = element_blank()                                         # Fjern mindre grids for ryddigere plot
  ) +
  labs(x = "Lag (forsinkelse)", y = "Autokorrelasjon")       +
  geom_bar(stat = "identity", fill = "#3498DB", color = "#2980B9", width = 0.7, size = 0.9, alpha = 0.95)
# Tydelige aksetitler

# Vis plottet
acf_V22
acf_V23