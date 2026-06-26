library(tidyverse)
library(lubridate)
theme.custom<- theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=15),
        panel.border = element_rect(linewidth=2))
library(readr)

##Hobo air temperature-----------
  
library(readxl)
library(dplyr)
  High_air_temp <- read_excel("High_air_temp.xlsx")
View(High_air_temp)

Medium_air_temp <- read_excel("Medium_temp.xlsx")
View(Medium_air_temp)

low_air_temp <- read_excel("low_temp.xlsx")
View(low_air_temp)

#check column names
names(High_air_temp)
names(Medium_air_temp)
names(low_air_temp)

#Add temp treatment
High_air_temp <- High_air_temp%>%
  mutate(`Growth Air Temp.` = "High")
Medium_air_temp <- Medium_air_temp%>%
  mutate(`Growth Air Temp.` = "Medium")
low_air_temp <- low_air_temp%>%
  mutate(`Growth Air Temp.` = "Low")

hobo_air <- bind_rows (High_air_temp,
                       Medium_air_temp,
                       low_air_temp
)

str(hobo_air)
head(hobo_air)

hobo_air <- hobo_air %>%
  mutate(`Growth Air Temp.` = factor(`Growth Air Temp.`,
                                 levels = c("High", "Medium", "Low")))
hobo_air <- hobo_air %>%
  mutate(`Date Time` = as.POSIXct(`Date Time`))
hobo_air <- hobo_air %>%
  arrange(`Growth Air Temp.`, `Date-Time (CDT)`)
theme.custom<- theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=15),
        panel.border = element_rect(linewidth=2))




#diurnal figures----------------------------------------

#define by treatment start date-------------------------------------
library(dplyr)
library(lubridate)

hobo_filtered <- hobo_air %>%
  mutate(datetime = `Date-Time (CDT)`) %>%
  filter(
    (`Growth Air Temp.` %in% c("Low", "Medium") & as.Date(datetime) >= as.Date("2026-05-11")) |
      (`Growth Air Temp.` == "High" & as.Date(datetime) >= as.Date("2026-05-15"))
  )

#create hourly diurnal average------------------------
hobo_diurnal <- hobo_filtered %>%
  mutate(Hour = hour(datetime)) %>%
  group_by(Hour, `Growth Air Temp.`) %>%
  summarise(
    mean_temp = mean(`Temperature , °C`, na.rm = TRUE),
    mean_rh   = mean(`RH , %`, na.rm = TRUE),
    .groups = "drop"
  )

#diurnal temperature plot------------------
Air_temp_plot <- ggplot(hobo_diurnal,
       aes(x = Hour,
           y = mean_temp,
           color = `Growth Air Temp.`, group = `Growth Air Temp.`)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c(
      "High" = "red",
      "Medium" = "orange",
      "Low" = "blue"
    )
  )+
  geom_point() +
  scale_x_continuous(
    breaks = seq(0, 24, by = 2),
    limits = c(0, 24),
    expand = expansion(add = c(0, 0))
  )+
  scale_y_continuous(
    limits = c(0, 40),breaks = seq(0, 40, by = 5),
    expand = expansion(add = c(0, 0))
  )+
  labs(x = "Hour of day",
       y = "Air temperature (°C)") +
  theme.custom

Air_temp_plot
#diurnal humidity plot----------------------
p_full <- ggplot(hobo_diurnal,
       aes(x = Hour,
           y = mean_rh,
           color = `Growth Air Temp.`)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c(
      "High" = "red",
      "Medium" = "orange",
      "Low" = "blue"
    )
  )+
  scale_x_continuous(
    breaks = seq(0, 24, by = 2),
    limits = c(0, 24),
    expand = expansion(add = c(0, 0))
  )+
  scale_y_continuous(
    limits = c(0, 100),breaks = seq(0, 100, by = 10),
    expand = expansion(add = c(0, 0))
  )+
  geom_point() +
  labs(x = "Hour of day",
       y = "Relative humidity (%)") +
 theme.custom

p_full


##merge plots
temp_humid_combine <- (Air_temp_plot + p_full)+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
temp_humid_combine 
temp_humid_combine  + plot_annotation(tag_levels = "A")
ggsave("temp_humid_combine .tiff",
       width = 8,
       height = 4,
       dpi = 600, compression = "lzw")
