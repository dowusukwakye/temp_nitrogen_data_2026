#The following are the codes for the analysis and data visualization of the 
#Air temperature and soil nitrogen supply experiment.

#load the important libraries
library(readxl)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(lme4)
library(car)
library(emmeans)
library(minpack.lm)
library(broom)
library(stringr)
library(cowplot)

#custom theme for data visualization
theme.custom<- theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=15),
        panel.border = element_rect(linewidth=2))

#upload the curve level data
tresp_data <- read_csv("curve_level_data.csv")
head(tresp_data)
view(tresp_data)

#tempereature response curve is generated using the third order polynomial used 
#by (smith and Dukes 2017).

#describe the quadratic term to estimate parameter values a, b, c for Vcmax and Jmax
tresp_data <- tresp_data%>% mutate(Tleaf_mean2 = Tleaf_mean^2) 
head(tresp_data) 
#create new plant ids 
tresp_data <- tresp_data %>% mutate(plant_id_new = str_extract(id, "^[^_]+"))
head(tresp_data)


#fitting model per plant id for Vcmax-------------------------------
quad_exp_fit_lm <- tresp_data %>%
  group_by(plant_id_new) %>% #group by plant id
  filter(!is.na(obs_vcmax)) %>%  # remove NA rows
  filter(n() >= 3) %>%                               # require at least 3 points
  group_modify(~{
    model <- lm(log(obs_vcmax) ~ Tleaf_mean + Tleaf_mean2, data = .x)
    data.frame(
      n_points = nrow(.x),
      intercept = coef(model)[1],# Points at Tleaf = Ta
      
      b1 = coef(model)[2],
      b2 = coef(model)[3],
      Topt = -coef(model)[2] / (2 * coef(model)[3])
    )
  })
head(quad_exp_fit_lm)
summary(quad_exp_fit_lm)

#writing the model fit as excel sheet
write_xlsx(quad_exp_fit_lm, "vcmax_tempresp_fits.xlsx")

#upload extracted a, b, and c values with treatment information for Vcmax 
vcmax_tempresp_fits <- read_csv("vcmax_tempresp_fits.csv")
vcmax_tempresp_fits <- vcmax_tempresp_fits %>%
  mutate(airtemp_factor = factor(airtemp_factor,
                                 levels = c("High", "Medium", "Low")))
head(vcmax_tempresp_fits)
view(vcmax_tempresp_fits)

#check distributions for a, b, and c
hist(vcmax_tempresp_fits$ a, main = "Distribution of a")
hist(vcmax_tempresp_fits$ b, main = "Distribution of b")
hist(vcmax_tempresp_fits$ c, main = "Distribution of c")

#using only stable fits
nvcmax_tempresp_fits <- vcmax_tempresp_fits %>%
  filter(c < 0,b <= 10, a <=10)
head(nvcmax_tempresp_fits)
view(nvcmax_tempresp_fits)

#check distribution for good fits
hist(nvcmax_tempresp_fits$ a, main = "Distribution of a")
hist(nvcmax_tempresp_fits$ b, main = "Distribution of b")
hist(nvcmax_tempresp_fits$ c, main = "Distribution of c")

##fit lmer model for parameter a
a_lmer <- lmer(a ~ Nfert * airtemp_factor + 
                 (1|Rack:airtemp_factor), data = nvcmax_tempresp_fits)
plot(residuals(a_lmer)~fitted(a_lmer))
summary(a_lmer)
Anova(a_lmer)
emmeans(a_lmer, ~ Nfert * airtemp_factor)
a_emm <- emmeans(a_lmer, ~ airtemp_factor)

##fit lmer model for parameter b
b_lmer <- lmer(b ~ Nfert * airtemp_factor + 
                 (1|Rack:airtemp_factor), data = nvcmax_tempresp_fits)
plot(residuals(b_lmer)~fitted(b_lmer))
summary(b_lmer)
Anova(b_lmer)
emmeans(b_lmer, ~ Nfert * airtemp_factor)
b_emm <- emmeans(b_lmer, ~ airtemp_factor)

##fit lmer model for parameter c
c_lmer <- lmer(c ~ Nfert * airtemp_factor + 
                 (1|Rack:airtemp_factor), data = nvcmax_tempresp_fits)
plot(residuals(c_lmer)~fitted(c_lmer))
summary(c_lmer)
Anova(c_lmer)
emmeans(c_lmer, ~ Nfert * airtemp_factor)
c_emm <- emmeans(c_lmer, ~ airtemp_factor)

#convert em means to data frame
a_emm_df <- as.data.frame(a_emm)
b_emm_df <- as.data.frame(b_emm)
c_emm_df <- as.data.frame(c_emm)

#calculating the mean parameters per treatment for curves
library(dplyr)
mean_params <- data.frame( 
  airtemp_factor = a_emm_df$airtemp_factor, 
  a = a_emm_df$emmean, 
  b = b_emm_df$emmean, 
  c = c_emm_df$emmean )
mean_params

#creating temperature sequence
temp_seq <- seq(15, 50, by = 0.5)

#generaating smooth curves
curve_data <- mean_params %>%
  rowwise() %>% mutate(
    temp = list(temp_seq), Vcmax = list(exp(a + b * temp_seq + c * temp_seq^2)) ) %>%
  unnest(cols = c(temp, Vcmax))

  #computing mean and standard errors 
vcmax_emmeans <- mean_params %>%
  mutate(
    Ta = case_when(airtemp_factor == "Low" ~ 23,
                   airtemp_factor == "Medium" ~ 30,
                   airtemp_factor == "High" ~ 37),
    Vcmax_Ta = exp(a + b * Ta + c * Ta^2),
    # upper bound
    Vcmax_upper = exp((a + a_emm_df$SE) + b * Ta + c * Ta^2),
    # lower bound
    Vcmax_lower = exp((a - a_emm_df$SE) + b * Ta + c * Ta^2)
  )

#figure --------------------------
library(ggplot2) 
Ta_colors <- c("Low" = "blue","Medium" = "orange", "High" = "red") 
Ta_linetypes <- c("Low" = "solid", "Medium" = "solid", "High" = "solid")

vcmax_plot <- ggplot() +
  geom_line(data = curve_data,
            aes(x = temp, y = Vcmax,
                color = as.factor(airtemp_factor),
                linetype = as.factor(airtemp_factor)),
            linewidth = 1.2)+
  geom_point(data = vcmax_emmeans,
             aes(x = Ta, y = Vcmax_Ta,
                 color = as.factor(airtemp_factor)),
             size = 4)+
  geom_errorbar(data = vcmax_emmeans,
                aes(x = Ta,
                    ymin = Vcmax_lower,
                    ymax = Vcmax_upper,
                    color = airtemp_factor),
                width = 1)+
  scale_color_manual(name = "Growth Air Temp.", values = Ta_colors) +
  scale_linetype_manual(name = "Growth Air Temp.", values = Ta_linetypes) +
  scale_y_continuous(
    limits = c(0, 500),
    breaks = seq(0, 500, by = 100),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "Leaf Temperature (°C)",
       y = expression(V[cmax]~(mu*mol~m^-2~s^-1))) +
  theme.custom
vcmax_plot
ggsave("vcmax_plot.tiff",
       width = 8,
       height = 6,
       dpi = 600)

#estimate vcmax25
vcmax_25_tempresp_fits <- vcmax_tempresp_fits%>%
  mutate(
    Vcmax25 = exp(a + b*25 + c*25^2)
  )
view(vcmax_25_tempresp_fits)

#write vcmax25
write_xlsx(vcmax25_tempresp_fits, "vcmax25_tempresp_fits.xlsx")

#jmax-------------------------------
##fiting model for jmax
quad_exp_jfit_lm <- tresp_data %>%
  group_by(plant_id_new) %>% #group by plant id
  filter(!is.na(obs_jmax)) %>%  # remove NA rows
  filter(n() >= 3) %>%                               # require at least 3 points
  group_modify(~{
    model <- lm(log(obs_jmax) ~ Tleaf_mean + Tleaf_mean2, data = .x)
    data.frame(
      n_points = nrow(.x),
      intercept = coef(model)[1],# Points at Tleaf = Ta
      
      b1 = coef(model)[2],
      b2 = coef(model)[3],
      Topt = -coef(model)[2] / (2 * coef(model)[3])
    )
  })

head(quad_exp_jfit_lm)
summary(quad_exp_jfit_lm)

#writing the model fit as excel sheet

write_xlsx(quad_exp_jfit_lm, "jmax_tempresp_fits1.csv")

#upload extracted a, b, and c values with treatment information for jmax 

jmax_tempresp_fits1 <- read_csv("jmax_tempresp_fits1.csv")
jmax_tempresp_fits1 <- jmax_tempresp_fits1 %>%
  mutate(airtemp_factor = factor(airtemp_factor,
                                 levels = c("High", "Medium", "Low")))
head(jmax_tempresp_fits1)
view(jmax_tempresp_fits1)

#check distributions for a, b, and c
hist(jmax_tempresp_fits1$ a, main = "Distribution of a")
hist(jmax_tempresp_fits1$ b, main = "Distribution of b")
hist(jmax_tempresp_fits1$ c, main = "Distribution of c")

#filtering jmax 
jmax_tempresp_filter <- jmax_tempresp_fits1 %>% 
  filter( c < 0, # must have peak 
          b > -0.01, # increasing before peak 
          b > 0,
          b<0.3,# tighter than Vcmax 
          a > -5, # avoid extreme negatives 
          a < 5, Topt > 10, Topt < 50 )
head(jmax_tempresp_filter)
view(jmax_tempresp_filter)

#check distributions for goodfits
hist(jmax_tempresp_filter$ a, main = "Distribution of a")
hist(jmax_tempresp_filter$ b, main = "Distribution of b")
hist(jmax_tempresp_filter$ c, main = "Distribution of c")

#fit lmer for parameter a
aj_lmer <- lmer(a ~ Nfert * airtemp_factor + 
                  (1|Rack:airtemp_factor), data = jmax_tempresp_filter)
plot(residuals(aj_lmer)~fitted(aj_lmer))
summary(aj_lmer)
Anova(aj_lmer)
emmeans(aj_lmer, ~ Nfert * airtemp_factor)
a_emm <- emmeans(aj_lmer, ~ airtemp_factor)

#fit lmer for parameter b
bj_lmer <- lmer(b ~ Nfert * airtemp_factor + 
                  (1|Rack:airtemp_factor), data = jmax_tempresp_filter)
plot(residuals(bj_lmer)~fitted(bj_lmer))
summary(bj_lmer)
Anova(bj_lmer)
emmeans(bj_lmer, ~ Nfert * airtemp_factor)
b_emm <- emmeans(bj_lmer, ~ airtemp_factor)

#fit lmer for parameter b
cj_lmer <- lmer(c ~ Nfert * airtemp_factor + 
                  (1|Rack:airtemp_factor), data = jmax_tempresp_filter)
plot(residuals(cj_lmer)~fitted(cj_lmer))
summary(cj_lmer)
Anova(cj_lmer)
emmeans(cj_lmer, ~ Nfert * airtemp_factor)
c_emm <- emmeans(cj_lmer, ~ airtemp_factor)

#convert emmeans to dataframe
aj_emm_df <- as.data.frame(a_emm)
bj_emm_df <- as.data.frame(b_emm)
cj_emm_df <- as.data.frame(c_emm)

#calculating the mean parameters per treatment for curves
library(dplyr)
mean_params <- data.frame( 
  airtemp_factor = aj_emm_df$airtemp_factor, 
  a = aj_emm_df$emmean, 
  b = bj_emm_df$emmean, 
  c = cj_emm_df$emmean )
mean_params

#creating temperature sequence
temp_seq <- seq(15, 50, by = 0.5)

#generating smooth curves
curve_data <- mean_params %>%
  rowwise() %>% mutate(
    temp = list(temp_seq), Jmax = list(exp(a + b * temp_seq + c * temp_seq^2)) ) %>%
  unnest(cols = c(temp, Jmax))

#computing mean and standard errors 
jmax_emmeans <- mean_params %>%
  mutate(
    Ta = case_when(airtemp_factor == "Low" ~ 23,
                   airtemp_factor == "Medium" ~ 30,
                   airtemp_factor == "High" ~ 37),
    Jmax_Ta = exp(a + b * Ta + c * Ta^2),
    # upper bound
    Jmax_upper = exp((a + aj_emm_df$SE) + b * Ta + c * Ta^2),
    # lower bound
    Jmax_lower = exp((a - aj_emm_df$SE) + b * Ta + c * Ta^2)
  )

#figure --------------------------------
library(ggplot2) 
Ta_colors <- c("Low" = "blue","Medium" = "orange", "High" = "red") 
Ta_linetypes <- c("Low" = "solid", "Medium" = "solid", "High" = "solid")

jmax_plot <-ggplot() +
  geom_line(data = curve_data,
            aes(x = temp, y = Jmax,
                color = as.factor(airtemp_factor),
                linetype = as.factor(airtemp_factor)),
            linewidth = 1.2)+
  geom_point(data = jmax_emmeans,
             aes(x = Ta, y = Jmax_Ta,
                 color = as.factor(airtemp_factor)),
             size = 4)+
  geom_errorbar(data = jmax_emmeans,
                aes(x = Ta,
                    ymin = Jmax_lower,
                    ymax = Jmax_upper,
                    color = airtemp_factor),
                width = 1)+
  scale_color_manual(name = "Growth Air Temp.", values = Ta_colors) +
  scale_linetype_manual(name = "Growth Air Temp.", values = Ta_linetypes) +
  scale_y_continuous(
    limits = c(0, 400),
    breaks = seq(0, 400, by = 100),
    expand = expansion(mult = c(0, 0))
  )+
  labs(x = "Leaf Temperature (°C)",
       y = expression(J[max]~(mu*mol~m^-2~s^-1))) +
  theme.custom
jmax_plot

#merging vcmax and jmax plot --------------------------
# combine 
vj_combine <- (vcmax_plot + jmax_plot)+
plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
vj_combine
vj_combine + plot_annotation(tag_levels = "A")

ggsave("vj_combine.tiff",
       width = 8,
       height = 4,
       dpi = 600, compression = "lzw")


#estimate jmax25-----------------------
jmax_25_tempresp_fits <- jmax_tempresp_fits1%>%
  mutate(
    Jmax25 = exp(a + b*25 + c*25^2)
  )
view(jmax_25_tempresp_fits)

#merge vcmax25 and jmax25-------------------
merged25_data <- vcmax_25_tempresp_fits%>%
  left_join(
    jmax_25_tempresp_fits %>%
      select(plant_id_new, Jmax25),
    by = "plant_id_new"
  )
merged25_data <- merged25_data %>%
  mutate(airtemp_factor = factor(airtemp_factor,
                                 levels = c("High", "Medium", "Low")))
head(merged25_data)
view(merged25_data)

#write merged vcmax25 and jmax25 and use later
write_xlsx(merged25_data, "vcmax25_jmax25.xlsx")

#jv data
#jv_data was extracted by using
#J:V=Vcmax(Ta)/Jmax(Ta) 
vcmax_ta_data <- vcmax_tempresp_fits %>%
  mutate(
    Ta = case_when(
      airtemp_factor == "Low" ~ 23,
      airtemp_factor == "Medium" ~ 30,
      airtemp_factor == "High" ~ 37
    ),
    Vcmax_Ta = exp(a + b * Ta + c * Ta^2)
  )

jmax_ta_data <- jmax_tempresp_fits1 %>%
  mutate(
    Ta = case_when(
      airtemp_factor == "Low" ~ 23,
      airtemp_factor == "Medium" ~ 30,
      airtemp_factor == "High" ~ 37
    ),
    Jmax_Ta = exp(a + b * Ta + c * Ta^2)
  )
jv_data <- vcmax_ta_data %>%
  select(plant_id_new, airtemp_factor, Nfert, Ta, Vcmax_Ta) %>%
  left_join(
    jmax_ta_data %>%
      select(plant_id_new, Jmax_Ta),
    by = "plant_id_new"
  )

jv_data <- jv_data %>%
  mutate(
    JV_ratio = Jmax_Ta / Vcmax_Ta
  )
jv_data <- jv_data %>%
  mutate(airtemp_factor = factor(airtemp_factor,
                                 levels = c("High", "Medium", "Low")))
head(jv_data)
view(jv_data)

##fit lmer model for Jmax/Vcmax
jv_lmer <- lmer(JV_ratio ~ Nfert * airtemp_factor + 
                  (1|airtemp_factor), data = jv_data)
plot(residuals(jv_lmer)~fitted(jv_lmer))
summary(jv_lmer)
Anova(jv_lmer)

##test trends
test(emtrends(jv_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'High')))
test(emtrends(jv_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'Medium')))
test(emtrends(jv_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'Low')))

##extract trends (not significant)
jv_emtrend_high <- summary(emtrends(jv_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'High')))
jv_emtrend_medium <- summary(emtrends(jv_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'Medium')))
jv_emtrend_low <- summary(emtrends(jv_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'Low')))

jv_intercept_high <- summary(emmeans(jv_lmer, ~1, at = list(Nfert = 0, airtemp_factor = 'High')))
jv_intercept_medium <- summary(emmeans(jv_lmer, ~1, at = list(Nfert = 0, airtemp_factor = 'Medium')))
jv_intercept_low <- summary(emmeans(jv_lmer, ~1, at = list(Nfert = 0, airtemp_factor = 'Low')))

##creating function
jv_func_high <- function(x){(jv_emtrend_high[1,2] * x + jv_intercept_high[1,2])}
jv_func_medium <- function(x){(jv_emtrend_medium[1,2] * x + jv_intercept_medium[1,2])}
jv_func_low <- function(x){(jv_emtrend_low[1,2] * x + jv_intercept_low[1,2])}

#pooled slope across all temperatures
jv_emtrend_all <- summary(
  emtrends(jv_lmer, ~1, var = "Nfert"))

#pooled intercerpt at Nfert = 0
jv_intercept_all <- summary(
  emmeans(jv_lmer, ~1, at = list(Nfert = 0))
)

##single pooled function
jv_func_all <- function(x)
{jv_emtrend_all[1,2] * x + jv_intercept_all[1,2]}

##plot-----------------------------------------------
jv_plot_new <- ggplot(aes(y = JV_ratio, x = Nfert,
           color = airtemp_factor),  
       data = jv_data)+
  geom_point(alpha = 0.6, size = 3) +
  scale_color_manual(values = c('High' = 'red', 'Medium' = 'orange', 'Low' =  'blue'))+
  
  scale_shape_manual(values = c('High' = 16,
                                'Medium' = 17,
                                'Low' = 15))+
  stat_function(fun = jv_func_all, color = "black", lwd = 2, 
                inherit.aes = FALSE)+
  stat_function(fun = jv_func_high, color = 'red', lwd = 2, lty = 2, alpha = 0.3, inherit.aes = FALSE)+
  stat_function(fun = jv_func_medium, color = 'orange', lwd = 2, lty = 2, alpha = 0.3, inherit.aes = FALSE)+
  stat_function(fun = jv_func_low, color = 'blue', lwd = 2, lty = 2, alpha = 0.3, inherit.aes = FALSE)+
  labs(color = 'Growth Air Temp.',  shape = 'Growth Air Temp.') +
  xlab('Soil nitrogen supply (ppm)') +
  ylab(expression(J[max] / V[cmax])) +theme.custom
jv_plot_new


#biomass----------------

library(readr)
biomass <- read_csv("biomass.csv")
biomass <- biomass %>%
  mutate(airtemp_factor = factor(airtemp_factor,
                                 levels = c("High", "Medium", "Low")))
head(biomass)
View(biomass)
###write out data for publication
write.csv(biomass, "biomass_2025_data.csv")
##explore data
hist(biomass$`Plant height (cm)`) #better height


  
##fit lme for height
plant_height_lmer <- lmer(`Plant height (cm)`~ Nfert * airtemp_factor + (1| Rack:airtemp_factor), 
                 data = biomass)
plot(resid(plant_height_lmer)~fitted(plant_height_lmer))#better after log transformation
summary(plant_height_lmer)
Anova(plant_height_lmer) 

#pooled slope across all temperatures
pl_emtrend_all <- summary(
  emtrends(plant_height_lmer, ~1, var = "Nfert"))

#pooled intercerpt at Nfert = 0
pl_intercept_all <- summary(
  emmeans(plant_height_lmer, ~1, at = list(Nfert = 0))
)
##single pooled function
pl_func_all <- function(x)
{pl_emtrend_all[1,2] * x + pl_intercept_all[1,2]}

##plot-------------------------------------------
height_plot <- ggplot(biomass, aes(x = Nfert, y = `Plant height (cm)`,
                    color = airtemp_factor)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm",
              se = FALSE,
              aes(group = airtemp_factor),
              formula = y ~ x,
              linewidth = 2)+
  scale_color_manual(values = c('High' = alpha('red', 0.3),
                                'Medium' = alpha('orange',0.3),
                                'Low' = alpha('blue', 0.3))) +
  stat_function(fun = pl_func_all,  xlim = c(0, 630),color = "black", lwd = 2, 
                inherit.aes = FALSE)+
  scale_x_continuous(limits = c(0, 700),breaks = seq(0, 700, by = 100),
                     expand = expansion(add = c(20, 10))
  )+
  scale_y_continuous(
    limits = c(0, 25),breaks = seq(0, 25, by = 5),
    expand = expansion(add = c(0, 0))
  )+
  labs(color = "Growth Air Temp.",
       x = "Soil nitrogen supply (ppm)",
       y = "Plant height (cm)") +
  theme.custom

height_plot

#explore aboveground-----------------------------------
hist(biomass$`Above ground biomass (g)`) #better avoveground
####fit lme for aboveground biomass
aboveground_lmer <- lmer(`Above ground biomass (g)`~ Nfert * airtemp_factor + (1| Rack:airtemp_factor), 
                          data = biomass)
plot(resid(aboveground_lmer)~fitted(aboveground_lmer))#better after log transformation
summary(aboveground_lmer)
Anova(aboveground_lmer)

#pooled slope across all temperatures
ab_emtrend_all <- summary(
  emtrends(aboveground_lmer, ~1, var = "Nfert"))

#pooled intercerpt at Nfert = 0
ab_intercept_all <- summary(
  emmeans(aboveground_lmer, ~1, at = list(Nfert = 0))
)
##single pooled function
ab_func_all <- function(x)
{ab_emtrend_all[1,2] * x + ab_intercept_all[1,2]}
#plot
aboveground_plot <- ggplot(biomass, aes(x = Nfert, y = `Above ground biomass (g)`,
                                   color = airtemp_factor)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm",
              se = FALSE,
              aes(group = airtemp_factor),
              formula = y ~ x,
              linewidth = 2)+
  scale_color_manual(values = c('High' = alpha('red', 0.3),
                                'Medium' = alpha('orange',0.3),
                                'Low' = alpha('blue', 0.3))) +
  stat_function(fun = ab_func_all,  xlim = c(0, 630),color = "black", lwd = 2, 
                inherit.aes = FALSE)+
  scale_x_continuous(limits = c(0, 700),breaks = seq(0, 700, by = 100),
                     expand = expansion(add = c(20, 10))
  )+
  scale_y_continuous(
    limits = c(0, 2.5),breaks = seq(0, 2.5, by = 0.5),
    expand = expansion(add = c(0, 0))
  )+
  labs(color = "Growth Air Temp.",
       x = "Soil nitrogen supply (ppm)",
       y = "Aboveground biomass (g)") +
  theme.custom

aboveground_plot

#explore belowground-----------------------------------------------
hist(biomass$Belowground_biomass) # better belowground
biomass <- biomass %>%
  mutate(belowground_g = Belowground_biomass/1000)
####fit lme for belowground biomass
Belowground_lmer <- lmer(belowground_g~ Nfert * airtemp_factor + (1| Rack:airtemp_factor), 
                         data = biomass)
plot(resid(Belowground_lmer)~fitted(Belowground_lmer))#better after log transformation
summary(Belowground_lmer)
Anova(Belowground_lmer) 

#pooled slope across all temperatures
bl_emtrend_all <- summary(
  emtrends(Belowground_lmer, ~1, var = "Nfert"))

#pooled intercerpt at Nfert = 0
bl_intercept_all <- summary(
  emmeans(Belowground_lmer, ~1, at = list(Nfert = 0))
)
##single pooled function
bl_func_all <- function(x)
{bl_emtrend_all[1,2] * x + bl_intercept_all[1,2]}

#plot----------------------------------------------------------------
belowground_plot <- ggplot(biomass, aes(x = Nfert, y = belowground_g,
                                        color = airtemp_factor)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm",
              se = FALSE,
              aes(group = airtemp_factor),
              formula = y ~ x,
              linewidth = 2, linetype = 2)+
  scale_color_manual(values = c('High' = alpha('red', 0.3),
                                'Medium' = alpha('orange',0.3),
                                'Low' = alpha('blue', 0.3))) +
  stat_function(fun = bl_func_all, xlim = c(0, 630),color = "black", lwd = 2, lty = 2, 
                inherit.aes = FALSE)+
  scale_x_continuous(limits = c(0, 700),breaks = seq(0, 700, by = 100),
                     expand = expansion(add = c(20, 10))
  )+
  scale_y_continuous(
    limits = c(0, 0.6),breaks = seq(0, 0.6, by = 0.1),
    expand = expansion(add = c(0, 0))
  )+
  labs(color = "Growth Air Temp.",
       x = "Soil nitrogen supply (ppm)",
       y = "Belowground biomass (g)") +
  theme.custom

belowground_plot

#root:shoot ratio -----------------------------------------------------------
biomass <- biomass %>%
  mutate(
    root_shoot_ratio = belowground_g/`Above ground biomass (g)`
  )

hist(biomass$root_shoot_ratio)

root_shoot_lmer <- lmer(root_shoot_ratio~ Nfert * airtemp_factor + (1| Rack:airtemp_factor), 
                         data = biomass)
plot(resid(root_shoot_lmer)~fitted(root_shoot_lmer))#better after log transformation
summary(root_shoot_lmer)
Anova(root_shoot_lmer) 
  
#pooled slope across all temperatures
rs_emtrend_all <- summary(
  emtrends(root_shoot_lmer, ~1, var = "Nfert"))

#pooled intercerpt at Nfert = 0
rs_intercept_all <- summary(
  emmeans(root_shoot_lmer, ~1, at = list(Nfert = 0))
)
##single pooled function
rs_func_all <- function(x)
{rs_emtrend_all[1,2] * x + rs_intercept_all[1,2]} 

#plot----------------------------------------------
root_shoot_plot <- ggplot(biomass, aes(x = Nfert, y = root_shoot_ratio,
                                        color = airtemp_factor)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm",
              se = FALSE,
              aes(group = airtemp_factor),
              formula = y ~ x,
              linewidth = 2)+
  scale_color_manual(values = c('High' = alpha('red', 0.3),
                                'Medium' = alpha('orange',0.3),
                                'Low' = alpha('blue', 0.3))) +
  stat_function(fun = rs_func_all, xlim = c(0, 630), color = "black", lwd = 2, lty = 1, 
                inherit.aes = FALSE)+
  scale_x_continuous(limits = c(0, 700),breaks = seq(0, 700, by = 100),
                     expand = expansion(add = c(20, 10))
  )+
  scale_y_continuous(
    limits = c(0, 1),breaks = seq(0, 1, by = 0.2),
    expand = expansion(add = c(0, 0))
  )+
  labs(color = "Growth Air Temp.",
       x = "Soil nitrogen supply (ppm)",
       y = "Root:Shoot ratio (g g⁻¹)") +
  theme.custom

root_shoot_plot  

##merge plot for journal------------------
library(patchwork)
biomass_figure <- 
  height_plot + aboveground_plot + belowground_plot+root_shoot_plot+
  plot_layout(guides = "collect", ncol = 2) &
  theme(legend.position = "bottom")

biomass_figure+
  plot_annotation(tag_levels = "A")

##export 
ggsave("biomass_figure.tiff",
       width = 6.7,
       height = 5.9,
       dpi = 600, compression = "lzw")

#chlorophyll calculations-------------------------------
library(readxl)
data <- read_excel("chlorophyll extraction.xlsx")
head(data)
data$avg_649 <- (data$A649 + data$A649.1 + data$A649.2)/3
data$avg_665 <- (data$A665 + data$A665.1 + data$A665.2)/3
data$chla_ug.ml <- (12.19 * data$avg_665) - (3.45 * data$avg_649) # ug mL-1, from Wellburn (1994)
data$chlb_ug.ml <- (21.99 * data$avg_649) - (5.32 * data$avg_665) # ug mL-1, from Wellburn (1994)
data$chla_g.ml <- data$chla_ug.ml / 1000000 # g mL-1
data$chlb_g.ml <- data$chlb_ug.ml / 1000000 # g mL-1
data$chla_g <- data$chla_g.ml * 10 # 10 mL of DMSO
data$chlb_g <- data$chlb_g.ml * 10 # 10 mL of DMSO
data$chla_g.m2 <- data$chla_g / (data$chl_area / 10000) # convert area to m2
data$chlb_g.m2 <- data$chlb_g / (data$chl_area / 10000) # convert area to m2
data$chla_mol.m2 <- data$chla_g.m2 / 893.51 # 893.51 g mol-1 chlorophyll a
data$chlb_mol.m2 <- data$chlb_g.m2 / 907.47 # 907.47 g mol-1 chlorophyll b
data$chla_mmol.m2 <- data$chla_mol.m2 * 1000
data$chlb_mmol.m2 <- data$chlb_mol.m2 * 1000
data$chl_wt_new <- data$chla_g + data$chlb_g
data$chl_marea <- data$chl_wt_new / (data$chl_area / 10000) #g m-2
data$chla_mmol.g <- data$chla_mmol.m2 * (1 / data$chl_marea)
data$chlb_mmol.g <- data$chlb_mmol.m2 * (1 / data$chl_marea)
data$chl_mmol.g <- data$chla_mmol.g + data$chlb_mmol.g #Chlmass #biochemical investment
hist(data$chl_mmol.g)
data$chl_mmol.m2 <- data$chla_mmol.m2 + data$chlb_mmol.m2 #Chlarea #light capture capacity
hist(data$chl_mmol.m2)
data$chl_mmol.m2_narea <- data$chl_mmol.m2 / data$narea
hist(data$chl_mmol.m2_narea)

#upload data vcmax25 and jmax25 for re-analysis (NB: Vcmax25 and Jmax25 data was extracted using 
#the same quadratic model) 
library(readr)

vcmax25_jmax25_only <- read_csv("vcmax25_jmax25_only.csv")
View(vcmax25_jmax25_only) #nmass, leaf biomass and leaf area is attached

#clean chlorophyll ids to make sure they have the same ids
library(stringr)

data <- data %>%
  mutate(plant_id_new = str_remove(plant_id_new, "D$"))
head(data)

#merging the two
vcmax25_jmax25_only <- vcmax25_jmax25_only %>%
  mutate(plant_id_new = as.character(plant_id_new))

data <- data %>%
  mutate(plant_id_new = as.character(plant_id_new))
head(data)
final_data <- data %>%
  left_join(vcmax25_jmax25_only, by = "plant_id_new")
final_data <- final_data %>%
  mutate(airtemp_factor = factor(airtemp_factor,
                                 levels = c("High", "Medium", "Low")))
view(final_data)

#estimate actual area for leaves used for chlorophyl extraction
final_data$area_m2 <- final_data$`leaf area (cm2)`/10000 #change cm2 to m2
final_data$Marea <- final_data$`Leaf biomass (g)`/final_data$area_m2
final_data$N_leaf <- final_data$nmass * final_data$`Leaf biomass (g)` #compute total N per leaf
final_data$Narea <- final_data$N_leaf / final_data$area_m2 #calculate Narea

#drop incomplete/NA rows
clean_final_data <- final_data%>%
  filter(!is.na(Vcmax25), !is.na(Jmax25))

###############################################################################
# p_rubisco(vcmax25, narea):
###############################################################################
#
# Calculates proportion of leaf nitrogen in Rubisco following equations from
# Niinemets et al. (1997) and Niinements et al. (1998)
# Function arguments:
#   - vcmax25     = maximum Rubisco carboxylation rate, standardized to 25degC
#                  (μmol m^-2 s^-1)
#   - narea       = leaf nitrogen per leaf area (gN m^-2)
#
# Returns:
# Vector with proportion of leaf N to Rubisco (p_rubisco; gN Rubisco gN^-1)
p_rubisco <- function(vcmax25, narea){
  
  vcr <- 20.5          # µmol CO2 g⁻¹ Rubisco s⁻¹
  Nr <- 0.16           # g N per g Rubisco
  
  p_rubisco <- (vcmax25 * Nr) / (vcr * narea)
  
  return(p_rubisco)
}
###proportion of N in rubisco
clean_final_data$propN_rubisco <- p_rubisco(vcmax25 = clean_final_data$Vcmax25, 
                                narea = clean_final_data$Narea)

### proportion of N in rubisco
#Vector with proportion of leaf N to bioenergetics (p_bioenergetics; 
                                                   # g N bioenergetics g N^-1)
p_bioenergetics <- function(jmax25, narea){
  Jmc <- 156                 # electron transport capacity per cyt f
  Nb  <- 0.1240695          # g N per µmol cyt f
  p_bioenergetics <- (jmax25 * Nb) / (Jmc * narea)
  return(p_bioenergetics)
}

clean_final_data$propN_bioenergetics <- p_bioenergetics(jmax25 = clean_final_data$Jmax25, 
 narea = clean_final_data$Narea)



### proportion of N in light harvesting

p_lightharvesting <- function(chlorophyll, nmass){
  Cb <- 2.75  # mmol chl per g N
  p_lightharvesting <- chlorophyll / (Cb * nmass)
  return(p_lightharvesting)
}
### proportion of N in light harvesting
clean_final_data$propN_lightharvesting <- p_lightharvesting(chlorophyll = clean_final_data$chl_mmol.g,
                                                nmass = clean_final_data$nmass)
hist(clean_final_data$propN_lightharvesting)## values too large
#tryin the light harvesting in area basis
p_lightharvesting_area <- function(chl_area, narea){
  Cb <- 2.75
  chl_area / (Cb * narea)
}
##apply it
clean_final_data$rho_lh <- p_lightharvesting_area(
  chl_area = clean_final_data$chl_mmol.m2,
  narea   = clean_final_data$Narea
)
### proportion of N in photosynthesis
clean_final_data$propN_photosynthesis = clean_final_data$propN_rubisco + clean_final_data$propN_bioenergetics + clean_final_data$rho_lh
hist(clean_final_data$propN_photosynthesis)

view(clean_final_data)

###write out data for publication
 write.csv(clean_final_data, "temp_nitrogen_2026.csv")


##figures for vcmax25, jmax25, Chlarea, Chlmass, Narea, Marea,
 #Nmass, p_rubisco, p_bioenergetics,p_lightharvesting
 
 ###Nmass----------------------------------------------------------
 ##explore data
 hist(clean_final_data$nmass)#pretty ok
 plot(nmass~Nfert, data = clean_final_data) #clear platteau
 #fit lme model
 nmass_lmer <- lmer(nmass ~ Nfert * airtemp_factor +
                      (1|Rack:airtemp_factor), data = clean_final_data)
 plot(resid(nmass_lmer) ~ fitted(nmass_lmer)) #mostly okay
 summary(nmass_lmer)
 Anova(nmass_lmer)

##test trends (All significant)
test(emtrends(nmass_lmer,~airtemp_factor, "Nfert"))#checks overall trends
test(emtrends(nmass_lmer, ~1, var = 'Nfert',at= list(airtemp_factor = 'High')))
test(emtrends(nmass_lmer, ~1, var = 'Nfert', at= list(airtemp_factor ='Medium')))
test(emtrends(nmass_lmer, ~1, var = 'Nfert', at= list(airtemp_factor = 'Low')))

##extract trends
nmass_emtrend_high <- summary(emtrends(nmass_lmer, ~1, var = 'Nfert',at= list(airtemp_factor = 'High')))
nmass_emtrend_medium <- summary(emtrends(nmass_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'Medium')))
nmass_emtrend_low <- summary(emtrends(nmass_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'Low')))

nmass_intercept_high <- summary(emmeans(nmass_lmer, ~1, at = list(Nfert = 0, airtemp_factor = 'High')))
nmass_intercept_medium <- summary(emmeans(nmass_lmer, ~1, at = list(Nfert = 0, airtemp_factor = 'Medium')))
nmass_intercept_low <- summary(emmeans(nmass_lmer, ~1, at = list(Nfert = 0, airtemp_factor ='Low')))

nmass_func_high <- function(x){(nmass_emtrend_high [1, 2] * x + nmass_intercept_high[1, 2])}
nmass_func_medium <- function(x){(nmass_emtrend_medium [1,2] * x + nmass_intercept_medium[1, 2])}
nmass_func_low <- function(x){(nmass_emtrend_low [1, 2] * x + nmass_intercept_low[1, 2])}

#pooled slope across all temperatures
nmass_emtrend_all <- summary(
  emtrends(nmass_lmer, ~1, var = "Nfert"))

#pooled intercerpt at Nfert = 0
nmass_intercept_all <- summary(
  emmeans(nmass_lmer, ~1, at = list(Nfert = 0))
)
##single pooled function
nmass_func_all <- function(x)
{nmass_emtrend_all[1,2] * x + nmass_intercept_all[1,2]}
#Plotting

##theme
theme.custom<- theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=15),
        panel.border = element_rect(linewidth=2))

nmass_plot <- ggplot(aes(y= nmass, x= Nfert, color = airtemp_factor), data = clean_final_data)+
  geom_point(alpha = 0.6, size = 3)+
  scale_color_manual(values = c('High' = 'red', 'Medium' = 'orange', 'Low' =  'blue'))+
  scale_shape_manual(values = c('High' = 16,
                                'Medium' = 17,
                                'Low' = 15))+
  stat_function(fun = nmass_func_all, color = "black", lwd = 2, 
                inherit.aes = FALSE)+
  stat_function(fun = nmass_func_high, color = 'red', lwd = 2, lty = 1, alpha = 0.4, inherit.aes = FALSE)+
  stat_function(fun = nmass_func_medium, color = 'orange', lwd = 2, lty = 1, alpha =0.4, inherit.aes = FALSE)+
  stat_function(fun = nmass_func_low, color = 'blue', lwd = 2, lty = 1, alpha = 0.4, inherit.aes = FALSE)+
  labs(color = 'Growth Air Temp.', shape = 'Growth Air Temp.') +
  xlab('Soil nitrogen supply (ppm)') +
  ylab(expression('N'[mass] * ' (g g' ^ '-1' * ')'))+theme.custom

##########################################################
nmass_plot
#####################
 #Narea
 hist(clean_final_data$Narea) #pretty okay
 plot(Narea ~ Nfert, data = clean_final_data)

 narea_lmer <- lmer(Narea ~ Nfert * airtemp_factor +
                      (1|Rack:airtemp_factor), data = clean_final_data)
 plot(resid(narea_lmer) ~ fitted(narea_lmer)) #mostly okay
 summary(narea_lmer)
 Anova(narea_lmer)

 ##test trends (all significant)
 test(emtrends(narea_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'High')))
 test(emtrends(narea_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'Medium')))
 test(emtrends(narea_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'Low')))
 
 ##extract trends
 narea_emtrend_high <- summary(emtrends(narea_lmer, ~1,
                                        var = 'Nfert', 
                                        at = list(airtemp_factor = 'High')))
 
 narea_emtrend_medium <- summary(emtrends(narea_lmer, ~1,
                                          var = 'Nfert',
                                          at = list(airtemp_factor = 'Medium')))
 narea_emtrend_low <- summary(emtrends(narea_lmer, ~1,
                                       var = 'Nfert',
                                       at = list(airtemp_factor = 'Low')))
 narea_intercept_high <- summary(emmeans(narea_lmer, ~1, 
                                         at = list(Nfert = 0, airtemp_factor = 'High')))
 narea_intercept_medium <- summary(emmeans(narea_lmer, ~1,
                                           at = list(Nfert = 0, airtemp_factor = 'Medium')))
 narea_intercept_low <- summary(emmeans(narea_lmer, ~1,
                                        at = list(Nfert = 0, airtemp_factor = 'Low')))
 
 
 ##creating functions
 narea_func_high <- function(x){
   (narea_emtrend_high[1,2] * x + narea_intercept_high[1,2])}
 
 narea_func_medium <- function(x){
   (narea_emtrend_medium[1,2] * x + narea_intercept_medium[1,2])}
 
 narea_func_low <- function(x){
   (narea_emtrend_low[1,2] * x + narea_intercept_low[1,2])}


 #pooled slope across all temperatures
 narea_emtrend_all <- summary(
   emtrends(narea_lmer, ~1, var = "Nfert"))
 
 #pooled intercerpt at Nfert = 0
narea_intercept_all <- summary(
   emmeans(narea_lmer, ~1, at = list(Nfert = 0))
 )
 ##single pooled function
 narea_func_all <- function(x)
 {narea_emtrend_all[1,2] * x + narea_intercept_all[1,2]}
 
 #ggplot 
 narea_plot <-ggplot(aes(y = Narea, x = Nfert, color = airtemp_factor),
                     data = clean_final_data)+
   geom_point(alpha = 0.6, size = 3)+
   scale_color_manual(values = c('High' = 'red', 'Medium' = 'orange', 'Low' =  'blue'))+
   scale_shape_manual(values = c('High' = 16,
                                 'Medium' = 17,
                                 'Low' = 15))+
   stat_function(fun = narea_func_all, xlim = c(0, 630),color = "black", lwd = 2, 
                 inherit.aes = FALSE)+
   stat_function(fun = narea_func_high, xlim = c(0, 630),color = 'red', lwd = 2, lty = 1, alpha = 0.3, inherit.aes = FALSE)+
   stat_function(fun = narea_func_medium, xlim = c(0, 630),color = 'orange', lwd = 2, lty = 1, alpha = 0.3, inherit.aes = FALSE)+
   stat_function(fun = narea_func_low, xlim = c(0, 630),color = 'blue', lwd = 2, lty = 1, alpha = 0.3, inherit.aes = FALSE)+
   scale_x_continuous(limits = c(0, 700),breaks = seq(0, 700, by = 100),
                      expand = expansion(add = c(20, 10))
   )+
   scale_y_continuous(
     limits = c(0, 12),breaks = seq(0, 12, by = 2),
     expand = expansion(add = c(0, 0))
   )+
   labs(color = 'Growth Air Temp.', shape = 'Growth Air Temp.') +
   xlab('Soil nitrogen supply (ppm)') +
   ylab(expression(N[area] ~ (g~m^{-2})))+ theme.custom 
 
 narea_plot
 
 
 ##Marea----------------------------------------------
 ##explore data
 hist(clean_final_data$Marea) #skewed
 hist(log(clean_final_data$Marea))# better
 plot(Marea~Nfert, data = clean_final_data) #clear plateau
 
 ##fit lme model
 Marea_lmer <- lmer(log(Marea) ~ Nfert * airtemp_factor + (1| Rack:airtemp_factor), 
                  data = clean_final_data)
 plot(resid(Marea_lmer)~fitted(Marea_lmer))#better after log transformation
 summary(Marea_lmer)
 Anova(Marea_lmer)

 #test trends (all significant)
 test(emtrends(Marea_lmer, ~1, var = 'Nfert', at = list(airtemp_factor='High')))
 test(emtrends(Marea_lmer, ~1, var = 'Nfert', at = list(airtemp_factor='Medium')))
 test(emtrends(Marea_lmer, ~1, var = 'Nfert', at = list(airtemp_factor='Low')))
 
 ##extract trends
 Marea_emtrend_high <- summary(emtrends(Marea_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'High')))
 Marea_emtrend_medium <- summary(emtrends(Marea_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'Medium')))
 Marea_emtrend_low <- summary(emtrends(Marea_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'Low')))
 Marea_intercept_high <- summary(emmeans(Marea_lmer, ~1, at = list(Nfert = 0, airtemp_factor = 'High')))
 Marea_intercept_medium <- summary(emmeans(Marea_lmer, ~1, at = list(Nfert = 0, airtemp_factor = 'Medium')))
 Marea_intercept_low <- summary(emmeans(Marea_lmer, ~1, at = list(Nfert = 0, airtemp_factor = 'Low')))
 
 ##creating functions
 Marea_func_high <- function(x){ exp(Marea_emtrend_high[1,2] * x + Marea_intercept_high[1,2])}
 Marea_func_medium <- function(x){exp(Marea_emtrend_medium[1,2] * x + Marea_intercept_medium[1,2])}
 Marea_func_low <- function(x){exp(Marea_emtrend_low[1,2] * x + Marea_intercept_low[1,2])}
 
 #pooled slope across all temperatures
 Marea_emtrend_all <- summary(
   emtrends(Marea_lmer, ~1, var = "Nfert"))
 
 #pooled intercerpt at Nfert = 0
 Marea_intercept_all <- summary(
   emmeans(Marea_lmer, ~1, at = list(Nfert = 0))
 )
 ##single pooled function
 Marea_func_all <- function(x){
   exp(Marea_emtrend_all[1,2] * x + Marea_intercept_all[1,2])
 }
 ##plot
 Marea_plot <-ggplot(aes(y = Marea, x = Nfert, color = airtemp_factor), data = clean_final_data)+
   geom_point(alpha = 0.6, size = 3)+
   scale_color_manual(values = c('High' = 'red', 'Medium' = 'orange', 'Low' =  'blue'))+
   scale_shape_manual(values = c('High' = 16,
                                 'Medium' = 17,
                                 'Low' = 15))+
   stat_function(fun = Marea_func_all, xlim = c(0, 630),color = "black", lwd = 2, 
                 inherit.aes = FALSE)+
   stat_function(fun = Marea_func_high, xlim = c(0, 630),color = 'red', lwd = 2, lty = 2, alpha =0.3, inherit.aes = FALSE)+
   stat_function(fun = Marea_func_medium, xlim = c(0, 630),color = 'orange', lwd = 2, lty = 1, alpha =0.3, inherit.aes = FALSE)+
   stat_function(fun = Marea_func_low, xlim = c(0, 630),color = 'blue', lwd = 2, lty = 1, alpha = 0.3, inherit.aes = FALSE)+
   scale_x_continuous(limits = c(0, 700),breaks = seq(0, 700, by = 100),
                      expand = expansion(add = c(20, 10))
   )+
   scale_y_continuous(
     limits = c(0, 100),breaks = seq(0, 100, by = 20),
     expand = expansion(add = c(0, 0))
   )+
   labs(color = 'Growth Air Temp.', shape = 'Growth Air Temp.') +
   xlab('Soil nitrogen supply (ppm)') +
   ylab(expression(M[area] ~ (g~m^{-2})))+theme.custom
 
 Marea_plot
 
 #before merging apply common theme
 common_theme <- theme(
   legend.position = "right",
   plot.margin = margin(5, 5, 5, 5)
 )
 
 Marea_plot <- Marea_plot + common_theme
 narea_plot <- narea_plot + common_theme
 nmass_plot <- nmass_plot + common_theme
 
 #merge plots-------------------------------------
 library(patchwork)
 n_allocation <- 
   Marea_plot + narea_plot + nmass_plot +
   plot_layout(guides = "collect", ncol = 2) &
   theme(legend.position = "bottom")
 
 n_allocation +
   plot_annotation(tag_levels = "A")
 
 ggsave("n_allocation.tiff",
        width = 6.7,
        height = 5.9,
        dpi = 600, compression = "lzw")
 
 
#vcmax25 ------------------------------------------------
 hist(clean_final_data$Vcmax25)
 hist(log(clean_final_data$Vcmax25))# better
 plot(Vcmax25~Nfert, data = clean_final_data) #clear plateau
 
 Vcmax25_lmer <- lmer(Vcmax25 ~ Nfert * airtemp_factor +
                      (1|Rack:airtemp_factor), data = clean_final_data)
 plot(resid(Vcmax25_lmer) ~ fitted(Vcmax25_lmer)) #mostly okay
 summary(Vcmax25_lmer)
 Anova(Vcmax25_lmer)

 ##test trends (all significant)
 test(emtrends(Vcmax25_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'High')))
 test(emtrends(Vcmax25_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'Medium')))
 test(emtrends(Vcmax25_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'Low')))
 
 ##extract trends
 Vcmax25_emtrend_high <- summary(emtrends(Vcmax25_lmer, ~1,
                                        var = 'Nfert', 
                                        at = list(airtemp_factor = 'High')))
 
 Vcmax25_emtrend_medium <- summary(emtrends(Vcmax25_lmer, ~1,
                                          var = 'Nfert',
                                          at = list(airtemp_factor = 'Medium')))
 Vcmax25_emtrend_low <- summary(emtrends(Vcmax25_lmer, ~1,
                                       var = 'Nfert',
                                       at = list(airtemp_factor = 'Low')))
 Vcmax25_intercept_high <- summary(emmeans(Vcmax25_lmer, ~1, 
                                         at = list(Nfert = 0, airtemp_factor = 'High')))
 Vcmax25_intercept_medium <- summary(emmeans(Vcmax25_lmer, ~1,
                                           at = list(Nfert = 0, airtemp_factor = 'Medium')))
 Vcmax25_intercept_low <- summary(emmeans(Vcmax25_lmer, ~1,
                                        at = list(Nfert = 0, airtemp_factor = 'Low')))
 
 ##creating functions
 Vcmax25_func_high <- function(x){
   (Vcmax25_emtrend_high[1,2] * x + Vcmax25_intercept_high[1,2])}
 
 Vcmax25_func_medium <- function(x){
   (Vcmax25_emtrend_medium[1,2] * x + Vcmax25_intercept_medium[1,2])}
 
 Vcmax25_func_low <- function(x){
   (Vcmax25_emtrend_low[1,2] * x + Vcmax25_intercept_low[1,2])}
 
 #pooled slope across all temperatures
 vc25_emtrend_all <- summary(
   emtrends(Vcmax25_lmer, ~1, var = "Nfert"))
 
 #pooled intercerpt at Nfert = 0
 vc25_intercept_all <- summary(
   emmeans(Vcmax25_lmer, ~1, at = list(Nfert = 0))
 )
 ##single pooled function
 vc25_func_all <- function(x)
 {vc25_emtrend_all[1,2] * x + vc25_intercept_all[1,2]}
 
 ##plot
 Vcmax25_plot <-ggplot(aes(y = Vcmax25, x = Nfert, color = airtemp_factor), data = clean_final_data)+
   geom_point(alpha = 0.6, size = 3)+
   scale_color_manual(values = c('High' = 'red', 'Medium' = 'orange', 'Low' =  'blue'))+
   scale_shape_manual(values = c('High' = 16,
                                 'Medium' = 17,
                                 'Low' = 15))+
   stat_function(fun = vc25_func_all, xlim = c(0, 630),color = "black", lwd = 2,lty = 2, 
                 inherit.aes = FALSE)+
   stat_function(fun = Vcmax25_func_high, xlim = c(0, 630),color = 'red', lwd = 2, lty = 2, alpha =0.3, inherit.aes = FALSE)+
   stat_function(fun = Vcmax25_func_medium, xlim = c(0, 630),color = 'orange', lwd = 2, lty = 2, alpha =0.3, inherit.aes = FALSE)+
   stat_function(fun = Vcmax25_func_low, xlim = c(0, 630),color = 'blue', lwd = 2, lty = 2, alpha = 0.3, inherit.aes = FALSE)+
   scale_x_continuous(limits = c(0, 700),breaks = seq(0, 700, by = 100),
                      expand = expansion(add = c(20, 10))
   )+
   scale_y_continuous(
     limits = c(0, 350),breaks = seq(0, 350, by = 50),
     expand = expansion(add = c(0, 0))
   )+
   labs(color = 'Growth Air Temp.') +
   xlab('Soil nitrogen supply (ppm)') +
   ylab(expression('V'[cmax25] * ' (µmol m' ^ '-2 ' * 's' ^ '-1' * ')'))+theme.custom
 
 Vcmax25_plot
 
  
 #jmax25 ----------------------------------------------
 hist(clean_final_data$Jmax25)
 hist(log(clean_final_data$Jmax25))# better
 plot(Jmax25~Nfert, data = clean_final_data) #clear plateau
 
 Jmax25_lmer <- lmer(Jmax25 ~ Nfert * airtemp_factor +
                        (1|Rack:airtemp_factor), data = clean_final_data)
 plot(resid(Jmax25_lmer) ~ fitted(Jmax25_lmer)) #mostly okay
 summary(Jmax25_lmer)
 Anova(Jmax25_lmer)
 
 ##test trends (all significant)
 test(emtrends(Jmax25_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'High')))
 test(emtrends(Jmax25_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'Medium')))
 test(emtrends(Jmax25_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'Low'))) 
 
 ##extract trends
 Jmax25_emtrend_high <- summary(emtrends(Jmax25_lmer, ~1,
                                          var = 'Nfert', 
                                          at = list(airtemp_factor = 'High')))
 Jmax25_emtrend_medium <- summary(emtrends(Jmax25_lmer, ~1,
                                            var = 'Nfert',
                                            at = list(airtemp_factor = 'Medium')))
 Jmax25_emtrend_low <- summary(emtrends(Jmax25_lmer, ~1,
                                         var = 'Nfert',
                                         at = list(airtemp_factor = 'Low')))
 Jmax25_intercept_high <- summary(emmeans(Jmax25_lmer, ~1, 
                                           at = list(Nfert = 0, airtemp_factor = 'High')))
 Jmax25_intercept_medium <- summary(emmeans(Jmax25_lmer, ~1,
                                             at = list(Nfert = 0, airtemp_factor = 'Medium')))
 Jmax25_intercept_low <- summary(emmeans(Jmax25_lmer, ~1,
                                          at = list(Nfert = 0, airtemp_factor = 'Low')))
 ##creating functions
 Jmax25_func_high <- function(x){
   (Jmax25_emtrend_high[1,2] * x + Jmax25_intercept_high[1,2])}
 
 Jmax25_func_medium <- function(x){
   (Jmax25_emtrend_medium[1,2] * x + Jmax25_intercept_medium[1,2])}
 
 Jmax25_func_low <- function(x){
   (Jmax25_emtrend_low[1,2] * x + Jmax25_intercept_low[1,2])}
 
 #pooled slope across all temperatures
 jm25_emtrend_all <- summary(
   emtrends(Jmax25_lmer, ~1, var = "Nfert"))
 
 #pooled intercerpt at Nfert = 0
 jm25_intercept_all <- summary(
   emmeans(Jmax25_lmer, ~1, at = list(Nfert = 0))
 )
 ##single pooled function
 jm25_func_all <- function(x)
 {jm25_emtrend_all[1,2] * x + jm25_intercept_all[1,2]}
 
 ##plot
 Jmax25_plot <-ggplot(aes(y = Jmax25, x = Nfert, color = airtemp_factor), data = clean_final_data)+
   geom_point(alpha = 0.6, size = 3)+
   scale_color_manual(values = c('High' = 'red', 'Medium' = 'orange', 'Low' =  'blue'))+
   scale_shape_manual(values = c('High' = 16,
                                 'Medium' = 17,
                                 'Low' = 15))+
   stat_function(fun = jm25_func_all, xlim = c(0, 630),color = "black", lwd = 2,lty = 2, 
                 inherit.aes = FALSE)+
   stat_function(fun = Jmax25_func_high, xlim = c(0, 630),color = 'red', lwd = 2, lty = 2, alpha =0.3, inherit.aes = FALSE)+
   stat_function(fun = Jmax25_func_medium, xlim = c(0, 630),color = 'orange', lwd = 2, lty = 2, alpha =0.3, inherit.aes = FALSE)+
   stat_function(fun = Jmax25_func_low, xlim = c(0, 630),color = 'blue', lwd = 2, lty = 2, alpha = 0.3, inherit.aes = FALSE)+
   scale_x_continuous(limits = c(0, 700),breaks = seq(0, 700, by = 100),
                      expand = expansion(add = c(20, 10))
   )+
   scale_y_continuous(
     limits = c(0, 450),breaks = seq(0, 450, by = 50),
     expand = expansion(add = c(0, 0))
   )+
   labs(color = 'Growth Air Temp.') +
   xlab('Soil nitrogen supply (ppm)') +
   ylab(expression('J'[max25] * ' (µmol m' ^ '-2 ' * 's' ^ '-1' * ')'))+theme.custom
 
 Jmax25_plot

 ##chl_area chl_mmol.m2 ------------------------------------------
 hist(clean_final_data$chl_mmol.m2)
 hist(log(clean_final_data$chl_mmol.m2))# better
 plot(chl_mmol.m2~Nfert, data = clean_final_data) #clear plateau
 
 chl_lmer <- lmer(chl_mmol.m2 ~ Nfert * airtemp_factor +
                       (1|Rack:airtemp_factor), data = clean_final_data)
 plot(resid(chl_lmer ) ~ fitted(chl_lmer )) #mostly okay
 summary(chl_lmer)
 Anova(chl_lmer)

 ##test trends (all significant)
 test(emtrends(chl_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'High')))
 test(emtrends(chl_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'Medium')))
 test(emtrends(chl_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'Low'))) 
 
 ##extract trends
 chl_emtrend_high <- summary(emtrends(chl_lmer, ~1,
                                         var = 'Nfert', 
                                         at = list(airtemp_factor = 'High')))
 chl_emtrend_medium <- summary(emtrends(chl_lmer, ~1,
                                           var = 'Nfert',
                                           at = list(airtemp_factor = 'Medium')))
 chl_emtrend_low <- summary(emtrends(chl_lmer, ~1,
                                        var = 'Nfert',
                                        at = list(airtemp_factor = 'Low')))
 chl_intercept_high <- summary(emmeans(chl_lmer, ~1, 
                                          at = list(Nfert = 0, airtemp_factor = 'High')))
 chl_intercept_medium <- summary(emmeans(chl_lmer, ~1,
                                            at = list(Nfert = 0, airtemp_factor = 'Medium')))
 chl_intercept_low <- summary(emmeans(chl_lmer, ~1,
                                         at = list(Nfert = 0, airtemp_factor = 'Low'))) 

 ##creating functions
 chl_func_high <- function(x){
   (chl_emtrend_high[1,2] * x + chl_intercept_high[1,2])}
 
 chl_func_medium <- function(x){
   (chl_emtrend_medium[1,2] * x + chl_intercept_medium[1,2])}
 
 chl_func_low <- function(x){
   (chl_emtrend_low[1,2] * x + chl_intercept_low[1,2])}
 
 #pooled slope across all temperatures
 chl_emtrend_all <- summary(
   emtrends(chl_lmer, ~1, var = "Nfert"))
 
 #pooled intercerpt at Nfert = 0
 chl_intercept_all <- summary(
   emmeans(chl_lmer, ~1, at = list(Nfert = 0))
 )
 ##single pooled function
 chl_func_all <- function(x)
 {chl_emtrend_all[1,2] * x + chl_intercept_all[1,2]}
 ##chl area plot
 chl_plot <-ggplot(
   clean_final_data %>% filter(chl_mmol.m2 >= 0.12),
   aes(y = chl_mmol.m2, x = Nfert, color = airtemp_factor)
 ) +
   geom_point(alpha = 0.6, size = 3)+
   scale_color_manual(values = c('High' = 'red', 'Medium' = 'orange', 'Low' =  'blue'))+
   scale_shape_manual(values = c('High' = 16,
                                 'Medium' = 17,
                                 'Low' = 15))+
   stat_function(fun = chl_func_all, xlim = c(0, 630),color = "black", lwd = 2,lty = 1, 
                 inherit.aes = FALSE)+
   stat_function(fun = chl_func_high, xlim = c(0, 630),color = 'red', lwd = 2, lty = 2, alpha =0.3, inherit.aes = FALSE)+
   stat_function(fun = chl_func_medium, xlim = c(0, 630),color = 'orange', lwd = 2, lty = 1, alpha =0.3, inherit.aes = FALSE)+
   stat_function(fun = chl_func_low, xlim = c(0, 630),color = 'blue', lwd = 2, lty = 1, alpha = 0.3, inherit.aes = FALSE)+
   scale_x_continuous(limits = c(0, 700),breaks = seq(0, 700, by = 100),
                      expand = expansion(add = c(20, 10))
   )+
   labs(color = 'Growth Air Temp.') +
   xlab('Soil nitrogen supply (ppm)') +
   ylab(expression(Chl[area] ~ (mmol~m^{-2}))) +
   theme.custom
 
 chl_plot
 

 #merge plots-------------------------------------
 library(patchwork)
 photosynthetic_components_figure <- 
   Vcmax25_plot + Jmax25_plot + chl_plot+jv_plot_new +
   plot_layout(guides = "collect", ncol = 2) &
   theme(legend.position = "bottom")
 
 photosynthetic_components_figure +
   plot_annotation(tag_levels = "A")
 
 ##export 
 ggsave(" photosynthetic_components_figure.tiff",
        width = 6.7,
        height = 5.9,
        dpi = 600, compression = "lzw")
 
 #p_rubisco -------------------------------------------------------
 hist(clean_final_data$propN_rubisco)
 hist(log(clean_final_data$propN_rubisco))# better
 plot(propN_rubisco~Nfert, data = clean_final_data) #clear plateau
 
 rubisco_lmer <- lmer(propN_rubisco~ Nfert * airtemp_factor +
                    (1|Rack:airtemp_factor), data = clean_final_data)
 plot(resid(rubisco_lmer) ~ fitted(rubisco_lmer)) #mostly okay
 summary(rubisco_lmer)
 Anova(rubisco_lmer)
 rubisco_anova_table <- Anova(rubisco_lmer)
 
 #test trends (all significant)
 test(emtrends(rubisco_lmer, ~1, var = 'Nfert', at = list(airtemp_factor='High')))
 test(emtrends(rubisco_lmer, ~1, var = 'Nfert', at = list(airtemp_factor='Medium')))
 test(emtrends(rubisco_lmer, ~1, var = 'Nfert', at = list(airtemp_factor='Low')))
 
 ##extract trends
 rub_emtrend_high <- summary(emtrends(rubisco_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'High')))
 rub_emtrend_medium <- summary(emtrends(rubisco_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'Medium')))
 rub_emtrend_low <- summary(emtrends(rubisco_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'Low')))
 rub_intercept_high <- summary(emmeans(rubisco_lmer, ~1, at = list(Nfert = 0, airtemp_factor = 'High')))
 rub_intercept_medium <- summary(emmeans(rubisco_lmer, ~1, at = list(Nfert = 0, airtemp_factor = 'Medium')))
 rub_intercept_low <- summary(emmeans(rubisco_lmer, ~1, at = list(Nfert = 0, airtemp_factor = 'Low')))
 
 ##creating functions
 ##creating functions
 rub_func_high <- function(x){
   (rub_emtrend_high[1,2] * x + rub_intercept_high[1,2])}
 
 rub_func_medium <- function(x){
   (rub_emtrend_medium[1,2] * x + rub_intercept_medium[1,2])}
 
 rub_func_low <- function(x){
   (rub_emtrend_low[1,2] * x + rub_intercept_low[1,2])}
 
 ##test trends (all significant)
 #pooled slope across all temperatures
 rubisco_emtrend_all <- summary(
   emtrends(rubisco_lmer, ~1, var = "Nfert"))
 
 #pooled intercerpt at Nfert = 0
 rubisco_intercept_all <- summary(
   emmeans(rubisco_lmer, ~1, at = list(Nfert = 0))
 )
 ##single pooled function
 rubisco_func_all <- function(x)
 {rubisco_emtrend_all[1,2] * x + rubisco_intercept_all[1,2]}
 
 
 ##p_rubisco plot
 rubisco_plot<-ggplot(
   clean_final_data %>% filter(propN_rubisco < 0.7),
   aes(y = propN_rubisco,
       x = Nfert,
       color = airtemp_factor)
 )  +
   geom_point(alpha = 0.6, size = 3)+
   scale_color_manual(values = c('High' = 'red', 'Medium' = 'orange', 'Low' =  'blue'))+
   scale_shape_manual(values = c('High' = 16,
                                 'Medium' = 17,
                                 'Low' = 15))+
   stat_function(fun = rubisco_func_all, xlim = c(0, 630),color = "black", lwd = 2, 
                 inherit.aes = FALSE)+
   stat_function(fun = rub_func_high, xlim = c(0, 630),color = 'red', lwd = 2, lty = 1, alpha =0.3, inherit.aes = FALSE)+
   stat_function(fun = rub_func_medium, xlim = c(0, 630),color = 'orange', lwd = 2, lty = 1, alpha =0.3, inherit.aes = FALSE)+
   stat_function(fun = rub_func_low, xlim = c(0, 630),color = 'blue', lwd = 2, lty = 2, alpha = 0.3, inherit.aes = FALSE)+
   scale_x_continuous(limits = c(0, 700),breaks = seq(0, 700, by = 100),
                      expand = expansion(add = c(20, 10))
   )+
   scale_y_continuous(
     limits = c(0, 0.8),breaks = seq(0, 0.8, by = 0.2),
     expand = expansion(add = c(0, 0))
   )+
   labs(color = 'Growth Air Temp.') +
   xlab('Soil nitrogen supply (ppm)') +
   ylab(expression(rho[rubisco] ~ (g*N ~ g*N^{-1}))) +
   theme.custom
 
 rubisco_plot
 
#pbioenergetics ------------------------------------------------------

 hist(clean_final_data$propN_bioenergetics)
 hist(log(clean_final_data$propN_bioenergetics))# better
 plot(propN_bioenergetics~Nfert, data = clean_final_data) #clear plateau 
 
 
 bioenergetics_lmer <- lmer(propN_bioenergetics~ Nfert * airtemp_factor +
                        (1|Rack:airtemp_factor), data = clean_final_data)
 plot(resid(bioenergetics_lmer) ~ fitted(bioenergetics_lmer)) #mostly okay
 summary(bioenergetics_lmer)
 Anova(bioenergetics_lmer)
 bio_anova_table     <- Anova(bioenergetics_lmer)
 
 #test trends (all significant)
 test(emtrends(bioenergetics_lmer, ~1, var = 'Nfert', at = list(airtemp_factor='High')))
 test(emtrends(bioenergetics_lmer, ~1, var = 'Nfert', at = list(airtemp_factor='Medium')))
 test(emtrends(bioenergetics_lmer, ~1, var = 'Nfert', at = list(airtemp_factor='Low')))
 
 ##extract trends
 bio_emtrend_high <- summary(emtrends(bioenergetics_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'High')))
 bio_emtrend_medium <- summary(emtrends(bioenergetics_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'Medium')))
 bio_emtrend_low <- summary(emtrends(bioenergetics_lmer, ~1, var = 'Nfert', at = list(airtemp_factor = 'Low')))
 bio_intercept_high <- summary(emmeans(bioenergetics_lmer, ~1, at = list(Nfert = 0, airtemp_factor = 'High')))
 bio_intercept_medium <- summary(emmeans(bioenergetics_lmer, ~1, at = list(Nfert = 0, airtemp_factor = 'Medium')))
 bio_intercept_low <- summary(emmeans(bioenergetics_lmer, ~1, at = list(Nfert = 0, airtemp_factor = 'Low')))
 
 ##creating functions
 ##creating functions
 bio_func_high <- function(x){
   (bio_emtrend_high[1,2] * x + bio_intercept_high[1,2])}
 
 bio_func_medium <- function(x){
   (bio_emtrend_medium[1,2] * x + bio_intercept_medium[1,2])}
 
 bio_func_low <- function(x){
   (bio_emtrend_low[1,2] * x + bio_intercept_low[1,2])}
 ##test trends (all significant)
 #pooled slope across all temperatures
 bioenergetics_emtrend_all <- summary(
   emtrends(bioenergetics_lmer, ~1, var = "Nfert"))
 
 #pooled intercerpt at Nfert = 0
 bioenergetics_intercept_all <- summary(
   emmeans(bioenergetics_lmer, ~1, at = list(Nfert = 0))
 )
 ##single pooled function
 bioenergetics_func_all <- function(x)
 {bioenergetics_emtrend_all[1,2] * x + bioenergetics_intercept_all[1,2]}
 
 ##p_bioenergetics plot
 bioenergetics_plot<-ggplot(
   clean_final_data %>% filter(propN_bioenergetics < 0.2),
   aes(y = propN_bioenergetics,
       x = Nfert,
       color = airtemp_factor)
 )+
   geom_point(alpha = 0.6, size = 3)+
   scale_color_manual(values = c('High' = 'red', 'Medium' = 'orange', 'Low' =  'blue'))+
   scale_shape_manual(values = c('High' = 16,
                                 'Medium' = 17,
                                 'Low' = 15))+
   stat_function(fun = bioenergetics_func_all, xlim = c(0, 630),color = "black", lwd = 2,lty = 1, 
                 inherit.aes = FALSE)+
   stat_function(fun = bio_func_high, xlim = c(0, 630),color = 'red', lwd = 2, lty = 1, alpha = 0.4, inherit.aes = FALSE)+
   stat_function(fun = bio_func_medium, xlim = c(0, 630),color = 'orange', lwd = 2, lty = 1, alpha =0.4, inherit.aes = FALSE)+
   stat_function(fun = bio_func_low, xlim = c(0, 630),color = 'blue', lwd = 2, lty = 1, alpha = 0.4, inherit.aes = FALSE)+
   scale_x_continuous(limits = c(0, 700),breaks = seq(0, 700, by = 100),
                      expand = expansion(add = c(20, 10))
   )+
   scale_y_continuous(
     limits = c(0, 0.125),breaks = seq(0, 0.125, by = 0.02),
     expand = expansion(add = c(0, 0))
   )+
   labs(color = 'Growth Air Temp.') +
   xlab('Soil nitrogen supply (ppm)') +
   ylab(expression(rho[bioenergetics] ~ (g*N ~ g*N^{-1}))) +
   theme.custom
 
 bioenergetics_plot

 #propN_lightharvesting --------------------------------------------------
 hist(clean_final_data$rho_lh)
 hist(log(clean_final_data$rho_lh))# better
 plot(rho_lh~Nfert, data = clean_final_data) #clear plateau 

 rho_lh_lmer <- lmer(rho_lh~ Nfert * airtemp_factor +
                              (1|Rack:airtemp_factor), data = clean_final_data)
 plot(resid(rho_lh_lmer) ~ fitted(rho_lh_lmer)) #mostly okay
 summary(rho_lh_lmer)
 Anova(rho_lh_lmer)
 lh_anova_table <- Anova(rho_lh_lmer)
 ##test trends (all significant)
 test(emtrends(rho_lh_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'High')))
 test(emtrends(rho_lh_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'Medium')))
 test(emtrends(rho_lh_lmer, ~ 1, var = 'Nfert', at = list(airtemp_factor = 'Low')))  

  
 ##extract trends
 rho_lh_emtrend_high <- summary(emtrends(rho_lh_lmer, ~1,
                                                var = 'Nfert', 
                                                at = list(airtemp_factor = 'High')))
 rho_lh_emtrend_medium <- summary(emtrends(rho_lh_lmer, ~1,
                                                  var = 'Nfert',
                                                  at = list(airtemp_factor = 'Medium')))
 rho_lh_emtrend_low <- summary(emtrends(rho_lh_lmer, ~1,
                                               var = 'Nfert',
                                               at = list(airtemp_factor = 'Low')))
 rho_lh_intercept_high <- summary(emmeans(rho_lh_lmer, ~1, 
                                                 at = list(Nfert = 0, airtemp_factor = 'High')))
 rho_lh_intercept_medium <- summary(emmeans(rho_lh_lmer, ~1,
                                                   at = list(Nfert = 0, airtemp_factor = 'Medium')))
 rho_lh_intercept_low <- summary(emmeans(rho_lh_lmer, ~1,
                                                at = list(Nfert = 0, airtemp_factor = 'Low'))) 
 ##creating functions
 rho_lh_func_high <- function(x){
   (rho_lh_emtrend_high[1,2] * x + rho_lh_intercept_high[1,2])}
 
 rho_lh_func_medium <- function(x){
   (rho_lh_emtrend_medium[1,2] * x + rho_lh_intercept_medium[1,2])}
 
 rho_lh_func_low <- function(x){
   (rho_lh_emtrend_low[1,2] * x + rho_lh_intercept_low[1,2])}
 
 #pooled slope across all temperatures
 lh_emtrend_all <- summary(
   emtrends(rho_lh_lmer, ~1, var = "Nfert"))
 
 #pooled intercerpt at Nfert = 0
 lh_intercept_all <- summary(
   emmeans(rho_lh_lmer, ~1, at = list(Nfert = 0))
 )
 ##single pooled function
 lh_func_all <- function(x)
 {lh_emtrend_all[1,2] * x + lh_intercept_all[1,2]}
 
 
 ##p_lightharvesting plot
 lightharvesting_plot<-ggplot(aes(y = rho_lh, x = Nfert, 
                      color = airtemp_factor), 
                      data = clean_final_data)+
   geom_point(alpha = 0.6, size = 3)+
   scale_color_manual(values = c('High' = 'red', 'Medium' = 'orange', 'Low' =  'blue'))+
   scale_shape_manual(values = c('High' = 16,
                                 'Medium' = 17,
                                 'Low' = 15))+
   stat_function(fun = lh_func_all, xlim = c(0, 630),color = "black", lwd = 2,lty = 1, 
                 inherit.aes = FALSE)+
   stat_function(fun = rho_lh_func_high, xlim = c(0, 630),color = 'red', lwd = 2, lty = 1, alpha =0.3, inherit.aes = FALSE)+
   stat_function(fun = rho_lh_func_medium, xlim = c(0, 630),color = 'orange', lwd = 2, lty = 1, alpha =0.3, inherit.aes = FALSE)+
   stat_function(fun = rho_lh_func_low, xlim = c(0, 630),color = 'blue', lwd = 2, lty = 2, alpha = 0.3, inherit.aes = FALSE)+
   scale_x_continuous(limits = c(0, 700),breaks = seq(0, 700, by = 100),
                      expand = expansion(add = c(20, 10))
   )+
   scale_y_continuous(
     limits = c(0, 0.08),breaks = seq(0, 0.08, by = 0.02),
     expand = expansion(add = c(0, 0))
   )+
   labs(color = 'Growth Air Temp.') +
   xlab('Soil nitrogen supply (ppm)') +
   ylab(expression(rho[lightharvesting] ~ (g*N ~ g*N^{-1}))) +
   theme.custom
 lightharvesting_plot
 
 
 #merge plots

library(patchwork)
proportion_N_components <- 
  rubisco_plot + bioenergetics_plot + lightharvesting_plot+
  plot_layout(guides = "collect", ncol = 2) &
  theme(legend.position = "bottom")

proportion_N_components +
  plot_annotation(tag_levels = "A")
##export 
ggsave(" proportion_N_components.tiff",
       width = 6.7,
       height = 5.9,
       dpi = 600, compression = "lzw")
  
 
##Tables

 # Run ANOVA for photosynthetic components -----------------------
 rubisco_anova_table <- Anova(rubisco_lmer)
 bio_anova_table     <- Anova(bioenergetics_lmer)
 lh_anova_table      <- Anova(rho_lh_lmer)

 # Combine tables
 combined_anova <- cbind(rubisco_anova_table,
                         bio_anova_table,
                         lh_anova_table)
 #subset (df, + x2 + p-values)
 anova_table_sub <- cbind(
   combined_anova[,2],                    # df (from first table)
   combined_anova[,c(1,3,                # rubisco: χ2, p
                     4,6,                # bio: χ2, p
                     7,9)]               # lh: χ2, p
 )
 
 colnames(anova_table_sub) <- c( #rename columns
   'df',
   'χ2_rubisco', 'P_rubisco',
   'χ2_bio', 'P_bio',
   'χ2_lh', 'P_lh'
 )
 
 rownames(anova_table_sub) <- c( #fix row names
   'Nitrogen (N)',
   'Temperature (T)',
   'N × T'
 )
 
 is.num <- sapply(anova_table_sub, is.numeric) #round values
 
 anova_table_sub[is.num] <- lapply(
   anova_table_sub[is.num],
   round, 3
 )
 
 anova_table_sub[anova_table_sub < 0.001] <- '<0.001' #format low significant values
 

 write.csv(anova_table_sub,
           'n_allocation_anova_table.xlsx')
 
 # Run ANOVA for temperature response parameters
 # Extract ANOVA tables for Vcmax parameters (a,b,c)
 a_anova <- Anova(a_lmer)
 b_anova <- Anova(b_lmer)
 c_anova <- Anova(c_lmer)
 # Combine
 abc_anova <- cbind(a_anova, b_anova, c_anova)
 # Subset (df + χ2 + p-values)
 abc_anova_sub <- cbind(abc_anova[,2],                     # df
   abc_anova[,c(1,3,                  # a: χ2, p
                4,6,                  # b: χ2, p
                7,9)]                 # c: χ2, p
 )
 # Rename columns
 colnames(abc_anova_sub) <- c(
   'df',
   'χ2_a', 'P_a',
   'χ2_b', 'P_b',
   'χ2_c', 'P_c'
 )
 # Rename rows
 rownames(abc_anova_sub) <- c(
   'Nitrogen (N)',
   'Temperature (T)',
   'N × T'
 )
 # Round
 is.num <- sapply(abc_anova_sub, is.numeric)
 abc_anova_sub[is.num] <- lapply(abc_anova_sub[is.num], round, 3)
 # Format small p-values
 abc_anova_sub[abc_anova_sub < 0.001] <- '<0.001'
 # Export
 
 write.csv(abc_anova_sub,
           'vcmax_temp_response_anova_table.csv')
 
 # Extract ANOVA tables for Jcmax parameters (a,b,c)
 aj_anova <- Anova(aj_lmer)
 bj_anova <- Anova(bj_lmer)
 cj_anova <- Anova(cj_lmer)
 
 # Combine
 abcj_anova <- cbind(aj_anova, bj_anova, cj_anova)
 # Subset (df + χ2 + p-values)
 abcj_anova_sub <- cbind(abcj_anova[,2],                     # df
                        abcj_anova[,c(1,3,                  # a: χ2, p
                                     4,6,                  # b: χ2, p
                                     7,9)]                 # c: χ2, p
 )
 # Rename columns
 colnames(abcj_anova_sub) <- c(
   'df',
   'χ2_a', 'P_a',
   'χ2_b', 'P_b',
   'χ2_c', 'P_c'
 )
 # Rename rows
 rownames(abcj_anova_sub) <- c(
   'Nitrogen (N)',
   'Temperature (T)',
   'N × T'
 )
 # Round
 is.num <- sapply(abcj_anova_sub, is.numeric)
 abcj_anova_sub[is.num] <- lapply(abcj_anova_sub[is.num], round, 3)
 # Format small p-values
 abcj_anova_sub[abcj_anova_sub < 0.001] <- '<0.001'
 # Export
 ##merging the vcmax and jmax tables
 # Convert row names to a column
 vcmax_table <- data.frame(
   Response = rownames(abc_anova_sub),
   Parameter = "Vcmax",
   abc_anova_sub,
   row.names = NULL
 )
 
 jmax_table <- data.frame(
   Response = rownames(abcj_anova_sub),
   Parameter = "Jmax",
   abcj_anova_sub,
   row.names = NULL
 )
 
 # Combine tables
 temp_response_anova <- rbind(
   vcmax_table,
   jmax_table
 )
 
 temp_response_anova
 write.csv(
   temp_response_anova,
   "temp_response_anova.csv",
   row.names = FALSE
 )
 # Run ANOVA for leaf mass per area and leaf nitrogen
 marea_anova_table <- Anova(Marea_lmer)
 nmass_anova_table <- Anova(nmass_lmer)
 narea_anova_table <- Anova(narea_lmer)
 
 # Combine tables
 combined_anova <- cbind(marea_anova_table,nmass_anova_table,
   narea_anova_table)
 # Subset (df + χ2 + p-values)
 anova_table_sub <- cbind(combined_anova[,2],combined_anova[,c(1,3, 
           4,6,7,9)] )                                           
 # Rename columns
 colnames(anova_table_sub) <- c(
   'df','χ2_Marea', 'P_Marea','χ2_Nmass', 'P_Nmass','χ2_Narea', 'P_Narea')
 # Rename rows
 rownames(anova_table_sub) <- c('Nitrogen (N)','Temperature (T)','N × T')
 # Round values
 is.num <- sapply(anova_table_sub, is.numeric)
 anova_table_sub[is.num] <- lapply(anova_table_sub[is.num], round, 3)
 
 # Format small p-values
 anova_table_sub[anova_table_sub < 0.001] <- '<0.001'
 write.csv(anova_table_sub, "marea_nmass_narea_anova_table.csv")
 # Run ANOVA for functional photosynthesis traits
 vcmax_anova <- Anova(Vcmax25_lmer)
 jmax_anova  <- Anova(Jmax25_lmer)
 chl_anova   <- Anova(chl_lmer)
 jv_anova    <- Anova(jv_lmer)
 
 # Combine tables
 combined <- cbind(vcmax_anova, jmax_anova, chl_anova, jv_anova)
#extract key columns
 table_sub <- cbind(
   combined[,2],                      # df
   combined[,c(1,3,                  # Vcmax25
               4,6,                  # Jmax25
               7,9,                  # Chlarea
               10,12)])
#rename columns
 colnames(table_sub) <- c(
   'df',
   'χ2_Vcmax25', 'P_Vcmax25',
   'χ2_Jmax25',  'P_Jmax25',
   'χ2_Chlarea', 'P_Chlarea',
   'χ2_JV',      'P_JV'
 )
#rename rows
 rownames(table_sub) <- c(
   'Nitrogen (N)',
   'Temperature (T)',
   'N × T'
 ) 
#format p-values
 is.num <- sapply(table_sub, is.numeric)
 table_sub[is.num] <- lapply(table_sub[is.num], round, 3)
 
 table_sub[table_sub < 0.001] <- '<0.001'
 
#write
 write.csv(table_sub, "vcmax_jmax25_chl_jv_table.csv")
 
 #run anova for morphology
 height_anova <- Anova(plant_height_lmer)
 agb_anova    <- Anova(aboveground_lmer)
 bgb_anova    <- Anova(Belowground_lmer)
 rs_anova     <- Anova(root_shoot_lmer)
 
 #combined ttable
 combined <- cbind(height_anova, agb_anova, bgb_anova, rs_anova)
 #extract values
 table_sub <- cbind(
   combined[,2],                       # df
   combined[,c(1,3,                   # height
               4,6,                   # AGB
               7,9,                   # BGB
               10,12)]                # R:S
 )
 
 #rename columns
 colnames(table_sub) <- c(
   'df',
   'χ2_Height', 'P_Height',
   'χ2_AGB',    'P_AGB',
   'χ2_BGB',    'P_BGB',
   'χ2_RS',     'P_RS'
 )

 #rename rows
 rownames(table_sub) <- c(
   'Nitrogen (N)',
   'Temperature (T)',
   'N × T'
 )
#format
 is.num <- sapply(table_sub, is.numeric)
 table_sub[is.num] <- lapply(table_sub[is.num], round, 3)
 
 table_sub[table_sub < 0.001] <- '<0.001' 

 #export
 write.csv(table_sub, "growth_allocation_anova.csv")
 
 
 head(vcmax_25_tempresp_fits)
 