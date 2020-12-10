rm(list = ls(all.names = TRUE))


### Libraries
library(tidyverse)
library(ggpubr)
library(lubridate)
library(data.table)
library(MASS)
library(sjPlot)

setwd()


# Graficos y tablas ---------------------------

load("output/DB/data plots vih.RData")

plot <- dat.plots.vih %>% 
  filter(mod.int == "aditivo", bimonth_index <16) %>% 
  mutate(exposicion = ifelse(year == 2020, "2020", "2018-2019"),
         cf = ifelse(exposicion == "2020" & cuarentenas == 1, cf, NA),
         cf_ub = ifelse(exposicion == "2020" & cuarentenas == 1, cf_ub, NA),
         cf_lb = ifelse(exposicion == "2020" & cuarentenas == 1, cf_lb, NA)) %>% 
  group_by(bimonth_index, year, exposicion) %>% summarise(n_lme = sum(n_lme),
                                                          yhat = sum(yhat),
                                                          cf = sum(cf),
                                                          ub = sum(ub),
                                                          lb = sum(lb),
                                                          cf_ub = sum(cf_ub),
                                                          cf_lb = sum(cf_lb)) %>% 
  group_by(bimonth_index, exposicion)%>% summarise(n_lme = mean(n_lme),
                                                    yhat = mean(yhat),
                                                    cf = mean(cf),
                                                    ub = mean(ub),
                                                    lb = mean(lb),
                                                    cf_ub = mean(cf_ub),
                                                    cf_lb = mean(cf_lb)) %>% 
  ggplot(aes(x = bimonth_index, y = n_lme, fill = exposicion)) +
  geom_point(alpha = .3, aes(color = exposicion)) +
  geom_line(aes(x = bimonth_index, y = yhat, color = exposicion)) +
  geom_ribbon(aes(ymin = lb, ymax = ub, group = exposicion), alpha = .3) +
  geom_line(aes(x = bimonth_index, y = cf, color = exposicion), linetype = 2, size = .3) +
  geom_ribbon(aes(ymin = cf_lb, ymax = cf_ub, group = exposicion), alpha = .15) +
  ylim(0, NA) +
  theme_minimal() + 
  labs(y = "Número de licencias", x = "Quincena", fill = "Periodo", color = "Periodo") +
  geom_vline(xintercept = 6, linetype = "dotted", color = "red") + 
  theme(legend.position = "bottom", axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

plot

ggsave(plot, filename="output/graf/nb sti/vih/aditivo.png", 
       width = 15, height = 12, dpi = 300)

plot <- dat.plots.vih %>% 
  filter(mod.int == "sexo", bimonth_index <16) %>% 
  mutate(exposicion = ifelse(year == 2020, "2020", "2018-2019"),
         cf = ifelse(exposicion == "2020" & cuarentenas == 1, cf, NA),
         cf_ub = ifelse(exposicion == "2020" & cuarentenas == 1, cf_ub, NA),
         cf_lb = ifelse(exposicion == "2020" & cuarentenas == 1, cf_lb, NA),
         sexo_trabajador = ifelse(sexo_trabajador=="M", "Hombres", "Mujeres")) %>% 
  group_by(bimonth_index, year, exposicion, sexo_trabajador) %>% summarise(n_lme = sum(n_lme),
                                                          yhat = sum(yhat),
                                                          cf = sum(cf),
                                                          ub = sum(ub),
                                                          lb = sum(lb),
                                                          cf_ub = sum(cf_ub),
                                                          cf_lb = sum(cf_lb)) %>% 
  group_by(bimonth_index, exposicion, sexo_trabajador)%>% summarise(n_lme = mean(n_lme),
                                                   yhat = mean(yhat),
                                                   cf = mean(cf),
                                                   ub = mean(ub),
                                                   lb = mean(lb),
                                                   cf_ub = mean(cf_ub),
                                                   cf_lb = mean(cf_lb)) %>% 
  ggplot(aes(x = bimonth_index, y = n_lme, fill = exposicion)) +
  geom_point(alpha = .3, aes(color = exposicion)) +
  geom_line(aes(x = bimonth_index, y = yhat, color = exposicion)) +
  geom_ribbon(aes(ymin = lb, ymax = ub, group = exposicion), alpha = .3) +
  geom_line(aes(x = bimonth_index, y = cf, color = exposicion), linetype = 2, size = .3) +
  geom_ribbon(aes(ymin = cf_lb, ymax = cf_ub, group = exposicion), alpha = .15) +
  ylim(0, NA) +
  theme_minimal() + 
  labs(y = "Número de licencias", x = "Quincena", fill = "Periodo", color = "Periodo") +
  geom_vline(xintercept = 6, linetype = "dotted", color = "red") + 
  facet_wrap("sexo_trabajador") +
  theme(legend.position = "bottom", axis.title = element_text(size = 12),
        axis.text = element_text(size = 12), strip.text = element_text(size = 14))

plot

ggsave(plot, filename="output/graf/nb sti/vih/sexo.png", 
       width = 15, height = 12, dpi = 300)


plot <- dat.plots.vih %>% 
  filter(mod.int == "prevision", bimonth_index <16) %>% 
  mutate(exposicion = ifelse(year == 2020, "2020", "2018-2019"),
         cf = ifelse(exposicion == "2020" & cuarentenas == 1, cf, NA),
         cf_ub = ifelse(exposicion == "2020" & cuarentenas == 1, cf_ub, NA),
         cf_lb = ifelse(exposicion == "2020" & cuarentenas == 1, cf_lb, NA)) %>% 
  group_by(bimonth_index, year, exposicion, prevision) %>% summarise(n_lme = sum(n_lme),
                                                                           yhat = sum(yhat),
                                                                           cf = sum(cf),
                                                                           ub = sum(ub),
                                                                           lb = sum(lb),
                                                                           cf_ub = sum(cf_ub),
                                                                           cf_lb = sum(cf_lb)) %>% 
  group_by(bimonth_index, exposicion, prevision)%>% summarise(n_lme = mean(n_lme),
                                                                    yhat = mean(yhat),
                                                                    cf = mean(cf),
                                                                    ub = mean(ub),
                                                                    lb = mean(lb),
                                                                    cf_ub = mean(cf_ub),
                                                                    cf_lb = mean(cf_lb)) %>% 
  ggplot(aes(x = bimonth_index, y = n_lme, fill = exposicion)) +
  geom_point(alpha = .3, aes(color = exposicion)) +
  geom_line(aes(x = bimonth_index, y = yhat, color = exposicion)) +
  geom_ribbon(aes(ymin = lb, ymax = ub, group = exposicion), alpha = .3) +
  geom_line(aes(x = bimonth_index, y = cf, color = exposicion), linetype = 2, size = .3) +
  geom_ribbon(aes(ymin = cf_lb, ymax = cf_ub, group = exposicion), alpha = .15) +
  ylim(0, NA) +
  theme_minimal() + 
  labs(y = "Número de licencias", x = "Quincena", fill = "Periodo", color = "Periodo") +
  geom_vline(xintercept = 6, linetype = "dotted", color = "red") + 
  facet_wrap("prevision") +
  theme(legend.position = "bottom", axis.title = element_text(size = 12),
        axis.text = element_text(size = 12), strip.text = element_text(size = 14))

plot

ggsave(plot, filename="output/graf/nb sti/vih/prevision.png", 
       width = 15, height = 12, dpi = 300)


plot <- dat.plots.vih %>% 
  filter(mod.int == "edad", bimonth_index <16, 
         !(grupo_edad %in% c("[10,20)","[70,80)"))) %>% 
  mutate(exposicion = ifelse(year == 2020, "2020", "2018-2019"),
         cf = ifelse(exposicion == "2020" & cuarentenas == 1, cf, NA),
         cf_ub = ifelse(exposicion == "2020" & cuarentenas == 1, cf_ub, NA),
         cf_lb = ifelse(exposicion == "2020" & cuarentenas == 1, cf_lb, NA),
         grupo_edad = relevel(grupo_edad, "[30,40)"),
         grupo_edad = relevel(grupo_edad, "[20,30)") ) %>% 
  group_by(bimonth_index, year, exposicion, grupo_edad) %>% summarise(n_lme = sum(n_lme),
                                                                     yhat = sum(yhat),
                                                                     cf = sum(cf),
                                                                     ub = sum(ub),
                                                                     lb = sum(lb),
                                                                     cf_ub = sum(cf_ub),
                                                                     cf_lb = sum(cf_lb)) %>% 
  group_by(bimonth_index, exposicion, grupo_edad)%>% summarise(n_lme = mean(n_lme),
                                                              yhat = mean(yhat),
                                                              cf = mean(cf),
                                                              ub = mean(ub),
                                                              lb = mean(lb),
                                                              cf_ub = mean(cf_ub),
                                                              cf_lb = mean(cf_lb)) %>% 
  ggplot(aes(x = bimonth_index, y = n_lme, fill = exposicion)) +
  geom_point(alpha = .3, aes(color = exposicion)) +
  geom_line(aes(x = bimonth_index, y = yhat, color = exposicion)) +
  geom_ribbon(aes(ymin = lb, ymax = ub, group = exposicion), alpha = .3) +
  geom_line(aes(x = bimonth_index, y = cf, color = exposicion), linetype = 2, size = .3) +
  geom_ribbon(aes(ymin = cf_lb, ymax = cf_ub, group = exposicion), alpha = .15) +
  ylim(0, NA) +
  theme_minimal() + 
  labs(y = "Número de licencias", x = "Quincena", fill = "Periodo", color = "Periodo") +
  geom_vline(xintercept = 6, linetype = "dotted", color = "red") + 
  facet_wrap("grupo_edad") +
  theme(legend.position = "bottom", axis.title = element_text(size = 12),
        axis.text = element_text(size = 12), strip.text = element_text(size = 14))

plot

ggsave(plot, filename="output/graf/nb sti/vih/edad.png", 
       width = 15, height = 12, dpi = 300)


tab.red.total <- dat.plots.vih %>% 
  filter(cuarentenas == 1, mod.int == "aditivo") %>% 
  group_by(mod.int) %>% 
  summarise(n_lme = sum(n_lme),
            yhat = sum(yhat),
            cf = sum(cf),
            ub = sum(ub),
            lb = sum(lb),
            cf_ub = sum(cf_ub),
            cf_lb = sum(cf_lb),
            dif = cf - yhat,
            dif_ub = cf_ub - ub,
            dif_lb = cf_lb - lb) %>% 
  pivot_wider(id_cols = mod.int, 
              values_from = c(dif, dif_lb, dif_ub)) 


tab.red.prev <- dat.plots.vih %>% 
  filter(cuarentenas == 1,
         mod.int == "prevision") %>% 
  group_by(prevision, mod.int) %>% 
  summarise(n_lme = sum(n_lme),
            yhat = sum(yhat),
            cf = sum(cf),
            ub = sum(ub),
            lb = sum(lb),
            cf_ub = sum(cf_ub),
            cf_lb = sum(cf_lb),
            dif = cf - yhat,
            dif_ub = cf_ub - ub,
            dif_lb = cf_lb - lb) %>% 
  pivot_wider(id_cols = mod.int, names_from = prevision, 
              values_from = c(dif, dif_lb, dif_ub)) %>% 
  summarise(mod.int, dif_FONASA, dif_lb_FONASA, dif_ub_FONASA, dif_ISAPRE, 
            dif_lb_ISAPRE, dif_ub_ISAPRE,
            dif = dif_FONASA - dif_ISAPRE,
            dif_lb = dif_lb_FONASA - dif_lb_ISAPRE,
            dif_ub = dif_ub_FONASA - dif_ub_ISAPRE) %>% 
  mutate(across(-mod.int, round), .keep = "all") %>% 
  summarise(mod.int,
            FONASA = paste0(dif_FONASA, " (", dif_lb_FONASA, "-", dif_ub_FONASA, ")"),
            ISAPRE = paste0(dif_ISAPRE, " (", dif_lb_ISAPRE, "-", dif_ub_ISAPRE, ")"),
            DIF = paste0(dif, " (", dif_lb, "-", dif_ub, ")"))

colnames(tab.red.prev) <- c("Type", "FONASA Count (CI 95%)", "ISAPRE Count (CI 95%)",
                            "Excess impact on FONASA Count (CI 95%)")

write.csv(tab.red.prev, file = "output/tab/vih/lme tab3 prev.csv")

tab.red.sex <- dat.plots.vih %>% 
  filter(cuarentenas == 1,
         mod.int == "sexo") %>%
  mutate(sexo_trabajador = ifelse(sexo_trabajador=="M", "Male", "Female")) %>% 
  group_by(mod.int, sexo_trabajador) %>% 
  summarise(n_lme = sum(n_lme),
            yhat = sum(yhat),
            cf = sum(cf),
            ub = sum(ub),
            lb = sum(lb),
            cf_ub = sum(cf_ub),
            cf_lb = sum(cf_lb),
            dif = cf - yhat,
            dif_ub = cf_ub - ub,
            dif_lb = cf_lb - lb) %>% 
  pivot_wider(id_cols = mod.int, names_from = sexo_trabajador, 
              values_from = c(dif, dif_lb, dif_ub)) %>% 
  summarise(mod.int, dif_Male, dif_lb_Male, dif_ub_Male, dif_Female, 
            dif_lb_Female, dif_ub_Female,
            dif = dif_Male - dif_Female,
            dif_lb = dif_lb_Male - dif_lb_Female,
            dif_ub = dif_ub_Male - dif_ub_Female) %>% 
  mutate(across(-(mod.int), round), .keep = "all") %>% 
  summarise(mod.int,
            Male = paste0(dif_Male, " (", dif_lb_Male, "-", dif_ub_Male, ")"),
            Female = paste0(dif_Female, " (", dif_lb_Female, "-", dif_ub_Female, ")"),
            DIF = paste0(dif, " (", dif_lb, "-", dif_ub, ")"))

colnames(tab.red.sex) <- c("Type", "Men Count (CI 95%)", "Women Count (CI 95%)", 
                           "Excess impact on Men Count (CI 95%)")

write.csv(tab.red.sex, file = "output/tab/vih/lme tab3 sexo.csv")

