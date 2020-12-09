
#### generating data for a commentary on the current trend on incidence of TB
#### on Latin America, based on WHO Global Report TB Data
#### Otavio Ranzani, October 2020


### getting data by the getTBinR package, which uses data from the WHO website

# install.packages("devtools")
# devtools::install_github("seabbs/getTBinR")

library(getTBinR)
library(tidyverse)
library(tidylog)
library(Hmisc)
library(ggrepel)
library(viridis)
library(cowplot)
library(ggpubr)

# checking data availability
data <- getTBinR::available_datasets

# download, 22/10/2020
tb_burden <- get_tb_burden(additional_datasets = "Estimates")
# checking regions as strings
Hmisc::describe(tb_burden$g_whoregion) # 6 regions, ok

# selecting Americas ## 890 lines
tb_burden_a <- tb_burden %>% filter(g_whoregion == "Americas")

# checking how many countries ## 46 countries and territories, 20 years from 2000 to 2019
tb_burden_a %>% select(country, year) %>% Hmisc::describe()

# not duplication neither typo. It is considered areas, islands, not specifically countries 

# reproducing 2019 Numbers for AMERICA, as 2020 Global report

tb_burden_a %>% filter(year == 2019) %>% 
summarise(total_cases = sum(e_inc_num),
          total_pop   = sum(e_pop_num),
          total_death = sum(e_mort_num),
          total_death_hiv = sum(e_mort_tbhiv_num),
          total_death_nhiv = sum(e_mort_exc_tbhiv_num),
          e_inc = total_cases/total_pop,
          e_inc_100 = e_inc * 100000)

# total_cases  total_pop     total_death   total_death_hiv      total_death_nhiv    e_inc e_inc_100
# 290537       1013411202       23389            5873            17464 0.000287         28.7


#### numbers for america, 12 countries, after 2014 to include in the commentary ####


### list of 12 Countries
countries <- c("Brazil", "Venezuela (Bolivarian Republic of)",  
               "Chile", "Paraguay","Uruguay", "Mexico",
               "Peru", "El Salvador", "Ecuador", "Argentina",
               "Colombia", "Panama")

tb_burden_a %>% 
  filter(year >=2014 & country %in% countries) %>% 
  group_by(year) %>% 
  summarise(total_cases = sum(e_inc_num),
            total_pop   = sum(e_pop_num),
            e_inc_100   = (total_cases/total_pop) * 100000)


230100/290537 # 0.7919817
230100-202290 # 27810
230100/202290 # 1.137476

41.7/38.4 # 1.085938

tb_burden_a %>% 
  filter(year >=2014 & grepl("Venez", country)) %>% 
  group_by(year, country) %>% 
  summarise(total_cases = sum(e_inc_num),
            total_pop   = sum(e_pop_num),
            e_inc_100   = (total_cases/total_pop) * 100000)

# year country                            total_cases total_pop e_inc_100
# 1  2014 Venezuela (Bolivarian Republic of)        8000  30042973      26.6
# 2  2015 Venezuela (Bolivarian Republic of)        8900  30081827      29.6
# 3  2016 Venezuela (Bolivarian Republic of)       10000  29851249      33.5
# 4  2017 Venezuela (Bolivarian Republic of)       13000  29402480      44.2
# 5  2018 Venezuela (Bolivarian Republic of)       14000  28887117      48.5
# 6  2019 Venezuela (Bolivarian Republic of)       13000  28515829      45.6

tb_burden_a %>% 
  filter(year >=2000 & country == "Argentina") %>% 
  group_by(year, country) %>% 
  summarise(total_cases = sum(e_inc_num),
            total_pop   = sum(e_pop_num),
            e_inc_100   = (total_cases/total_pop) * 100000)

# year country   total_cases total_pop e_inc_100
# 1  2010 Argentina        8400  40895751      20.5
# 2  2011 Argentina       11000  41320497      26.6
# 3  2012 Argentina       10000  41755188      23.9
# 4  2013 Argentina       10000  42196034      23.7
# 5  2014 Argentina       11000  42637508      25.8
# 6  2015 Argentina       11000  43075416      25.5
# 7  2016 Argentina       12000  43508459      27.6
# 8  2017 Argentina       12000  43937143      27.3
# 9  2018 Argentina       12000  44361150      27.1
# 10 2019 Argentina       13000  44780675      29.0

29.0 / 20.5 # 1.414634



tb_burden_a %>% 
  filter(year >=2010 & country == "Brazil") %>% 
  group_by(year, country) %>% 
  summarise(total_cases = sum(e_inc_num),
            total_pop   = sum(e_pop_num),
            e_inc_100   = (total_cases/total_pop) * 100000)


# year country total_cases total_pop e_inc_100
# 1  2010 Brazil        86000 195713637      43.9
# 2  2011 Brazil        88000 197514541      44.6
# 3  2012 Brazil        88000 199287292      44.2
# 4  2013 Brazil        89000 201035904      44.3
# 5  2014 Brazil        87000 202763744      42.9
# 6  2015 Brazil        87000 204471759      42.5
# 7  2016 Brazil        88000 206163056      42.7
# 8  2017 Brazil        92000 207833825      44.3
# 9  2018 Brazil        96000 209469320      45.8
# 10 2019 Brazil        96000 211049519      45.5

45.5 / 42.9 # 1.060606

96000 - 87000 # 9000


#### Plots for Commentary ####

#### preparation for plot ####

# axis
dictionary <- get_data_dict()

# label
tb_burden_a_2019 <- tb_burden_a %>% filter(year == 2019 & country %in% countries)
tb_burden_a_2019 <- tb_burden_a_2019 %>% 
  mutate(country = ifelse(grepl("Venezuela", country), "Venezuela*", country))

# colours
colours_pa = viridis::viridis(n=12)
colours_pa2 <-ggsci::pal_lancet("lanonc")(9)

# alpha overtime (manually coded)
#alpha_if <- c(rep(0.4,114), rep(1,114))
alpha_if <- c(rep(0.4,15), rep(1,5))
alpha_if <- rep(alpha_if, 12)


#### eFigure 1 ####
getTBinR::plot_tb_burden_summary(metric_label = "Estimated tuberculosis incidence (all forms) per 100 000 population", 
                                 legend = "none", facet = "Area", scales = "free_y", 
                                 compare_to_world = FALSE, years = 2000:2019) +
                                 scale_y_continuous(breaks = 2000:2019, labels = 2000:2019)

#### eFigure 2 ####
tb_burden_a %>% 
  filter(country %in% countries & year >=2000) %>% 
  ggplot(aes(x = year, y = e_inc_100k, col = country, fill = country)) +
  geom_line(na.rm = TRUE, size = 1.0) + 
  geom_ribbon(aes(ymin = e_inc_100k_lo, 
                  ymax = e_inc_100k_hi, col = NULL), alpha = 0.2, na.rm = TRUE) +
  geom_line(aes(x = year, y = c_newinc_100k), col = "black", size = 1.1) + 
  theme(plot.margin = unit(c(1,1,1,1), "lines"), legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 12),
        axis.text = element_text(colour = "black", size = 10)) +
  facet_wrap(~country, ncol = 3, scales = "free") +
  scale_colour_viridis(end = 1, begin = 0.2, direction = -1, 
                       discrete = TRUE, option = "viridis") + 
  scale_fill_viridis(end = 1,  begin = 0.2,  direction = -1,
                     discrete = TRUE, option = "viridis") + 
  theme_minimal() + theme(legend.position = "none") +
  labs(x = "Year", y = "Rate per 100 000 population", 
       caption = "Coloured lines: Estimated tuberculosis incidence (all forms), shadow represents 95% confidence intervals. Black lines: Tuberculosis case notification")


#### Figure 1, main figure ####

# Incidence
plot1 <- tb_burden_a %>% 
  filter(country %in% countries & year >=2000) %>% 
  ggplot(aes(x = year, y = e_inc_100k, 
             group = country, col = country, fill=country)) +
  geom_line(size = 1.2, lty = 1, alpha = alpha_if) + #ylim (0,130) + 
  scale_color_discrete(colours_pa) + 
  geom_text_repel(data = tb_burden_a_2019,
                  aes(label = country), 
                  direction = "y", 
                  force = 5,
                  segment.size = 0.35,
                  na.rm = TRUE,
                  xlim = c(2020.3,2022),
                  ylim = c(0,130),
                  segment.colour = "black") + 
  coord_cartesian(xlim = c(2000, 2019), clip = 'off') +   
  theme(plot.margin = unit(c(1,4,1,1), "lines"), legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 12),
        axis.text = element_text(colour = "black", size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(x = "Year", 
       y = "Estimated tuberculosis incidence (all forms) per 100 000 population") +
  scale_y_log10(breaks = c(seq(0,50,5), seq(60,200, 20)), limits = c(12,200)) + 
  scale_x_continuous(breaks = seq(2000,2019,1)) 
  plot1
  
# Case notification
plot2 <- tb_burden_a %>% 
  filter(country %in% countries & year >=2000) %>% 
  ggplot(aes(x = year, y = c_newinc_100k, 
             group = country, col = country, fill=country)) +
  geom_line(size = 1.2, lty = 1, alpha = alpha_if) + # ylim (0,130) + 
  scale_color_discrete(colours_pa) + 
  geom_text_repel(data = tb_burden_a_2019,
                  aes(label = country), 
                  direction = "y", 
                  force = 5,
                  segment.size = 0.35,
                  na.rm = TRUE,
                  xlim = c(2020.5,2022),
                  ylim = c(0,130),
                  segment.colour = "black") + 
  coord_cartesian(xlim = c(2000, 2019), clip = 'off') +   
  theme(plot.margin = unit(c(1,4,1,1), "lines"), legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 12),
        axis.text = element_text(colour = "black", size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(x = "Year", 
       y = "Tuberculosis case notification rate per 100 000 population") +
  scale_x_continuous(breaks = seq(2000,2019,1)) +
  scale_y_log10(breaks = c(seq(0,50,5), seq(60,200, 20)), limits = c(12,200))
  plot2
  
# Notified cases
 plot3 <- tb_burden_a %>% 
  filter(country %in% countries & year >=2000) %>% 
  ggplot(aes(x = year, y = e_inc_num, 
             group = country, col = country, fill=country)) +
  geom_line(size = 1.2, lty = 1, alpha = alpha_if) +  
  scale_color_discrete(colours_pa) + 
  geom_text_repel(data = tb_burden_a_2019,
                  aes(label = country), 
                  direction = "y", 
                  force = 5,
                  segment.size = 0.35,
                  na.rm = TRUE,
                  xlim = c(2020,2021.75),
                  ylim = c(0,100000),
                  segment.colour = "black") + 
  coord_cartesian(xlim = c(2000, 2019), clip = 'off') +   
  theme(plot.margin = unit(c(1,4,1,1), "lines"), legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 12),
        axis.text = element_text(colour = "black", size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(x = "Year", 
       y =  "Estimated number of tuberculosis incident cases (all forms)") + 
   scale_y_log10(labels = scales::unit_format(
     unit = "k", scale = 1e-3, accuracy = 0.1), 
     breaks = c(500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000,
                  40000, 50000, 100000),
     limits = c(480, 100000)) + 
   scale_x_continuous(breaks = seq(2000,2019,1)) 
plot3

## 
quartz( ,21,7) # openning for Mac

figure1 <- cowplot::plot_grid(plot1, plot2, plot3, ncol = 3,
                   labels = c("Estimated incidence", "  Case notification", "    Estimated cases  "),
                   label_x = .02)
ggsave2(figure1,"figure1.pdf", device = "pdf",)

ggpubr::ggarrange(plot1, plot2, plot3, ncol=3)
