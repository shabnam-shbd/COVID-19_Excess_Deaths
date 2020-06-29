#library(COVID19)
library(tidyverse)
#library(lubridate)
#library(magrittr)
library(ggplot2)
library(ggthemes)
#library(plotly)
library(ggtext)
library(ggpubr)

df <- readr::read_csv("https://www.mortality.org/Public/STMF/Outputs/stmf.csv", skip=1)

deaths_male <- df %>% 
  rename(Deaths = DTotal) %>% 
  filter(Sex == "m") %>% 
  select(CountryCode, Year, Week, Deaths, Sex) %>% 
  mutate(CountryCode = recode(CountryCode,
                              "DEUTNP" = "DEU",
                              "GBRTENW" = "GBR"),
         Country = recode(CountryCode, 
                          "AUT" = "Austria",
                          "BEL" = "Belgium",
                          "DNK" = "Denmark",
                          "ESP" = "Spain", 
                          "GBR" = "Great Britain",
                          "DEU" = "Germany", 
                          "NLD" = "Netherlands", 
                          "PRT" = "Portugal", 
                          "SWE" = "Sweden",
                          "USA" = "United States")) %>% 
  filter(!CountryCode %in% c("FIN", "NOR", "ISL")) 

#Calculate rolling mean of deaths occurring from 2000 to 2019
deaths_male_mean <- deaths_male %>% 
  filter(Year != 2020) %>% 
  group_by(Country, Week) %>% 
  summarise(Mean_deaths = Deaths %>% mean() %>% round()) %>% 
  ungroup() 

#Calculate excess deaths occurring in 2000 
deaths_male_excess <- deaths_male %>% 
  left_join(deaths_male_mean, by = c("Country", "Week")) %>% 
  mutate(Excess_deaths = Deaths - Mean_deaths) 


deaths_male_excess %>% 
  ggplot() + 
  geom_line(aes(Week, Deaths, group = Year), col = "lightgrey", alpha = .75) + 
  geom_line(aes(Week, Mean_deaths), col = "grey", size = .75, alpha = 1) + 
  geom_line(data = . %>% filter(Year == 2020), aes(Week, Deaths), col = "red", size = 1.1) + 
  facet_wrap(~Country, scales = "free") + 
  labs(caption = "Source: The Human Mortality Database",
       title = "Weekly deaths of male population <b style = 'color:darkorange'>in 2020</b> vs 
                <span style = 'color:steelblue'>past years</span> and 
                <b style = 'color:steelblue'>their mean</b>") +
  theme_minimal() + 
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = ggtext::element_markdown(size = 16)
  ) +
  NULL




deaths_female <- df %>% 
  rename(Deaths = DTotal) %>% 
  filter(Sex == "f") %>% 
  select(CountryCode, Year, Week, Deaths, Sex) %>% 
  mutate(CountryCode = recode(CountryCode,
                              "DEUTNP" = "DEU",
                              "GBRTENW" = "GBR"),
         Country = recode(CountryCode, 
                          "AUT" = "Austria",
                          "BEL" = "Belgium",
                          "DNK" = "Denmark",
                          "ESP" = "Spain", 
                          "GBR" = "Great Britain",
                          "DEU" = "Germany", 
                          "NLD" = "Netherlands", 
                          "PRT" = "Portugal", 
                          "SWE" = "Sweden",
                          "USA" = "United States")) %>% 
  filter(!CountryCode %in% c("FIN", "NOR", "ISL")) 

#Calculate rolling mean of deaths occurring from 2000 to 2019
deaths_female_mean <- deaths_female %>% 
  filter(Year != 2020) %>% 
  group_by(Country, Week) %>% 
  summarise(Mean_deaths = Deaths %>% mean() %>% round()) %>% 
  ungroup() 

#Calculate excess deaths occurring in 2000 
deaths_female_excess <- deaths_female %>% 
  left_join(deaths_female_mean, by = c("Country", "Week")) %>% 
  mutate(Excess_deaths = Deaths - Mean_deaths) 


deaths_female_excess %>% 
  ggplot() + 
  geom_line(aes(Week, Deaths, group = Year), col = "lightgrey", alpha = .75) + 
  geom_line(aes(Week, Mean_deaths), col = "grey", size = .75, alpha = 1) + 
  geom_line(data = . %>% filter(Year == 2020), aes(Week, Deaths), col = "red", size = 1.1) + 
  facet_wrap(~Country, scales = "free") + 
  labs(caption = "Source: The Human Mortality Database",
       title = "Weekly deaths of female population <b style = 'color:darkorange'>in 2020</b> vs 
                <span style = 'color:steelblue'>past years</span> and 
                <b style = 'color:steelblue'>their mean</b>") +
  theme_minimal() + 
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = ggtext::element_markdown(size = 16)
  ) 

################################
## GBR SEX
################################
deaths_gbr <- df %>% 
  rename(Deaths = DTotal) %>% 
  filter(Sex %in% c("m", "f")) %>% 
  select(CountryCode, Year, Week, Deaths, Sex) %>% 
  mutate(CountryCode = recode(CountryCode,
                              "DEUTNP" = "DEU",
                              "GBRTENW" = "GBR"),
         Country = recode(CountryCode, 
                          "AUT" = "Austria",
                          "BEL" = "Belgium",
                          "DNK" = "Denmark",
                          "ESP" = "Spain", 
                          "GBR" = "Great Britain",
                          "DEU" = "Germany", 
                          "NLD" = "Netherlands", 
                          "PRT" = "Portugal", 
                          "SWE" = "Sweden",
                          "USA" = "United States"),
         Sex = recode(Sex, 
                          "m" = "Male",
                          "f" = "Female" )) %>% 
  filter(!CountryCode %in% c("FIN", "NOR", "ISL")) 

#Calculate rolling mean of deaths occurring from 2000 to 2019
deaths_gbr_mean <- deaths_gbr %>% 
  filter(Year != 2020) %>% 
  group_by(Country, Week) %>% 
  summarise(Mean_deaths = Deaths %>% mean() %>% round()) %>% 
  ungroup() 

#Calculate excess deaths occurring in 2000 
deaths_gbr_excess <- deaths_gbr %>% 
  left_join(deaths_gbr_mean, by = c("Country", "Week")) %>% 
  mutate(Excess_deaths = Deaths - Mean_deaths) 

deaths_gbr_excess %>% 
  filter(CountryCode == "GBR") %>%
  ggplot() + 
  geom_line(aes(Week, Deaths, group = Year), col = "lightgrey", alpha = .75) + 
  geom_line(aes(Week, Mean_deaths), col = "grey", size = .75, alpha = 1) + 
  geom_line(data = . %>% filter(Year == 2020), aes(Week, Deaths), col = "red", size = 1.1) + 
 # ylim(3000,12000)+ 
  scale_y_continuous(breaks=seq(3000,15000,1000)) +
  facet_wrap(~Sex) +
  labs(caption = "Source: The Human Mortality Database",
       title = "Weekly deaths of population <b style = 'color:darkorange'>in 2020</b> vs 
                <span style = 'color:steelblue'>past years</span> and 
                <b style = 'color:steelblue'>their mean</b>") +
  theme_bw() + 
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = ggtext::element_markdown(size = 16)
  )
ggsave("abc.png", width = 4, height = 4)

################################
## GBR MALE AGE
################################
deaths_gbr_age <- df %>% 
  select(CountryCode, Year, Week, D0_14, D15_64,
         D65_74, D75_84, D85p, Sex) %>% 
  mutate(CountryCode = recode(CountryCode,
                              "GBRTENW" = "GBR"),
         Country = recode(CountryCode, 
                          "GBR" = "Great Britain"),
         Sex = recode(Sex, 
                      "m" = "Male",
                      "f" = "Female",
                      "b" = "Both"))  %>%
  filter(Sex %in% c("Male", "Female","Both")) %>% 
  filter(CountryCode == "GBR")
  
#Calculate rolling mean of deaths occurring from 2000 to 2019
deaths_gbr_age_male_mean <- deaths_gbr_age %>% 
  filter(Sex %in% c("Male")) %>% 
  filter(Year != 2020) %>% 
  group_by(Country, Week) %>% 
  summarise(Mean_D0_14 = D0_14 %>% mean() %>% round(),
            Mean_D15_64 = D15_64 %>% mean() %>% round(),
            Mean_D65_74 = D65_74 %>% mean() %>% round(),
            Mean_D75_84 = D75_84 %>% mean() %>% round(),
            Mean_D85p = D85p %>% mean() %>% round()) %>% 
  ungroup() 

#Calculate excess deaths occurring in 2000 
deaths_gbr_age_male_excess <- deaths_gbr_age %>% 
  filter(Sex %in% c("Male")) %>% 
  left_join(deaths_gbr_age_male_mean, by = c("Country", "Week")) %>% 
  mutate(Excess_D0_14 = D0_14 - Mean_D0_14,
         Excess_D15_64 = D15_64 - Mean_D15_64,
         Excess_D65_74 = D65_74 - Mean_D65_74,
         Excess_D75_84 = D75_84 - Mean_D75_84,
         Excess_D85p = D85p - Mean_D85p) 



ggfun <- function(data, Deaths, Mean_Deaths, title1,ylim1,ylim2){
  data %>% mutate(title = title1) %>%
    filter(CountryCode == "GBR") %>% 
    ggplot() + 
    geom_line(aes(Week, !! sym(Deaths), group = Year), col = "lightgrey", alpha = .75) + 
    geom_line(aes(Week, !! sym(Mean_Deaths)), col = "grey", size = .75, alpha = 1) + 
    geom_line(data = . %>% filter(Year == 2020), aes(Week, !! sym(Deaths)), col = "red", size = 1.1) + 
    #labs(title = title1) +
    ylim(ylim1,ylim2) +
    theme_bw() +
    facet_grid(. ~ title) +
    theme(strip.text = element_text(size = 12, face = "bold"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          plot.title = ggtext::element_markdown(size = 12)) }

p1 <- ggfun(data=deaths_gbr_age_male_excess, Deaths="D0_14", Mean_Deaths="Mean_D0_14",
            title1 = "Male Age Group 0-14",ylim1=15,ylim2=100)
p2 <- ggfun(data=deaths_gbr_age_male_excess, Deaths="D15_64", Mean_Deaths="Mean_D15_64",
            title1 = "Male Age Group 15-64",ylim1=500,ylim2=2500)
p3 <- ggfun(data=deaths_gbr_age_male_excess, Deaths="D65_74", Mean_Deaths="Mean_D65_74",
            title1 = "Male Age Group 65-74",ylim1=500,ylim2=2500)
p4 <- ggfun(data=deaths_gbr_age_male_excess, Deaths="D75_84", Mean_Deaths="Mean_D75_84",
            title1 = "Male Age Group 75-84",ylim1=500,ylim2=4000)
p5 <- ggfun(data=deaths_gbr_age_male_excess, Deaths="D85p", Mean_Deaths="Mean_D85p",
            title1 = "Male Age Group 85+",ylim1=500,ylim2=6500)

library(ggpubr)
ggarrange(p1, p2, p3,p4,p5, ncol = 3, nrow = 2)


ggsave("abc.png", width = 4, height = 4)

################################
## GBR FEMALE AGE
################################
deaths_gbr_age <- df %>% 
  select(CountryCode, Year, Week, D0_14, D15_64,
         D65_74, D75_84, D85p, Sex) %>% 
  mutate(CountryCode = recode(CountryCode,
                              "GBRTENW" = "GBR"),
         Country = recode(CountryCode, 
                          "GBR" = "Great Britain"),
         Sex = recode(Sex, 
                      "m" = "Male",
                      "f" = "Female",
                      "b" = "Both"))  %>%
  filter(Sex %in% c("Female", "Fefemale","Both")) %>% 
  filter(CountryCode == "GBR")

#Calculate rolling mean of deaths occurring from 2000 to 2019
deaths_gbr_age_female_mean <- deaths_gbr_age %>% 
  filter(Sex %in% c("Female")) %>% 
  filter(Year != 2020) %>% 
  group_by(Country, Week) %>% 
  summarise(Mean_D0_14 = D0_14 %>% mean() %>% round(),
            Mean_D15_64 = D15_64 %>% mean() %>% round(),
            Mean_D65_74 = D65_74 %>% mean() %>% round(),
            Mean_D75_84 = D75_84 %>% mean() %>% round(),
            Mean_D85p = D85p %>% mean() %>% round()) %>% 
  ungroup() 

#Calculate excess deaths occurring in 2000 
deaths_gbr_age_female_excess <- deaths_gbr_age %>% 
  filter(Sex %in% c("Female")) %>% 
  left_join(deaths_gbr_age_female_mean, by = c("Country", "Week")) %>% 
  mutate(Excess_D0_14 = D0_14 - Mean_D0_14,
         Excess_D15_64 = D15_64 - Mean_D15_64,
         Excess_D65_74 = D65_74 - Mean_D65_74,
         Excess_D75_84 = D75_84 - Mean_D75_84,
         Excess_D85p = D85p - Mean_D85p) 



ggfun <- function(data, Deaths, Mean_Deaths, title1,ylim1,ylim2){
  data %>% mutate(title = title1) %>%
    filter(CountryCode == "GBR") %>% 
    ggplot() + 
    geom_line(aes(Week, !! sym(Deaths), group = Year), col = "lightgrey", alpha = .75) + 
    geom_line(aes(Week, !! sym(Mean_Deaths)), col = "grey", size = .75, alpha = 1) + 
    geom_line(data = . %>% filter(Year == 2020), aes(Week, !! sym(Deaths)), col = "steelblue", size = 1.1) + 
    #labs(title = title1) +
    ylim(ylim1,ylim2) +
    theme_bw() +
    facet_grid(. ~ title) +
    theme(strip.text = element_text(size = 12, face = "bold"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          plot.title = ggtext::element_markdown(size = 12)) }

fp1 <- ggfun(data=deaths_gbr_age_female_excess, Deaths="D0_14", Mean_Deaths="Mean_D0_14",
            title1 = "Female Age Group 0-14",ylim1=15,ylim2=100)
fp2 <- ggfun(data=deaths_gbr_age_female_excess, Deaths="D15_64", Mean_Deaths="Mean_D15_64",
            title1 = "Female Age Group 15-64",ylim1=500,ylim2=2500)
fp3 <- ggfun(data=deaths_gbr_age_female_excess, Deaths="D65_74", Mean_Deaths="Mean_D65_74",
            title1 = "Female Age Group 65-74",ylim1=500,ylim2=2500)
fp4 <- ggfun(data=deaths_gbr_age_female_excess, Deaths="D75_84", Mean_Deaths="Mean_D75_84",
            title1 = "Female Age Group 75-84",ylim1=500,ylim2=4000)
fp5 <- ggfun(data=deaths_gbr_age_female_excess, Deaths="D85p", Mean_Deaths="Mean_D85p",
            title1 = "Female Age Group 85+",ylim1=500,ylim2=6500)

#ggarrange(p1, p2, p3,p4,p5, ncol = 3, nrow = 2)


figure <- ggarrange(p1, p2, p3,p4,p5 + font("x.text", size = 10),
                    ncol = 3, nrow = 2)
annotate_figure(figure,
                top = text_grob("Weekly female population deaths in 2020 vs previous 10 years and their mean", color = "black", face = "bold", size = 14),
                bottom = text_grob("Data Source: The Human Mortality Database", color = "steelblue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Number of deaths", color = "black", rot = 90),
                #right = "I'm done, thanks :-)!",
                #fig.lab = "Figure 1", fig.lab.face = "bold"
)


figure <- ggarrange(fp1, fp2, fp3,fp4,fp5,p1, p2, p3,p4,p5,
                    ncol = 5, nrow = 2)
annotate_figure(figure,
                top = text_grob("Weekly female population deaths in 2020 vs previous 10 years and their mean", color = "black", face = "bold", size = 14),
                bottom = text_grob("Data Source: The Human Mortality Database", color = "steelblue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Number of deaths", color = "black", rot = 90),
                #right = "I'm done, thanks :-)!",
                #fig.lab = "Figure 1", fig.lab.face = "bold"
)

ggsave("abc.png", width = 4, height = 4)
