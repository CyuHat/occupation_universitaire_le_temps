# libraries ----
pacman::p_load(tidyverse)
source("Script/utilities.R")

# Options ----
theme_set(theme_bw())

# Data ----
lt1 <- 
  read_csv("MyData/lt1.csv") %>% 
  replace_na(
    list(
      position = "neutre",
      lieu = "",
      acteur = "",
      ton = "neutre",
      critique = ""
    )
  ) %>% 
  filter(sujet=="oui",
         !str_detect(ton, "désarroi des étudiants et des")) %>% 
  select(-sujet) %>% 
  mutate(
    position2 = case_when(
      str_detect(position, "pour") ~ "pour",
      str_detect(position, "contre") ~ "contre",
      str_detect(position, "neutre") ~ "neutre",
      str_detect(position, "mélange") ~ "mélange"
    ),
    ton2 = case_when(
      str_detect(ton, "les étudiants, négatif pour l'université") ~ "neutre",
      str_detect(ton, "critique|objectif") ~ "critique",
      str_detect(ton, "neutre") ~ "neutre",
      str_detect(ton, "négatif") ~ "négatif",
      str_detect(ton, "positif") ~ "positif"
    ),
    auteur = if_else(str_detect(auteur, "Le Temps") | 
                       is.na(auteur),
                     "Le Temps", auteur),
    lieu = if_else(lieu == "", "Suisse", lieu),
    suisse = str_detect(lieu, "[Ss]uisse|Lausanne|Genève|Zürich|Neuchâtel|Fribourg|romandes?|générales?|spécifique|EPFL")
  ) %>% 
  filter(!str_detect(model, "notre suivi de la semaine"),
         !str_detect(google_titre, "Les députés allemands|Une attaque israélienne tue trois humanitaires à Gaza|Jésus, Trump et les souliers"),
         suisse) %>% 
  select(-suisse)

# Analyse ----
# Nombre d'article dans le temps
lt1 %>% 
  mutate(date = date(date)) %>% 
  ggplot(aes(date)) +
  annotate(geom = "rect",
           xmin = ymd("2024-04-25"),
           xmax = ymd("2024-06-10"),
           ymin = 0,
           ymax = Inf,
           fill = "lightgrey",
           alpha = 0.6
  ) +
  geom_histogram(fill = "cyan", alpha = 0.6, color = "black") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%B",
    expand = c(0.001, 0)
  ) +
  scale_y_continuous(breaks = 1:10, expand = c(0, 0.04)) +
  theme(panel.grid.minor = element_blank()) +
  labs(
    title = "Nombre d'articles sur les occupations universitaires\nen Suisse",
    subtitle = "Année: 2024",
    x = "Date",
    y = "Nombre d'articles sur le sujet"
  )

ggsave("Results/Figures/Figure_0.PNG",
       scale = 1.7)


# Nomtre d'article par auteur
lt1 %>% 
  count(auteur, sort = TRUE) %>% 
  mutate(auteur = fct_reorder(auteur, n),
         prop = n/sum(n)) %>% 
  ggplot(aes(prop, auteur, fill = auteur=="Laure Lugon Zugravu")) +
  geom_col(color = "black", alpha = 0.6) +
  geom_text(aes(label = n), nudge_x = 0.005) +
  scale_x_continuous(expand = c(0, 0.0018), labels = scales::percent) +
  labs(
    title = "Nombre d'articles sur les occupations universitaires\nen Suisse",
    subtitle = "par auteur",
    x = "Nombre d'articles sur le sujet",
    y = "Auteurs"
    ) +
  theme(
    legend.position = "none"
  )

ggsave("Results/Figures/Figure_3.PNG",
       scale = 1.7)

lt1$position3 <- position3

lt1 %>% 
  glimpse()
  
lt1 %>% 
  filter(!str_detect(auteur, "collective|Ola")) %>% 
  count(position3) %>% 
  mutate(prop = n/sum(n))

