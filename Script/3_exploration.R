# Libraries ----
pacman::p_load(tidyverse, janitor, 
               skimr, tidytext,
               plotly)

# Options ----
theme_set(theme_bw())

# Data ----
df1 <- read_rds("MyData/df1.rds")

# Publication Zugravu dans le temps
df1 %>% 
  ggplot(aes(date)) +
  geom_histogram( fill = "cyan",
                  color = "black") +
  labs(title = "Publication de Zugravu dans le temps",
       subtitle = "2022 - 2024")

# Publication Zugravu 2024
df1 %>% 
  filter(date > date("2023-12-31")) %>% 
  ggplot(aes(date)) +
  geom_histogram( fill = "cyan",
                  color = "black") +
  labs(title = "Publication de Zugravu dans le temps",
       subtitle = "Année: 2024")

# Publication Zugravu 2024
df1 %>% 
  filter(annee == 2024) %>% 
  mutate(date = floor_date (date, unit = "month")) %>% 
  count(date, type) %>% 
  ggplot(aes(date, n, fill = type)) +
  geom_col() +
  labs(title = "Publications de Zugravu par type",
       subtitle = "Année: 2024")


# Articles en lien avec le sujet
# TODO: Filtrer les article en lien avec le sujet
df1 %>% 
  filter(annee == 2024) %>% 
  mutate(sujet_robust = if_else(date < "2024-05-01", "Non", sujet_robust),
         date = floor_date (date, unit = "month")) %>% 
  count(date, sujet_robust) %>% 
  ggplot(aes(date, n, fill = sujet_robust)) +
  annotate("rect", 
           xmin = date("2024-04-15"), xmax = date("2024-05-17"),
           ymin = 0, ymax = 13,
           alpha = 0.4) +
  geom_col(color = "white") +
  scale_y_continuous(breaks = 0:13) +
  labs(title = "Zugravu: Articles en lien avec le sujet des occupations et Israël-Palestine",
       subtitle = "Année: 2024",
       y = "Nombre d'articles",
       x = "Date",
       fill = "En lien avec le sujet?"
  ) +
  theme(legend.position = "bottom") +
  scale_fill_viridis_d()

ggsave(filename = "Results/Figures/Figure_1.png",
       scale = 1.7)

df1 %>% 
  filter(
    # publication >= "2024-05-01",
    # publication <= "2024-05-30",
    sujet_robust == "Oui") %>% 
  arrange(publication) %>% 
  select(titre, type) %>% 
  count(type) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(type, prop, fill = type)) +
  geom_col(color = "black") +
  geom_label(aes(label = n)) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(begin = 0.4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Zugravu: Articles en lien avec le sujet des occupations et Israël-Palestine",
    subtitle = "Selon le format de la publication",
    y = "Proportion",
    x = "Format"
  )

ggsave(file = "Results/Figures/Figure_2.png",
       scale = 1.7)

# TODO: Régler le problème 
# df1 %>% 
#   filter(sujet_robust=="Oui",
#          annee==2024) %>% 
#   arrange(publication) %>% 
#   select(titre, publication)

df1 %>% 
  filter(annee == 2024, month(date)==6) %>% 
  arrange(date) %>% 
  select(titre, date)

df1 %>% 
  filter(titre == "Anne Hiltpold: «Le militantisme n’a pas sa place à l’école genevoise»") %>% 
  pull(model)


# Thématique
# TODO: Top des thématiques par année
df1 %>% 
  filter(annee > 2021) %>% 
  mutate(annee = factor(annee)) %>% 
  select(annee, thematique) %>% 
  separate_rows(thematique, sep = ", ") %>% 
  group_by(annee) %>% 
  count(thematique) %>%
  ungroup() %>% 
  mutate(thematique = reorder_within(thematique, n, annee)) %>% 
  slice_max(n, n = 10,
            by = annee) %>% 
  arrange(annee, -n) %>% 
  ggplot(aes(n, thematique, fill = annee)) +
  geom_col() +
  facet_wrap(~annee, scales = "free") +
  scale_y_reordered() +
  theme(legend.position = "top") +
  scale_fill_viridis_d()

# TODO: Elle n'a pas pris parole que dans le temps: donc tout le temps de parole médiatique sur tous les médias romans
df1 %>% 
  filter(publication >= "2024-05-01",
         publication <= "2024-08-01") %>% 
  arrange(publication) %>% 
  select(titre, sujet_robust) %>% 
  print(n = 50)

g3 <- 
  df1 %>%
  mutate(date_2 = floor_date(date, "months"),
         ton = case_when(
           str_detect(ton, "Négatif") ~ "Négatif",
           str_detect(ton, "Positif") ~ "Positif",
           str_detect(ton, "Neutre") ~ "Neutre",
           .default = "Neutre"
         )) %>% 
  count(date_2, ton) %>% 
  ggplot(aes(date_2, n, fill = ton)) +
  geom_col() +
  labs(
    title = "Évolution de la négativité des articles dans le temps"
  ) +
  scale_fill_viridis_d()

ggplotly(g3)


df1 %>% 
  filter(date >= "2024-05-01",
         sujet_robust == "Oui",
         type != "opinion") %>% 
  select(titre)
