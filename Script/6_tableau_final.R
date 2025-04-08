# Library ----
pacman::p_load(
  tidyverse, DT
)

span <- function(x, color="blue"){
  str_c('<span style="color:', color, '">', x, "</span>")
}

# Data ----
for_url <- 
  read_csv("Data/article_le_temps_universites.csv") %>% 
  select(
    titre = title,
    url = `link-href`
  )

read_csv("MyData/uni1.csv") %>%
  left_join(for_url) %>% 
  filter() %>% 
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
         !str_detect(titre, "Les députés allemands|Une attaque israélienne tue trois humanitaires à Gaza|Jésus, Trump et les souliers"),
         suisse) %>% 
  select(-suisse, -info, -time, -modifie, -model, -position, -ton) %>% 
  rename_with(~str_remove(.x, "\\d|google_|_href")) %>% 
  mutate(
    url = str_c('<a href="', url, '">Lien</a>'),
    date = date(date),
    texte = str_trunc(texte, 100),
    ton = if_else(ton == "positif", "neutre", ton),
    # position = position3,
    position = case_match(
      position,
      "pour" ~ span("pour", "green"),
      "contre" ~ span("contre", "red"),
      .default = span(position, "blue")
    )
  ) %>% 
  select(-ton) %>% 
  arrange(date) %>% 
  DT::datatable(escape = FALSE)
