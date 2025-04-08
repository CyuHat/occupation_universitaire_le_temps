# Libraries ----
pacman::p_load(
  tidyverse, janitor
)

# Options ----
first_clean <- function(data){
  data %>% 
    select(-1, -2) %>% 
    clean_names()
}

clean_date <- function(date_text){
  date_text %>% 
    str_split_i(".? / ", 1) %>% 
    str_remove_all("Publié le | à \\d{2}:\\d{2}")
}

clean_json <- function(json_text){
  json_text %>% 
    str_remove_all('\\[|\\]|\\{|\\}|\\"|\\:|auteur|author|texte?|content|categories|labels?')
}

minmax <- function(vector){
  list(
    min = min(vector),
    max = max(vector)
  )
}

# Data ----
# Articles en rapport avec les universités
  # Du 28-04-2024 au 04-01-2025
df_uni <-
  read_csv("Data/article_le_temps_universites.csv") %>% 
  first_clean() %>% 
  drop_na(date) %>% 
  transmute(
    titre = title,
    description = abstract,
    date = dmy(map_chr(date, clean_date)),
    auteur = map_chr(author, clean_json),
    contenu = content,
    lien = link_href
  )

write_rds(df_uni, file = "MyData/df_uni.rds")

# Articles sur Israël-Palestine
  # DU 29-04-2024 au 19-12-2024
df_tip <- 
  read_csv("Data/le_temps_israel_palestine.csv") %>% 
  first_clean() %>% 
  transmute(
    titre,
    description = "",
    date = dmy(map_chr(infos, clean_date)),
    auteur = author,
    contenu = map_chr(text, clean_json),
    lien = lien_href
  )

write_rds(df_tip, file = "MyData/df_tip.rds")

# Article sur le compte de Zugravu
  # Du 21-12-2021 au 26-10-2024
df_zug <-
  read_csv("Data/le_temps_zugravu.csv") %>% 
  first_clean() %>% 
  transmute(
    titre,
    description,
    date = dmy(map_chr(infos, clean_date)),
    auteur = map_chr(auteur, clean_json),
    auteur = str_remove(auteur, "^,"),
    contenu = map_chr(texte, clean_json),
    lien = lien_href
  )

write_rds(df_zug, file = "MyData/df_zug.rds")

# Create the initial dataset
df0 <- 
  bind_rows(
    df_uni,
    df_tip,
    df_zug
  ) %>% 
  replace_na(list(
    description = " ",
    auteur = " "
  )) %>% 
  mutate(content_length = str_length(contenu)) %>%
  arrange(titre, -content_length) %>% 
  group_by(titre) %>% 
  slice_head(n=1) %>% 
  select(-content_length) %>% 
  ungroup()

# Save the initial dataset
write_rds(df0, file = "MyData/df0.rds")
