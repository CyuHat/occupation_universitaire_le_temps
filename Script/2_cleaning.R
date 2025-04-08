# Libraries ----
pacman::p_load(
  tidyverse, janitor, gt
)

remove_brackets <- function(text, other="sujet"){
  str_remove_all(
    text,
    str_c("\\{|\\}|\\:|", other)
  ) %>% 
    str_trim()
}

# Options ----


# Data ----
df1 <- 
  read_rds("MyData/df1.rds") %>% 
  mutate(
    auteur = str_trunc(auteur, 20)
  )

df2 <- 
  df1 %>% 
  mutate(
    info = str_remove_all(info, "\\[|\\]")
  ) %>%
  separate(info, 
           into = c("sujet", "position", 
                    "lieu", "acteurs", "ton",
                    "critique", "type"),
           sep = "; "
           ) %>% 
  mutate(
    sujet = remove_brackets(sujet, "sujet"),
    position = remove_brackets(position, "position"),
    lieu = remove_brackets(lieu, "lieu"),
    acteurs = remove_brackets(acteurs, "acteurs"),
    ton = remove_brackets(ton, "ton"),
    critique = remove_brackets(critique, "critique"),
    type = remove_brackets(type, "type"),
    palestine = str_detect(full_text, "(P|p)alestinn?(ien|e)?"),
    wokisme = str_detect(full_text, "(W|w)ok(ism|st)?es?")
  ) %>% 
  filter(
    sujet=="oui" | palestine,
    date >= "2024-05-01",
    !str_detect(lien, "google"),
    str_detect(lieu, "(S|s)uisse")
  ) %>% 
  select(-sujet, -palestine)

# Save the dataset
write_rds(df2, file = "MyData/df2.rds")

# TEST ----
# Tableau
df2 %>% 
  arrange(titre, desc(type), desc(position), desc(ton)) %>% 
  group_by(titre) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  select(titre, date, url=lien, auteur, texte=contenu, type, lieu, acteurs, critique, position) %>%
  mutate(
    url = str_c(
      '<a href="',
      url,
      '">lien</a>'
    ),
    url = map(url, html),
    texte = str_trunc(texte, 75)
  ) %>% 
  mutate(
    across(type:position, remove_long)
  ) %>% 
  distinct() %>% 
  arrange(date) %>% 
  DT::datatable()

# TASK ----