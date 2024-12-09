# Libraries ----
pacman::p_load(tidyverse, janitor)

# Options ----

# Data ----
df0 <- 
  read_csv("Data/le_temps_zugravu.csv") %>% 
  clean_names() %>% 
  select(-1:-2, -lien, -lieu) %>% 
  mutate(
    auteur = str_remove_all(auteur, '\\[|\\]|\\{|\\}|\\"|\\:|auteur'),
    texte = str_remove_all(texte, '\\[|\\]|\\{|\\}|\\"|\\:|texte'),
    infos = str_remove_all(infos, "(Publié|Modifié) le ")) %>%
  separate(infos, into = c("publication", "modification"), 
           sep = "\\. \\/ ") %>% 
  mutate(
    publication = dmy_hm(publication),
    modification = dmy_hm(modification),
    model = paste(lien_href, titre, description, texte)
  )

write_csv(df0, "MyData/df0.csv")

df0 <- 
  read_csv("Data/le_temps_zugravu.csv") %>% 
  clean_names() %>% 
  select(-1:-2, -lien, -lieu) %>% 
  mutate(
    auteur = str_remove_all(auteur, '\\[|\\]|\\{|\\}|\\"|\\:|auteur'),
    texte = str_remove_all(texte, '\\[|\\]|\\{|\\}|\\"|\\:|texte'),
    infos = str_remove_all(infos, "(Publié|Modifié) le ")) %>%
  separate(infos, into = c("publication", "modification"), 
           sep = "\\. \\/ ") %>% 
  mutate(
    publication = dmy_hm(publication),
    modification = dmy_hm(modification),
    model = paste(lien_href, titre, description, texte)
  )

write_csv(df0, "MyData/df0.csv")