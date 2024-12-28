# Libraries ----
pacman::p_load(tidyverse, janitor, elmer, tictoc)

# Options ----
sysmsg <- "
  Dans le cadre d'une recherche sur les occupations des universités à la suite du conflit entre Israël et la Palestine, tu es une machine avancée de traitement naturel du langage pour les chercheurs. Tu aides les chercheurs à detecter les sujets et les labels. Le sujet concerne l'occupation des universités en Suisse à la suite du conflit entre Israël et la Palestine. Indique si le texte est à propos des occupations universitaires qui ont fait suite au conflit entre Israël et la Palestine [{oui} ou {non}], la position du texte sur les occupations universitaies à savoir si l'auteur du texte semble contre les occupations des université est neutre ou est pour [{pour}, {contre} ou {neutre}], dans quel lieu ça se passe (ex. {Genève}), qui sont les acteurs (ex. {étudiants, professeurs, Netanyahou}), le ton du texte [{positif}, {negatif} ou {neutre}], les critiques adressées dans le texte (ex. {naïveté}). Voici un exemple de format.
  sujet: {oui}; position: {contre}; lieu: {Genève}; acteurs: {étudiants, professeurs}; ton: {négatif}; critique: {naïveté}
  "

sysexp <- "
Voici trois exemples: 

A Bethléem, les habitants pensent plus à Gaza qu’aux célébrations de NoëlCette année, à Bethléem, il n’y a pas eu de cérémonie pour l’illumination du fameux sapin de douze mètres de haut. Ce dernier est d’ailleurs inexistant, tout comme la crèche en bois d’olivier et le marché de Noël. Il y a quelques taxis çà et là, deux policiers et des habitants attablés aux terrasses d’un café sous les arcades.
sujet: {non}; position: {contre}; lieu: {Bethléem}; acteurs: {habitants de Bethléem}; ton: {négatif}; critique: {}

Une vaste civilisation amazonienne dévoilée par les yeux perçants d’un laser «Claude Lévi-Strauss disait de l’Amazonie qu’elle est un Moyen Age dont on ne connaîtrait pas la Rome. D’une certaine manière, on peut dire que le lidar complète l’archéologie de terrain pour nous dévoiler en filigrane ce qu’aurait pu être cette Rome.» L’ethno-anthropologue français Philippe Descola, spécialiste des populations d’Amazonie équatorienne, ne cache pas sa satisfaction concernant les résultats de l’étude qu’il cosigne dans Science.
sujet: {non}; position: {}; lieu: {Amazonie}; acteurs: {Philippe Descola}; ton: {neutre}; critique: {}

A Columbia, au cœur de la bataille idéologique autour d’Israël On rentre dans le campus de l’université de Columbia, dans l’Upper West Side de Manhattan à New York, presque comme on franchirait le portique d’un aéroport. Un agent de sécurité s’égosille pour que les étudiants sortent à l’avance leur carte d’identification universitaire. Kippa sur la tête, un jeune parlemente avec un employé de l’université. Cet étudiant rabbinique se forme dans un séminaire théologique situé juste à côté. Il a parfois des cours à Columbia mais n’a pas le précieux sésame.
sujet: {oui}; position: {neutre}; lieu: {Université de Columbia, Manhattan à New York}; acteurs: {agent de sécurité, étudiants}; ton: {négatif}; critique: {idéologique}
"

chat <- chat_ollama(
  model = "qwen2.5:14b",
  system_prompt = c(sysmsg, sysexp)
)



# Data ----
# lt0 <- 
  # read_csv("Data/Occupation.csv",
              # col_select = c(-1,-2, 
              #  # -url, -description,
              #  lien, 
              #  -titre))
lt0 <- 
  read_csv("Data/le_temps_israel_palestine.csv") %>% 
  clean_names() %>% 
  select(-1, -2) %>% 
  select(google_titre = titre,
         info = infos,
         auteur = author,
         text,
         labels
         ) %>% 
  mutate(google_titre = str_remove(google_titre, "Plus - "),
         time = str_extract(info, "\\d{2}\\:\\d{2}"),
         time = str_c("00:", time),
         time = hms(time),
         date = str_split_i(info, "Modifié le", 1),
         date = str_remove_all(date, "Publié le | à | \\/ "),
         date = dmy_hm(date),
         modifie = str_split_i(info, "Modifié le", 2),
         modifie = str_remove_all(modifie, "à |\\."),
         modifie = dmy_hm(modifie),
         texte = str_remove_all(text, '\\[|\\]|\\{|\\}|\\"|\\:|text'),
         labels = str_remove_all(labels, '\\[|\\]|\\{|\\}|\\"|\\:|labels')
         # auteur = str_replace_na(auteur)
         ) %>% 
  select(-text) %>% 
  arrange(date) %>% 
  # slice(-1, -2) %>% 
  filter(texte!="")
  # TODO: Nettoyer les auteurs 

write_csv(lt0, file = "MyData/lt0.csv")

# Tagging
tic() # 20min
lt1 <-
  lt0 %>% 
  mutate(model = str_c(google_titre, texte, sep = " "),
         result = map_chr(model, chat$chat, .progress = TRUE)
         ) %>% 
  separate(result,
           into = c("sujet", "position", "lieu", "acteur", "ton", "critique"),
           sep = "; ") %>% 
  mutate(
    sujet = str_remove_all(sujet, "sujet: \\{|\\}"),
    position = str_remove_all(position, "position: \\{|\\}"),
    lieu = str_remove_all(lieu, "lieu: \\{|\\}"),
    acteur = str_remove_all(acteur, "acteurs: \\{|\\}"),
    ton = str_remove_all(ton, "ton: \\{|\\}"),
    critique = str_remove_all(critique, "critique: \\{|\\}"),
    sujet = if_else(!sujet %in% c("oui", "non"), "oui", sujet)
  )
toc()

write_csv(lt1, "MyData/lt1.csv")

lt1 %>% 
  filter(auteur == "Laure Lugon Zugravu") %>% 
  pull(google_titre)
  glimpse()
  count(auteur, sort = TRUE) %>% 
  print(n = 100)

lt1 %>% 
  filter(sujet == "oui", str_detect(model, "(U|u)niversité")) %>% 
  count(auteur, sort = TRUE)