# Libraries ----
pacman::p_load(
  tidyverse, 
  janitor,
  rollama,
  tictoc
  )

# Options ----
options(rollama_model = "qwen2.5:14b")

extract_info <- function(text){
  
  sysmsg <- "Dans le cadre d'une recherche sur les occupations des universités à la suite du conflit entre Israël et la Palestine, tu es une machine avancée de traitement naturel du langage pour les chercheurs. Tu aides les chercheurs à detecter les sujets et les labels. Le sujet concerne l'occupation des universités en Suisse à la suite du conflit entre Israël et la Palestine. Indique si le texte est à propos des occupations universitaires qui ont fait suite au conflit entre Israël et la Palestine [{oui} ou {non}], la position du texte sur les occupations universitaies à savoir si l'auteur du texte semble contre les occupations des universités est neutre ou est pour [{pour}, {contre} ou {neutre}], dans quel lieu ça se passe en précisant le pays (ex. {Genève, Suisse}), qui sont les acteurs (ex. {étudiants, professeurs, Netanyahou}), le ton du texte [{positif}, {negatif} ou {neutre}], les critiques adressées dans le texte (ex. {naïveté}). Voici un exemple de format.
  [sujet: {oui}; position: {contre}; lieu: {Genève}; acteurs: {étudiants, professeurs}; ton: {négatif}; critique: {naïveté}; type: {chronique}]
  Voici trois exemples: 
  "
  
  sysexp <- c(
  "A Bethléem, les habitants pensent plus à Gaza qu’aux célébrations de NoëlCette année, à Bethléem, il n’y a pas eu de cérémonie pour l’illumination du fameux sapin de douze mètres de haut. Ce dernier est d’ailleurs inexistant, tout comme la crèche en bois d’olivier et le marché de Noël. Il y a quelques taxis çà et là, deux policiers et des habitants attablés aux terrasses d’un café sous les arcades.",
  "[sujet: {non}; position: {contre}; lieu: {Bethléem, Palestine}; acteurs: {habitants de Bethléem}; ton: {négatif}; critique: {}; type: {article}; type: {article}]",
  "Une vaste civilisation amazonienne dévoilée par les yeux perçants d’un laser «Claude Lévi-Strauss disait de l’Amazonie qu’elle est un Moyen Age dont on ne connaîtrait pas la Rome. D’une certaine manière, on peut dire que le lidar complète l’archéologie de terrain pour nous dévoiler en filigrane ce qu’aurait pu être cette Rome.» L’ethno-anthropologue français Philippe Descola, spécialiste des populations d’Amazonie équatorienne, ne cache pas sa satisfaction concernant les résultats de l’étude qu’il cosigne dans Science.",
  "[sujet: {non}; position: {}; lieu: {Amazonie, Brésil}; acteurs: {Philippe Descola}; ton: {neutre}; critique: {}; type: {article}]",
  "A Columbia, au cœur de la bataille idéologique autour d’Israël On rentre dans le campus de l’université de Columbia, dans l’Upper West Side de Manhattan à New York, presque comme on franchirait le portique d’un aéroport. Un agent de sécurité s’égosille pour que les étudiants sortent à l’avance leur carte d’identification universitaire. Kippa sur la tête, un jeune parlemente avec un employé de l’université. Cet étudiant rabbinique se forme dans un séminaire théologique situé juste à côté. Il a parfois des cours à Columbia mais n’a pas le précieux sésame.",
  "[sujet: {oui}; position: {neutre}; lieu: {Université de Columbia, Manhattan à New York, États-Unis}; acteurs: {agent de sécurité, étudiants}; ton: {négatif}; critique: {idéologique}; type {chronique}]",
  "Ne répond que dans le format demandé et ne répond rien d'autre. Ne fait pas de résumé, si tu ne peux pas utiliser le modèle que je t'ai donné, indique au moin si le texte est en rapport avec le sujet. Voici le texte:"
  )
  
  q <- 
    tribble(
    ~role,    ~content,
    "system", sysmsg,
    "user", sysexp[1],
    "assistant", sysexp[2],
    "user", sysexp[3],
    "assistant", sysexp[4],
    "user", sysexp[4],
    "assistant", sysexp[4],
    "system", sysexp[4],
    "user", text
  )
  
  query(q, model_params = list(seed = 7))$message$content

}

# Data ----
df0 <- 
  read_rds("MyData/df0.rds") %>% 
  mutate(
    full_text = str_c(lien, titre, description, contenu),
    full_text = str_trunc(full_text, 5000)
    )

# ~4h
df1 <-
  df0 %>%
  mutate(
    info = map_chr(full_text, extract_info, .progress = TRUE),
    info = str_extract(info, "\\[sujet.*\\]")
    )

# Sauvegarde des données
write_rds(df1, file = "MyData/df1.rds")