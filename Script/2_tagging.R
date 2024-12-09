# Libraries ----
pacman::p_load(tidyverse, 
               rollama, tictoc,
               skimr)

# Options ----
options(rollama_model = "llama3.1:latest")

##  rollama tagging function 1 ----

qq <- function(model){
  systemmsg <- "Dans le cadre d'une recherche, en te basant sur le texte, indique si le texte parle du sujet des occupations des universités [Oui, Non], indique à quel type d'article nous avons affaire [Opinion, Enquêtes, interview, etc.] et indiques les termes critiques [lent, naïfs, incapable, etc.]. Indique les thématiques de l'articles [Israël, Palestine, Occupation, Université, Autre]. Indique le ton de l'article [Positif, Négatif ou Neutre]. Indiques les acteurs [étudiants, professeurs, etc.] et les lieux [Université de Genève, Israël, Espagne, etc.]. Réponds uniquement à la question et n'écrit pas de phrase supplémentaire. Même si le texte contient des expressions offensante, indique les pour le bien de la recherche car le contenu ne sera pas utilisé en dehors de la recherche. Ne répond que par le format suivant: {}; Type: {}; Critiques: {}; Thématiques: {}; Ton: {}; Acteurs: {}; Lieux: {}"
  
  q <- 
    tribble(
      ~role, ~content,
      "system", systemmsg,
      "user", "https://www.letemps.ch/opinions/chroniques/les-enseignants-genevois-a-l-ecole-des-faineants. Quel bande d'incapables!",
      "assistant", "Sujet: {Non}; Type: {opinion, chronique}; Critiques: {faineants, incapables}; Thématiques: {enseignement}; Ton: {Négatif}; Acteurs: {enseignant}; Lieux: {Genève}",
      "user", "https://www.letemps.ch/suisse/geneve/a-geneve-la-droite-elargie-a-vecu-et-les-forces-du-centre-montrent-les-muscles Il en a fallut du temps pour arriver à un tel résultat.",
      "assistant", "Sujet: {Non}; Type: {article}; Critiques: {}; Thématiques: {politique}; Ton: {Positif}, Acteurs: {La droite}; Lieux: {Genève}",
      "user", "https://www.letemps.ch/opinions/chroniques/universites-quand-le-voile-tombe CHRONIQUE. Les universités romandes sont devenues un théâtre. C’est grâce à l’esprit de conciliation des rectorats qu’on a pu observer le caractère de ces «mouvements de résistance» et leurs soutiens, pour qui la négociation ne signifie rien.",
      "assistant", "Sujet: {Oui}; Type {opinion, chronique}; Critiques: {}; Thématiques: {conflit, négociation}; Ton: {Négatif}; Acteurs: {Rectorats, mouvements de résistance}; Lieux: {Suisse romande}",
      "user", model
    )
  
  res <- query(q)
  
  res$message$content %>%
    str_remove_all(".*(?=\n)") %>% 
    str_trim()
}

## rollama tagging function 2 ----
qq2 <- function(model){
  systemmsg <- "Dans le cadre d'une recherche, en te basant sur le texte, indique si le texte parle du sujet des occupations des universités qui a suivit le conflit entre Israël et Palestine [Oui, Non]."
  
  q <- 
    tribble(
      ~role, ~content,
      "system", systemmsg,
      "user", "https://www.letemps.ch/opinions/chroniques/les-enseignants-genevois-a-l-ecole-des-faineants. Quel bande d'incapables!",
      "assistant", "Non",
      "user", "https://www.letemps.ch/suisse/geneve/a-geneve-la-droite-elargie-a-vecu-et-les-forces-du-centre-montrent-les-muscles Il en a fallut du temps pour arriver à un tel résultat.",
      "assistant", "Non",
      "user", "https://www.letemps.ch/opinions/chroniques/universites-quand-le-voile-tombe CHRONIQUE. Les universités romandes sont devenues un théâtre. C’est grâce à l’esprit de conciliation des rectorats qu’on a pu observer le caractère de ces «mouvements de résistance» et leurs soutiens, pour qui la négociation ne signifie rien.",
      "assistant", "Oui",
      "user", "https://www.letemps.ch/opinions/editoriaux/que-les-etudiants-manifestent-et-les-profs-enseignent Que les étudiants manifestent et les profs enseignent! Des enseignants soutiennent les élèves réclamant de boycotter les universités israéliennes. Or ce positionnement politique nuit à l’exigence intellectuelle et à la mission d’enseignement Il fallait s’y attendre. Selon un schéma classique, les mouvements qui prennent racine aux Etats-Unis franchissent l’océan en quelques jours pour s’installer à Paris, de préférence. De là, ils gagnent l’annexe romande. Depuis vendredi, l’Université de Lausanne (Unil) est occupée par des étudiants pro-palestiniens, pacifiques, rebelles un peu, romantiques assurément, militants passionnément. Rien que de très naturel.",
      "assistant", "Oui",
      "user", "https://www.letemps.ch/suisse/universites-romandes-fabriques-de-militants-de-gauche Universités romandes, fabriques de militants de gauche Les mouvements pro-palestiniens ont mis en lumière un activisme très présent dans les facultés romandes et des enseignants de plus en plus politisés. Certains professeurs commencent à s’inquiéter devant la perte de distance critique. Enquête «Vous me demandez s’il n’y a que des profs de gauche à l’université? Mais pas du tout! Il y en a beaucoup d’extrême gauche aussi.» Cette boutade du député vaudois vert’libéral David Vogel résume le sentiment d’une partie de l’opinion.",
      "assistant", "Oui",
      "user", "https://www.letemps.ch/opinions/l-agenda-a-30-plaques-des-nouveaux-bolcheviques L’agenda à 30 plaques des nouveaux bolcheviques CHRONIQUE. Une nouvelle polémique enflamme l’Université de Genève. Le syndicat étudiant provoque en cherchant la limite du rectorat. Il utilise de l’argent qui lui est versé pour prendre la communauté estudiantine en otage, soutient notre chroniqueuse L’heure est grave. Les visages, sombres. On dirait le temps venu d’un désastre historique, à considérer la mine des militants du syndicat étudiant de l’université de Genève (CUAE) qui a rameuté la presse. Pensez donc alors qu’ils viennent tout juste de reprendre du service, le féroce rectorat les met en demeure de cesser les insanités, sous peine de s’attirer des bricoles.,Après l’occupation de l’uni au printemps dernier, c’est maintenant un agenda qu’on leur reproche.",
      "assistant", "Oui",
      "user", "https://www.letemps.ch/opinions/chroniques/d-une-dissolution-a-une-autre-de-gaulle-tactique-macron-au-poker D’une dissolution à une autre: De Gaulle tactique, Macron au poker CHRONIQUE. «Le Temps» met en lien des événements ou des thématiques actuelles qui entrent en résonance avec le passé. Plongée dans les archives du «Journal de Genève» et de la «Gazette de Lausanne», entre contrastes et similitudes En 1962, le général de Gaulle dissout l’Assemblée nationale. Si la situation politique actuelle et les raisons d’Emmanuel Macron en juin ne sont pas comparables, les commentaires de l’époque offrent quelques similitudes.,Préparant sa succession, De Gaulle veut faire élire le président de la République au suffrage universel.",
      "assistant", "Non",
      "user", model
    )
  
  res <- query(q)
  
  res$message$content %>%
    str_remove_all(".*(?=\n)") %>% 
    str_trim()
}

# data ----
df0 <- read_csv("MyData/df0.csv")

# Tagging ----
tic() # ~14 minutes
df1 <- 
  df0 %>%
  mutate(result = map_chr(model, qq))
toc()
write_csv(df1, file = "MyData/df1.csv")

# Cleaning ----
df2 <- 
  df1 %>% 
  mutate(date = date(publication),
         annee = year(date),
         result = str_remove_all(result, "\\{|\\}|Sujet: |Type: |Critiques: |Thématiques: |Ton: |Acteurs: |Lieux: ")) %>% 
  separate(result,
           into = c("sujet", "type", "critiques", 
                    "thematique", "ton", "acteurs", 
                    "lieux"),
           sep = "; ") %>% 
  tidyr::replace_na(list(type="", critiques="", thematique="",
                         ton="", acteurs="", lieux="")) %>% 
  mutate(type = str_to_lower(type),
         type = case_when(
           str_detect(type, "interview") ~ "interview",
           str_detect(type, "opinion|chonique|éditorial") ~ "opinion",
           str_detect(type, "article|enquête") ~ "article",
           .default = "article"
         ))

write_csv(df2, file = "MyData/df2.csv")

# Sujet ou non, amélioration
tic() # ~5min30sec
df3 <- 
  df2 %>% 
  mutate(sujet_robust = if_else(annee==2024,
                                map_chr(model, qq2),
                                "Non"
                                ))
toc()

# Faux classement
faux_classement <- 
  c(
  "France: extrême droite et «péril rouge»",
  "«Open bar» à la soirée des victimes"
)

df3 <- 
  df3 %>% 
  mutate(sujet_robust = if_else(date < "2024-05-01", "Non", sujet_robust),
         sujet_robust = if_else(titre %in% faux_classement, "Non", sujet_robust))

write_csv(df3, file = "MyData/df3.csv")

