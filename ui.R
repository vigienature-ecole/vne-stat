#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs) # to hide / show 
library(shinyBS) # for the collapse part 
library(magrittr)
#library(shinyWidgets)


#import module and fonctions
lapply(file.path("R", dir("R")), source)

# Define UI for application that draws a histogram
fluidPage(
  useShinyjs(),
  # add custom css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  div(
    style="padding: 1px 0px; width: '100%'",
    titlePanel(
      title="", windowTitle="VNE-stats"
    )
  ),
  navbarPage(
    # remove the nav bar (navigation will be only working with buttons)
    tags$head(tags$style(type = 'text/css','.navbar{display:none;}')),
    id = "vne_stats",
    # Login page ----
    tabPanel(
      title = "Connexion à Vigie-Nature École Stats",
      value = "login",
      mod_login_ui("login")
    ),
    # Control page ----
    tabPanel(
      title = "Lancer une analyse de données",  
      value = "input",
      column(
        style='min-height:500px; border: 10px solid white; padding: 10px; border-radius: 20px; background: #DDEDDD', width = 4, align="center",
        h2("Étape 1"),
        h3("Importer des données"),
        selectInput(
          "import",
          "Sélectionner les données à importer", 
          "Choisir un jeu de données",
        ),
        htmlOutput("help_import_ui"),
        actionButton(
          "view_raw_data", 
          "Voir les données brutes", 
          style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"
        )
      ),
      column(
        style='min-height:500px; border: 10px solid white; padding: 10px; border-radius: 20px; background: #DDEDDD', width = 4, align="center",
        h2("Étape 2"),
        h3("Manipuler les données"),
        helpText("Cette étape permet de passer de données brutes (qui sont sur des milliers de lignes) à des données résumées ou filtrées (entre une et une centaines de lignes) qu’il sera possible d’interpréter."),
        selectInput("manipulate", 
                    "Sélectionner le calcul à effectuer", 
                    "Choisir une opération"),
        htmlOutput("help_manip"),
        selectInput("variable_group", "Regrouper vos résultats selon une variable (facultatif)", "Choisir une variable", selected = "Choisir une variable"),
        selectInput("variable_filter", "Filtrer selon une variable (facultatif)", "Choisir une variable", selected = "Choisir une variable"),
        textOutput("help_level"),
        selectInput("variable_level", "Filtrer selon une valeur de la variable", "Choisir une valeur", selected = "Choisir une valeur"),
        selectInput("species_filter", "Filtrer selon une espece (facultatif)", "Choisir une espèce", selected = "Choisir une espèce"),
        actionButton("view_res_manip", "Voir le résultat du calcul", 
                     style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"
        )
      ),
      column(
        style='min-height:500px; border: 10px solid white; padding: 10px; border-radius: 20px; background: #DDEDDD', 
        width = 4, align="center",
        h2("Étape 3"),
        h3("Visualiser les données"),
        helpText("Cette étape permet de représenter les données de façon à conclure. Il est possible de faire des graphiques, des cartes ou des tableaux ordonnées."),
        htmlOutput("help_visu_ui"),
        actionButton("view_res_visu", "Visualiser les données", 
                     style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%")
      )
    )
    ,
    # Classical result page ----
    tabPanel("Résultats", value = "results",
             HTML("<br>"),
             actionButton("new_analysis_top", "Retour à la page de contrôle", 
                          style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"),
             HTML("<br><br>"),
             bsCollapse(id = "results",
                        bsCollapsePanel(
                          title = "Données importées", tags$div(style = 'overflow-x: scroll',
                                                                DT::dataTableOutput("data_output")
                          )
                        ),
                        bsCollapsePanel(
                          title = "Manipulation", 
                          tags$div(style = 'overflow-x: scroll',
                          DT::dataTableOutput("manip_output"),
                          htmlOutput("error_manip") %>% 
                            tagAppendAttributes(style = 'color:green;font-weight: bolder;')
                          )
                        ),
                        bsCollapsePanel(
                          
                          title = "Visualisation", 
     
                          tags$div(style = 'overflow-y: scroll; height: 70vh',
                          htmlOutput("title_graph") |>
                            tagAppendAttributes(style = 'font-weight: bolder;text-align: center;font-size: x-large;'),
                          plotOutput("visu_graph_output"),
                          htmlOutput("error_vis") %>% 
                            tagAppendAttributes(style = 'color:green;font-weight: bolder;'),
                          htmlOutput("help_vis_out") %>% 
                            tagAppendAttributes(style = 'color:green;font-weight: bolder;'),
                          br(),
                          br(),
                          br(),
                          br(),
                          helpText("la méthode utilisée pour produire les graphiques est une méthode d’exploration des données brutes (data visualisation), et ne peut être en aucun cas utilisée pour directement publier des résultats. En effet, pour répondre de façon scientifique à des questions en écologie, il faudrait effectuer certaines corrections statistiques pour prendre en compte certains biais, notamment liés à l’échantillonnage. Il faut donc être très prudent dans l’interprétation de ces résultats.")
                          )
                        )
             ),
             HTML("<br>"),
             actionButton("new_analysis_bottom", "Retour à la page de contrôle", 
                          style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"),
             HTML("<br><br>")
    ),
    # Insect network page ----
    tabPanel("Réseaux", value = "reseau",
             actionButton("new_analysis_top_network", "Retour à la page de contrôle",
                          style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"),
             HTML("<br><br>"),
             mod_network_ui("reseaux")
             ),
    # Biolit network page ----
    tabPanel("Réseaux biolit", value = "reseau_biolit",
             actionButton("new_analysis_top_network_biolit", "Retour à la page de contrôle",
                          style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"),
             HTML("<br><br>"),
             mod_network_biolit_ui("reseaux_biolit")
    ),
    # Sauvage habitats result page ----
    tabPanel("Habitats sauvages", value = "sauvages_habitats",
             actionButton("new_analysis_top_sauvages_habitats", "Retour à la page de contrôle",
                          style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"),
             HTML("<br><br>"),
             mod_habitats_sauvages_ui("habitats_sauvages")
    ),
    # Birdmap page ----
    tabPanel("Carte oiseaux", value = "map_birds",
             actionButton("new_analysis_top_map_birds", "Retour à la page de contrôle",
                          style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"),
             HTML("<br><br>"),
             mod_map_birds_ui("map_birds")
    ),
    # study_plant_spipoll page ----
    tabPanel("Carte insectes", value = "study_plant_spipoll",
             actionButton("new_analysis_top_study_plant_spipoll", "Retour à la page de contrôle",
                          style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"),
             HTML("<br><br>"),
             mod_study_plant_spipoll_ui("study_plant_spipoll")
    )
  )
)

