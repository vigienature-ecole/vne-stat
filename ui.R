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
library(shinyWidgets)


#import module and fonctions
# lapply(file.path("R", dir("R")), source)

# Define UI for application that draws a histogram
fluidPage(
  useShinyjs(),
  useShinydashboard(),
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
    tags$head(tags$style(type = 'text/css','.navbar{display:none;}')),
    id = "vne_stats",
    # tabPanel(
    #   title = "Connexion à Vigie-Nature École Stats",
    #   value = "login",
    #   mod_login_ui("login")
    # ),
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
          "start_data", 
          "Voir les données brutes", 
          style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"
        )
      ),
      column(
        h3("Paramètres de la représentation du réseau"),
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
        actionButton("start_manip", "Voir le résultat du calcul", 
                     style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"
        )
      ),
      column(
        style='min-height:500px; border: 10px solid white; padding: 10px; border-radius: 20px; background: #DDEDDD', 
        width = 4, align="center",
        h2("Étape 3"),
        h3("Visualiser les données"),
        helpText("Cette étape permet de représenter les données de façon à conclure. Il est possible de faire des graphiques, des cartes ou des tableaux ordonnées."),
        selectInput("visualize", "Sélectionner le type de visualisation", "Choisir un type de visualisation", selected = "Choisir un type de visualisation"),
        htmlOutput("help_visu_ui"),
        actionButton("start_visu", "Visualiser les données", 
                     style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%")
      )
    )
    ,
    # Show a plot of the generated distribution
    tabPanel("Résultats", value = "results",
             HTML("<br>"),
             actionButton("new_analysis_top", "Lancer une nouvelle analyse", 
                          style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"),
             HTML("<br><br>"),
             bsCollapse(id = "results",
                        bsCollapsePanel(
                          title = "Données importées", DT::dataTableOutput("data_output")
                        ),
                        bsCollapsePanel(
                          title = "Manipulation", DT::dataTableOutput("manip_output"),
                          htmlOutput("error_manip") %>% 
                            tagAppendAttributes(style = 'color:green;font-weight: bolder;')
                        ),
                        bsCollapsePanel(
                          title = "Visualisation", 
                          htmlOutput("graph_info"),
                          plotOutput("visu_graph_output"),
                          DT::dataTableOutput("visu_table_output"),
                          htmlOutput("error_map") %>% 
                            tagAppendAttributes(style = 'color:red;font-weight: bolder;'),
                          htmlOutput("error_vis") %>% 
                            tagAppendAttributes(style = 'color:green;font-weight: bolder;'),
                          htmlOutput("help_vis_out") %>% 
                            tagAppendAttributes(style = 'color:green;font-weight: bolder;')
                        )
             ),
             HTML("<br>"),
             actionButton("new_analysis_bottom", "Lancer une nouvelle analyse", 
                          style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"),
             HTML("<br><br>")
    )#,
    # tabPanel("Réseaux", value = "reseau",
    #          actionButton("new_analysis_top_network", "Lancer une nouvelle analyse", 
    #                       style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"),
    #          HTML("<br><br>"),
    #          mod_network_ui("reseaux")
    #          )
  )
)

