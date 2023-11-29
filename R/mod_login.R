# This module allow the user to login
# on validation the rest of the app is loaded

mod_login_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      h2("Bienvenue sur l'outil statistique de Vigie-Nature École"),
      br(),
      style='min-height:500px; border: 10px solid white; padding: 10px; border-radius: 20px; background: #DDEDDD', width = 6, align="center", offset = 3,
      selectInput(ns("type_login"), "Vous êtes :", c("Choisir une catégorie", "Enseignant ou enseignante", "Élève", "Étudiant ou étudiante", "Chercheuse ou chercheur", "Équipe VNE", "Autre")),
      textInput(ns("type_precis_login"), "Présisez (facultatif sauf si choix autre)"),
      textInput(ns("utilisation_detail"), "Dites nous pourquoi vous utilisez cette application (facultatif)"),
      htmlOutput(ns("error"))  %>% 
        tagAppendAttributes(style = 'color:red;font-weight: bolder;'),
      actionButton(ns("valid_login"), "Valider")
    )
  )
}


mod_login_server <- function(id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # When user press validate
    # Record values, go to application page
    observeEvent(input$valid_login, {
      
      
      
      # check errors
      error = FALSE
      text_error = NULL
      
      if (input$type_login == "Choisir une catégorie") {
        error = TRUE
        text_error <- paste(text_error, "\nchoisir une catégorie")
      }
      
      if (input$type_login == "Autre" & input$type_precis_login == ""){
        error = TRUE
        text_error <- paste(text_error, "\najouter une description de catégorie autre")
      }
      
      output$error <- renderText(
        ifelse(error, 
               paste("Impossible de passer à l'étape suivante : pour continuer, vous devez :", text_error),
               NULL
        )
      )
      
      if(!error){
        cat("login to app\n")
        # store login info into file
        #Create a unique file name
        fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
        cat("write login information")
        dir.create("data/data_users_temp")
        # Write the file to the local system
        write.csv(
          data.frame(type_login = input$type_login,
                     type_precis_login = input$type_precis_login,
                     utilisation_detail = input$utilisation_detail,
                     time = Sys.time()),
          file = file.path("data/data_users_temp", fileName), 
          row.names = FALSE, quote = TRUE
        )
        
        # go to input page
        updateTabsetPanel(session = parent_session, "vne_stats",
                          selected = "input")
      }
      
    })
  })
}
