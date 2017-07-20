
create_assignment <- function() {

  library(miniUI)
  library(shiny)
  ui <- miniPage(

    ## Define the UI blocks
    gadgetTitleBar("Create an assignment"),
    miniContentPanel(
      fillRow(flex = c(7, 3), height = "60px",
              uiOutput("i_source")),
      fillRow(flex = c(7, 3), height = "60px",
              uiOutput("i_custom")),
      fillRow(flex = c(7, 3), height = "60px",
              uiOutput("i_lang")),
      fillRow(flex = c(7, 3), height = "60px",
              textInput(inputId = "o_title", label = "Assignment title", placeholder = "type your title here!")),
    fillRow(flex = c(7, 3), height = "60px",
            uiOutput("i_domain")),
      fillRow(flex = c(7, 3), height = "60px",
              uiOutput("i_exercise")),
      htmlOutput("preview")
    )
  )

  server <- function(input, output, session) {


    # Data source
    sources <- c("edu", "edudata", "custom")
    names(sources) <- c("Demo exercises from edu-package",
                        "From edudata-package",
                        "own custom package"
                        )

    output$i_source <- renderUI({
      radioButtons("o_source",
                   label = "Select source for exercises",
                   choices =  sources,
                   selected = "edu",
                   inline = TRUE,
                   width = "98%")
    })

    ## If source "local" selected, then display this dialog for choosing the local files
    # output$i_files <- renderUI({
    #   if (input$o_source == "local"){
    #     fileInput(inputId = "o_files",
    #               label = "Add local exercises",
    #               accept = c(".yaml", ".yml"),
    #               multiple = TRUE,
    #               buttonLabel = "Scan",
    #               placeholder = "Select yml-files")
    #   } else list()
    # })

    output$i_custom <- renderUI({
      if (input$o_source == "custom"){
        selectInput(inputId = "o_custom",
                    label = "Select your own custom package",
                    choices = installed.packages()[, 1],
                    selected = "edudata")
      } else list()
    })

    ## Scan assignments based on select source

    scan_assingments <- reactive({

      # scan assignments
      ## if from edu-package, then just read all the rda files in ./data/ folder and pile them up
      require(dplyr)
      if (input$o_source == "edu"){
        datasets <- data(package = "edu")$result[, "Item"]
        all_assignments <- data_frame()
        for (i in datasets){
          tmp <- get(data(list = i, package = "edu", envir = environment()))
          all_assignments <- bind_rows(all_assignments,tmp)
        }
      }
      ## if from edudata-package, then just read all the rda files in ./data/ folder and pile them up
      if (input$o_source == "edudata"){
        if ("edudata" %in% installed.packages()[, 1]){

          datasets <- data(package = "edudata")$result[, "Item"]
          all_assignments <- data_frame()
          for (i in datasets){
            tmp <- get(data(list = i, package = "edudata", envir = environment()))
            all_assignments <- bind_rows(all_assignments,tmp)
          }

        } else {
          stop("Install edudata-package from Github using devtools::install_github('ropengov/edudata')")
        }
      }
      # if (input$o_source == "local"){
      #
      #   inFile <- input$o_files
      #
      #   assign_ymls <- inFile$datapath
      #   all_assignments <- data_frame()
      #   for (i in assign_ymls){
      #     tmp <- yaml::yaml.load_file(i)
      #     tmp_d <- plyr::ldply(tmp, data.frame, stringsAsFactors=FALSE)
      #     all_assignments <- bind_rows(all_assignments,tmp_d)
      #   }
      # }
      if (input$o_source == "custom"){

          datasets <- data(package = input$o_custom)$result[, "Item"]
          all_assignments <- data_frame()
          for (i in datasets){
            tmp <- get(data(list = i, package = input$o_custom, envir = environment()))
            all_assignments <- bind_rows(all_assignments,tmp)
          }

      }

      all_assignments[is.na(all_assignments)] <- ""
      return(all_assignments)
    })

    output$i_lang <- renderUI({

      all_assignments <- scan_assingments()
      langs <- gsub("lang_", "", names(all_assignments)[grepl("lang", names(all_assignments))])

      radioButtons("o_lang",
                  label = "Select language",
                  choices =  langs,
                  selected = "en",
                  inline = TRUE,
                  width = "98%")
    })

    output$i_domain <- renderUI({

      all_assignments <- scan_assingments()
      domains <- unique(all_assignments$domain)

      selectInput("o_domain",
                label = "Select domains",
                choices =  domains,
                selected = domains[1],
                # inline = TRUE,
                multiple = TRUE,
                selectize = TRUE)
    })

    output$i_exercise <- renderUI({

      all_assignments <- scan_assingments()
      lang_ver <- paste0("lang_", input$o_lang)

      ids <- all_assignments[all_assignments$domain %in% input$o_domain,][["id"]]
      # Replace all non-translated exercise + language pairs with text "Translation not available"
      exercises <- all_assignments[all_assignments$id %in% ids,][[lang_ver]]
      exercises <- ifelse(is.na(exercises), "Translation not available", exercises)
      domains <- all_assignments[all_assignments$id %in% ids,][["domain"]]
      grades <- all_assignments[all_assignments$id %in% ids,][["grade"]]
      grades <- ifelse(is.na(grades), "not graded", grades)

      names(ids) <- paste0(domains,": '",grades, "' " , exercises)


      checkboxGroupInput("o_exercise",
                label = "Select exercises",
                choices =  ids,
                selected = ids[1], width = "98%")
    })

    observeEvent(input$done, {

      yamlheader <- paste0("#' ---\n",
                           "#' title: ", input$o_title,"\n",
      #                     "#' author: ",Sys.info()[['user']],"\n",
                           "#' date: '`r Sys.time()`'\n",
                           "#' output:\n",
                           "#'   html_document:\n",
                           "#'     theme: united\n",
                           "#'     toc: true\n",
                           "#'     toc_float: true\n",
                           "#'     number_sections: yes\n",
                           "#'     code_folding: show\n",
                           "#' ---\n\n",
                            # "#' # Exercises\n\n",
                           "#' \n\n")
      rstudioapi::insertText(yamlheader)



      # Which domains the assignments come from
      all_assignments <- scan_assingments()
      domains <- unique(all_assignments[all_assignments$id %in% input$o_exercise,][["domain"]])

      for (dom in 1:length(domains)){

        ids <- all_assignments[all_assignments$domain %in% domains[dom] & all_assignments$id %in% input$o_exercise,][["id"]]
        lang_ver <- paste0("lang_", input$o_lang)

        txt <- paste0("\n\n",
                      "#' # ",domains[dom],"\n",
                      "#' \n\n")
        rstudioapi::insertText(txt)

        for (i in 1:length(ids)){

          txt <- paste0("\n\n",
                        "#' *",dom,".",i,". ",all_assignments[all_assignments$id %in% ids[i],][[lang_ver]]," *\n",
                        # "#' \n",
                        # "#' \n",
                        all_assignments[all_assignments$id %in% ids[i],][["extra"]],"\n",
                        "#+ ", ids[i], ", eval = FALSE\n",
                        "default_answer()",
                        "\n\n\n")
          rstudioapi::insertText(txt)

        }
      }

      # Print the correct answers
      rstudioapi::insertText("\n\n#' ********************************************\n#' ")


      domains <- unique(all_assignments[all_assignments$id %in% input$o_exercise,][["domain"]])

      for (dom in 1:length(domains)){

        ids <- all_assignments[all_assignments$domain %in% domains[dom] & all_assignments$id %in% input$o_exercise,][["id"]]
        lang_ver <- paste0("lang_", input$o_lang)

        txt <- paste0("\n\n",
                      "#' # Correct answers: ",domains[dom],"\n",
                      "#' \n\n")
        rstudioapi::insertText(txt)

      for (i in 1:length(ids)){

        txt <- paste0("\n\n",
                      "#' *",dom,".",i,". ",all_assignments[all_assignments$id %in% ids[i],][[lang_ver]],"*\n",
                      "#' \n",
                      "#+ ", ids[i], "_answer, eval = FALSE\n",
                      all_assignments[all_assignments$id %in% ids[i], "ans"],
                      "\n\n\n")
        rstudioapi::insertText(txt)

      }
      }

        stopApp()
      })

  }
  viewer <- dialogViewer("Create an assignment", width = 1200, height = 1000)
  runGadget(ui, server, viewer = viewer)
  }
