
create_assignment <- function() {

  library(miniUI)
  library(shiny)
  ui <- miniPage(
    gadgetTitleBar("Create an assignment"),
    miniContentPanel(
      fillRow(flex = c(7, 3), height = "60px",
              uiOutput("i_lang")),
    fillRow(flex = c(7, 3), height = "60px",
            uiOutput("i_domain")),
      fillRow(flex = c(7, 3), height = "60px",
              uiOutput("i_exercise")),
      htmlOutput("preview")
    )
  )

  server <- function(input, output, session) {

    require(dplyr)
    # scan assignments
    assign_ymls <- list.files(system.file(package = "edu", ... = "data/"),pattern = ".yml", full.names = TRUE)

    all_assignments <- data_frame()
    for (i in assign_ymls){
      tmp <- yaml::yaml.load_file(i)
      tmp_d <- plyr::ldply(tmp, data.frame, stringsAsFactors=FALSE)
      all_assignments <- bind_rows(all_assignments,tmp_d)
    }

    langs <- gsub("lang_", "", names(all_assignments)[grepl("lang", names(all_assignments))])

    output$i_lang <- renderUI({
      radioButtons("o_lang",
                  label = "Select language",
                  choices =  langs,
                  selected = "en",
                  inline = TRUE,
                  width = "98%")
    })

    output$i_domain <- renderUI({

      domains <- unique(all_assignments$domain)

      radioButtons("o_domain",
                label = "Select domain",
                choices =  domains,
                selected = domains[1],
                inline = TRUE,
                width = "98%")
    })

    output$i_exercise <- renderUI({

      lang_ver <- paste0("lang_", input$o_lang)

      ids <- all_assignments[all_assignments$domain %in% input$o_domain,][["id"]]
      # Replace all non-translated exercise + language pairs with text "Translation not available"
      exercises <- all_assignments[all_assignments$id %in% ids,][[lang_ver]]
      exercises <- ifelse(is.na(exercises), "Translation not available", exercises)
      names(ids) <- exercises


      checkboxGroupInput("o_exercise",
                label = "Select exercises",
                choices =  ids,
                selected = ids[1], width = "98%")
    })

    observeEvent(input$done, {

      yamlheader <- paste0("#' ---\n",
                           "#' title: ", input$o_domain,"\n",
                           "#' author: ",Sys.info()[['user']],"\n",
                           "#' date: '`r Sys.time()`'\n",
                           "#' output:\n",
                           "#'   html_document:\n",
                           "#'     theme: united\n",
                           "#'     toc: true\n",
                           "#'     toc_float: true\n",
                           "#'     number_sections: yes\n",
                           "#'     code_folding: show\n",
                           "#' ---\n\n",
                           "#' # Exercises\n\n")
      rstudioapi::insertText(yamlheader)

      ids <- input$o_exercise
      lang_ver <- paste0("lang_", input$o_lang)

      for (i in 1:length(ids)){

        txt <- paste0("\n\n",
                      "#' *",i,". ",all_assignments[all_assignments$id %in% ids[i],][[lang_ver]],"*\n",
                      "#' \n",
                      "#+ ", ids[i], ", eval = FALSE\n",
                      "replace_this_with_your_answer()",
                      "\n\n\n")
        rstudioapi::insertText(txt)

      }

      # Print the correct answers
      rstudioapi::insertText("\n\n#' ********************************************\n#' # Correct answers\n\n")

      for (i in 1:length(ids)){

        txt <- paste0("\n\n",
                      "#' *",i,". ",all_assignments[all_assignments$id %in% ids[i],][[lang_ver]],"*\n",
                      "#' \n",
                      "#+ ", ids[i], "_answer, eval = FALSE\n",
                      all_assignments[all_assignments$id %in% ids[i], "ans"],
                      "\n\n\n")
        rstudioapi::insertText(txt)

      }

        stopApp()
      })

  }
  viewer <- dialogViewer("Create an assignment", width = 1200, height = 1000)
  runGadget(ui, server, viewer = viewer)
  }
