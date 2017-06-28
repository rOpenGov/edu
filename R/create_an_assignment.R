
create_assignment <- function() {

  library(miniUI)
  library(shiny)
  ui <- miniPage(
    gadgetTitleBar("Add an assignment"),
    miniContentPanel(
      fillRow(flex = c(7, 3), height = "60px",
              uiOutput("i_lang")),
    fillRow(flex = c(7, 3), height = "60px",
            uiOutput("i_domain")),
      fillRow(flex = c(7, 3), height = "60px",
              uiOutput("i_question")),
      htmlOutput("preview")
    )
  )

  server <- function(input, output, session) {

    require(dplyr)
    raw <- pref <- yaml::yaml.load_file(system.file(package = "edu", ... = "data/assignments.yml"))
    raw_d <- plyr::ldply (raw, data.frame, stringsAsFactors=FALSE)

    langs <- c("en","fi","se","ru")
    names(langs) <- c("English","Suomi","Svenska","Russian")

    output$i_lang <- renderUI({
      radioButtons("o_lang",
                  label = "language / kieli / språk / язык?",
                  choices =  langs,
                  selected = langs[1],
                  inline = TRUE,
                  width = "98%")
    })

    translate_title <- data_frame(en = c("Select domain", "Select question"),
                                  fi = c("Valitse osio", "Valitse kysymys"),
                                  se = c("Välj domän","Välj fråga"),
                                  ru = c("Выберите домен","Выберите вопрос"))


    translate_domain <- data_frame(en = c("basics","import","export","tidy","transform","visualise","model","communicate","automate"),
                                   fi = c("perusteet","tuo","vie","siivoa","muokkaa","visualisoi","mallinna","kommunikoi","automatisoi"),
                                   se = c("grunderna","import","exportera", "snyggt", "omvandla", "visualisera", "modell", "kommunicera", "automatisera"),
                                   ru = c("основы","импорт","экспорт", "аккуратная", "преобразование", "визуализация", "модель", "общаться", "автоматизировать"))

    output$i_domain <- renderUI({

      domains <- translate_domain$en
      names(domains) <- translate_domain[[input$o_lang]]

      selectInput("o_domain",
                label = translate_title[[1, input$o_lang,]],
                choices =  domains,
                selected = domains[1],
                width = "98%")
    })

    output$i_question <- renderUI({

      q <- paste0("q_", input$o_lang)

      ids <- raw_d[raw_d$domain %in% input$o_domain,][["id"]]
      names(ids) <- raw_d[raw_d$id %in% ids,][[q]]


      checkboxGroupInput("o_question",
                label = translate_title[[2, input$o_lang,]],
                choices =  ids,
                selected = ids[1], width = "98%")
    })

    observeEvent(input$done, {

      yamlheader <- paste0("#' ---\n",
                           "#' title: ", translate_domain[translate_domain$en %in% input$o_domain, input$o_lang],"\n",
                           "#' author: ",Sys.info()[['user']],"\n",
                           "#' date: '`r Sys.time()`'\n",
                           "#' ---\n\n")
      rstudioapi::insertText(yamlheader)

      ids <- input$o_question
      ans <- raw_d[raw_d$id %in% ids, "ans"]
      q <- paste0("q_", input$o_lang)

      for (i in 1:length(ids)){

        txt <- paste0("\n\n",
                      "#' *",raw_d[raw_d$id %in% ids[i],][[q]],"*\n",
                      "#' \n",
                      "#+ ", ids[i], ", eval = FALSE\n",
                      "replace_this_with_your_answer()",
                      "\n\n\n")
        rstudioapi::insertText(txt)

      }

      rstudioapi::insertText("\n\n#' ********************************************\n#' # Correct answers \n#' ********************************************\n\n")

      for (i in 1:length(ids)){

        txt <- paste0("\n\n",
                      "#' *",raw_d[raw_d$id %in% ids[i],][[q]],"*\n",
                      "#' \n",
                      "#+ ", ids[i], ", eval = FALSE\n",
                      ans[i],
                      "\n\n\n")
        rstudioapi::insertText(txt)

      }




        stopApp()
      })

  }
  viewer <- dialogViewer("Add an assignment", width = 1200, height = 1000)
  runGadget(ui, server, viewer = viewer)
  }
