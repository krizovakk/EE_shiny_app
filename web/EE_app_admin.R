# .............................................................................. rds - DONE

# creates init_params.R
# run only once or when restarting

# params <- data.frame(
#   name = c("low_spot", "aktual_spot", "prirazka_nakup", "marzeMin", "marzeDop", "stari_OTCcen", "online", "typ_vypoctu", "banner"),
#   # name = c("low_spot", "aktual_spot", "param3", "param4", "param5", "param6", "online", "typ_vypoctu", "banner"),
#   value = c(24.25, 24.35, 0.3, 1.2, 6, 30, 1, 1, "Naceneni v indikativnim rezimu.")
#   # value = c(24.25, 24.35, 26, 27, 28, 29, 1, 1, "Naceneni v indikativnim rezimu.")
# )
# 
# saveRDS(params, "params.rds")

# .............................................................................. global - DONE

library(shiny) # aplikace
library(bslib) # aplikace (layouty)
library(shinycssloaders) # wip spinner
library(shinyjs) # blokovani tlacitka pro stazeni pdf

library(tidyverse)
library(plotly)
library(readxl)
library(writexl)

library(zoo) # ?
library(rvest) # html
library(DT) # render table

app_online <- TRUE
options(shiny.maxRequestSize = 30*1024^2)
options(shiny.launch.browser = TRUE)

# .............................................................................. UI - DONE


ui <- tagList(
  
  tags$head(                               # admin plovouci tlacitko vpravo dole
    tags$style(HTML("
    #open_admin {
      position: fixed;
      bottom: 50px;
      right: 50px;
      z-index: 9999;
      
      background-color: #2C7BE5;
      color: white;
      border: none;
      border-radius: 50px;
      padding: 10px 18px;
      box-shadow: 0 4px 10px rgba(0,0,0,0.2);
    }

    #open_admin:hover {
      background-color: #1b5fc7;
      cursor: pointer;
    }
  "))
  ),
  
  useShinyjs(),
  
  page_fillable(
    
    title = "Kalkulačka fixní ceny EE",            # nadpis zalozky v prohlizeci
    
    tags$div(
      style = "display:flex; align-items:center; gap:20px;",
      tags$img(
        src = "22743_SPP_logo spp_final update.jpg",
        height = "50px"
      ),
      
      span("Kalkulačka fixní ceny EE",
           style = "font-size: 28px;"),  
      
      input_dark_mode(id = "mode"),
      
      uiOutput("banner"),
      
      tags$a(
        href = "https://www.spp.cz/",                        # prozatimni adresa
        target = "_blank",
        style = "
          margin-left: auto;
          background-color: #FFD700;
          color: black;
          padding: 6px 12px;
          border-radius: 8px;
          text-decoration: none;
          font-size: 16px;
        ",
        "→ Kalkulačka fixní ceny ZP"
      ),
    ),
    
    tags$head(                             # zablokuje zmenu ACQ pri scrollovani
      tags$script(HTML("
    document.addEventListener('wheel', function(e) {
      if (document.activeElement.type === 'number') {
        e.preventDefault();
      }
    }, { passive: false });"))
    ),
    
    layout_columns(                      
      
      col_widths = c(8, 4),
      
      list(
        
        card( 
          
          card_header("1. Diagram k přesvátkování", class = "fs-5", style = "color: #808080;"),
          
          fileInput(
            inputId = "upload",
            label = NULL,        
            buttonLabel = "Nahrajte diagram",
            placeholder = "",
            accept = c(".xls", ".xlsx"),
            width = "48%"
          ),
          
          tags$div("Nahrajte historický diagram ve formátu XLS/XLSX. Soubor musí obsahovat dva sloupce: timestamp a diagram spotřeby v MWh za období min. 12 měsíců.",
                   style = "color: grey; margin-bottom: 5px; font-style: italic;"), #font-size:0.85em;
          
          plotlyOutput("plot", width = "100%")),
          # plotOutput("plot", width = "100%")),
        
        card(
          
          card_header("2. Vstupní informace", class = "fs-5", style = "color: #808080;"),
          
          layout_columns(
            
            col_widths = c(6, 6),
            
            list(
              
              div(
                
                selectizeInput("text1", tagList("Obchodník", span("*", style="color:red")), 
                               choices = c("" ,sort(c(
                                 
                                 # admin
                                 "krizovak", 
                                 "janouskovam", 
                                 "capousekl", 
                                 "micietaj",
                                 "danielt",
                                 "danielt",
                                 "purmenskyl",
                                 "valiso",
                                 "psencikj",
                                 # b2b
                                 "donatovak",
                                 "krizp",
                                 "aurednikl",
                                 "skocilm",
                                 "stefekm",
                                 # kam
                                 "lukesr",
                                 "adamekt",
                                 "burianm",
                                 "kucharikm",
                                 "komarkovaz"
                               ))),
                               width="60%")
              ),
              
              div(
                
                textInput("text2", tagList("Zákazník", span("*", style="color:red")), width="60%")),
              
              div(
                
                dateRangeInput("date",
                               tagList("Období dodávky", span("*", style="color:red")),
                               width = "60%",
                               separator = " - ",
                               # start = start_date,
                               # end = end_date,
                               min = floor_date(Sys.Date(), "month") + months(1),
                               max = floor_date(Sys.Date() %m+% years(4), "year")))
              
              #   tags$div("Období dodávky pracuje s plynárenskými dny.",
              #            style = "color: grey; margin-bottom: 5px; font-style: italic;") #font-size:0.85em;
              #   
            ),
            
            list(
              
              numericInput("acq1", "ACQ 2026 v MWh", "", width="60%"),
              numericInput("acq2", "ACQ 2027 v MWh", "", width="60%"),
              numericInput("acq3", "ACQ 2028 v MWh", "", width="60%"),
              numericInput("acq4", "ACQ 2029 v MWh", "", width="60%")
            ) # konec 2. karty, 1. sloupce
          ))), # konec 1. sloupce
      
      card( 
        
        card_header(tags$span("3. Přesvátkování a výpočet ceny", class = "fs-5", style = "color: #808080;")),
        
        layout_columns(
          
          col_widths = c(6, 6),
          fill = FALSE,
          
          actionButton(
            inputId = "run_indik",
            label = "Výpočet indikativní ceny",
            style = "border-color: #2C7BE5;",
            width = "100%"),
          
          actionButton(
            inputId = "run",
            label = "Výpočet závazné ceny",
            style = "background-color: #2C7BE5; color: white; border-color: #2C7BE5;",
            width = "100%")),
        
        div(
          id = "calc_spinner",
          style = "display:none; text-align:center; margin-top:15px;",
          
          div(class = "spinner-border text-warning", role = "status"),
          div("Probíhá výpočet...", style = "margin-top:5px;")
        ),
        
        DT::DTOutput("results") %>% withSpinner(type = 6, color = "#2C7BE5"),
        
        HTML('<span style="color:#808080; font-style: italic;">Předávací ani prodejní cena neobsahují náklad na BSD, financování a toleranci.</span>'),
        
        fluidRow(
          column(12,
                 textAreaInput(
                   inputId = "note",
                   label = "Volitelná poznámka do PDF reportu",
                   value = "",
                   rows = 3,
                   width = "100%"))),
        
        downloadButton("downloadReport", "Stáhnout PDF report"),
        
        actionButton("open_admin", "Admin"),
        
        uiOutput("admin_panel")
        
      ) # konec 2. karty a 2. sloupce
    ), # konec layout columns (2 hlavni sloupce
    
  )) # konec UI

# .............................................................................. SERVER - *** WIP *** 


server <- function(input, output, session) {
  
  source("EE_analyza.R")
  
  # ================================================================ admin panel
  admin_mode <- reactiveVal(FALSE)
  admin_unlocked <- reactiveVal(FALSE)
  admin_visible <- reactiveVal(FALSE)
  
  observeEvent(input$open_admin, {
    admin_mode(TRUE)
    admin_visible(!admin_visible())
  })
  
  observeEvent(input$admin_login, {
    if (input$admin_pwd == "123") {
      admin_unlocked(TRUE)
    } else {
      admin_unlocked(FALSE)
      showNotification("Nesprávné administrátorské heslo", type = "error")
    }
  })
  
  output$admin_panel <- renderUI({
    if (!admin_visible()) return(NULL)
    if (!admin_unlocked()) {
      return(
        tagList(
          card_header("4. Admin přístup", 
                      class = "fs-5",
                      style = "color: #808080"),
          passwordInput("admin_pwd", "Heslo"),
          actionButton("admin_login", "Odemknout")))
    }
    
    tagList(
      card_header("4. Admin sekce",
                  class = "fs-5",
                  style = "color: #808080"),
      div(id = "params_table_wrapper",
          DT::DTOutput("params_table")))
  })
  
  params <- reactiveVal(readRDS("params.rds"))
  
  output$params_table <- DT::renderDT({
    DT::datatable(
      params(),
      editable = TRUE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE))
  }, server = FALSE)
  
  observeEvent(input$params_table_cell_edit, {
    info <- input$params_table_cell_edit
    df <- params()
    df[info$row, info$col] <- DT::coerceValue(
      info$value,
      df[info$row, info$col]
    )
    params(df)
    saveRDS(df, "params.rds")
  })
  # ================================================================ admin panel
  
  # ---- RDS ----
  
  params <- reactiveVal(readRDS("params.rds"))
  
  observe({
    invalidateLater(5000, session)  # aktualizace kazdych 5 s
    params(readRDS("params.rds"))
  })
  
  get_param <- function(name) {
    params()$value[params()$name == name] # funkce, ktera nacita parametry
  }
  
  typ_vypoctu <- reactiveVal(NULL)   # "indikativni" / "zavazny"
  vysledek_r  <- reactiveVal(NULL)
  
  # typ <- reactive({params$value[params$name == "typ_vypoctu"]}) # ??? tohle mozna vubec nepotrebujeme
  

  
  # ---- DEFAULT HODNOTY PRO DATE RANGE ---- 
  
  observe({
    
    start_date <- floor_date(Sys.Date(), "month") + months(1)
    end_date   <- ceiling_date(start_date, "month")
    
    updateDateRangeInput(
      session,
      "date",
      start = start_date,
      end = end_date,
      min = start_date,
      max = floor_date(Sys.Date() %m+% years(4), "year")
    )
  })
  
  # ---- OMEZENI PROVOZU APLIKACE ----
  
  observe({
    
    online <- as.numeric(get_param("online"))
    
    if (online != 1) {
      showModal(modalDialog(
        title = "Aplikace je z provozních důvodů momentálně nedostupná",
        "Pro nacenění kontaktujte Nákupní oddělení.",
        easyClose = FALSE,
        footer = NULL
      ))
      session$close()
      return()
    }
  })
  
  output$banner <- renderUI({
    
    typ_vypoctu_param <- as.numeric(get_param("typ_vypoctu"))
    banner_text <- get_param("banner")
    
    if (is.na(typ_vypoctu_param)) return(NULL)
    
    if (typ_vypoctu_param == 0) {
      
      div(
        style = "
        background-color: #fff3cd;
        border-left: 5px solid #FFD700;
        color: #808080;
        padding: 12px;
        border-radius: 5px;
        margin-bottom: 5px;
      ",
        HTML(paste0("<b>Upozornění:</b> ", banner_text))
      )
      
    } else {
      NULL
    }
  })
  
  # ---- REAKTIVNÍ NAČTENÍ EXCELU ----
  
  data_upload <- reactive({
    req(input$upload)
    upld <- readxl::read_excel(input$upload$datapath, range = cell_cols(1:2))
    upld$datum_cas <- lubridate::dmy_hm(upld$datum_cas)
    upld
  })
  
  # ---- PLOTLY ----
  
  output$plot <- renderPlotly({
    
    profil <- data_upload() %>% drop_na() 
    req(nrow(profil) > 0)
    colnames(profil) <- c("datum", "profilMWh")
    
    p <- ggplot(profil, aes(datum, profilMWh)) +
      geom_line(colour = "#2C7BE5") +
      labs(x = "",
           y = "MWh",
           title = "Diagram spotřeby klienta") +
      scale_x_datetime(
        date_breaks = "1 day", 
        # date_breaks = "1 week", # zobrazuje vzdy pondeli
        date_labels = "%Y-%m-%d",
        expand = c(0, 0)) +
      scale_y_continuous(breaks = waiver())+
      # scale_y_continuous(breaks = seq(0, 700, by = 50))+
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
    ggplotly(p)
  })
  
  # ---- ZADANI OBDOBI DODAVKY ----
  
  dates <- reactive({
    req(input$date)
    
    list(
      start = lubridate::floor_date(input$date[1], "month"),
      end   = lubridate::floor_date(input$date[2], "month")
    )
  })  
  
  observeEvent(input$run, {
    removeNotification(id = NULL)
    d <- dates()
    
    shinyjs::disable("checkbox")
    
    updateDateRangeInput(
      session,
      "date",
      start = d$start,
      end   = d$end
    )
  })
  
  # ---- SPUSTENI VYPOCTU ----
  
  spust_vypocet <- function() {
    
    shinyjs::show("calc_spinner")
    on.exit(shinyjs::hide("calc_spinner"))
    
    d <- dates()
    req(input$upload, input$date, input$text1, input$text2)
    
    profil <- read_excel(input$upload$datapath) %>%
      select("datum" = 1, "profilMWh" = 2) %>% 
      mutate(mesic = month(datum),
             rok = year(datum))  
    
    analyza_data(
      profil,
      d$start,
      d$end,
      obch = input$text1,
      zak  = input$text2,
      acq1 = input$acq1,
      acq2 = input$acq2,
      acq3 = input$acq3,
      acq4 = input$acq4,
      typ_vypoctu = typ_vypoctu()
    )
  }
  
  observeEvent(input$run_indik, {
    typ_vypoctu("indikativni")
    vysledek_r(spust_vypocet())
  })
  
  # observeEvent(input$run, {
  #   typ_vypoctu("zavazny")
  #   vysledek_r(spust_vypocet())
  # })
  
  observeEvent(input$run, {
    showModal(modalDialog(
      title = "Opravdu chcete spustit výpočet závazné ceny?",
      "Tato akce nahradí poslední závazný výpočet pro daného zákazníka.",
      footer = tagList(
        modalButton("Zrušit"),  
        actionButton("confirm_run", "Ano, spustit") 
      ),
      easyClose = TRUE,
      size = "l"  # volitelné (s, m, l)
      # style = "width: 800px;"
    ))
  })
  
  observeEvent(input$confirm_run, {
    removeModal() 
    typ_vypoctu("zavazny")
    vysledek_r(spust_vypocet())
  })
  
  output$results <- DT::renderDT({
    
    result <- vysledek_r()
    req(result)
    
    fix_cena <- result$fix_cena
    validate(need(is.data.frame(fix_cena), ""))
    
    DT::datatable(
      fix_cena,
      rownames = FALSE,
      colnames = NULL, 
      options = list(
        dom = 't',       # odstraní paging a search
        ordering = FALSE,
        paging = FALSE)) %>% 
      
      DT::formatStyle(
        "Hodnota",   # sloupec, podle kterého barvíme
        target = 'cell',  # jen buňky, ne celý řádek
        color = DT::styleEqual(
          c("Pouze indikativní", "Závazná"),
          c("red", "green")
        )
      )
  })
  
  # ---- GENEROVANI PDF ----
  
  observe({
    if (!is.null(vysledek_r()) &&
        vysledek_r()$typ_vypoctu == "zavazny") {
      shinyjs::enable("downloadReport")
    } else {
      shinyjs::disable("downloadReport")
    }
  })
  
  
  # observe({
  #   if (typ_vypoctu() == "zavazny" && !is.null(vysledek_r())) {
  #     shinyjs::enable("downloadReport")
  #   } else {
  #     shinyjs::disable("downloadReport")
  #   }
  # })
  # 
  
  tms_now <- as.POSIXct(Sys.time(), tz = "Europe/Prague")
  format(tms_now, "%Y-%m-%d %H:%M:%S")
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("VypocetFixCenyZP_report_", tms_now, "_", input$text1, "_", input$text2, ".pdf")
    },
    contentType = "application/pdf",
    content = function(file) {
      
      if (typ_vypoctu() != "zavazny") {
        showNotification("PDF lze stáhnout pouze pro závazný výpočet.", type = "error")
        return(NULL)
      }
      
      if (is.null(vysledek_r())) {
        showNotification("Nejprve spusťte výpočet.", type = "error")
        return(NULL)
      }
      
      result <- vysledek_r()
      
      rmarkdown::render(
        input = "report.Rmd",
        output_format = rmarkdown::pdf_document(),
        output_file = file,
        params = list(
          obchodnik = input$text1,
          zakaznik = input$text2,
          datum_od = input$date[1],
          datum_do = input$date[2],
          profil = data_upload(),
          fwd = result$fwd,
          fix_cena = result$fix_cena,
          note = input$note
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
} # konec serveru


# .............................................................................. APP


shinyApp(ui, server)
