
# rsconnect::deployApp("C:/Users/krizova/Documents/R/02 cenoveKalkukacky/_vyvoj/shiny_app")

library(shiny) # aplikace
library(bslib) # aplikace (layouty)
library(shinycssloaders) # wip spinner
library(shinyWidgets)

library(tidyverse)
library(plotly)
library(readxl)
library(writexl)

library(zoo) # ?
library(rvest) # html
library(DT) # render table


app_online <- TRUE
options(shiny.launch.browser = TRUE)
options(shiny.maxRequestSize = 30*1024^2)

start_date <- as.Date(floor_date(now %m+% months(1), "month"))+days(1) # pracuje v UTC
end_date   <- as.Date(ceiling_date(now %m+% months(1), "month"))


# ---------------------------------------------------- UI


ui <- page_fillable(
  
  title = "Kalkulačka fixní ceny EE", # nadpis zalozky v prohlizeci
  
  tags$div(
    style = "display:flex; align-items:center; gap:20px;",
    tags$img(
      src = "22743_SPP_logo spp_final update.jpg",
      height = "50px"
    ),
    span("Kalkulačka fixní ceny EE",
         style = "font-size: 28px;"),  # nadpis aplikace
    
    input_dark_mode(id = "mode",
                    style = "margin-left: auto;"), 
  ),
  
  
  layout_columns( # cards beside each other
    
    col_widths = c(8, 4),
    
    # levý sloupec (obsahuje 2 karty pod sebou)
    list(
      
      # karta 1
      card( 
        
        card_header("1. Zobrazení diagramu", class = "fs-5", style = "color: #3783d4;"),
        
        fileInput(
          inputId = "upload",
          label = NULL,         # skryjeme původní label
          buttonLabel = "Nahrajte diagram",
          placeholder = "",
          accept = c(".xls", ".xlsx"),
          width = "48%"
        ),
        
        tags$div("Nahrajte historický diagram ve formátu XLS/XLSX.
                     Soubor musí obsahovat dva sloupce: timestamp a diagram spotřeby v MWh za období min. 12 měsíců.",
                 style = "color: grey; margin-bottom: 5px; font-style: italic;font-size:0.85em;"),
        
        
        withSpinner(
          plotlyOutput("plot", height = "500px"),
          type = 6, color = "dodgerblue4"
        )
      ),
      
      # karta 2
      card(
        
        card_header("2. Vstupní informace", class = "fs-5", style = "color: #3783d4;"),
        
        layout_columns(
          col_widths = c(3, 3, 5),
          
          div(
            style = "display:flex; justify-content:center;",
            textInput("text1", tagList("Obchodník", span("*", style="color:red")), width="90%")
          ),
          
          div(
            style = "display:flex; justify-content:center;",
            textInput("text2", tagList("Zákazník", span("*", style="color:red")), width="90%")
          ),
          
          div(
            style = "display:flex; justify-content:center;",
            dateRangeInput("date",
                           tagList("Období dodávky", span("*", style="color:red")),
                           width = "90%",
                           separator = " - ",
                           start = start_date,
                           end = end_date,
                           min = start_date,
                           max = floor_date(Sys.Date() %m+% years(4), "year")-minutes(15)
            )
          )
        )
      ) # konec 2. karty, 1. sloupce
    ), # konec 1. sloupce
    
    # ===== 2. SLOUPEC ===== to do
    
    card( 
      
      card_header(tags$span("3. Přesvátkování a výpočet ceny", class = "fs-5", style = "color: #3783d4;")),
      
      actionButton(
        inputId = "run", 
        label = "Výpočet ceny",
        class = "btn btn-primary"),
      
      DT::DTOutput("results") %>% withSpinner(type = 6, color = "dodgerblue4"),
      
      
      HTML('<span style="color:#3783d4; font-style: italic; ">* Předávací ani prodejní cena neobsahují náklad na BSD a toleranci.</span>'),
      
      
      fluidRow(
        column(12,
               textAreaInput(
                 inputId = "note",
                 label = "Poznámka do PDF reportu",
                 value = "",
                 rows = 3,
                 width = "100%"))),
      
      downloadButton("downloadReport", 
                     label = "Stáhnout PDF report")
    ) # konec 2. karty a 2. sloupce
  ) # konec layout columns (2 hlavni sloupce)
) # konec UI


# ---------------------------------------------------- SERVER


server <- function(input, output, session) {
  
  source("EE_analyza.R")

  # ---- OMEZENI DENNI DOBY PROVOZU APLIKACE ----
  
  
  if (!app_online) {
    showModal(modalDialog(
      title = "Aplikace je z provozních důvodů momentálně nedostupná",
      "Prosím, kontaktujte Nákupní oddělení.",
      easyClose = FALSE,
      footer = NULL
    ))
    session$close()
    return()
  }

  
  # ---- REAKTIVNÍ NAČTENÍ EXCELU ----
  
  data_upload <- reactive({
    req(input$upload)
    upld <- read_excel(input$upload$datapath)
    upld <- upld[, 1:2] # range A:B
    upld <- upld %>% 
      mutate(timestamp = as.POSIXct(datum_cas, format = "%d.%m.%Y %H:%M", 
                                    tz = "Europe/Prague"))
  })
  
  # ---- PLOT ----
  
  output$plot <- renderPlotly({
    
    upld <- data_upload()
    req(upld)
    
    y_limits <- range(upld$profil, na.rm = TRUE)
    
    p <- ggplot(upld, aes(timestamp, profil)) +
      geom_line(color = "dodgerblue4") +
      labs(x = "datum", y = "diagram spotřeby [MWh]") +
      scale_x_datetime(
        date_breaks = "1 week",
        date_labels = "%Y-%m-%d",
        expand = c(0, 0)) +
      theme_light()+
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(p)
    
  })

  
  # ---- ZADANI OBDOBI DODAVKY ----
  
  dates <- reactive({
    req(input$date)
    
    list(
      start = lubridate::floor_date(input$date[1], "month"),
      end   = lubridate::ceiling_date(input$date[2], "month")-minutes(15)
    )
  })  
  
  observeEvent(input$run, {
    d <- dates()
    
    updateDateRangeInput(
      session,
      "date",
      start = d$start,
      end   = d$end
    )
  })
    

  # ---- SPUSTENI VYPOCTU ----

  
  vysledek <- eventReactive(input$run, {
    
    req(input$upload, input$date, input$text1, input$text2)
    
    upld <- read_excel(input$upload$datapath) %>%
      mutate(timestamp = as.POSIXct(datum_cas, format = "%d.%m.%Y %H:%M", 
                                    tz = "Europe/Prague"))
      select(timestamp, "profilMWh" = 2) %>% 
      mutate(mesic = month(datum),
             rok = year(datum))  # načtení nahraného profilu
      
    
    start <- as.Date(format(input$date[1], "%Y-%m-01"))
    end <- as.Date(format(input$date[2], "%Y-%m-01"))
    obch <- input$text1
    zak <- input$text2
    
    delOd <- start
    delDo <- end
    
    analyza_data(
      upld,
      start,
      end,
      obch = input$text1,
      zak  = input$text2,
      path = "data/"
    )
    
  })
  
  output$results <- DT::renderDT({
    
    result <- vysledek()
    
    fix_cena <- result$fix_cena
    validate(need(is.data.frame(fix_cena), "Výsledek není datová tabulka"))
    
    DT::datatable(
      fix_cena,
      rownames = FALSE,
      options = list(
        dom = 't',       # odstraní paging a search
        ordering = FALSE,
        paging = FALSE
      )
    )
    
  })

  # ---- GENEROVANI PDF ----
    
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("VypocetFixCenyZP_report_", Sys.time(), ".pdf")
    },
    contentType = "application/pdf",
    content = function(file) {
      
      result <- vysledek()
      
      rmarkdown::render(
        input = "report.Rmd",
        output_format = "pdf_document",
        output_file = file,
        params = list(
          obchodnik = input$text1,
          zakaznik = input$text2,
          datum_od = input$date[1],
          datum_do = input$date[2],
          upld = data_upload(),
          # plot_diagram = output$plot,
          fwd = result$fwd,
          otc = result$otc,
          fix_cena = result$fix_cena
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}


# ---------------------------------------------------- APP


shinyApp(ui, server)


