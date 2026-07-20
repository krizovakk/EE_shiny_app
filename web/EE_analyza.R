# ------------------------------------------------------------------------------ TEST


# profil <- read_excel("C:/Users/krizova/Documents/R/02 cenoveKalkukacky/EE/EE_input_profil.xlsx")
# 
# delOd <- as.POSIXct("2026-07-01", tz = "Europe/Prague")
# delDo <- as.POSIXct("2027-12-31", tz = "Europe/Prague")
# 
# obch <- "krizova"
# zak <- "krizova2"
# 
# typ_vypoctu <- "indikativni"

# ------------------------------------------------------------------------------ SETUP


require(tidyverse)
require(readxl)
require(openxlsx)


# ------------------------------------------------------------------------------ ANALYZA


analyza_data <- function(profil, delOd, delDo, obch, zak, acq1, acq2, acq3, acq4, acq5, typ_vypoctu, priplatek) {
  
  tms_now <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), tz = "Europe/Prague")
  
message(paste("NOW", tms_now))
message(paste(delOd, " az ", delDo))
  
  params <- readRDS("params.rds")
  rezim <- as.numeric(params$value[params$name == "rezim"])
  
  conditionPLA <- hour(tms_now)<20&rezim == 1
  if (!conditionPLA){
    showNotification(
      paste("Po 15. hodině již není možné generovat závazné nabídky."),
      type = "message",
      duration = 15
    )
    typ_vypoctu <- "indikativni"
  }
  
  message(conditionPLA)
  message(rezim)  
  

  
  
  
  ########################################### START PRESVATKOVANI ##############################################################
  
  # ---------------------------------------------------------------------------- FUNC :: presvatkovani
  
  load <- profil %>% 
    mutate(timestamp = as.POSIXct(timestamp, format = "%d.%m.%Y %H:%M", tz = "Europe/Prague"))
  
  leden1 <- load %>% filter(month(timestamp) == 1 & day(timestamp) == 1) %>% slice(49) # 49 je 12:00
  nr <- wday(leden1$timestamp, week_start = 1) >= 3 
  
  diagram <- load %>%
    
    mutate( # casove promenne
      
      year = lubridate::year(timestamp),
      date = lubridate::date(timestamp),
      iso_week = lubridate::isoweek(timestamp),
      weekday = lubridate::wday(timestamp, week_start = 1),
      md = paste0(month(timestamp), "-", day(timestamp)) # kvuli definici svatku
      
    ) %>% 
    
    mutate( # klasifikace dne
      
      typeDay = ifelse(weekday %in% c(6,7), "wknd", "reg"),
      holiday = case_when(md == "1-1" ~ "novy_rok",
                          md == "5-1" ~ "svatek_prace",
                          md == "5-8" ~ "konec_valky",
                          md == "7-5" ~ "cyril_metod",
                          md == "7-6" ~ "jan_hus",
                          md == "9-28" ~ "sv_vaclav",
                          md == "10-28" ~ "vznik_statu",
                          md == "11-17" ~ "den_student",
                          md == "12-24" ~ "stedry_den",
                          md == "12-25" ~ "bozi_hod",
                          md == "12-26" ~ "sv_stepan",
                          as.Date(date) %in% as.Date(c("2024-03-29", "2025-04-18", "2026-04-03", "2027-03-26")) ~ "easter_friday",
                          as.Date(date) %in% as.Date(c("2024-03-30", "2025-04-19", "2026-04-04", "2027-03-27")) ~ "easter_saturday",
                          as.Date(date) %in% as.Date(c("2024-03-31", "2025-04-20", "2026-04-05", "2027-03-28")) ~ "easter_sunday",
                          as.Date(date) %in% as.Date(c("2024-04-01", "2025-04-21", "2026-04-06", "2027-03-29")) ~ "easter_monday",
                          TRUE ~ NA),
      dst = case_when(as.Date(date) %in% as.Date(c("2024-03-31", "2025-03-30", "2026-03-29", "2027-03-28")) ~ "ltc",
                      as.Date(date) %in% as.Date(c("2024-10-27", "2025-10-26", "2026-10-25", "2027-10-31")) ~ "zmc",
                      TRUE ~ NA)
      
    ) %>% # indexace 15' a uprava tydne
    
    group_by(date) %>% mutate(porm15 = row_number()) %>% ungroup() %>% 
    mutate(upd_week = if (nr) {dense_rank(iso_week) - 1} else {dense_rank(iso_week)},
           
    ) %>% 
    
    mutate( # definice parovani
      
      parovani = if_else(
        !is.na(holiday) & typeDay == "reg",
        "nejblizsi vsedni",
        "puvodni profil")
      
    ) %>% 
    
    select(timestamp, date, typeDay, holiday, dst, iso_week, "week" = upd_week, weekday, porm15, profil, parovani) 
  
  rm(leden1)
  
  
  # ------------------------------------------------------------------------------ pull value to pair - OK
  
  # 1) par :: do sloupce parHodnota kopirujeme komplet puvodni profil
  # 2) idx_prepocet :: index radku, ktere maji "nejbl. vsedni" a ktere bude treba prepsat
  # 3) par$parHodnota[idx_prepocet] :: 
  
  
  par <- diagram %>% 
    mutate(
      parHodnota = profil) # duplikujeme profil do noveho sloupce, kde budou upravovany konkr. pripady
  
  idx_prepocet <- which(par$parovani == "nejblizsi vsedni") # index pro potreby vyplneni     
  
  par$parHodnota[idx_prepocet] <- 
    par[idx_prepocet, ] %>%
    rowwise() %>%
    mutate(
      val = {
        i <- which(
          par$date == date &
            par$porm15 == porm15
        )[1]
        
        idx_ok <- which(
          par$typeDay == "reg" & 
            par$parovani == "puvodni profil" & 
            par$porm15 == porm15
        )
        
        par$profil[
          idx_ok[which.min(abs(idx_ok - i))]
        ]
      }
    ) %>%
    pull(val)      
  
  par_dist <- par %>% 
    arrange(timestamp) %>%
    distinct(week, weekday, porm15, .keep_all = TRUE) # odstrani duplicitni weekdays&weeks (na konci roku)
  
  
  # ------------------------------------------------------------------------------ set year to rescale - OK
  
  
  val_y1 <- unique(max(lubridate::year(diagram$date)))+1 # v pripade, ze nebude diagram za jeden kalendarni rok, ale treba prelom (proto max)
  val_y2 <- val_y1+3
  
  
  # ------------------------------------------------------------------------------ create future - WIP 
  
  
  Lyear <- seq(val_y1, val_y2)
  
  for(i in Lyear) {
    
    timeline <- tibble(
      
      timestamp = seq(
        from = as.POSIXct(paste0(i, "-01-01 00:00:00"), tz = "Europe/Prague"),
        to   = as.POSIXct(paste0(i, "-12-31 23:59:00"), tz = "Europe/Prague"),
        by   = "15 min"))
    
    leden1 <- timeline %>% filter(month(timestamp) == 1 & day(timestamp) == 1) %>% slice(49) # 49 je 12:00
    nr <- wday(leden1$timestamp, week_start = 1) >= 3
    
    # --------------------------- future frame
    
    future <- tibble(
      
      timestamp = seq(
        from = as.POSIXct(paste0(i, "-01-01 00:00:00"), tz = "Europe/Prague"),
        to   = as.POSIXct(paste0(i, "-12-31 23:59:00"), tz = "Europe/Prague"),
        by   = "15 min")) %>% 
      
      mutate(  # casove promenne
        
        date = lubridate::date(timestamp),
        iso_week = lubridate::isoweek(timestamp),
        weekday = lubridate::wday(timestamp, week_start = 1),
        md = paste0(month(timestamp), "-", day(timestamp)) # kvuli definici svatku
      ) %>% 
      
      mutate( # klasifikace dne
        
        typeDay = case_when(weekday %in% c(6,7) ~ "wknd", TRUE ~ "reg"),
        holiday = case_when(md == "1-1" ~ "novy_rok",
                            md == "5-1" ~ "svatek_prace",
                            md == "5-8" ~ "konec_valky",
                            md == "7-5" ~ "cyril_metod",
                            md == "7-6" ~ "jan_hus",
                            md == "9-28" ~ "sv_vaclav",
                            md == "10-28" ~ "vznik_statu",
                            md == "11-17" ~ "den_student",
                            md == "12-24" ~ "stedry_den",
                            md == "12-25" ~ "bozi_hod",
                            md == "12-26" ~ "sv_stepan",
                            as.Date(date) %in% as.Date(c("2026-04-03", "2027-03-26", "2028-04-14", "2029-03-30", "2030-04-19")) ~ "easter_friday",
                            as.Date(date) %in% as.Date(c("2026-04-04", "2027-03-27", "2028-04-15", "2029-03-31", "2030-04-20")) ~ "easter_saturday",
                            as.Date(date) %in% as.Date(c("2026-04-05", "2027-03-28", "2028-04-16", "2029-04-01", "2030-04-21")) ~ "easter_sunday",
                            as.Date(date) %in% as.Date(c("2026-04-06", "2027-03-29", "2028-04-17", "2029-04-02", "2030-04-22")) ~ "easter_monday",
                            TRUE ~ NA),
        dst = case_when(as.Date(date) %in% as.Date(c("2026-03-29", "2027-03-28", "2028-03-26", "2029-03-25", "2030-03-31")) ~ "ltc",
                        as.Date(date) %in% as.Date(c("2026-10-25", "2027-10-31", "2028-10-29", "2029-10-28", "2030-10-27")) ~ "zmc",
                        TRUE ~ NA)
        
      ) %>% # indexace 15' a uprava tydne
      
      group_by(date) %>% mutate(porm15 = seq_along(date)) %>% ungroup() %>% 
      mutate(week_start = lubridate::floor_date(timestamp, "week", week_start = 1),
             upd_week = if (nr) {dense_rank(iso_week) - 1} else {dense_rank(iso_week)}
      ) %>% 
      
      select(timestamp, date, typeDay, holiday, dst, iso_week, "week" = upd_week, weekday, porm15) 
    
    
    # --------------------------- df pro join 
    
    fill <- diagram %>% 
      select(week, weekday, porm15, profil) %>% 
      filter(week <= 1 | week >= 52) %>% 
      group_by(weekday, porm15) %>% 
      summarise(join_fill = mean(profil), .groups = "drop") %>% ungroup() # prumer kolem prelomu roku pro doplneni
    
    dst <- diagram %>% 
      filter(!is.na(dst)) %>% # dst budeme prepisovat natvrdo hodnotami z puvodniho profilu
      select(dst, porm15, "join_dst" = profil)
    
    hol <- diagram %>% 
      filter(!is.na(holiday)) %>% # svatky budeme prepisovat natvrdo hodnotami z puvodniho profilu
      select(holiday, porm15, "join_hol" = profil)
    
    
    # --------------------------- df pro join 
    
    join <- future %>% 
      
      left_join(par_dist %>% select(week, weekday, porm15, "parProfil" = parHodnota), 
                by = c("week", "weekday", "porm15")) %>% 
      
      left_join(fill,
                by = c("weekday", "porm15")) %>% 
      
      left_join(dst,
                by = c("dst", "porm15")) %>% 
      
      left_join(hol, 
                by = c("holiday", "porm15")) %>% 
      mutate(final = coalesce(join_dst,
                              join_hol,
                              parProfil,
                              join_fill))
    
    presv <- join %>% 
      select(timestamp, "presvProfil" = final)
    
    
    assign(paste0(i, "_presv"), presv)
    rm(presv)
    
  }
  
  presv_diagram <- dplyr::bind_rows(mget(ls(pattern = "presv$"))) # presvatkovany diagram s rozlisenim 15'
  
  rm(list = ls()[grepl("_presv$", ls())])
  rm(diagram)
  rm(dst)
  rm(fill)
  rm(future)
  rm(hol)
  rm(leden1)
  rm(load)
  rm(join)
  rm(par)
  rm(par_dist)
  rm(timeline)
  
  showNotification(
    "Přesvátkování diagramu proběhlo v pořádku.",
    type = "message",
    duration = 7)
  

  ########################################### KONEC PRESVATKOVANI ##############################################################
  
  
  
  
  
  # ---------------------------------------------------------------------------- INPUT :: HPFC - OK
  

  a <- read_excel(file.path("data_exchange", "denni_data", "HPFC.xlsx"))
  
  hpfc <- a %>% 
    rename("datum" = 1) %>% 
    mutate(timestamp = as.POSIXct(datum, format = "%d.%m.%Y %H:%M", tz = "Europe/Prague")) %>% 
    select(timestamp,  CZ) 
  
message("HPFC krivka - ok")
   
  
  # ------------------ kontrola ***
  
  fwdcheck <- hpfc %>% filter(timestamp>=delOd)
  
  # test
  # fwdcheck [20,3] <- NA
  
  conditionFWD <- any(is.na(fwdcheck$CZ))|any(fwdcheck$CZ == 0)
  if (conditionFWD) {
    stop('Neuplna HPFC krivka, kontaktuj Nakup.')
  }
  
  
  # ---------------------------------------------------------------------------- INPUT :: OTC
  
  
  pre <- read.csv(file.path("data_exchange", "Power.csv"), 
                  header = F, sep = ",",
                  fileEncoding = "Windows-1250")[1,1] # jen datum pro check
  otc_tms <- mdy_hm(pre, tz = "Europe/Prague")
  
message(paste("OTC", otc_tms))
  
  riskMargin <- as.numeric(params$value[params$name == "riskMargin"])
  odchylka <- as.numeric(params$value[params$name == "odchylka"])
  rizeniOdch <- as.numeric(params$value[params$name == "rizeniOdch"])
  
  pricti <- riskMargin + odchylka + rizeniOdch
  
  b <- read.csv(file.path("data_exchange", "Power.csv"), 
                header = F, sep = ",", 
                fileEncoding = "Windows-1250") # test
  otc <- b %>%
    select("season" = 1, "price" = 2) %>% 
    slice(7:24) %>% 
    mutate(
      price = as.numeric(str_replace(price, ",", ".")) + pricti, # zde se pricitaji prirazky k cene
      year = case_when(
        str_detect(season, "2025|25") ~ 2025,
        str_detect(season, "2026|26") ~ 2026,
        str_detect(season, "2027|27") ~ 2027,
        str_detect(season, "2028|28") ~ 2028,
        str_detect(season, "2029|29") ~ 2029,
        str_detect(season, "2030|30") ~ 2030,
        TRUE ~ NA
      ),
      quater = case_when(
        str_detect(season, "Q1") ~ "Q1",
        str_detect(season, "Q2") ~ "Q2",
        str_detect(season, "Q3") ~ "Q3",
        str_detect(season, "Q4") ~ "Q4",
        TRUE ~ NA
      ),
      month = case_when(
        str_detect(season, "Jan-") ~ 1,
        str_detect(season, "Feb-") ~ 2,
        str_detect(season, "Mar-") ~ 3,
        str_detect(season, "Apr-") ~ 4,
        str_detect(season, "May-") ~ 5,
        str_detect(season, "Jun-") ~ 6,
        str_detect(season, "Jul-") ~ 7,
        str_detect(season, "Aug-") ~ 8,
        str_detect(season, "Sep-") ~ 9,
        str_detect(season, "Oct-") ~ 10,
        str_detect(season, "Nov-") ~ 11,
        str_detect(season, "Dec-") ~ 12,
        TRUE ~ NA
      ),
      cal = as.character(ifelse(str_detect(season, "^\\d{4}$"), paste0("Cal", str_remove(year, "^..")), NA))
    ) %>% 
    unite(product, c("cal", "month", "quater", "year"),
          sep = "/", na.rm = T, remove = FALSE) 
  
  message("OTC ceny - ok")  

  
  # ------------------ kontrola ***
  
  conditionCSV <- difftime(tms_now, otc_tms, units = "mins") > as.numeric(params$value[params$name == "stari_OTCcen"])
  if (conditionCSV) {
    showNotification(
      "Nejsou k dispozici aktuální tržní ceny, nabídka je pouze indikativní.",
      type = "warning",
      duration = 15)
    typ_vypoctu <- "indikativni"
  }
  
  
  # ---------------------------------------------------------------------------- CREATE :: frame - OK
  
  
  frameOd <- as.POSIXct("2026-01-01 00:00", tz = "Europe/Prague")
  frameDo <- as.POSIXct("2030-12-31 23:45", tz = "Europe/Prague")
  framePer <- seq(from = frameOd, to = frameDo, by = "15 min")
  
  start_datetime <- lubridate::as_datetime(delOd, tz = "Europe/Prague")
  end_datetime   <- lubridate::as_datetime(delDo, tz = "Europe/Prague") +
    lubridate::hours(23) + lubridate::minutes(45)
  delPer <- seq(from = start_datetime, to = end_datetime, by = "15 min")
  
  frame <- data.frame(framePer) %>% 
    mutate(
      date = date(framePer),
      year = year(framePer),
      month = month(framePer),
      quater = case_when(
        month(framePer) <= 3 ~ "Q1",
        month(framePer) <= 6 ~ "Q2",
        month(framePer) <= 9 ~ "Q3",
        month(framePer) <= 12 ~ "Q4"),
      day = day(framePer),
      dodavka = ifelse(framePer %in% delPer, 1, 0), 
      facq = case_when(year == 2026 ~ "acq1",
                       year == 2027 ~ "acq2",
                       year == 2028 ~ "acq3",
                       year == 2029 ~ "acq4",
                       year == 2030 ~ "acq5")) %>% 
    select(framePer, date, year, quater, month, day, dodavka, facq) %>% filter(dodavka == 1)  
  
message("Frame - ok")

  
  # ---------------------------------------------------------------------------- CREATE :: join
  
  
  join <- frame %>%
    
    # OTC 
    
    left_join(otc %>% 
                select(year, month, "mPrice" = price), by = c("year", "month")) %>% 
    left_join(otc %>% 
                select(year, quater, "qPrice" = price), by = c("year", "quater")) %>% 
    left_join(otc %>%
                filter(!is.na(cal)) %>%
                select(year, "yPrice" = price), by = c("year")) %>% 
    mutate(otcPrice = ifelse(!is.na(yPrice), yPrice, 
                             ifelse(!is.na(qPrice), qPrice, mPrice))) %>% 
    
    # HPFC krivka
    
    left_join(hpfc, by = c("framePer" = "timestamp")) %>% drop_na(CZ) %>% # drop na smaze NA z many to many joinu
    
    group_by(year) %>% mutate(yMean = mean(CZ, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(year, quater) %>% mutate(qMean = mean(CZ, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(year, month) %>% mutate(mMean = mean(CZ, na.rm = T)) %>% 
    ungroup() %>% 
    
    mutate(ratio = round(ifelse(!is.na(yPrice), CZ/yMean, 
                                ifelse(!is.na(qPrice), CZ/qMean, CZ/mMean)),3),
           EURmwh = otcPrice*ratio) %>% 
    
    select(-mPrice, -qPrice, -yPrice, -yMean, -qMean, -mMean) %>%
    
    # presvatkovany diagram
    
    left_join(presv_diagram, by = c("framePer" = "timestamp")) %>% 
    mutate(EUR = round(EURmwh*presvProfil, 3))
  
message("Join - ok")
  
  
  # ------------------ kontrola ***

  # test
  # data_vstup[11, 6] <- NA
  # data_vstup[18, 4] <- -5

  conditionOTC <- any(is.na(join$otcPrice))
  if (conditionOTC) {
    stop("Momentalne neni dostupna vstupni cena pro jeden z produktů. Zkus vypocet znovu za 15 min.")
  }
 
  conditionPROF <- any(is.na(join$presvProfil))
  if (conditionPROF) {
    stop('Neuplny profil, zkontroluj vstupni data.')
  }
 
  conditionZAP <- any(join$presvProfil < 0)
  if (conditionZAP) {
    stop('Zaporna data v profilu, zkontroluj vstupni data.')
  }
  
  # acq_map <- c(
  #   if (exists("acq1")) c(acq1 = acq1),
  #   if (exists("acq2")) c(acq2 = acq2),
  #   if (exists("acq3")) c(acq3 = acq3),
  #   if (exists("acq4")) c(acq4 = acq4),
  #   if (exists("acq5")) c(acq5 = acq5)
  # )
  # 
  # df_acq <- join %>%
  #   group_by(facq) %>%
  #   summarise(acq = round(sum(presvProfil), 0), .groups = "drop") %>% 
  #   mutate(
  #     inp = as.numeric(acq_map[match(as.character(facq), names(acq_map))]),
  #     inp_round = round(inp, 0),
  #     missing_inp = is.na(inp_round),
  #     check = !missing_inp & acq == inp_round
  #   )
  # 
  # if (any(df_acq$missing_inp)) {
  #   missing_facq <- paste(df_acq$facq[df_acq$missing_inp], collapse = ", ")
  #   showNotification(
  #     paste0("Chybi ACQ pro: ", missing_facq, "."),
  #     type = "error",
  #     duration = 15)
  #   # return(NULL)
  # }
  # 
  # conditionACQ <- any(!df_acq$check)
  # if (conditionACQ) {
  #   showNotification(
  #     "Zadané ACQ nesouhlasí se součtem v profilu, zkontrolujte vstupní data.",
  #     type = "error",
  #     duration = 15)
  #   return(NULL)
  # }

  
  # ---------------------------------------------------------------------------- CALCULATE :: EUR FWD
  
  
  low_spot <- as.numeric(params$value[params$name == "low_spot"])
  aktual_spot <- as.numeric(params$value[params$name == "aktual_spot"])
  cnb_kurz <- as.numeric(params$value[params$name == "cnb_kurz"])
  
  
  fwd <- read_excel(file.path("data_exchange", "denni_data", "001_FWD_SPP-CZ_KAM_Aktual.xlsm"),
                    sheet = "Forward", skip = 4) %>% 
    rename("mesic" = 1, "PFC" = 2, "FX" = 3) %>%  
    filter(mesic > tms_now) %>%   # hodnoty fwd krivky od nasledujiciho mesice
    select(mesic, FX) %>% 
    mutate(year = year(mesic),
           month = month(mesic),
           swap = (FX-low_spot)*1000,
           FXrecalc = aktual_spot+swap/1000,
           FX_akt = cnb_kurz) %>% 
    select(year, month, FXrecalc, FX_akt)
  
message("FWD krivka - ok")
  
  # czk <- join %>% 
  #   group_by(year, month) %>% summarise(mwhMonth = round(sum(presvProfil), 0),
  #                                       eurMonth = round(sum(EUR), 0)) %>% ungroup() %>% 
  #   mutate(eurmwhMonth = round(eurMonth/mwhMonth, 3)) %>% 
  #   left_join(fwd, by = c("year", "month"))
  # 
  # kurz <- mean(czk$FXrecalc)
  kurz_akt <- cnb_kurz

  
  # ---------------------------------------------------------------------------- CALCULATE :: fix_cena 
  
  
  # vypocty 
  
  acq <- join %>% 
    group_by(year) %>% summarise(rocni_odber = round(sum(presvProfil, na.rm = T), 0))
  
  mean_EURmwh <- mean(join$EURmwh) # prumerna cena pro obdobi dodavky
  suma_profil <- sum(join$presvProfil)
  suma_EUR <- sum(join$EUR)
  
  predavaciCena <- suma_EUR/suma_profil
  nakladDiagram_pct <- predavaciCena/mean_EURmwh
  nakladDiagram_eur <- predavaciCena-mean_EURmwh
  
  predavaciCena_czk <- predavaciCena*cnb_kurz # -------------------------------------------------- !!! pouzivame POUZE CNB kurz !!!
  
  print("Vypocty probehly :-)")
  
  # ------------------ kontrola ***
  
  conditionNPF <- nakladDiagram_eur < 0
  if (conditionNPF) {
    showNotification(
      "Záporný náklad na diagram, zkontrolujte relevantnost dat.",
      type = "warning",
      duration = 15)
  }
  
  condition4GW <- any(acq$rocni_odber > 250000)
  # condition4GW <- any(acq$rocni_odber > 2500)
  if (condition4GW) {
    typ_vypoctu <- "indikativni"
    showNotification(
      "Zadané ACQ přesahuje 2,5 GWh, pro závaznou nabídku kontaktujte Nákupní oddělení.",
      type = "warning",
      duration = 30)
  }
  
  
  # ---------------------------------------------------------------------------- CREATE :: marze - IP
  
  
  marzeMin <- as.numeric(params$value[params$name == "marzeMin"])
  marzeDop <- as.numeric(params$value[params$name == "marzeDop"])

  txt_marzeMin <- sprintf("%.2f", marzeMin) # text, zobrazuje cislo s presne 2 decimals
  txt_marzeDop <- sprintf("%.2f", marzeDop)
  
  
  # ---------------------------------------------------------------------------- TABLE :: fix_cena - OK
  
  
  fix_cena <- data.frame(
    Parametr = c(
      "Obchodník",
      "Zákazník",
      "Období dodávky",
      "Vytvoření nabídky",
      "Platnost nabídky",
      "Typ nabídky",
      "CQ [MWh]",
      "ACQ [MWh]",
      "Předávací cena pro obchod [€]",
      "Předávací cena pro obchod [CZK]",
      "HM1 [€]",
      "Prodejní cena pro zákazníka [€]",
      "Prodejní cena pro zákazníka [CZK]",
      "Příplatek za specifika"
    ),
    
    Hodnota = c(
      obch,
      zak,
      paste0(delOd, " až ", delDo),
      format(as.POSIXct(Sys.time(), tz = "Europe/Prague", origin = "1899-12-30"), "%F %R"),
      paste0(format(as.POSIXct(Sys.Date(), origin = "1899-12-30"), "%F"), " 15:00"),
      ifelse(typ_vypoctu == "indikativni", paste("Pouze indikativní"), paste("Závazná")),
      round(suma_profil, 2),
      paste(
        paste(acq$year, acq$rocni_odber, sep = ": "),
        collapse = ", "),
      round(predavaciCena, 2),
      round(predavaciCena_czk, 2),
      paste("Minimální:", txt_marzeMin, " /  Doporučená:", txt_marzeDop),
      paste("Minimální:",  round(predavaciCena+marzeMin, 2), 
            " /  Doporučená:",  round(predavaciCena+marzeDop, 2)),
      paste("Minimální:", round(predavaciCena_czk+marzeMin*cnb_kurz, 2), 
            " /  Doporučená:", round(predavaciCena_czk+marzeDop*cnb_kurz, 2)),
      priplatek
    )
  )
  
  
  # ---------------------------------------------------------------------------- REPORTS :: txt, pdf 
  
  # simple txt
  
  on.exit({
    log <- paste(tms_now, typ_vypoctu, obch, zak, round(suma_profil, 2), delOd, delDo, round(predavaciCena, 2), sep = ";")
    write(log, "pdf_logy/kalkulackaEE_log.txt", append = TRUE)
  })
  
  message(typ_vypoctu)
  
  # pdf pro Nakup

  # if (typ_vypoctu == "zavazny") {
  #   
  #   timestamp <- format(tms_now, "%Y%m%d_%H%M", tz = "Europe/Prague")
  #   
  #   p <- ggplot(profil, aes(datum, profilMWh)) +
  #     geom_col(fill = "gold") +
  #     labs(x = "měsíc dodávky",
  #          y = "profil spotřeby [MWh]",
  #          title = "Profil spotřeby klienta") +
  #     scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
  #     scale_y_continuous(breaks = seq(0, 700, by = 50))+
  #     theme_light() +
  #     theme(axis.text.x = element_text(angle = 90))
  #   
  #   rmarkdown::render(
  #     input = "reportNakup.Rmd",
  #     output_file = paste0("pdf_logy/proNakup_VypocetFixCenyZP_report_",
  #                          timestamp, "_",
  #                          obch, "_", zak, "_", fin_cenaEUR, ".pdf"),
  #     output_format = "pdf_document",
  #     params = list(
  #       obchodnik = obch,
  #       zakaznik = zak,
  #       datum_od = delOd,
  #       datum_do = delDo,
  #       profil = profil,
  #       plot_profil = p,
  #       denni = denni, 
  #       fwd = fwd,
  #       otc = otc,
  #       fix_cena = fix_cena)
  #   )
  #   
  # } else {
  #   message("Automatické ukládání PDF přeskočeno – indikativní výpočet")
  # }

  
  # ---------------------------------------------------------------------------- RETURN :: fix_cena - OK
  
    
  return(list(
    # profil = profil,
    fwd = fwd,
    otc = otc,
    fix_cena = fix_cena
  ))
}
