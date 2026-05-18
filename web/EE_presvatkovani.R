# ------------------------------------------------------------------------------ packages - OK


packages <- c(
  "tidyverse",
  "readxl",
  "writexl",
  "lubridate", # je v tidyverse
  # "zoo",
  "plotly",
  "shiny" # aplikace
)

invisible(lapply(packages, library, character.only = TRUE))


# ------------------------------------------------------------------------------ paths and load - OK


path <- "C:/Users/krizova/Documents/R/02 cenoveKalkukacky/EE/EE_presvatkovani/data/"
path_ele <- "X:/Nakup _ NEW/Elektřina/Ele_M/Kalkulacka/presvatkovani/"
path_out <- "C:/Users/krizova/Documents/R/02 cenoveKalkukacky/EE/EE_presvatkovani/results/"

load <- read_excel(paste0(path_ele, "Savencia_import_2025_15min.xlsx"), sheet = "r") %>% 
  mutate(datum_cas = str_remove(datum_cas, ":00\\s...$"),
         timestamp = as.POSIXct(datum_cas, format = "%Y-%m-%d %H:%M", tz = "Europe/Prague"))


# ------------------------------------------------------------------------------ create diagram and define pairing - OK


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
  
  # filter(porm15 == 49) %>% 
  
  select(timestamp, date, typeDay, holiday, dst, iso_week, "week" = upd_week, weekday, porm15, profil, parovani) 


nrow(diagram) == 35040
view(diagram)
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
val_y2 <- val_y1+2


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
    select(timestamp, "presvProfil" = final, week, weekday, holiday, dst)
  
  
  assign(paste0("presv_", i), presv)
  rm(presv)
  
}

presv_diagram <- dplyr::bind_rows(mget(ls(pattern = "^presv")))
