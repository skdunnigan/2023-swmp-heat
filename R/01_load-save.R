source(here('R', '00_loadpkgs.R'))

fun_in <- function(x) sum(x, na.rm = TRUE)
fun_in2 <- function(x) max(x, na.rm = TRUE)

rkb_dat <- SWMPr::import_local(here('data'), station_code = 'rkbuhmet') %>%  
            SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) 

rkb_prcp <- SWMPr::aggreswmp(rkb_dat, FUN = fun_in, by = "months", params = c('totprcp')) %>% mutate(prcp_in = totprcp * 0.0393701)

rkb <- rkb_dat %>% 
  SWMPr::aggreswmp(by = "months", FUN = max()) %>%
  select(datetimestamp, atemp, rh) %>% 
  mutate(dew_pt = atemp - ((100-rh)/5.),
         dew_pt_f = ((dew_pt*9/5) + 32),
         atemp_f = ((atemp * 9/5) + 32)) %>% 
  select(datetimestamp, dew_pt_f, atemp_f) %>% 
  dplyr::left_join(rkb_prcp, by = "datetimestamp") %>% 
  select(-totprcp) %>% 
  mutate(year = lubridate::year(datetimestamp),
         month = lubridate::month(datetimestamp, label = TRUE)) %>% 
  filter(month %in% c("Jun", "Jul"))

save(rkb, file = here::here('output', 'data', 'rkb.RData'))

rm(rkb, rkb_dat, rkb_prcp)

apa_dat <- SWMPr::import_local(here('data'), station_code = 'apaebmet') %>%  
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) 

apa_prcp <- SWMPr::aggreswmp(apa_dat, FUN = fun_in, by = "months", params = c('totprcp')) %>% mutate(prcp_in = totprcp * 0.0393701)

apa <- apa_dat %>% 
  SWMPr::aggreswmp(by = "months") %>%
  select(datetimestamp, atemp, rh) %>% 
  mutate(dew_pt = atemp - ((100-rh)/5.),
         dew_pt_f = ((dew_pt*9/5) + 32),
         atemp_f = ((atemp * 9/5) + 32)) %>% 
  select(datetimestamp, dew_pt_f, atemp_f) %>% 
  dplyr::left_join(apa_prcp, by = "datetimestamp") %>% 
  select(-totprcp) %>% 
  mutate(year = lubridate::year(datetimestamp),
         month = lubridate::month(datetimestamp, label = TRUE)) %>% 
  filter(month %in% c("Jun", "Jul"))

save(apa, file = here::here('output', 'data', 'apa.RData'))

rm(apa, apa_dat, apa_prcp)

gtm_dat <- SWMPr::import_local(here('data'), station_code = 'gtmpcmet') %>%  
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) 

gtm_prcp <- SWMPr::aggreswmp(gtm_dat, FUN = fun_in, by = "months", params = c('totprcp')) %>% mutate(prcp_in = totprcp * 0.0393701)

gtm <- gtm_dat %>% 
  SWMPr::aggreswmp(by = "months") %>%
  select(datetimestamp, atemp, rh) %>% 
  mutate(dew_pt = atemp - ((100-rh)/5.),
         dew_pt_f = ((dew_pt*9/5) + 32),
         atemp_f = ((atemp * 9/5) + 32)) %>% 
  select(datetimestamp, dew_pt_f, atemp_f) %>% 
  dplyr::left_join(gtm_prcp, by = "datetimestamp") %>% 
  select(-totprcp) %>% 
  mutate(year = lubridate::year(datetimestamp),
         month = lubridate::month(datetimestamp, label = TRUE)) %>% 
  filter(month %in% c("Jun", "Jul"))

save(gtm, file = here::here('output', 'data', 'gtm.RData'))

rm(gtm, gtm_dat, gtm_prcp)