library(googlesheets)
gs_auth(new_user = TRUE)
gs_ls()
for_gs <- gs_title("resultados_reales")
for_gs2<-gs_key("1I-Fmc-rEsjqrKwmoddgf5vhoNpgtcfgb9lFy-I6SujM")
for_gs_sheet <- gs_read(for_gs)

gap_ss <- gs_gap()
# shiny_token <- gs_auth() # authenticate w/ your desired Google identity here
# saveRDS(shiny_token, "Quinielas_Grupo_A/shiny_app_token.rds")