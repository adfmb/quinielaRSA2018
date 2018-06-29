
# n <- 5
# filler <- matrix("-", nrow = n, ncol = n,
#                  dimnames = list(NULL, paste0("V", seq_len(n))))

## prepare the OAuth token and set up the target sheet:
##  - do this interactively
##  - do this EXACTLY ONCE

# shiny_token <- gs_auth() # authenticate w/ your desired Google identity here
# saveRDS(shiny_token, "../Quinielas_Grupo_A/shiny_app_token.rds")
# ss <- gs_new("10_read-write-private-sheet",
#              row_extent = n, col_extent = n, input = filler)
# ss$sheet_key # 10kYZGTfXquVUwvBXH-8M-p01csXN6MNuuTzxnDdy3Pk

## if you version control your app, don't forget to ignore the token file!
## e.g., put it into .gitignore

googlesheets::gs_auth(token = "../Quinielas_Grupo_A/shiny_app_token.rds")
sheet_key <- "1I-Fmc-rEsjqrKwmoddgf5vhoNpgtcfgb9lFy-I6SujM"
ss <- googlesheets::gs_key(sheet_key)
