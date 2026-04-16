library(tidyverse)
library(robotoolbox)
library(janitor)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(rJava)
library(mailR)
library(stringr)

# Charger les variables d'environnement
api_token <- Sys.getenv("KOBO_API_TOKEN")
form_id <- Sys.getenv("KOBO_FORM_ID")


# Vérifier les variables d'environnement
if (is.na(api_token) ||
    is.na(form_id) || api_token == "" || form_id == "") {
  stop(
    "L'API Token ou l'ID du formulaire est manquant. Vérifiez vos variables d'environnement."
  )
}

# URL de l'API KoboToolbox
url <- paste0("https://kf.kobotoolbox.org/api/v2/assets/",
              form_id,
              "/data/")

# Effectuer la requête GET
response <- GET(url, add_headers(Authorization = paste("Token", api_token)))
# Vérifier la réponse HTTP
if (status_code(response) != 200) {
  cat("Erreur lors de la récupération des données KoboToolbox.\n")
  cat(content(response, as = "text"), "\n")
  stop("Arrêt du script.")
}

# Convertir la réponse JSON en liste
data <- content(response, as = "parsed", simplifyVector = TRUE)$results

# Vérifier si des soumissions existent
if (length(data) == 0) {
  quit(status = 0)
}

# Extraire les informations des participants
participants <- data %>%
  mutate(
    iid = `_id`,
    email = `_2_Votre_mail`,
    nom = `_1_Votre_nom`,
    submission_time = `_submission_time`
  ) %>%
  select(iid, email, nom, submission_time)

# Charger la liste des soumissions déjà envoyées
sent_submissions_file <- "sent_submissions.json"

if (file.exists(sent_submissions_file)) {
  sent_submissions <- fromJSON(sent_submissions_file)
} else {
  sent_submissions <- data.frame(id = character(), stringsAsFactors = FALSE)
}

# Filtrer les nouvelles soumissions
data_df <- as_tibble(data)

new_submissions <- data_df %>%
  filter(!`_id` %in% sent_submissions$id)

if (nrow(new_submissions) == 0) {
  cat("Aucune nouvelle soumission.\n")
  quit(status = 0)
}

# Ajouter les nouveaux IDs à la liste
updated_ids <- unique(c(sent_submissions$id, new_submissions$`_id`))

write_json(data.frame(id = updated_ids), sent_submissions_file, pretty = TRUE)

# Charger  pour l'email
email_user <- Sys.getenv("MAIL_USERNAME_GHA")
email_pass <- Sys.getenv("MAIL_PASSWORD_GHA")

if (email_user == "" || email_pass == "") {
  stop("Identifiants email manquants.")
}

for (i in seq_len(nrow(new_submissions))) {
  submission_id <- new_submissions$`_id`[i]
  
  # Récupérer la ligne correspondante dans data
  row_index <- which(data$`_id` == submission_id)
 
  nom     <- data$`_1_Votre_nom`[row_index]
  email   <- data$`_2_Votre_mail`[row_index]
  date_sub <- data$`_submission_time`[row_index]
  
  if (is.na(email) || email == "")
    next
  
  
  
  
  body_mail <- paste0(
    "<p>Bonjour,</p>",
    "<p>Reception du mail de soumission.</p>",
    "<p>Cordialement,<br>",
    "<b>Système automatisé de recrutement</b></p>"
  )
  send.mail(
    from = email_user,
    to = "koglogerard@gmail.com",
    subject = paste("Nouvelle soumission -", nom),
    body = body_mail,
    smtp = list(
      host.name = "smtp.gmail.com",
      port = 587,
      user.name = email_user,
      passwd = email_pass,
      tls = TRUE
    ),
    authenticate = TRUE,
    send = TRUE,
    html = TRUE
  )
  
  cat("Email envoyé pour :", nom, "\n")
}
