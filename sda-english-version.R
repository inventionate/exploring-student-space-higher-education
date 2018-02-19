# ***********************************************************************
# Strukturierte Datenanalyse Studienalltag ------------------------------
# ***********************************************************************

# ***********************************************************************
# Paket laden -----------------------------------------------------------
# ***********************************************************************

pacman::p_load_gh("Inventionate/TimeSpaceAnalysis")

# ***********************************************************************
# Repository authentifizieren -------------------------------------------
# ***********************************************************************

source("/Users/fabianmundt/Library/Mobile Documents/com~apple~CloudDocs/Dissertation/Sozioanalyse/repository.R")

# ***********************************************************************
# Daten laden -----------------------------------------------------------
# ***********************************************************************

# Basisdaten
loadFromLocalRepo(names(asearch("name:datensatz_allgemeine_angaben")))
loadFromLocalRepo(names(asearch("name:datensatz_demografische_angaben")))
loadFromLocalRepo(names(asearch("name:datensatz_eltern")))
loadFromLocalRepo(names(asearch("name:datensatz_schulzeit")))
loadFromLocalRepo(names(asearch("name:datensatz_kindheit_und_jugend")))
loadFromLocalRepo(names(asearch("name:datensatz_uebergangsphase")))
loadFromLocalRepo(names(asearch("name:datensatz_studienwahlmotive")))
loadFromLocalRepo(names(asearch("name:datensatz_studienwahl")))
loadFromLocalRepo(names(asearch("name:datensatz_studienwahl")))
# Semesterdaten
loadFromLocalRepo(names(asearch("name:datensatz_persoenliche_situation_ws_1516")))
loadFromLocalRepo(names(asearch("name:datensatz_gewoehnliche_studienwoche_ws_1516")))
loadFromLocalRepo(names(asearch("name:daten_freizeitgestaltung_1516")))
# Geometrisches Modell
source("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/mca-english-version.R")

# ***********************************************************************
# Datensatz zusammenstellen ---------------------------------------------
# ***********************************************************************

daten_hintergrund_1516 <- bind_cols(
  datensatz_allgemeine_angaben, 
  datensatz_demografische_angaben, 
  datensatz_eltern, 
  datensatz_schulzeit, 
  datensatz_kindheit_und_jugend, 
  datensatz_uebergangsphase, 
  datensatz_studienwahlmotive, 
  datensatz_studienwahl,
  datensatz_persoenliche_situation_ws_1516[, 19:26]) %>%
  filter(semester_id_1516 == "Wintersemester-15-16") %>%
  select(
    -interviewbereitschaft,
    -semester_id_1516,
    -semester_id_16,
    -semester_id_1617,
    -feedback_1516,
    -feedback_16,
    -feedback_1617)

# ***********************************************************************
# Allgemeiner Scan ------------------------------------------------------
# ***********************************************************************

# Allgemeiner Scan nach eta^2
daten_hintergrund_1516_eta2 <- colnames(daten_hintergrund_1516)[-1] %>% map_dfr(
  function(x) supvar_stats(mca_studienalltag, daten_hintergrund_1516, x, impute_ncp = 2)$var %>% tail(., n = 1)
) %>% add_column(variable = colnames(daten_hintergrund_1516)[-1], .before = 1)

# 1. Dimension
daten_hintergrund_1516_eta2 %>% select(variable, dim = Dim.1) %>% 
  arrange(desc(dim)) %>% as_tibble()

# 2. Dimension
daten_hintergrund_1516_eta2 %>% select(variable, dim = Dim.2) %>% 
  arrange(desc(dim)) %>% as_tibble()

# 1. Ebene/ Gesamtmodell
daten_hintergrund_1516_eta2 %>% select(variable, dim_1 = Dim.1, dim_2 = Dim.2) %>% 
  mutate(dim = dim_1 + dim_2) %>% arrange(desc(dim)) %>% as_tibble()

# ***********************************************************************
# Abitur Note -----------------------------------------------------------
# ***********************************************************************

daten_abitur_note <- daten_hintergrund_1516 %>% select(abitur_note) %>% 
  mutate(
    abitur_note = factor(cut(abitur_note, include.lowest=TRUE, right=TRUE, 
                      breaks= c(1, 1.75, 2.45, 2.95, 3.7),
                      labels = c("Abiturnote: sehr gut", "Abiturnote: gut", "Abiturnote: befriedigend", "Abiturnote: ausreichend")),
                      levels = c("Abiturnote: sehr gut", "Abiturnote: gut", "Abiturnote: befriedigend", "Abiturnote: ausreichend"))
  )

supvar_abitur_note <- supvar_stats(mca_studienalltag, daten_abitur_note, "abitur_note", impute = TRUE)
supvar_abitur_note

sda_abitur_note_12 <- fviz_gda_quali_supvar(mca_studienalltag, daten_abitur_note, "abitur_note",
                                         title = "Studienalltag (WS15/16) — Note Abitur", 
                                         path = TRUE, axes = c(1,2))
sda_abitur_note_12

fviz_gda_quali_ellipses(mca_studienalltag, daten_abitur_note, "abitur_note", title = "Studienalltag (WS15/16) — Note Abitur")

# ***********************************************************************
# Abitur Land -----------------------------------------------------------
# ***********************************************************************

supvar_abitur_land <- supvar_stats(mca_studienalltag, daten_hintergrund_1516, "abitur_land")
supvar_abitur_land

sda_abitur_land_12 <- fviz_gda_quali_supvar(mca_studienalltag, daten_hintergrund_1516, "abitur_land",
                                            title = "Studienalltag (WS15/16) — Land Abitur", 
                                            path = FALSE, axes = c(1,2))
sda_abitur_land_12

# ***********************************************************************
# Beruf Vater -----------------------------------------------------------
# ***********************************************************************

supvar_beruf_vater <- supvar_stats(mca_studienalltag, daten_hintergrund_1516, "beruf_vater")
supvar_beruf_vater

sda_beruf_vater_12 <- fviz_gda_quali_supvar(mca_studienalltag, daten_hintergrund_1516, "beruf_vater",
                                            title = "Studienalltag (WS15/16) — Beruf Vater", 
                                            path = FALSE, axes = c(1,2))
sda_beruf_vater_12

# ***********************************************************************
# Geschlecht ------------------------------------------------------------
# ***********************************************************************

supvar_geschlecht <- supvar_stats(mca_studienalltag, daten_hintergrund_1516, "geschlecht")
supvar_geschlecht

sda_geschlecht_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "geschlecht", 
                                          title = "Studienalltag (WS15/16) — Geschlecht", facet = TRUE)
sda_geschlecht_12

  # ***********************************************************************
# Alter -----------------------------------------------------------------
# ***********************************************************************

daten_alter <- daten_hintergrund_1516 %>% select(alter) %>%
  mutate(alter = fct_recode(alter, 
                            "18 bis 19" = "18", 
                            "18 bis 19" = "19", 
                            "20" = "20", 
                            "21 bis 22" = "21", 
                            "21 bis 22" = "22", 
                            "23 und älter" = "23 bis 25",  
                            "23 und älter" = "26 bis 29",  
                            "23 und älter" = "30 bis 38"))

supvar_alter <- supvar_stats(mca_studienalltag, daten_alter, "alter")
supvar_alter

sda_alter_12 <- fviz_gda_quali_supvar(mca_studienalltag, daten_alter,  "alter", 
                        title = "Studienalltag (WS15/16) — Alter")
sda_alter_12

# ***********************************************************************
# Geschwister -----------------------------------------------------------
# ***********************************************************************

supvar_geschwister <- supvar_stats(mca_studienalltag, daten_hintergrund_1516, "geschwister")
supvar_geschwister

sda_geschwister_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "geschwister", 
                        title = "Studienalltag (WS15/16) — Geschwister") 
sda_geschwister_12

# ***********************************************************************
# Staatsangehörigkeit ---------------------------------------------------
# ***********************************************************************

daten_staatsangehoerigkeit <- daten_hintergrund_1516 %>% 
  select(staatsangehörigkeit) %>%
  mutate(staatsangehörigkeit = fct_recode(
    staatsangehörigkeit,
    "deutsch" = "deutsch", 
    "doppelte" = "deutsch und französisch",
    "doppelte" = "deutsch und griechisch", 
    "doppelte" = "deutsch und serbisch",
    "doppelte" = "deutsch und türkisch", 
    "doppelte" = "doppelte ohne weitere Angaben", 
    "nicht deutsch" = "griechisch",
    "nicht deutsch" =  "italienisch",
    "nicht deutsch" = "kosovarisch", 
    "nicht deutsch" = "norwegisch", 
    "nicht deutsch" = "palästinensisch", 
    "nicht deutsch" = "türkisch", 
    "nicht deutsch" = "ukrainisch",
    "nicht deutsch" = "USA"))


supvar_staatsangehoerigkeit <- supvar_stats(mca_studienalltag, daten_staatsangehoerigkeit, "staatsangehörigkeit")
supvar_staatsangehoerigkeit

sda_staatsangehoerigkeit_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_staatsangehoerigkeit, "staatsangehörigkeit", 
                                                    title = "Studienalltag (WS15/16) — Staatsangehörigkeit")
sda_staatsangehoerigkeit_12

# ***********************************************************************
# Religionszugehörigkeit ------------------------------------------------
# ***********************************************************************

daten_religionszugehoerigkeit <- daten_hintergrund_1516 %>% 
  select(religionszugehörigkeit) %>%
  mutate(religionszugehörigkeit = fct_recode(
    religionszugehörigkeit,
    NULL = "andere", 
    "Christentum" = "Christentum", 
    "Islam" = "Islam", 
    NULL = "Judentum", 
    NULL = "keine Antwort", 
    "keine Religionsgemeinschaft" = "keine Relisgionsgemeinschaft"))

supvar_religionszugehoerigkeit <- supvar_stats(mca_studienalltag, daten_religionszugehoerigkeit, "religionszugehörigkeit")
supvar_religionszugehoerigkeit

sda_religionszugehoerigkeit_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_religionszugehoerigkeit,  "religionszugehörigkeit",
                                                       title = "Studienalltag (WS15/16) — Religionszugehörigkeit")
sda_religionszugehoerigkeit_12

# ***********************************************************************
# Bücherbesitz ----------------------------------------------------------
# ***********************************************************************

daten_buecherbesitz <- daten_hintergrund_1516 %>%
  mutate(bücherbesitz = fct_recode(
    bücherbesitz, 
    "1-50 Bücher" = "1-10 Bücher", 
    "1-50 Bücher" = "11-50 Bücher", 
    "51-250 Bücher" = "51-100 Bücher", 
    "51-250 Bücher" = "101-250 Bücher", 
    "mehr als 250 Bücher" = "251-500 Bücher", 
    "mehr als 250 Bücher" =  "mehr als 500 Bücher"))

supvar_buecherbesitz <- supvar_stats(mca_studienalltag, daten_buecherbesitz, "bücherbesitz")
supvar_buecherbesitz


sda_buecherbesitz_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_buecherbesitz, "bücherbesitz", 
                                             title = "Studienalltag (WS15/16) — Bücherbesitz")
sda_buecherbesitz_12

sda_buecherbesitz_supvar_12 <- fviz_gda_quali_supvar(mca_studienalltag, daten_buecherbesitz, "bücherbesitz", 
                                                     title = "Studienalltag (WS15/16) — Bücherbesitz", 
                                                     path = TRUE, scale_text = FALSE, size_text = 6) +
  scale_size_continuous(range = c(3,6))
sda_buecherbesitz_supvar_12
ggsave("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/SF books.pdf",sda_buecherbesitz_supvar_12)

# ***********************************************************************
# Schulabschluss Vater --------------------------------------------------
# ***********************************************************************

daten_schulabschluss_vater <- daten_hintergrund_1516 %>%
  mutate(
    schulabschluss_vater = fct_recode(
      schulabschluss_vater,
      "Hauptschule/keinen" = "unbekannt",
      "Hauptschule/keinen" = "keinen Schulabschluss", 
      "Hauptschule/keinen" = "Hauptschule"))

supvar_schulabschluss_vater <- supvar_stats(mca_studienalltag, daten_schulabschluss_vater, "schulabschluss_vater")
supvar_schulabschluss_vater

sda_schulabschluss_vater_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_schulabschluss_vater, "schulabschluss_vater", 
                                                       title = "Studienalltag (WS15/16) — Schulabschluss Vater", 
                                                       relevel = c("Abitur", "Realschule", "Hauptschule/keinen"))
sda_schulabschluss_vater_12
ggsave("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/SF school father.pdf",sda_schulabschluss_vater_12)

# ***********************************************************************
# Schulabschluss Mutter -------------------------------------------------
# ***********************************************************************

daten_schulabschluss_mutter <- daten_hintergrund_1516 %>%
  mutate(
    schulabschluss_mutter = fct_recode(
      schulabschluss_mutter,
      "Hauptschule/keinen" = "unbekannt",
      "Hauptschule/keinen" = "keinen Schulabschluss", 
      "Hauptschule/keinen" = "Hauptschule"))

supvar_schulabschluss_mutter <- supvar_stats(mca_studienalltag, daten_schulabschluss_mutter, "schulabschluss_mutter")
supvar_schulabschluss_mutter

sda_schulabschluss_mutter_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_schulabschluss_mutter, "schulabschluss_mutter", 
                                                       title = "Studienalltag (WS15/16) — Schulabschluss Mutter", 
                                                       relevel = c("Abitur", "Realschule", "Hauptschule/keinen"))
sda_schulabschluss_mutter_12
ggsave("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/SF school mother.pdf",sda_schulabschluss_mutter_12)

# ***********************************************************************
# Berufsabschluss Mutter ------------------------------------------------
# ***********************************************************************

supvar_berufsabschluss_mutter <- supvar_stats(mca_studienalltag, daten_hintergrund_1516, "berufsabschluss_mutter")
supvar_berufsabschluss_mutter

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "berufsabschluss_mutter", 
                        title = "Studienalltag (WS15/16) — Nachschulischer Abschluss Mutter",
                        relevel = rev(c("unbekannt", "keinen Berufsabschluss", "Fachakademie", "Fachhochschule", "Universität")))

# ***********************************************************************
# Berufsabschluss Vater -------------------------------------------------
# ***********************************************************************

supvar_berufsabschluss_vater <- supvar_stats(mca_studienalltag, daten_hintergrund_1516, "berufsabschluss_vater")
supvar_berufsabschluss_vater

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "berufsabschluss_vater", 
                        title = "Studienalltag (WS15/16) — Nachschulischer Abschluss Vater",
                        relevel = rev(c("unbekannt", "keinen Berufsabschluss", "Fachakademie", "Fachhochschule", "Universität")))

# ***********************************************************************
# Geburtsland Mutter ----------------------------------------------------
# ***********************************************************************

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "geburtsland_mutter",
                        title = "Studienalltag (WS15/16) — Geburtsland Mutter")

# ***********************************************************************
# Geburtsland Vater -----------------------------------------------------
# ***********************************************************************

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "geburtsland_vater",
                        title = "Studienalltag (WS15/16) — Geburtsland Vater")

# ***********************************************************************
# Schulbesuch Hauptschule -----------------------------------------------
# ***********************************************************************

supvar_hauptschule <- supvar_stats(mca_studienalltag, daten_hintergrund_1516, "hauptschule")
supvar_hauptschule

sda_hauptschule_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "hauptschule",
                        title = "Studienalltag (WS15/16) — Hauptschule")
sda_hauptschule_12

sda_hauptschule_supvar_12 <- fviz_gda_quali_supvar(mca_studienalltag, daten_hintergrund_1516, "hauptschule",
                                                   title = "Studienalltag (WS15/16) — Hauptschule", path = TRUE, scale_text = FALSE, size_text = 6) + 
  scale_size_continuous(range = c(3,6))
sda_hauptschule_supvar_12

# ***********************************************************************
# Schulbesuch Realschule ------------------------------------------------
# ***********************************************************************

supvar_realschule <- supvar_stats(mca_studienalltag, daten_hintergrund_1516, "realschule")
supvar_realschule

sda_realschule_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "realschule",
                        title = "Studienalltag (WS15/16) — Realschule") 
sda_realschule_12

sda_realschule_supvar_12 <- fviz_gda_quali_supvar(mca_studienalltag, daten_hintergrund_1516, "realschule",
                                                title = "Studienalltag (WS15/16) — Realschule", path = TRUE, scale_text = FALSE, size_text = 6) + 
  scale_size_continuous(range = c(3,6)) 
sda_realschule_supvar_12

# ***********************************************************************
# Schulbesuch Gymnasium -------------------------------------------------
# ***********************************************************************

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "gymnasium", title = "Studienalltag (WS15/16) — Gymnasium")

# ***********************************************************************
# Schulbesuch Gesamtschule ----------------------------------------------
# ***********************************************************************

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "gesamtschule", palette = FALSE, title = "Studienalltag (WS15/16) — Gesamtschule")

# ***********************************************************************
# Wohngebiet aufgewachsen -----------------------------------------------
# ***********************************************************************

daten_wohngebiet <- daten_hintergrund_1516 %>%
  mutate(
    wohngebiet_aufgewachsen = fct_recode(
      wohngebiet_aufgewachsen,
      "Dorf" = "Bauernhof"))

supvar_wohngebiet <- supvar_stats(mca_studienalltag, daten_wohngebiet, "wohngebiet_aufgewachsen")
supvar_wohngebiet

sda_wohngebiet_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_wohngebiet, "wohngebiet_aufgewachsen",
                        title = "Studienalltag (WS15/16) — Wohngebiet aufgewachsen",
                        relevel = rev(c("Dorf", "Vorort", "Stadt", "Großstadt")))
sda_wohngebiet_12

sda_wohngebiet_supvar_12 <- fviz_gda_quali_supvar(mca_studienalltag, daten_wohngebiet, "wohngebiet_aufgewachsen",
                                                  title = "Studienalltag (WS15/16) — Wohngebiet aufgewachsen", path = TRUE, scale_text = FALSE, size_text = 6,
                                                  relevel = rev(c("Dorf", "Vorort", "Stadt", "Großstadt"))) + 
  scale_size_continuous(range = c(3,6))
sda_wohngebiet_supvar_12

# ***********************************************************************
# Schüleraustausch ------------------------------------------------------
# ***********************************************************************

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "schüleraustausch", title = "Studienalltag (WS15/16) — Schüleraustausch")

# ***********************************************************************
# Alternative Zeitmodelle -----------------------------------------------
# ***********************************************************************

supvar_stats(mca_studienalltag, daten_hintergrund_1516, "alternative_zeitmodelle")

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "alternative_zeitmodelle", palette = FALSE,
                        title = "Studienalltag (WS15/16) — Alternative Zeitmodelle")

# ***********************************************************************
# Beziehung zur Schule --------------------------------------------------
# ***********************************************************************

daten_beziehung <- daten_hintergrund_1516 %>%
  mutate(beziehung = fct_recode(
    beziehung,
    "Schule: schlecht" = "Bez. Schule: sehr schlecht",
    "Schule: schlecht" = "Bez. Schule: schlecht",
    "Schule: weder noch" = "Bez. Schule: weder noch",
    "Schule: gut" = "Bez. Schule: gut",
    "Schule: sehr gut" = "Bez. Schule: sehr gut", 
    NULL = "Bez. Schule: kann ich nicht sagen"))

supvar_beziehung <- supvar_stats(mca_studienalltag, daten_beziehung, "beziehung")
supvar_beziehung

sda_beziehung_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_beziehung, "beziehung",
                        title = "Studienalltag (WS15/16) — Beziehung zur Schule",
                        relevel = rev(c("Schule: schlecht", "Schule: weder noch", "Schule: gut", "Schule: sehr gut")))
sda_beziehung_12

sda_beziehung_supvar_12 <- fviz_gda_quali_supvar(mca_studienalltag, daten_beziehung, "beziehung",
                                              relevel = c("Schule: schlecht", "Schule: weder noch", "Schule: gut", "Schule: sehr gut"),
                                              title = "Studienalltag (WS15/16) — Beziehung zur Schule", path = TRUE, scale_text = FALSE, size_text = 6) + 
  scale_size_continuous(range = c(3,6)) 
sda_beziehung_supvar_12

# ***********************************************************************
# Vorbereitung durch die Schule -----------------------------------------
# ***********************************************************************

daten_vorbereitung <- daten_hintergrund_1516 %>%
  mutate(vorbereitung = fct_recode(
    vorbereitung,
    "Vorbereitung: schlecht" = "Vorbereitung: sehr schlecht",#  "Vorbereitung: gut", "Vorbereitung: sehr gut",
    #"Vorbereitung: gut", 
    #"Vorbereitung: sehr gut", 
    NULL = "Vorbereitung: kann ich nicht sagen"))

supvar_vorbereitung <- supvar_stats(mca_studienalltag, daten_vorbereitung, "vorbereitung")
supvar_vorbereitung

sda_vorbereitung_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_vorbereitung, "vorbereitung",
                        title = "Studienalltag (WS15/16) — Vorbereitung durch die Schule",
                        relevel = rev(c("Vorbereitung: schlecht", "Vorbereitung: weder noch", "Vorbereitung: gut", "Vorbereitung: sehr gut")))
sda_vorbereitung_12

sda_vorbereitung_supvar_12 <- fviz_gda_quali_supvar(mca_studienalltag, daten_vorbereitung, "vorbereitung",
                                              title = "Studienalltag (WS15/16) — Vorbereitung durch die Schule", path = TRUE, scale_text = FALSE, size_text = 6,
                                              relevel = rev(c("Vorbereitung: schlecht", "Vorbereitung: weder noch", "Vorbereitung: gut", "Vorbereitung: sehr gut"))) + 
  scale_size_continuous(range = c(3,6))
sda_vorbereitung_supvar_12

# ***********************************************************************
# Zweitstudium ----------------------------------------------------------
# ***********************************************************************

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "anderes_studium_begonnen", 
                        title = "Studienalltag (WS15/16) — Zweitstudium")

supvar_stats(mca_studienalltag, daten_hintergrund_1516, "anderes_studium_begonnen")

# ***********************************************************************
# Hochschulabschluss ----------------------------------------------------
# ***********************************************************************

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "hochschulabschluss",
                        title = "Studienalltag (WS15/16) — Hochschulabschluss")

supvar_stats(mca_studienalltag, daten_hintergrund_1516, "hochschulabschluss")

# ***********************************************************************
# Berufsabschluss -------------------------------------------------------
# ***********************************************************************

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "berufsabschluss",
                        title = "Studienalltag (WS15/16) — Berufsabschluss")

supvar_stats(mca_studienalltag, daten_hintergrund_1516, "berufsabschluss")

# ***********************************************************************
# Studienwahlgrund ------------------------------------------------------
# ***********************************************************************

daten_wichtigster_grund_studienwahl <- daten_hintergrund_1516 %>%
  mutate(wichtigster_grund_studienwahl = fct_recode(
    wichtigster_grund_studienwahl,
    NULL = "wichtigster Grund Studium: Ansehen",
    NULL = "wichtigster Grund Studium: Verdienst",
    NULL = "wichtigster Grund Studium: wissenschaftliches Interesse",
    NULL = "wichtigster Grund Studium: kleinstes Übel"))

supvar_wichtigster_grund_studienwahl <- supvar_stats(mca_studienalltag, daten_wichtigster_grund_studienwahl, "wichtigster_grund_studienwahl")
supvar_wichtigster_grund_studienwahl

sda_wichtigster_grund_studienwahl_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_wichtigster_grund_studienwahl, "wichtigster_grund_studienwahl", 
                                                                palette = FALSE, title = "Studienalltag (WS15/16) — Studienwahl")
sda_wichtigster_grund_studienwahl_12


# ***********************************************************************
# Hochschulwahlgrund ----------------------------------------------------
# ***********************************************************************

daten_wichtigster_grund_hochschulwahl <- daten_hintergrund_1516 %>%
  mutate(wichtigster_grund_hochschulwahl = fct_recode(
    wichtigster_grund_hochschulwahl,
    NULL = "wichtigster Grund PH: Wahlmöglichkeit"))

supvar_wichtigster_grund_hochschulwahl <- supvar_stats(mca_studienalltag, daten_wichtigster_grund_hochschulwahl, "wichtigster_grund_hochschulwahl")
supvar_wichtigster_grund_hochschulwahl

sda_wichtigster_grund_hochschulwahl_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_wichtigster_grund_hochschulwahl, 
                                                               "wichtigster_grund_hochschulwahl", palette = FALSE,
                                                               title = "Studienalltag (WS15/16) — Hochschulwahl")
sda_wichtigster_grund_hochschulwahl_12

# ***********************************************************************
# Bachelorart -----------------------------------------------------------
# ***********************************************************************

supvar_bachelorart <- supvar_stats(mca_studienalltag, daten_hintergrund_1516, "bachelorart")
supvar_bachelorart

sda_bachelorart_12 <- fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "bachelorart",
                                           title = "Studienalltag (WS15/16) — Gewähltes Lehramt") 
sda_bachelorart_12
ggsave("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/SF ba type.pdf",sda_bachelorart_12)

# ***********************************************************************
# Europalehramt ---------------------------------------------------------
# ***********************************************************************

supvar_stats(mca_studienalltag, daten_hintergrund_1516, "europalehramt")

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "europalehramt", 
                        title = "Studienalltag (WS15/16) — Europalehramt")

# ***********************************************************************
# Masterpläne -----------------------------------------------------------
# ***********************************************************************

supvar_stats(mca_studienalltag, daten_hintergrund_1516, "masterpläne")

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "masterpläne", 
                        title = "Studienalltag (WS15/16) — Masterpläne")

# ***********************************************************************
# Erststudium -----------------------------------------------------------
# ***********************************************************************

supvar_stats(mca_studienalltag, daten_hintergrund_1516, "erstes_studiensemester")

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "erstes_studiensemester",
                        title = "Studienalltag (WS15/16) — Erstes Studiensemester an der PH Karlsruhe") 

# ***********************************************************************
# O-Phase ---------------------------------------------------------------
# ***********************************************************************

supvar_stats(mca_studienalltag, daten_hintergrund_1516, "orientierungsphase")

fviz_gda_quali_ellipses(mca_studienalltag, daten_hintergrund_1516, "orientierungsphase",
                        title = "Studienalltag (WS15/16) — O-Phase")

# ***********************************************************************
# Speichern der Analysen in der Repository ------------------------------
# ***********************************************************************
