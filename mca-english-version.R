# ***********************************************************************
# Soziale Topologie Studienalltag ---------------------------------------
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

loadFromLocalRepo(names(asearch("name:datensatz_allgemeine_angaben")))
# Wintersemester 15/16
loadFromLocalRepo(names(asearch("name:daten_zeitverwendung_1516")))
loadFromLocalRepo(names(asearch("name:daten_studienorte_1516")))
loadFromLocalRepo(names(asearch("name:daten_rahmenbedingungen_1516")))
# Sommersemester 16
loadFromLocalRepo(names(asearch("name:daten_zeitverwendung_16")))
loadFromLocalRepo(names(asearch("name:daten_studienorte_16")))
loadFromLocalRepo(names(asearch("name:daten_rahmenbedingungen_16")))
# Wintersemester 16/17
loadFromLocalRepo(names(asearch("name:daten_zeitverwendung_1617")))
loadFromLocalRepo(names(asearch("name:daten_studienorte_1617")))
loadFromLocalRepo(names(asearch("name:daten_rahmenbedingungen_1617")))

# ***********************************************************************
# Datensatz zusammenstellen ---------------------------------------------
# ***********************************************************************

daten_studienalltag_1516 <- bind_cols(
  daten_zeitverwendung_1516, 
  daten_studienorte_1516, 
  daten_rahmenbedingungen_1516)

daten_studienalltag_16 <- bind_cols(
  daten_zeitverwendung_16, 
  daten_studienorte_16, 
  daten_rahmenbedingungen_16)

daten_studienalltag_1617 <- bind_cols(
  daten_zeitverwendung_1617, 
  daten_studienorte_1617, 
  daten_rahmenbedingungen_1617)

daten_studienalltag_gesamt <- bind_rows(
  daten_studienalltag_1516, 
  daten_studienalltag_16,
  daten_studienalltag_1617
) 

# Gruppenangaben
gruppen <- list(spalten = c(ncol(daten_zeitverwendung_1516), 
                            ncol(daten_studienorte_1516), 
                            ncol(daten_rahmenbedingungen_1516)),
                namen = c("Time use", 
                          "Study places",
                          "General conditions"))

# IDs generieren
daten_studienalltag_1516_id <- datensatz_allgemeine_angaben %>% filter(semester_id_1516 == "Wintersemester-15-16") %>% select(questionnaire_id) %>% data.frame() %>%
  mutate(questionnaire_id = paste0(questionnaire_id, ""))
nrow(daten_studienalltag_1516_id)

daten_studienalltag_16_id <- datensatz_allgemeine_angaben %>% filter(semester_id_16 == "Sommersemester-16") %>% select(questionnaire_id) %>% data.frame() %>%
  mutate(questionnaire_id = paste0(questionnaire_id, "_1"))
nrow(daten_studienalltag_16_id)

daten_studienalltag_1617_id <- datensatz_allgemeine_angaben %>% filter(semester_id_1617 == "WS-16-17") %>% select(questionnaire_id) %>% data.frame() %>%
  mutate(questionnaire_id = paste0(questionnaire_id, "_2"))
nrow(daten_studienalltag_1617_id)

rowname_id <- rbind(daten_studienalltag_1516_id, daten_studienalltag_16_id, daten_studienalltag_1617_id)

# ***********************************************************************
# Umgang mit fehlenden Werten -------------------------------------------
# ***********************************************************************

# Imputieren
imputiert_daten_studienalltag_gesamt <- imputeMCA(daten_studienalltag_gesamt %>% data.frame() %>% mutate_all(as.factor), ncp = 2)

# ID check
nrow(imputiert_daten_studienalltag_gesamt$completeObs) == nrow(rowname_id)

# Passive Individuen extrahieren
passive_ind <- imputiert_daten_studienalltag_gesamt$completeObs %>% slice((nrow(daten_studienalltag_1516) + 1):nrow(daten_studienalltag_gesamt))  

# Individuen mit Fragebogencodes versehen
daten_gda_studienalltag <- daten_studienalltag_1516 %>% 
  bind_rows(passive_ind) %>%
  add_column(id = rowname_id$questionnaire_id) %>%
  mutate_all(funs(as.factor)) %>%
  data.frame() %>%
  column_to_rownames("id")

# Passive Kategorien bestimmen
excl_studienalltag <- get_index_mod(daten_gda_studienalltag, pattern = "kann ich nicht sagen")

# ***********************************************************************
# Übersetzung der Kategorien --------------------------------------------
# ***********************************************************************

daten_gda_studienalltag %<>% rownames_to_column() %>% transmute(
  rowname,
  self_study = fct_recode(selbststudienzeit,
                         "Self-study: up to 7h" = "Selbststudium: bis 7 Stunden",
                         "Self-study: 7-15h" = "Selbststudium: 7-15 Stunden",
                         "Self-study: 15-25h" = "Selbststudium: 15-25 Stunden",
                         "Self-study: more than 25h" = "Selbststudium: mehr als 25 Stunden"),
  time_pattern = fct_recode(zeitmuster,
                            "Time pattern: private time" = "Zeitmuster: Private Zeit",
                            "Time pattern: self-study" = "Zeitmuster: Selbststudium",
                            "Time pattern: work" = "Zeitmuster: Erwerbsarbeit",
                            "Time pattern: commuter" = "Zeitmuster: Pendler/innen"),
  leisure_pattern = fct_recode(freizeitmuster,
                               "Leisure pattern: cultural/political" = "Freizeit: kulturell/politisch",
                               "Leisure pattern: party/chill" = "Freizeit: feiern/chillen",
                               "Leisure pattern: retreated/religious" = "Freizeit: eingekehrt/religiös"),
  courses_number = fct_recode(lehrveranstaltungen_anzahl,
                              "Coureses: up to 10" = "Anzahl Lehrveranstaltungen: bis 10",
                              "Coureses: 11-12" = "Anzahl Lehrveranstaltungen: 11 bis 12",
                              "Coureses: more than 12" = "Anzahl Lehrveranstaltungen: mehr als 12"),
  learning_group = fct_recode(selbstorganisierte_lerngruppe,
                              "Learning group: no" = "Lerngruppe: nein",
                              "Learning group: yes" = "Lerngruppe: ja"),
  schedule = fct_recode(veranstaltungsplan,
                        "Schedule: dissatisfied" = "Mit dem VP unzufrieden",
                        "Schedule: satisfied" = "Mit dem VP zufrieden"),
  temporal_situation = fct_recode(zeitliche_situation,
                                  "Temporal situation: neither good nor bad" = "Zeitliche Situation: weder gut noch schlecht",
                                  "Temporal situation: good" = "Zeitliche Situation: gut",
                                  "Temporal situation: cannot say" = "Zeitliche Situation: kann ich nicht sagen"),
  attention = fct_recode(aufmerksamkeit,
                         "Attention: in the morning" = "Aufmerksamkeit: morgens",
                         "Attention: midday" = "Aufmerksamkeit: mittags",
                         "Attention: afternoons" = "Aufmerksamkeit: nachmittags",
                         "Attention: evening/night" = "Aufmerksamkeit: abends/nachts",
                         "Attention: cannot say" = "Aufmerksamkeit: kann ich nicht sagen"),
  learning_place_alone = fct_recode(lernort_alleine,
                                    "Preferred place of learning: Home" = "Präferierter Lernort: Zuhause",
                                    "Preferred place of learning: PH" = "Präferierter Lernort: PH"),
  residence_lecture_time = fct_recode(wohnort_vorlesungszeit,
                                      "Residence: Own apartment" = "Wohnort: eigene Wohnung",
                                      "Residence: Shared flat" = "Wohnort: WG/ Wohnheim",
                                      "Residence: Parents" = "Wohnort: Eltern"),
  residence_semester_km = fct_recode(wohnort_semester_km,
                                     "Distance to Uni: up to 5km" = "Wohnortentfernung zur PH: bis 5km",
                                     "Distance to Uni: 5-20km" = "Wohnortentfernung zur PH: 5–20km",
                                     "Distance to Uni: 20-40km" = "Wohnortentfernung zur PH: 20–40km",
                                     "Distance to Uni: more than 40km" = "Wohnortentfernung zur PH: mehr als 40km"),
  learning_places_university = fct_recode(lernorte_ph,
                                  "Learning places Uni: none" = "Lernorte PH: keiner",
                                  "Learning places Uni: 1-2" = "Lernorte PH: 1-2",
                                  "Learning places Uni: more than 2" = "Lernorte PH: mehr als 2"),
  learning_places_not_university = fct_recode(lernorte_nicht_ph,
                                      "Learning places not Uni: 1-2" = "Lernorte außerhalb der PH: 1-2",
                                      "Learning places not Uni: more than 2" = "Lernorte außerhalb der PH: mehr als 2"),
  learn_university = fct_recode(lernen_ph,
                                "Learning at the Uni: often" = "Lernen an der PH: oft",
                                "Learning at the Uni: sometimes" = "Lernen an der PH: machmal",
                                "Learning at the Uni: rarely" = "Lernen an der PH: selten"),
  learn_not_university = fct_recode(lernen_nicht_ph,
                                    "Learning at other places: often" = "Lernen an anderen Orten: oft",
                                    "Learning at other places: sometimes" = "Lernen an anderen Orten: manchmal",
                                    "Learning at other places: rarely" = "Lernen an anderen Orten: selten"),
  relaxation_university = fct_recode(entspannung_hochschule,
                                     "Relaxation at the Uni: often" = "Entspannung am Hochschulort: oft",
                                     "Relaxation at the Uni: sometimes" = "Entspannung am Hochschulort: manchmal",
                                     "Relaxation at the Uni: rarely" = "Entspannung am Hochschulort: selten"),
  eating_university = fct_recode(essen_hochschule,
                                 "Eating at the Uni: often" = "Essen am Hochschulort: oft",
                                 "Eating at the Uni: sometimes" = "Essen am Hochschulort: manchmal",
                                 "Eating at the Uni: rarely" = "Essen am Hochschulort: selten"),
  financing_parents = fct_recode(finanzierung_eltern,
                                 "Financing by parents: yes" = "Finanzierung Eltern: ja",
                                 "Financing by parents: no" = "Finanzierung Eltern: nein"),
  financing_work_lecture_time = fct_recode(finanzierung_arbeit_vorlesungszeit,
                                           "Financing by work lecture time: yes" = "Finanzierung Arbeit Vorlesungszeit: ja",
                                           "Financing by work lecture time: no" = "Finanzierung Arbeit Vorlesungszeit: nein"),
  financing_work_semester_holidays = fct_recode(finanzierung_arbeit_semesterferien,
                                                "Financing by work lecture-free time: yes" = "Finanzierung Arbeit vorlesungsfreie Zeit: ja",
                                                "Financing by work lecture-free time: no" = "Finanzierung Arbeit vorlesungsfreie Zeit: nein"),
  financial_situation = fct_recode(finanzielle_situation,
                                   "Financial situation: poor" = "Finanzielle Situation: schlecht",
                                   "Financial situation: good" = "Finanzielle Situation: gut",
                                   "Financial situation: cannot say" = "Finanzielle Situation: kann ich nicht sagen"),
  status_study = fct_recode(stellenwert_studium,
                            "Importance of study: important" = "Stellenwert Studium: wichtig",
                            "Importance of study: rather important" ="Stellenwert Studium: eher wichtig",
                            "Importance of study: unimportant" = "Stellenwert Studium: unwichtig",
                            "Importance of study: cannot say" = "Stellenwert Studium: kann ich nicht sagen"),
  friends_university_number = fct_recode(freunde_ph_anzahl,
                                         "Uni Friends: up to 2" = "PH Freunde: bis 2",
                                         "Uni Friends: 3-5" = "PH Freunde: 3 bis 5",
                                         "Uni Friends: more than 5" = "PH Freunde: mehr als 5"),
  exchange_teachers = fct_recode(austausch_lehrende,
                                 "Exchange with teachers: often" = "Austausch mit Lehrenden: oft",
                                 "Exchange with teachers: sometimes" = "Austausch mit Lehrenden: manchmal",
                                 "Exchange with teachers: rarely" = "Austausch mit Lehrenden: selten",
                                 "Exchange with teachers: never" = "Austausch mit Lehrenden: nie"),
  arrival_university = fct_recode(anreise_hochschule,
                                  "Arrival University: on foot" = "Anreise Hochschule: zu Fuß",
                                  "Arrival University: bicycle" = "Anreise Hochschule: Fahrrad",
                                  "Arrival University: train" = "Anreise Hochschule: Bahn",
                                  "Arrival University: car" = "Anreise Hochschule: Auto")
) %>% column_to_rownames()

# ***********************************************************************
# Spezifische Multiple Korrespondenzanalyse berechnen -------------------
# ***********************************************************************

# Spezifische MCA berechnen
mca_studienalltag <- MCA(daten_gda_studienalltag,
                         excl = excl_studienalltag, 
                         graph = FALSE,
                         tab.disj = imputeMCA(daten_gda_studienalltag %>% slice(1:nrow(daten_studienalltag_1516)), ncp = 2)$tab.disj,
                         ind.sup = (nrow(daten_studienalltag_1516) + 1):nrow(daten_studienalltag_gesamt))
# Eigenwerte potten
plot_eigenvalues <- add_theme(fviz_eig(mca_studienalltag, choice = "eigenvalue")) + 
  xlab("Axes") + ylab("Eigenvalues") + coord_fixed(ratio = 50);plot_eigenvalues
ggsave("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/eigenvalues.pdf", plot_eigenvalues, width = 10, height = 10, units = "cm")

# Modified Rates, um eine Entscheidung bzgl. der zu interpretierenden Achsen treffen zu können (vgl. Le Roux/Rouanet 2004: 209)
modified_rates(mca_studienalltag)

# Auswahl der Eigenwerte
mca_studienalltag$eig %>% data.frame() %>% select(1,4,5) %>% slice(1:7)

# Beitrag der einzelnen Gruppen zur Gesamtvarianz
gda_describe_group(mca_studienalltag, gruppen$spalten, gruppen$namen)$overall_variance_group_ctr
gda_describe_group(mca_studienalltag, gruppen$spalten, gruppen$namen)$dim_variance_group_ctr[,1:5]

# ***********************************************************************
# Speichern der Datentabelle --------------------------------------------
# ***********************************************************************

Hmisc::describe(mca_studienalltag$call$X)

reporttools::tableNominal(mca_studienalltag$call$X, comment = FALSE)

# ***********************************************************************
# Spezifische Multiple Korrespondenzanalyse Visualisierungen ------------
# ***********************************************************************

# Individuen

# Achse 1 und 2
plot_mca_ind_12 <- fviz_gda_conc_ellipse(mca_studienalltag, title = "", fill = "transparent", axis_lab_name = "Axis") ;plot_mca_ind_12
ggsave("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/Cloud of individuals.pdf", plot_mca_ind_12,width = 25, height = 25, units = "cm")

# Achse 1
plot_mca_achse_1 <- fviz_gda_var_axis(mca_studienalltag, group = gruppen$spalten, group_names = gruppen$namen, axis = 1,
                                      title = "Cloud of categories — Axis 1", axis_lab_name = "Axis", 
                                      group_lab_name = "Variable groups");plot_mca_achse_1
desc_mca_achse_1 <- gda_describe_axis(mca_studienalltag, axis = 1)
ggsave("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/Cloud of categories - axis 1.pdf", plot_mca_achse_1,width = 25, height = 25, units = "cm")

# Achse 2
plot_mca_achse_2 <- fviz_gda_var_axis(mca_studienalltag, group = gruppen$spalten, group_names = gruppen$namen, axis = 2,
                                      title = "Cloud of categories — Axis 2",  
                                      axis_lab_name = "Axis", group_lab_name = "Variable groups");plot_mca_achse_2
desc_mca_achse_2 <- gda_describe_axis(mca_studienalltag, axis = 2)
ggsave("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/Cloud of categories - axis 2.pdf", plot_mca_achse_2,width = 25, height = 25, units = "cm")

# Repräsentationsquadrat
fviz_mca_var_corr(mca_studienalltag, axes = c(1,2)) + xlim(0, 0.25) + ylim(0, 0.25)
fviz_mca_var_corr(mca_studienalltag, axes = c(3,4)) + xlim(0, 0.25) + ylim(0, 0.25)

# Biplot Achse 1 und 2
plot_mca_biplot_12 <- fviz_gda_var(mca_studienalltag, group = gruppen$spalten, group_names = gruppen$namen, 
                                   title = "", contrib = "auto", individuals = TRUE, 
                                   axis_lab_name = "Axis", group_lab_name = "Variable groups");plot_mca_biplot_12
ggsave("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/Biplot.pdf", plot_mca_biplot_12, width = 25, height = 25, units = "cm")

 # ***********************************************************************
# Hierarchische Clusteranalyse der sMCA ---------------------------------
# ***********************************************************************

# hcpc_studienalltag <- HCPC(mca_studienalltag, nb.par = 6)

hcpc_studienalltag <- HCPC(mca_studienalltag, nb.clust = 3, graph = FALSE, nb.par = 6)

df_clust <- hcpc_studienalltag$data.clust %>% as_tibble() %>% mutate(clust = glue("Profile {clust}"))

# ***********************************************************************
# Hierarchische Clusteranalyse Visualisierungen -------------------------
# ***********************************************************************

plot_clusteranalyse_12 <- fviz_gda_quali_ellipses(mca_studienalltag, df_clust, "clust", facet = FALSE,  
                                                  title = "HCPC", plot_eta2 = FALSE, axis_lab_name = "Axis");plot_clusteranalyse_12
ggsave("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/HCPC ellipsis.pdf", plot_clusteranalyse_12, width = 25, height = 25, units = "cm")

plot_clusteranalyse_facets_12 <- fviz_gda_quali_ellipses(mca_studienalltag, df_clust, "clust", facet = TRUE, ncol = 3,
                                                         title = "HCPC", plot_eta2 = FALSE, axis_lab_name = "Axis");plot_clusteranalyse_facets_12
ggsave("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/HCPC facets.pdf", plot_clusteranalyse_facets_12, width = 25, height = 25, units = "cm")

# Dendrogram
plot_dendrogram <- fviz_dendrogram(hcpc_studienalltag, 
                                   palette = c("#4DAF4A", "#E41A1C", "#377EB8"), 
                                   cluster = 3, 
                                   hline = 0.04, 
                                   hlabel = "3 Cluster",
                                   linetype = "longdash",
                                   pointsize = 0,
                                   cut_upper = 0.04,
                                   title = "HCPC Dendrogram");plot_dendrogram
ggsave("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/Dendrogram.pdf", plot_dendrogram, width = 20, height = 17, units = "cm")

# Auswahl hinzufügen
id_cluster_1_interview <- c("Mia" = "45675", "Svenja" = "54902", "Sophie" = "22704", "Jule" = "20316")
id_cluster_2_interview <- c("Oliver" = "55542", "Hedda" = "21234")
id_cluster_3_interview <- c("Paul" = "37493", "Leonie" = "56740", "Xana" = "28592")
id_cluster_4_interview <- c("Kathrin" = "42373")
id_ind_interview <- c(id_cluster_1_interview, id_cluster_2_interview, id_cluster_3_interview, id_cluster_4_interview)

# Koordinaten und Namen der Profile bestimmen
profiles <- mca_studienalltag$ind$coord %>% data.frame() %>% rownames_to_column(var = "name") %>% 
  bind_cols(., tibble(cluster = hcpc_studienalltag$data.clust$clust)) %>% 
  filter(name %in% id_ind_interview) %>% 
  mutate(name = fct_recode(
    name,
    "Mia" = "45675", 
    "Svenja" = "54902",
    "Oliver" = "55542", 
    "Kathrin" = "42373",
    "Paul" = "37493", 
    "Leonie" = "56740", 
    "Sophie" = "22704", 
    "Jule" = "20316", 
    "Xana" = "28592",
    "Hedda" = "21234"))

# Clusterplot Interviewpartner/innen
pacman::p_unload(TimeSpaceAnalysis);pacman::p_load(TimeSpaceAnalysis)
plot_clusteranalyse_interview_12 <- fviz_gda_quali_ellipses(mca_studienalltag, df_clust, "clust", facet = FALSE, show_prop = TRUE,
                                                            title = "", plot_eta2 = FALSE, axis_lab_name = "Axis") + 
  geom_label_repel(data = profiles, inherit.aes = FALSE, aes(x = Dim.1, y = Dim.2, label = name, colour = cluster), 
                   size = 5, alpha = 1, segment.colour = "black", segment.size = 1.5) +
  designate_axes(0.63, -0.025, c("Intensity +", "Intensity -")) +
  designate_axes(0.025, 0.75, c("Distance -", "Distance +"), rotate = TRUE);plot_clusteranalyse_interview_12
ggsave("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/HCPC profiles.pdf", plot_clusteranalyse_interview_12, width = 25, height = 25, units = "cm")

# Biplot Achse 1 und 2 Interviewpartner/innen
plot_mca_biplot_cluster_12 <- fviz_gda_var(mca_studienalltag, group = gruppen$spalten, group_names = gruppen$namen, 
                                           title = "Biplot", contrib = "auto", individuals = TRUE, axis_lab_name = "Axis", 
                                           group_lab_name = "Variable groups") +
  geom_label_repel(data = profiles, inherit.aes = FALSE, aes(x = Dim.1, y = Dim.2, label = name),
                   size = 5, alpha = 0.85, segment.colour = "black", segment.size = 1.5) +
  scale_colour_brewer(name = "Variable groups", palette = "Set1") +
  scale_shape_manual(name = "Variable groups", values = c(15, 17, 18));plot_mca_biplot_cluster_12
ggsave("/Users/fabianmundt/LemaS Cloud/Topological MMR/Diagramme MCA/Biplot profiles.pdf", plot_mca_biplot_cluster_12, width = 25, height = 25, units = "cm")
