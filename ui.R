
library(shiny)

dienste <- list("Beratung" = 1, "Bereitst. bes. Fähigk." = 2, "Diebstahl" = 3, "Kampf" = 4, "Körperliche Hilfe" = 5, "Lastentransport" = 6, "Schutz" = 7, "Suche" = 8, "Spionage" = 9, "Personentransport" = 10, "Wache" = 11, "Keiner" = 12)
fähigkeiten <- list("Keine" = 1, "Astralsinn" = 2, "Bes. Angriff" = 3, "Folgeschaden" = 4, "Formlosigkeit I" = 5, "Formlosigkeit II" = 6, "Fortpflanzungsfähigk." = 7, "Lebenssinn" = 8, "Regeneration I" = 9, "Regeneration II" = 10, "Resistenz gegen profane Angriffe" = 11, "Resistenz gegen [MERKMAL]" = 12, "Immunität gegen profane Angriffe" = 13, "Immunität gegen [MERKMAL]" = 14, "Schreckgestalt I" = 15, "Suche nach Unbek." = 16, "Verb. zum Beschw." = 17, "Verwundbarkeit" = 18, "Wasserwesen" = 19, "Zus. Aktion" = 20)

shinyUI(fluidPage(theme = "bootstrap.css",

  # Application title
  titlePanel("Golembau_R"),
  fluidRow(
    column(3,
           h4("KONSTRUKTIONSSCHWIERIGKEIT"),
           wellPanel(
             
             sliderInput("material", label = h5("Materialerschwernis"),
                         min = 0, max = 21, value = 0),
             sliderInput("affinität", label = h5("Affinität"),
                         min =-7, max = 21, value =0),
             fluidRow(
             column(6,
             radioButtons("größe", 
                               label = h5("Größe"), 
                               choices = list("Winzig" = 1, "Sehr Klein" = 2, 
                                              "Klein" = 3, "Mittel" = 4,
                                              "Groß" = 5, "Sehr Groß" = 6),
                               selected = 3)
             ),
            column(6,
             radioButtons("material2", 
                               label = h5("Materialart"), 
                               choices = list("Holz" = 1, "Stein" = 2, 
                                              "Lehm" = 3, "Metall" = 4,
                                              "Sand" = 5),
                               selected = 1)
             
             
              )),
            fluidRow(
              column(6,checkboxInput("gliedmaßen", label = "zusätzliche Gliedmaßen", value = FALSE)),
              column(6,checkboxInput("flügel", label = "Flügel", value = FALSE))
           )
           )),
    column(3,
           h4("EIGENSCHAFTEN und WERTE"),
               wellPanel(
                 br(),
                 br(),
                 fluidRow(
                   column(6,
                   p(actionButton("mu_plus", label = "MU+1", width = "55px"), actionButton("mu_minus", label = "MU-1", width = "55px")),
                   p(actionButton("kl_plus", label = "KL+1", width = "55px"), actionButton("kl_minus", label = "KL-1", width = "55px")),
                   p(actionButton("in_plus", label = "IN+1", width = "55px"), actionButton("in_minus", label = "IN-1", width = "55px")),
                   p(actionButton("ch_plus", label = "CH+1", width = "55px"), actionButton("ch_minus", label = "CH-1", width = "55px")),
                   p(actionButton("ff_plus", label = "FF+1", width = "55px"), actionButton("ff_minus", label = "FF-1", width = "55px")),
                   p(actionButton("ge_plus", label = "GE+1", width = "55px"), actionButton("ge_minus", label = "GE-1", width = "55px")),
                   p(actionButton("ko_plus", label = "KO+1", width = "55px"), actionButton("ko_minus", label = "KO-1", width = "55px")),
                   p(actionButton("kk_plus", label = "KK+1", width = "55px"), actionButton("kk_minus", label = "KK-1", width = "55px")),
                   actionButton("aup_plus", label = "AU+100", width = "115px")
                 ),
                 column(6,
                        p(actionButton("lep_plus", label = "LE+5", width = "55px"), actionButton("lep_minus", label = "LE-5", width = "55px")),
                        p(actionButton("ini_plus", label = "INI+1", width = "55px"), actionButton("ini_minus", label = "INI-1", width = "55px")),
                        p(actionButton("rs_plus", label = "RS+1", width = "55px"), actionButton("rs_minus", label = "RS-1", width = "55px")),
                        p(actionButton("gs_plus", label = "GS+1", width = "55px"), actionButton("gs_minus", label = "GS-1", width = "55px")),
                        p(actionButton("mr_plus", label = "MR+1", width = "55px"), actionButton("mr_minus", label = "MR-1", width = "55px")),
                        p(actionButton("at_plus", label = "AT+1", width = "55px"), actionButton("at_minus", label = "AT-1", width = "55px")),
                        p(actionButton("pa_plus", label = "PA+1", width = "55px"), actionButton("pa_minus", label = "PA-1", width = "55px")),
                        p(actionButton("tp_plus", label = "TP+1", width = "55px"), actionButton("tp_minus", label = "TP-1", width = "55px")),
                        actionButton("aup_minus", label = "AU-100", width = "115px")
                 )
                 ),
                 br(),
                 br(),
                 br()
                  )

           
           ),
    column(3,
           h4("TALENTE"),
           wellPanel(
             textInput("talent1", label = "", value = "Talent eingeben..."),
             p(actionButton("t1aktivieren", label = "Aktivieren", width = "100px"),
               actionButton("t1löschen", label = "Entfernen", width = "100px"),
               actionButton("t1_plus", label = "+", width = "20px"),
               actionButton("t1_minus", label = "-", width = "20px")),

             textInput("talent2", label = "", value = "Talent eingeben..."),
             p(actionButton("t2aktivieren", label = "Aktivieren", width = "100px"),
               actionButton("t2löschen", label = "Entfernen", width = "100px"),
               actionButton("t2_plus", label = "+", width = "20px"),
               actionButton("t2_minus", label = "-", width = "20px")),
             
             textInput("talent3", label = "", value = "Talent eingeben..."),
             p(actionButton("t3aktivieren", label = "Aktivieren", width = "100px"),
               actionButton("t3löschen", label = "Entfernen", width = "100px"),
               actionButton("t3_plus", label = "+", width = "20px"),
               actionButton("t3_minus", label = "-", width = "20px")),
             
             textInput("talent4", label = "", value = "Talent eingeben..."),
             p(actionButton("t4aktivieren", label = "Aktivieren", width = "100px"),
               actionButton("t4löschen", label = "Entfernen", width = "100px"),
               actionButton("t4_plus", label = "+", width = "20px"),
               actionButton("t4_minus", label = "-", width = "20px")),
             
             br()
           )

           ),
      column(3,
             h4("DIENSTE"),
             wellPanel(
       
        selectInput("dienst1", label ="", 
                    choices = dienste, selected = 12),
        selectInput("dienst2", label ="", 
                    choices = dienste, selected = 12),
        selectInput("dienst3", label ="", 
                    choices = dienste, selected = 12),
        selectInput("dienst4", label ="", 
                    choices = dienste, selected = 12),
        selectInput("dienst5", label ="", 
                    choices = dienste, selected = 12),
        selectInput("dienst6", label ="", 
                    choices = dienste, selected = 12),
        selectInput("dienst7", label ="", 
                    choices = dienste, selected = 12))
      )
  ),
  fluidRow(
    column(3,
           h4("FÄHIGKEITEN"),
           wellPanel(
             selectInput("fähigkeit1", label ="", 
                         choices = fähigkeiten, selected = 1),
             selectInput("fähigkeit2", label ="", 
                         choices = fähigkeiten, selected = 1),
             selectInput("fähigkeit3", label ="", 
                         choices = fähigkeiten, selected = 1),
             selectInput("fähigkeit4", label ="", 
                         choices = fähigkeiten, selected = 1),
             selectInput("fähigkeit5", label ="", 
                         choices = fähigkeiten, selected = 1),
             selectInput("fähigkeit6", label ="", 
                         choices = fähigkeiten, selected = 1),
             selectInput("fähigkeit7", label ="", 
                         choices = fähigkeiten, selected = 1),
             selectInput("fähigkeit8", label ="", 
                         choices = fähigkeiten, selected = 1),
            checkboxInput("gift", label = h4("Fähigkeit: Gift"), value = FALSE),
            radioButtons("giftart", label = "Gift-Typ",
                         choices = list("Blut/Waffe" = 1, "Kontakt" = 2,
                                        "Atem" = 3),selected = 1),
            sliderInput("giftstufe", label = "Giftstufe",
                        min = 1, max = 20, value = 1))

           ),

    column(3,
           h4("BESCHWÖRUNGSMODIFIKATOR"),
           wellPanel(
           sliderInput("thesis", label = "Thesis",
                       min = 0, max = 7, value = 0),
           sliderInput("beschaffenheit", label = "Materialbeschaffenheit",
                       min = -3, max = 3, value = 0),
           sliderInput("bekleidung", label = "Bekleidung",
                       min = 0, max = 2, value = 0),
           sliderInput("kerzen", label = "Kerzen",
                       min = 0, max = 7, value = 0),
           sliderInput("kreide", label = "Kreide",
                       min = 0, max = 7, value = 0),
           sliderInput("donaria", label = "Donaria",
                       min = 0, max = 7, value = 0),
           sliderInput("ort", label = "Ort",
                       min = -7, max = 7, value = 0),
           sliderInput("sterne", label = "Sterne",
                       min = -7, max = 7, value = 0),
           fluidRow(
             column(6,
                     checkboxInput("bannschwert", label = "Bannschwert", value = FALSE)
                     ),
             column(6,
                    checkboxInput("golembauer", label = "Golembauer", value = FALSE)
                     )
           )
           )
           ),
    column(6,
           h4("ERGEBNIS"),
           wellPanel(
             h4(strong((textOutput("material")))),
             br(),
             br(),
             fluidRow(
               column(6,
                      p(strong("Beschwörung:"), textOutput("beschwörung", inline = T))
               ),
               column(6, 
                      p(strong("Beherrschung:"), textOutput("beherrschung", inline = T))
               )
               
             ),
             p(strong("Kosten:"), textOutput("kosten", inline = T), "AsP, davon ein", textOutput("kosten_p", inline = T), "permanent"),
             p(strong("Thesis:"), "Magiekundeprobe erschwert um", textOutput("thesis_erschwernis", inline=T), "bei Zeitaufwand von", textOutput("thesis_dauer", inline = T), "ZE"),
             fluidRow(
               column(3,
                      p(strong("MU:"), textOutput("mu", inline = T)),
                      p(strong("FF:"), textOutput("ff", inline = T)),
                      br(),
                      p(strong("IB:"), textOutput("ib", inline = T)),
                      p(strong("Angriff:")),
                      p(strong("GS:"), textOutput("gs", inline = T))  
               ),
               column(3,
                      p(strong("KL:"), textOutput("kl", inline = T)),
                      p(strong("GE:"), textOutput("ge", inline = T)),
                      br(),
                      p(strong("PA:"), textOutput("pa", inline = T)),
                      p(strong("DK:"), textOutput("dk", inline = T)),
                      p(strong("AuP:"), textOutput("au", inline = T))  
               ),
               column(3,
                      p(strong("IN:"), textOutput("int", inline = T)),
                      p(strong("KO:"), textOutput("ko", inline = T)),
                      br(),
                      p(strong("LeP:"), textOutput("le", inline = T)),
                      p(strong("AT:"), textOutput("at", inline = T)),
                      p(strong("MR:"), textOutput("mr", inline = T))  
               ),
               column(3,
                      p(strong("CH:"), textOutput("ch", inline = T)),
                      p(strong("KK:"), textOutput("kk", inline = T)),
                      br(),
                      p(strong("RS:"), textOutput("rs", inline = T)),
                      p(strong("TP:"), textOutput("tp", inline = T))
                      
               )
               
             ),
             p(strong("Startloyalität:"), textOutput("lo", inline = T)),
             p(strong("Besondere Eigenschaften:"), "Immunität gegen Merkmal Form, leichte Empfindlichkeit gegen geweihte Objekte/mittlere Empfindlichkeit gegenüber geweihten Objekten der Gegengottheit des angerufenen Erzdämons", textOutput("eigenschaften", inline = T), sep=""),
             p(strong("Talente:"), textOutput("talente", inline = T)),
             p(strong("Mögliche Dienste:"),  "Bewegung, Warte, Gefolgschaft", textOutput("dienste", inline = T))
             
            
           ))
           
    
      )
  )
)