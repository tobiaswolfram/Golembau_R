#dataframes

dienste <<- c("Beratung","Bereitstellung besonderer Fähigkeiten","Diebstahl","Kampf","Körperliche Hilfe","Lastentransport","Schutz","Suche","Spionage","Personentransport","Wache","Keiner")
fähigkeiten <<- c("Keine","Astralsinn","Besonderer Angriff","Folgeschaden","Formlosigkeit I","Formlosigkeit II","Fortpflanzungsfähigkeit","Lebenssinn","Regeneration I","Regeneration II","Resistenz gegen profane Angriffe","Resistenz gegen [Merkmal]","Immunität gegen profane Angriffe","Immunität gegen [MERKMAL]","Schreckgestalt I","Suche nach Unbek.","Verbindung zum Beschwörer","Verwundbarkeit","Wasserwesen","Zusätzliche Aktion")
fähigkeiten_kosten <<- c(0,5,2,4,4,7,12,4,4,7,5,5,10,10,4,7,5,0,9)
material_names <<- c("Holz", "Stein", "Lehm", "Metall", "Sand")
größe_names <<- c("Winziger","Sehr kleiner","Kleiner","Mittelgroßer","Großer","Sehr großer")
größe_dk <<- c("H", "H", "HN", "HN", "NS", "NS")
größe_klasse <<- c("sehr kleiner Gegner", "sehr kleiner Gegner", "kleiner Gegner", "", "großer Gegner", "sehr großer Gegner")
größe_erschwernis <<- c(3,3,0,3,6,9)
materialwerte <<- cbind.data.frame(c("Holz", "Lehm", "Stein", "Metall","Sand"),c(8,7,6,4,11),c(8,6,6,5,14),c(4,2,2,1,4), c(4,4,4,4,4), c(0,2,3,8,0), c(60,90,90,120,50), c(200,300,300,400,200), c(3,3,5,7,4), c(6,5,2,1,3), c(10,15,15,20,15), c(10,12,14,16,12), c(1,1,1,1,2), c(1,1,1,1,2), c(1,1,1,1,2), c(6,4,2,1,8), c(10,10,8,6,12), c(15,18,20,24,14), c(12, 14, 18, 20, 12),  c("Verwundbarkeit (Feuer)", "Immunität (Feuer, Stich)", "Immunität (Feuer, Stich)", "Immunität (Feuer, Stich)", "Immunität (Feuer, Stich), Verwundbarkeit (Schnitt)"), c("1W6+7", "1W6+8", "1W6+9", "1W6+10", "1W6+7"))
names(materialwerte) <<- c("Material", "IB", "AT", "PA", "TP1", "TP2", "LE", "AU", "RS", "GS", "MR", "MU", "KL", "IN", "CH", "FF", "GE", "KO", "KK", "Besonderheiten", "Startloyaliät") 
größenmultiplikator <<- cbind(c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(0.125, 0.25,0.5,1,1.5,2.25), c(0.125, 0.25,0.5,1,1.5,2.25), c(2/3*2/3*2/3, 2/3*2/3, 2/3, 1, 4/3, 4/3*4/3), c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(1,1,1,1,1,1)) 
größenadditor <<- cbind(c(0,0,0,0,0,0), c(0,0,0,0,0,0), c(0,0,0,0,0,0), c(0,0,0,0,0,0), c(0,0,0,0,0,0), c(0,0,0,0,0,0), c(0,0,0,0,0,0), c(0,0,0,0,0,0), c(0,0,0,0,0,0), c(3,2,1,0,-1,-2), c(0,0,0,0,0,0), c(0,0,0,0,0,0), c(0,0,0,0,0,0), c(0,0,0,0,0,0), c(0,0,0,0,0,0), c(6, 4, 2, 0 , -2 , -4), c(6, 4, 2, 0 , -2 , -4), c(-6,-4,-2,0,2,4), c(-6,-4,-2,0,2,4), c(0,0,0,0,0,0), c(0,0,0,0,0,0)) #spalte 10-19
kosten <<- c(6,6,8,12,16,20)
loyalität <<- c(7,8,9,10,7)
wertesteigerung <<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) 
talentnamen <<- c("","","","")
talentwerte <<- c(0,0,0,0)

shinyServer(function(input, output) {
  
  
  #Erschwernis

  

  val <- reactiveValues(material = materialwerte[,2:19], größe_m = größenmultiplikator[,2:19], größe_a = größenadditor[,2:19], erschwernis = 0, wertesteigerung = wertesteigerung, talentnamen = talentnamen, talentwerte = talentwerte)
  
  konstruktionsschwierigkeit <- reactive({
    (input$material+input$affinität+größe_erschwernis[as.integer(input$größe)]+(if(input$gliedmaßen == TRUE){7}else{0})+(if(input$flügel == TRUE){4}else{0}))
  })
  
  beschwörungsmodifikator <- reactive({
    (input$sterne+input$ort+input$beschaffenheit-input$thesis-input$bekleidung-input$kerzen-input$kreide-input$donaria-(if(input$bannschwert == TRUE){1}else{0})-(if(input$golembauer == TRUE){3}else{0}))
  })
  
  observeEvent(input$mu_plus, {
    val$erschwernis = val$erschwernis +2
    val$wertesteigerung[11] = val$wertesteigerung[11]+1
  }) 
  
  observeEvent(input$kl_plus, {
    val$erschwernis = val$erschwernis +2
    val$wertesteigerung[12] = val$wertesteigerung[12]+1
  }) 
  
  observeEvent(input$int_plus, {
    val$erschwernis = val$erschwernis +2
    val$wertesteigerung[13] = val$wertesteigerung[13]+1
  }) 
  
  observeEvent(input$ch_plus, {
    val$erschwernis = val$erschwernis +2
    val$wertesteigerung[14] = val$wertesteigerung[14]+1
  }) 
  
  observeEvent(input$ff_plus, {
    val$erschwernis = val$erschwernis +2
    val$wertesteigerung[15] = val$wertesteigerung[15]+1
  }) 
  
  observeEvent(input$ge_plus, {
    val$erschwernis = val$erschwernis +2
    val$wertesteigerung[16] = val$wertesteigerung[16]+1
  }) 
  
  observeEvent(input$ko_plus, {
    val$erschwernis = val$erschwernis +2
    val$wertesteigerung[17] = val$wertesteigerung[17]+1
  }) 
  
  observeEvent(input$kk_plus, {
    val$erschwernis = val$erschwernis +2
    val$wertesteigerung[18] = val$wertesteigerung[18]+1
  }) 
  
  observeEvent(input$mu_minus, {
    if (val$wertesteigerung[11] > 0) {
      val$erschwernis = val$erschwernis -2
      val$wertesteigerung[11] = val$wertesteigerung[11]-1
    }
    }) 
  
  observeEvent(input$kl_minus, {
    if (val$wertesteigerung[12] > 0) {
      val$erschwernis = val$erschwernis -2
      val$wertesteigerung[12] = val$wertesteigerung[12]-1
    }
    }) 
  
  observeEvent(input$int_minus, {
    if (val$wertesteigerung[13] > 0) {
      val$erschwernis = val$erschwernis -2
      val$wertesteigerung[13] = val$wertesteigerung[13]-1
    }
    }) 
  
  observeEvent(input$ch_minus, {
    if (val$wertesteigerung[14] > 0) {
      val$erschwernis = val$erschwernis -2
      val$wertesteigerung[14] = val$wertesteigerung[14]-1
    }
    }) 
  
  observeEvent(input$ff_minus, {
    if (val$wertesteigerung[15] > 0) {
      val$erschwernis = val$erschwernis -2
      val$wertesteigerung[15] = val$wertesteigerung[15]-1
    }
    }) 
  
  observeEvent(input$ge_minus, {
    if (val$wertesteigerung[16] > 0) {
      val$erschwernis = val$erschwernis -2
      val$wertesteigerung[16] = val$wertesteigerung[16]-1
    }
    }) 
  
  observeEvent(input$ko_minus, {
    if (val$wertesteigerung[17] > 0) {
      val$erschwernis = val$erschwernis -2
      val$wertesteigerung[17] = val$wertesteigerung[17]-1
    }
    }) 
  
  observeEvent(input$kk_minus, {
    if (val$wertesteigerung[18] > 0) {
      val$erschwernis = val$erschwernis -2
      val$wertesteigerung[18] = val$wertesteigerung[18]-1
    }
  }) 

  observeEvent(input$ini_plus, {
    val$erschwernis = val$erschwernis +1
    val$wertesteigerung[1] = val$wertesteigerung[1]+1
  })
  
  observeEvent(input$at_plus, {
    val$erschwernis = val$erschwernis +4
    val$wertesteigerung[2] = val$wertesteigerung[2]+1
  })
  
  observeEvent(input$pa_plus, {
    val$erschwernis = val$erschwernis +4
    val$wertesteigerung[3] = val$wertesteigerung[3]+1
  })
  
  observeEvent(input$tp_plus, {
    val$erschwernis = val$erschwernis +4
    val$wertesteigerung[5] = val$wertesteigerung[5]+1
  })
  
  observeEvent(input$lep_plus, {
    val$erschwernis = val$erschwernis +2
    val$wertesteigerung[6] = val$wertesteigerung[6]+5
  })
  
  observeEvent(input$aup_plus, {
    val$erschwernis = val$erschwernis +1
    val$wertesteigerung[7] = val$wertesteigerung[7]+100
  })
  
  observeEvent(input$rs_plus, {
    val$erschwernis = val$erschwernis +1
    val$wertesteigerung[8] = val$wertesteigerung[8]+1
  })
  
  observeEvent(input$gs_plus, {
    val$erschwernis = val$erschwernis +1
    val$wertesteigerung[9] = val$wertesteigerung[9]+1
  })
  
  observeEvent(input$mr_plus, {
    val$erschwernis = val$erschwernis +1
    val$wertesteigerung[10] = val$wertesteigerung[10]+1
  })
  
  
  
  observeEvent(input$ini_minus, {
    if (val$wertesteigerung[1] > 0) {
    val$erschwernis = val$erschwernis -1
    val$wertesteigerung[1] = val$wertesteigerung[1]-1
    }
  })
  
  observeEvent(input$at_minus, {
    if (val$wertesteigerung[2] > 0) {
    val$erschwernis = val$erschwernis -4
    val$wertesteigerung[2] = val$wertesteigerung[2]-1
    }
  })
  
  observeEvent(input$pa_minus, {
    if (val$wertesteigerung[3] > 0) {
    val$erschwernis = val$erschwernis -4
    val$wertesteigerung[3] = val$wertesteigerung[3]-1
    }
  })
  
  observeEvent(input$tp_minus, {
    if (val$wertesteigerung[5] > 0) {
    val$erschwernis = val$erschwernis -4
    val$wertesteigerung[5] = val$wertesteigerung[5]-1
    }
  })
  
  observeEvent(input$lep_minus, {
    if (val$wertesteigerung[6] > 0) {
    val$erschwernis = val$erschwernis -2
    val$wertesteigerung[6] = val$wertesteigerung[6]-5
    }
  })
  
  observeEvent(input$aup_minus, {
    if (val$wertesteigerung[7] > 0) {
    val$erschwernis = val$erschwernis -1
    val$wertesteigerung[7] = val$wertesteigerung[7]-100
    }
  })
  
  observeEvent(input$rs_minus, {
    if (val$wertesteigerung[8] > 0) {
    val$erschwernis = val$erschwernis -1
    val$wertesteigerung[8] = val$wertesteigerung[8]-1
    }
  })
  
  observeEvent(input$gs_minus, {
    if (val$wertesteigerung[9] > 0) {
    val$erschwernis = val$erschwernis -1
    val$wertesteigerung[1] = val$wertesteigerung[1]-1
    }
  })
  
  observeEvent(input$mr_minus, {
    if (val$wertesteigerung[10] > 0) {
    val$erschwernis = val$erschwernis -1
    val$wertesteigerung[1] = val$wertesteigerung[1]-1
    }
  })
  
  
  größe <- reactive({input$größe})
  material2 <- reactive({input$material2})

  basiswerte <- reactive({
    (((val$material[as.integer(material2()),])*val$größe_m[as.integer(größe()),])+val$größe_a[as.integer(größe()),])+val$wertesteigerung
    })

  beherrschung <- reactive({
    if (round(basiswerte()[12]) <= 6){
      round(basiswerte()[12])
    }
    else 6 + 2*(round(basiswerte()[12]-6))
  })
  
  # Talente
  
  observeEvent(input$t1aktivieren, {
    val$talentnamen[1] = input$talent1
    val$talentwerte[1] = 5
    val$erschwernis = val$erschwernis+4
  })
  
  observeEvent(input$t1löschen, {
    if (val$talentnamen[1] != ""){
    val$talentnamen[1] = ""
    val$talentwerte[1] = 0
    val$erschwernis = val$erschwernis -4
    }
  })
  
  observeEvent(input$t1_plus, {
    val$talentwerte[1] = val$talentwerte[1]+3
    val$erschwernis = val$erschwernis +1
  })  
  
  observeEvent(input$t1_minus, {
    if (val$talentwerte[1] >= 8){
      val$talentwerte[1] = val$talentwerte[1]-3
      val$erschwernis = val$erschwernis -1
    } 
    else {val$talentwerte[1] = 5}
  })  
  
  observeEvent(input$t2aktivieren, {
    val$talentnamen[2] = input$talent2
    val$talentwerte[2] = 5
    val$erschwernis = val$erschwernis+4
  })
  
  observeEvent(input$t2löschen, {
    if (val$talentnamen[2] != ""){
      val$talentnamen[2] = ""
      val$talentwerte[2] = 0
      val$erschwernis = val$erschwernis -4
    }
  })
  
  observeEvent(input$t2_plus, {
    val$talentwerte[2] = val$talentwerte[2]+3
    val$erschwernis = val$erschwernis +1
  })  
  
  observeEvent(input$t2_minus, {
    if (val$talentwerte[2] >= 8){
      val$talentwerte[2] = val$talentwerte[2]-3
      val$erschwernis = val$erschwernis -1
    } 
    else {val$talentwerte[2] = 5}
  }) 
  
  observeEvent(input$t3aktivieren, {
    val$talentnamen[3] = input$talent3
    val$talentwerte[3] = 5
    val$erschwernis = val$erschwernis+4
  })
  
  observeEvent(input$t3löschen, {
    if (val$talentnamen[3] != ""){
      val$talentnamen[3] = ""
      val$talentwerte[3] = 0
      val$erschwernis = val$erschwernis -4
    }
  })
  
  observeEvent(input$t3_plus, {
    val$talentwerte[3] = val$talentwerte[3]+3
    val$erschwernis = val$erschwernis +1
  })  
  
  observeEvent(input$t3_minus, {
    if (val$talentwerte[3] >= 8){
      val$talentwerte[3] = val$talentwerte[3]-3
      val$erschwernis = val$erschwernis -1
    } 
    else {val$talentwerte[3] = 5}
  }) 

  observeEvent(input$t4aktivieren, {
    val$talentnamen[4] = input$talent4
    val$talentwerte[4] = 5
    val$erschwernis = val$erschwernis+4
  })
  
  observeEvent(input$t4löschen, {
    if (val$talentnamen[4] != ""){
      val$talentnamen[4] = ""
      val$talentwerte[4] = 0
      val$erschwernis = val$erschwernis -4
    }
  })
  
  observeEvent(input$t4_plus, {
    val$talentwerte[4] = val$talentwerte[4]+3
    val$erschwernis = val$erschwernis +1
  })  
  
  observeEvent(input$t4_minus, {
    if (val$talentwerte[4] >= 8){
      val$talentwerte[4] = val$talentwerte[4]-3
      val$erschwernis = val$erschwernis -1
    } 
    else {val$talentwerte[4] = 5}
  }) 
  # Dienste
  
  dienste_erschwernis <- reactive({
    (if(input$golembauer == FALSE){1}else{0})*((if(input$dienst2 != 12){3}else{0})+(if(input$dienst3 != 12){3}else{0}))+(if(input$dienst4 != 12){3}else{0})+(if(input$dienst5 != 12){3}else{0})+(if(input$dienst6 != 12){3}else{0})+(if(input$dienst7 != 12){3}else{0})
  })
  
  # Fähigkeiten
  
  fähigkeiten_erschwernis <- reactive({
    (if(input$fähigkeit1 != 1){fähigkeiten_kosten[as.integer(input$fähigkeit1)]}else{0})+(if(input$fähigkeit2 != 1){fähigkeiten_kosten[as.integer(input$fähigkeit2)]}else{0})+(if(input$fähigkeit3 != 1){fähigkeiten_kosten[as.integer(input$fähigkeit3)]}else{0})+(if(input$fähigkeit4 != 1){fähigkeiten_kosten[as.integer(input$fähigkeit4)]}else{0})+(if(input$fähigkeit5 != 1){fähigkeiten_kosten[as.integer(input$fähigkeit5)]}else{0})+(if(input$fähigkeit6 != 1){fähigkeiten_kosten[as.integer(input$fähigkeit6)]}else{0})+(if(input$fähigkeit7 != 1){fähigkeiten_kosten[as.integer(input$fähigkeit7)]}else{0})+(if(input$fähigkeit8 != 1){fähigkeiten_kosten[as.integer(input$fähigkeit8)]}else{0})+gift_erschwernis()
  })
  
  gift_erschwernis <- reactive({
    round((if(input$gift == TRUE){1}else{0})*(if(input$giftart == 1){0.5}else if(input$giftart == 1){1}else{2})*input$giftstufe)
  })
  
  gesamterschwernis <- reactive({
    val$erschwernis + beschwörungsmodifikator() + konstruktionsschwierigkeit() + dienste_erschwernis() + fähigkeiten_erschwernis()
  })

  # Output
  
  output$material <- renderText({ 
   paste(größe_names[as.integer(input$größe)], " ", material_names[as.integer(input$material2)],"golem",sep="")
    })
  
  
  output$kosten <- renderText({
    paste(kosten[as.integer(input$größe)], "W6", sep = "")
  })
  
  output$kosten_p <- renderText({
    if (input$golembauer == FALSE) {
      paste("1/20")
    } else {
      paste("1/50")
    }  
  })
  
  output$thesis_erschwernis <- renderText({
    paste(round((gesamterschwernis()+input$thesis)/2))
  })
  
  output$thesis_dauer <- renderText({
    paste(beherrschung()*3)
  })
  
  output$beschwörung <- renderText({
    if (gesamterschwernis() < 0){
      paste(gesamterschwernis(),sep="")
    } else paste("+", gesamterschwernis(), sep="")
    
      })
  
  output$beherrschung <- renderText({
    paste("+", beherrschung(), sep="")
  })
  
  output$mu <- renderText({ 
    paste(round(basiswerte()[11]))
  })
  
  output$kl <- renderText({ 
    paste(round(basiswerte()[12]))
  })  
  
  output$int <- renderText({ 
    paste(round(basiswerte()[13]))
  })  
  
  output$ch <- renderText({ 
    paste(round(basiswerte()[14]))
  })  
  
  output$ff <- renderText({ 
    if (round(basiswerte()[15]) > 0){
    paste(round(basiswerte()[15]))
    }
    else {
    paste("0")  
    }
  })
  
  output$ge <- renderText({ 
    paste(round(basiswerte()[16]))
  })
  
  output$ko <- renderText({ 
    paste(round(basiswerte()[17]))
  })
  
  output$kk <- renderText({ 
    paste(round(basiswerte()[18]))
  })  
  
  output$ib <- renderText({ 
    paste(round(basiswerte()[1]))
  })
  
  output$at <- renderText({ 
    paste(round(basiswerte()[2]))
  })   
  
  output$pa <- renderText({ 
    paste(round(basiswerte()[3]))
  }) 
  
  output$tp <- renderText({ 
    if (trunc(basiswerte()[4]/2) > 0 && basiswerte()[4]%%2 == 1 && round(basiswerte()[5]) >= 1){
      paste(trunc(basiswerte()[4]/2), "W6+", (basiswerte()[4]%%2), "W3+", round(basiswerte()[5]), sep="") 
    } else if (trunc(basiswerte()[4]/2) > 0 && basiswerte()[4]%%2 == 1 && round(basiswerte()[5]) < 1){
      paste(trunc(basiswerte()[4]/2), "W6+", (basiswerte()[4]%%2), "W3", sep="") 
    } else if (trunc(basiswerte()[4]/2) > 0 && basiswerte()[4]%%2 == 0 && round(basiswerte()[5]) >= 1){
      paste(trunc(basiswerte()[4]/2), "W6+", round(basiswerte()[5]), sep="")
    } else if (trunc(basiswerte()[4]/2) > 0 && basiswerte()[4]%%2 == 0 && round(basiswerte()[5]) < 1){
      paste(trunc(basiswerte()[4]/2), "W6", sep="")
    } else if (trunc(basiswerte()[4]/2) == 0 && round(basiswerte()[5]) >= 1){
      paste("1W3+", round(basiswerte()[5]), sep="") 
    } else if (trunc(basiswerte()[4]/2) == 0 && round(basiswerte()[5]) < 1){
      paste("1W3", sep="")
    }  
    
    }) 
  
  output$le <- renderText({ 
    paste(round(basiswerte()[6]))
  }) 
  
  output$au <- renderText({ 
    paste(round(basiswerte()[7]))
  }) 
  
  output$rs <- renderText({ 
    paste(round(basiswerte()[8]))
  }) 
  
  output$gs <- renderText({ 
      if (round(basiswerte()[9]) > 0){
        paste(round(basiswerte()[9]))
      }
      else {
        paste("1")  
      }
  }) 
  
  output$mr <- renderText({ 
    paste(round(basiswerte()[10]))
  }) 
  
  output$dk <- renderText({
    paste(größe_dk[as.integer(input$größe)])
  })
  
  output$lo <- renderText({
    if (input$golembauer == FALSE) {
      paste("1W6+", loyalität[as.integer(input$material2)], sep = "")
    } else {
      paste(loyalität[as.integer(input$material2)]+6)
    }  
  })
  
  output$eigenschaften <- renderText({
    paste(",", materialwerte[as.integer(input$material2),20], if(input$fähigkeit1 != 1){paste(", " , fähigkeiten[as.integer(input$fähigkeit1)], sep="")}else{""}, if(input$fähigkeit2 != 1){paste(", " , fähigkeiten[as.integer(input$fähigkeit2)], sep="")}else{""},if(input$fähigkeit3 != 1){paste(", " , fähigkeiten[as.integer(input$fähigkeit3)], sep="")}else{""}, if(input$fähigkeit4 != 1){paste(", " , fähigkeiten[as.integer(input$fähigkeit4)], sep="")}else{""},if(input$fähigkeit5 != 1){paste(", " , fähigkeiten[as.integer(input$fähigkeit5)], sep="")}else{""}, if(input$fähigkeit6 != 1){paste(", " , fähigkeiten[as.integer(input$fähigkeit6)], sep="")}else{""}, if(input$fähigkeit7 != 1){paste(", " , fähigkeiten[as.integer(input$fähigkeit7)], sep="")}else{""}, if(input$fähigkeit8 != 1){paste(", " , fähigkeiten[as.integer(input$fähigkeit8)], sep="")}else{""}, if(input$gift == TRUE && input$giftart == 1){paste(", " , "Blut-/Waffengift(", input$giftstufe, ")", sep="")}else if(input$gift == TRUE && input$giftart == 2){paste(", " , "Kontaktgift(", input$giftstufe, ")", sep="")} else if (input$gift == TRUE && input$giftart == 3) {paste(", " , "Atemgift(", input$giftstufe, ")", sep="")} else {paste("")}, if (input$gliedmaßen == TRUE){paste(", zusätzliche Gliedmaßen")}, if (input$flügel == TRUE){paste(", fliegend")}, ", ", größe_klasse[as.integer(input$größe)] , sep = "") 
  })
  
  output$talente <- renderText({
    paste(if(input$talent1 != "Talent eingeben..." && val$talentwerte[1] > 0){paste(val$talentnamen[1], " " ,val$talentwerte[1], sep="")}else{""}, if(input$talent2 != "Talent eingeben..." && val$talentwerte[2] > 0){paste(", " , val$talentnamen[2], " " ,val$talentwerte[2], sep="")}else{""},if(input$talent3 != "Talent eingeben..." && val$talentwerte[3] > 0){paste(", " , val$talentnamen[3], " " ,val$talentwerte[3], sep="")}else{""}, if(input$talent4 != "Talent eingeben..." && val$talentwerte[4] > 0){paste(", " , val$talentnamen[4], " " ,val$talentwerte[4], sep="")}else{""}, sep = "") 
  })
  
  output$dienste <- renderText({
    paste(if(input$dienst1 != 12){paste(", " , dienste[as.integer(input$dienst1)], sep="")}else{""}, if(input$dienst2 != 12){paste(", " , dienste[as.integer(input$dienst2)], sep="")}else{""},if(input$dienst3 != 12){paste(", " , dienste[as.integer(input$dienst3)], sep="")}else{""}, if(input$dienst4 != 12){paste(", " , dienste[as.integer(input$dienst4)], sep="")}else{""},if(input$dienst5 != 12){paste(", " , dienste[as.integer(input$dienst5)], sep="")}else{""}, if(input$dienst6 != 12){paste(", " , dienste[as.integer(input$dienst6)], sep="")}else{""}, if(input$dienst1 != 12){paste(", " , dienste[as.integer(input$dienst1)], sep="")}else{""}, if(input$dienst7 != 12){paste(", " , dienste[as.integer(input$dienst7)], sep="")}else{""}, sep = "")
  }) # der Reihe nach ausfüllen!
  
})