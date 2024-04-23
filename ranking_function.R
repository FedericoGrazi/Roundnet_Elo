# ELO FUNCTIONS ####
elo_c <- function(numplay, numrow, white1, white2, black1, black2, score, cdevsteams,
                     cratsteams,dscore = double(2*numplay),weights,ntourn) { # Capire perchè 2*np
  escore <- numeric(numplay)
  ascore <- numeric(numplay)
  escorek <- 0
  K = 20
  Kn = 40
  
  Kpl <- ifelse(ntourn >4, Kn, K)
  
  # Create Empty Vectors
  for (k in 1:numplay) {
    escore[k] <- 0
    ascore[k] <- 0
  }
  
  for (k in 1:numrow) {
    # Cumulative Scores for each player
    ascore[white1[k]] <- ascore[white1[k]] + score[k]
    ascore[white2[k]] <- ascore[white2[k]] + score[k]
    ascore[black1[k]] <- ascore[black1[k]] + 1 - score[k]
    ascore[black2[k]] <- ascore[black2[k]] + 1 - score[k]
    
    # E(s|r,rj) - expected score
    escorek <- 1 / (1 + 10^(-( (cratsteams[white1[k]] - cratsteams[black1[k]] ))/400))
    escore[white1[k]] <- escore[white1[k]] + escorek
    escore[white2[k]] <- escore[white2[k]] + escorek
    
    # \sum g(RD_j)(s_j-E(s|r,rj,RDj))
    dscore[white1[k]] <- dscore[white1[k]] +  Kpl[black1[k]] * weights[k] * (score[k] - escorek)
    dscore[white2[k]] <- dscore[white2[k]] +  Kpl[black1[k]] * weights[k] * (score[k] - escorek)
    
    # E(s|r,rj,RDj) - expected score
    escorek <- 1 / (1 + 10^(-((cratsteams[black1[k]] - cratsteams[white1[k]]))/400))
    escore[black1[k]] <- escore[black1[k]] + escorek
    escore[black2[k]] <- escore[black2[k]] + escorek
    

    # \sum g(RD_j)(s_j-E(s|r,rj,RDj))
    dscore[black1[k]] <- dscore[black1[k]] +  Kpl[white1[k]] * weights[k] * (1 - score[k] - escorek)
    dscore[black2[k]] <- dscore[black2[k]] +  Kpl[white1[k]] * weights[k] * (1 - score[k] - escorek)
  }
  
  dscore[(numplay+ 1):(2 * numplay)] <- ascore
  lout <- dscore
  return(lout)
}

elo_ets  <- function(x, status = NULL, whichtourn = 0, ita = T){
  
  x = separate(x, Play1, into = c("Giocatore1", "Giocatore2"), sep = " & ",remove = T)
  x = separate(x, Play2, into = c("Giocatore3", "Giocatore4"), sep = " & ",remove = T)
  
  giocatori = sort(unique(c(x$Giocatore1, x$Giocatore2, x$Giocatore3, x$Giocatore4)))
  
  np <- length(giocatori)
  nm <- length(unique(x$cat))
  
  cat_values <- NULL
  for (i in seq_along(giocatori)) {
    giocatorei <- giocatori[i]
    cat_values[i] <- unique(x$cat[which(giocatorei == x$Giocatore1 | giocatorei == x$Giocatore2 | giocatorei == x$Giocatore3 | giocatorei == x$Giocatore4)])
  }
  names(cat_values) <- giocatori
  
  x$Giocatore1 <- match(x$Giocatore1, giocatori)
  x$Giocatore2 <- match(x$Giocatore2, giocatori)
  x$Giocatore3 <- match(x$Giocatore3, giocatori)
  x$Giocatore4 <- match(x$Giocatore4, giocatori)
  
  if(!exists("initval")) load("D:/Personal Statistics/rcb/Ranking/Tables/initval.rda")
  
  if (!is.null(status)) {
    npadd <- giocatori[!(giocatori %in% status$giocatore)]
    zv <- rep(0, length(npadd))
    npstatus <- data.frame(giocatore = npadd,
                           # Rating = rep(init[1], length(npadd)),
                           Rating = as.numeric(initval$initrats[match(cat_values[names(cat_values) %in% npadd],initval$categories)]),
                           Games = zv, Win = zv,  Loss = zv,nTourn = zv,
                           LastTourn = rep(time,length(npadd)))
    if (!("Games" %in% names(status))) 
      status <- cbind(status, Games = 0)
    if (!("Win" %in% names(status))) 
      status <- cbind(status, Win = 0)
    if (!("Loss" %in% names(status))) 
      status <- cbind(status, Loss = 0)
    if (!("nTourn" %in% names(status))) 
      status <- cbind(status, nTourn = 0)
    if (!("LastTourn" %in% names(status))) 
      status <- cbind(status, LastTourn = time)
    
    status <- rbind(status[, c("giocatore", "Rating", 
                               "Games", "Win", "Loss", "nTourn","LastTourn")], npstatus)
    
    rinit <- ifelse(status[[2]]<900,900,status[[2]])
    ngames <- status[[3]]
    nwin <- status[[4]]
    nloss <- status[[5]]
    ntourn <- status[[6]]
    nlasttourn <- status[[7]]
    names(rinit) <-  names(ngames) <- status$giocatore
  }else {
    # rinit <- rep(init[1], length.out = np)
    # dinit <- rep(init[2], length.out = np)
    rinit <- as.numeric(initval$initrats[match(cat_values,initval$categories)])
    ngames <- nwin <- nloss <- rep(0, length.out = np)
    ntourn <- rep(0, np)
    nlasttourn <- rep(time,np)
    names(rinit) <- names(ngames) <- names(ntourn) <- names(nlasttourn) <- names(ngames) <- names(nwin) <- names(nloss)<- giocatori
    
    
  }
  
  curplay <- match(giocatori, names(rinit))
  
  orats <- rinit[-curplay]
  ongames <- ngames[-curplay]
  onwin <- nwin[-curplay]
  onloss <- nloss[-curplay]
  ontourn <- ntourn[-curplay]
  olasttourn <- nlasttourn[-curplay]
  
  crats <- rinit[curplay]
  ngames <- ngames[curplay]
  nwin <- nwin[curplay]
  nloss <- nloss[curplay]
  ntourn <- ntourn[curplay]
  ntourn <- ntourn +1 
  nlasttourn <- nlasttourn[curplay]
  
  
  histry <- array(NA,
                  dim = c(np, 4),
                  dimnames = list(giocatori, 
                                  c("Rating", "Games", "Wins","Category")
                  ))
  histry<- as.data.frame(histry)
  
  nameteams <- data.frame(G1 = c(x$Giocatore1,x$Giocatore3),G2 = c(x$Giocatore2,x$Giocatore4))
  sq <- unique(paste(nameteams$G1,nameteams$G2,sep = " "))
  split_cols <- strsplit(sq, " ")
  g1 <- as.numeric(sapply(split_cols, `[`, 1))
  g2 <- as.numeric(sapply(split_cols, `[`, 2))
  
  # Unique Team's Rating
  cratsteams <- c((crats[g1]+crats[g2])/2,(crats[g2]+crats[g1])/2)
  # cratsteams <- c(crats[g1]^2/(crats[g1]+crats[g2])+crats[g2]^2/(crats[g1]+crats[g2]),crats[g2]^2/(crats[g2]+crats[g2])+crats[g1]^2/(crats[g1]+crats[g2]))
  cratsteams <- cratsteams[sort(names(cratsteams))]

  x <- split(x, x$cat)
  players <- lapply(x, function(y) unique(c(y$Giocatore1, y$Giocatore2, y$Giocatore3, y$Giocatore4)))
  
  for (i in 1:nm) {
    traini <- x[[i]]
    nr <- nrow(traini)
    
    playi <- players[[i]]
    
    ngamesi <- tabulate(c(traini$Giocatore1, traini$Giocatore2,traini$Giocatore3, traini$Giocatore4), np)
    
    dscores <- elo_c(numplay = np, numrow = nr, white1 = traini$Giocatore1, white2 = traini$Giocatore2,
                        black1 = traini$Giocatore3,black2 = traini$Giocatore4, score = traini$Score,
                         cratsteams =  cratsteams,weights=traini$Weight,ntourn = ntourn)
    
    cdscores <- dscores[playi]
    # ascore <- dscores[np:2*np][playi]
    # names(ascore) <- giocatori[playi]
    
    crats[playi] <- crats[playi] + cdscores

    ngamesi <- tabulate(c(traini$Giocatore1,traini$Giocatore2,traini$Giocatore3,traini$Giocatore4), np)
    trainiplw <- c(traini$Giocatore1[traini$Score > .5], traini$Giocatore2[traini$Score > .5],traini$Giocatore3[traini$Score < .5],traini$Giocatore4[traini$Score < .5])
    trainipll <- c(traini$Giocatore1[traini$Score < .5], traini$Giocatore2[traini$Score < .5],traini$Giocatore3[traini$Score > .5],traini$Giocatore4[traini$Score > .5])
    ngames <- ngames + ngamesi
    nwin <- nwin + tabulate(trainiplw, np)
    nloss <- nloss + tabulate(trainipll, np)
    
    nlasttourn <- rep(time,length(nlasttourn))
    
    histry[playi, 1] <- round(cdscores)
    histry[, 2] <- ngames
    histry[, 3] <- nwin
    # histry[playi, 4] <- round(ascore[!is.na(names(ascore))],1)
    histry[playi, 4] <- rep(unique(x[[i]]$cat), length(playi))
  }
  
  
  player <- suppressWarnings(as.numeric(names(c(crats, orats))))
  if (any(is.na(player))) 
    player <- names(c(crats, orats))
  
  dfout <- data.frame(
    giocatore = player,
    Rating = round(c(crats, orats),0), 
    Games = c(ngames, ongames),
    Win = c(nwin, onwin),
    Loss = c(nloss, onloss),
    nTourn = c(ntourn, ontourn),
    LastTourn = c(nlasttourn,olasttourn),
    stringsAsFactors = FALSE)
  rownames(dfout) <- NULL
  
  dfout <- dfout[order(dfout$Rating, decreasing = TRUE), ]
  rownames(dfout) <- 1:nrow(dfout)
  
  if(ita){
    load("D:/Personal Statistics/rcb/Ranking/Tables/ita_players.rda")
    itadfout <- dfout[dfout$giocatore %in% ita_players,]
    rownames(itadfout) <- 1:nrow(itadfout)
    itahistry <- histry[rownames(histry) %in% ita_players,]
    ita.lout <- list(ratings = itadfout, history = itahistry)
    lout <- list(ratings = dfout, history = histry, ita = ita.lout, type = "Elo")
  }else{
    lout <- list(ratings = dfout, history = histry, type = "Elo")
    
  }
  class(lout) <- "rating"
  return(lout)
}

elo_tornei <-  function(data, elo_ratings = NULL, coeff = 20, elo_base = 1000, diff_max = 0.92,win_ass = 400, conta_tornei_attivo = TRUE, coeff_nuovi = 40){



  partecipanti <- sort(unique(c(data$g1,data$g2,data$g3,data$g4)))


  if (!is.null(elo_ratings)) {
    ckeep <-  c("giocatore","nuovo_elo","conta_tornei")
    elo_ratings = as.data.frame(elo_ratings[,ckeep])
    npadd <- partecipanti[!(partecipanti %in% elo_ratings$giocatore)]
    zv <- rep(0, length(npadd))

    new_rank <- data.frame(giocatore = npadd,
                           nuovo_elo = rep(elo_base, length(npadd)),
                           conta_tornei = rep(0, length(npadd)))

    if (!("nuovo_elo" %in% names(elo_ratings)))
      elo_ratings <- cbind(elo_ratings, nuovo_elo = 0)
    if (!("conta_tornei" %in% names(elo_ratings)))
      elo_ratings <- cbind(elo_ratings, conta_tornei = 0)

    elo_ratings <- as_tibble(rbind(elo_ratings, new_rank))

  }else {
    elo_ratings <- tibble(
      giocatore = partecipanti,
      nuovo_elo = elo_base,
      conta_tornei = 0
    )
  }

  totset = data$w1 + data$w2

  data$w1 = case_when(
    data$w1/totset == 2/3 ~.75,
    data$w1/totset == 1/3 ~.25,
    TRUE ~ data$w1/totset)

  data$w2 = case_when(
    data$w2/totset == 2/3 ~.75,
    data$w2/totset == 1/3 ~.25,
    TRUE ~ data$w2/totset)


  plindex = match(partecipanti, elo_ratings$giocatore) # Associo numero nel rating ad ogni partecipante

  onplay = elo_ratings$giocatore[plindex]   # stesso di partecipatni
  outplay = elo_ratings$giocatore[-plindex] # giocatori che non partecipano al torneo
  onrat = elo_ratings$nuovo_elo[plindex]    # rating dei partecipanti
  outrat = elo_ratings$nuovo_elo[-plindex]  # rating dei non partecipanti
  onct = elo_ratings$conta_tornei[plindex]  # conta_tornei dei partecipanti
  outct = elo_ratings$conta_tornei[-plindex]# conta_tornei dei partecipanti


  g1 = match(data$g1,elo_ratings$giocatore)
  g2 = match(data$g2,elo_ratings$giocatore)
  g3 = match(data$g3,elo_ratings$giocatore)
  g4 = match(data$g4,elo_ratings$giocatore)

  data$elo1 = elo_ratings$nuovo_elo[g1]
  data$elo2 = elo_ratings$nuovo_elo[g2]
  data$elo3 = elo_ratings$nuovo_elo[g3]
  data$elo4 = elo_ratings$nuovo_elo[g4]

  data$ct1 = elo_ratings$conta_tornei[g4]
  data$ct2 = elo_ratings$conta_tornei[g4]
  data$ct3 = elo_ratings$conta_tornei[g4]
  data$ct4 = elo_ratings$conta_tornei[g4]

  data$game = 1:nrow(data)
  data$w = ifelse(totset == 1,.75,1)




  last <-  data %>%
    group_by(game) %>%
    mutate(
      telo1 = mean(c(elo1,elo2)),
      telo2 = mean(c(elo3,elo4)),

      expA = 1/(1+10^((telo2-telo1)/win_ass)),
      expB = 1/(1+10^((telo1-telo2)/win_ass)),

      diffA = case_when(
        expA > diff_max ~ w1 - diff_max,
        expA < 1- diff_max ~ w1- (1- diff_max),
        TRUE ~ w1-expA),
      diffB = case_when(
        expB > diff_max ~ w2 - diff_max,
        expB < 1- diff_max ~ w2 - (1-diff_max),
        TRUE ~ w2-expB),
      eloA1 = case_when( ct1<4~ w * coeff_nuovi * diffA, elo1 >2400 ~ w * 10 *diffA, TRUE ~ w * coeff * diffA),
      eloA2 = case_when( ct2<4~ w * coeff_nuovi * diffA, elo2 >2400 ~ w * 10 *diffA, TRUE ~ w * coeff * diffA),
      eloB3 = case_when( ct3<4~ w * coeff_nuovi * diffB, elo3 >2400 ~ w * 10 *diffB, TRUE ~ w * coeff * diffB),
      eloB4 = case_when( ct4<4~ w * coeff_nuovi * diffB, elo4 >2400 ~ w * 10 *diffB, TRUE ~ w * coeff * diffB)
    ) %>%
    ungroup %>%
    pivot_longer(cols = g1:g4, values_to = "giocatore") %>%
    mutate(punt = case_when(
      name == "g1" ~ eloA1,
      name == "g2" ~ eloA2,
      name == "g3" ~ eloB3,
      name == "g4" ~ eloB4,
    )) %>%
    group_by(giocatore) %>%
    summarize(elo_dell_evento = sum(punt))

  last = last[order(last$giocatore),]

  if(conta_tornei_attivo == T) onct <- onct +1

  elo_ratings = tibble(
    giocatore = c(onplay,outplay),
    elo_dell_evento = c(round(last$elo_dell_evento,0), rep(0,length(outplay))),
    nuovo_elo = c(onrat+last$elo_dell_evento,outrat),
    conta_tornei = c(onct, outct)
  )

  elo_ratings$nuovo_elo = ifelse(elo_ratings$nuovo_elo<900,900,elo_ratings$nuovo_elo)
  elo_ratings = arrange(elo_ratings,-nuovo_elo)


  print(elo_ratings, n= 100)
  return(elo_ratings)
}

merge_elo_ratings <- function(elo1,elo2){
  
  elo1_chg <- elo1 %>% 
    transmute(
      giocatore = giocatore,
      elo1 = nuovo_elo,
      tornei_elo1 = conta_tornei
    )
  
  elo2_chg <- elo2 %>% 
    transmute(
      giocatore = giocatore,
      elo2 = nuovo_elo,
      tornei_elo2 = conta_tornei
    )
  
  
  elo_tot <- suppressMessages( full_join(elo1_chg,elo2_chg) %>% 
                                 group_by(giocatore) %>% 
                                 mutate(conta_tornei = case_when(
                                   is.na(tornei_elo2) ~ tornei_elo1,
                                   is.na(tornei_elo1) ~ tornei_elo2,
                                   tornei_elo1>tornei_elo2 ~ tornei_elo1,
                                   tornei_elo2>tornei_elo1 ~ tornei_elo2,
                                   TRUE ~ tornei_elo1
                                 ),
                                 nuovo_elo = case_when(
                                   is.na(elo1) ~ elo2,
                                   is.na(elo2) ~ elo1,
                                   tornei_elo1>tornei_elo2 ~ elo1,
                                   tornei_elo2>tornei_elo1 ~ elo2,
                                   TRUE ~ elo1
                                 )) %>% 
                                 select(giocatore,nuovo_elo, conta_tornei)
  )
  
  elo_tot = arrange(elo_tot,-nuovo_elo)
  return(elo_tot)
}

extract_elo_table <-  function(data, elo_ratings = NULL, coeff = 20, elo_base = 1000, diff_max = 0.92,win_ass = 400, conta_tornei_attivo = TRUE, coeff_nuovi = 40){
  
  library(tidyverse)
  
  if(is.null(elo_ratings)){
    elo_ratings <- data.frame(giocatore = as.character(data[1,1]), nuovo_elo = elo_base,conta_tornei = 0)
  }else if(ncol(elo_ratings) == 2){
    elo_ratings <- elo_ratings
  }else if(ncol(elo_ratings)==3){
    elo_ratings <- elo_ratings[,c(1,3)]
  }else if(ncol(elo_ratings)==4){
    elo_ratings <- elo_ratings[,c(1,3,4)]
  }else{
    print("Controllare dimensioni e formato degli elo inseriti")
    break()
  }
  
  
  
  
  data <- data %>% 
    mutate(tot_set = w1+w2,
           w1 = case_when(
             w1/tot_set == 2/3 ~ 0.75,
             w1/tot_set == 1/3 ~ 0.25,
             TRUE ~ w1/tot_set
           ),
           w2 = case_when(
             w2/tot_set == 2/3 ~ 0.75,
             w2/tot_set == 1/3 ~ 0.25,
             TRUE ~ w2/tot_set
           ),
           w = case_when(
             tot_set == 1~.75,
             TRUE ~1
           )) %>% 
    select(-tot_set)
  
  
  w <- data %>% 
    mutate(game = 1:nrow(data)) %>% 
    select(game, w)
  
  data <- data %>% select(-w)
  
  partecipanti <- data %>% 
    pivot_longer(names_to = "squadra",values_to = "giocatore", cols = g1:g4) %>% 
    group_by(giocatore) %>% 
    summarize(n=n()) %>% 
    mutate(partecipante = 0) %>% 
    select(-n)
  
  elo_ratings_full <- suppressMessages( full_join(elo_ratings,partecipanti) %>% 
                                          mutate(
                                            nuovo_elo = ifelse(is.na(nuovo_elo),elo_base, nuovo_elo),
                                            conta_tornei = ifelse(is.na(conta_tornei),0, conta_tornei),
                                          ) %>% 
                                          select(-partecipante) )
  
  rob <- data %>% 
    mutate(perc1 = 1,perc2 = 3,perc3=4,perc4=5,
           ct1 = 1, ct2 = 2, ct3 = 3, ct4 = 4)
  
  
  for(i in 1:nrow(data)){
    for(j in 1:4){
      a <- data[i,j]
      indent <- which(elo_ratings_full[,1] == as.character(a))
      
      rob[i,j+6] <- elo_ratings_full$nuovo_elo[indent] 
      rob[i,j+10] <- elo_ratings_full$conta_tornei[indent]
      
    }
  }
  
  last <-  rob %>% 
    mutate(game = 1:nrow(rob),
           w = w$w) %>% 
    group_by(game) %>% 
    mutate(
      pot1 = mean(c(perc1,perc2)),
      pot2 = mean(c(perc3,perc4)),
      expA = 1/(1+10^((pot2-pot1)/win_ass)),
      expB = 1/(1+10^((pot1-pot2)/win_ass)),
      diffA = case_when(
        expA > diff_max ~ w1 - diff_max,
        expA < 1- diff_max ~ w1- (1- diff_max),
        TRUE ~ w1-expA),
      diffB = case_when(
        expB > diff_max ~ w2 - diff_max,
        expB < 1- diff_max ~ w2 - (1-diff_max),
        TRUE ~ w2-expB),
      eloA1 = case_when( ct1<4~ w * coeff_nuovi * diffA, perc1 >2400 ~ w * 10 *diffA, TRUE ~ w * coeff * diffA),
      eloA2 = case_when( ct2<4~ w * coeff_nuovi * diffA, perc2 >2400 ~ w * 10 *diffA, TRUE ~ w * coeff * diffA),
      eloB3 = case_when( ct3<4~ w * coeff_nuovi * diffB, perc3 >2400 ~ w * 10 *diffB, TRUE ~ w * coeff * diffB),
      eloB4 = case_when( ct4<4~ w * coeff_nuovi * diffB, perc4 >2400 ~ w * 10 *diffB, TRUE ~ w * coeff * diffB)
    ) 
  
  return(last)
}

## ELO SIMULATIONS ####
# PlayerRatings::elo

create_players <- function(data){
  data %>% 
    transmute(
      g1 = sub(" &.*", "", V1),
      g2 = sub(".*& ", "", V1),
      g3 = sub(" &.*", "", V2),
      g4 = sub(".*& ", "", V2))
}
create_teams <- function(data){
  data %>% 
    mutate(
      t1 =  paste(g1,g2,sep = " & "),
      t2 =  paste(g3,g4,sep = " & ")
    )}
simul_bracket <- function(new_def){
  for(i in 1:nrow(new_def)){
    w1t = 0
    w2t = 0
    n = 0
    while(n<3){
      w1 = rbinom(1,1,new_def$expA[i])
      w2 = 1-w1
      
      w1t = w1t+w1
      w2t = w2t+w2
      n = n+1
      
    }
    new_def$w[i] <-  case_when(w1t > w2t ~1,
                               TRUE ~2)
  }
  return(new_def)
}
create_bracket <- function(last, g1 = c(1,8,4,5,2,7,3,6),g2 = c(16,9,13,12,15,10,14,11),rank =  c(1,8,4,5,2,7,3,6)){
  
  brack1 <-  suppressMessages( tibble(g1 ,g2 ) %>% 
                                 left_join(last, by = c("g1"= "rank")) %>% 
                                 rename(V1 = "value") %>% 
                                 left_join(last, by = c("g2"= "rank")) %>% 
                                 rename(V2 = "value") %>% 
                                 select(V1,V2) )
  
  new_def <- insert_elo(create_players(brack1),elo_ratings,plus = 5) %>% 
    mutate(w = 0) %>% 
    simul_bracket()
  
  win_br1= tibble(value = "A")
  for(i in 1:nrow(brack1)){
    win_br1= win_br1 %>% add_row(value = as.character(brack1[i,new_def$w[i]])) %>% filter(value !="A")
  }
  win_br1 = win_br1 %>% 
    mutate(rank )
  
  return(win_br1)
  
}

bracket.glicko.simul <- function(topn, g1 = c(1,8,4,5,2,7,3,6),g2 = c(16,9,13,12,15,10,14,11),rank =  c(1,8,4,5,2,7,3,6), elo , pl){
  
  
  x = bracket = data.frame(Play1 = topn$team[g1], Play2 = topn$team[g2])
  
  x = separate(x, Play1, into = c("Giocatore1", "Giocatore2"), sep = " & ",remove = T)
  x = separate(x, Play2, into = c("Giocatore3", "Giocatore4"), sep = " & ",remove = T)
  

  x$Giocatore1 <- match(x$Giocatore1,pl)
  x$Giocatore2 <- match(x$Giocatore2,pl)
  x$Giocatore3 <- match(x$Giocatore3,pl)
  x$Giocatore4 <- match(x$Giocatore4,pl)
  x$elo1 <- elo[x$Giocatore1]
  x$elo2 <- elo[x$Giocatore2]
  x$elo3 <- elo[x$Giocatore3]
  x$elo4 <- elo[x$Giocatore4]
  x$telo1 <- (x$elo1+x$elo2)/2
  x$telo2 <- (x$elo3+x$elo4)/2
  x$exp1 <-  1/(1+10^((x$telo2-x$telo1)/400))

  x$set1 <- rbinom(nrow(x),3,x$exp1)
  x$Score <- ifelse(x$set1>1,1,0)
  
  nextround <- character(nrow(x))
  for(l in 1:nrow(x)) nextround[l] <- bracket[l,ifelse(x$Score[l]==1, 1,2)]
  dfout <- data.frame(team = nextround, rank)
  dfout <- dfout[order(dfout$rank),]

  return(dfout)
}

insert_elo <- function(data,elo_ratings, plus = 5,win_ass = 400, diff_max = .92 ){
  
  data <- data %>% mutate(game = 1:nrow(data)) 
  rob <- data %>% 
    mutate(perc1 = 1,perc2 = 3,perc3=4,perc4=5)
  
  for(i in 1:nrow(data)){
    for(j in 1:4){
      a <- data[i,j]
      indent <- which(elo_ratings$giocatore == as.character(a))
      rob[i,j+plus] <- elo_ratings$Rating[indent] 
    }
  }
  
  rob <- rob %>% 
    mutate(game = 1:nrow(rob)) %>% 
    group_by(game) %>% 
    mutate(
      pot1 = mean(c(perc1,perc2)),
      pot2 = mean(c(perc3,perc4)),
      expA = case_when(
        1/(1+10^((pot2-pot1)/win_ass)) > diff_max ~diff_max ,
        1/(1+10^((pot2-pot1)/win_ass)) <1- diff_max ~1-diff_max,
        TRUE ~ 1/(1+10^((pot2-pot1)/win_ass))))
  
  return(rob)
  
}

simul_torneo <-  function(data, elo_ratings,win_ass = 400,diff_max = 0.92){
  library(tidyverse)
  
  elo_ratings <<- elo_ratings[,c(1,3,4)]
  
  gir <- 
    data %>% 
    mutate(rwo  = 1:nrow(simul)) %>% 
    group_by(rwo)%>%
    mutate(elo = mean(c(e1,e2))) %>% 
    ungroup %>% 
    mutate(era = rank(-elo)) %>% 
    arrange(era) %>% 
    mutate(gironi = rep(c(1:4,4:1),times = 2)) %>% 
    select(g1,g2,elo,gironi) %>% 
    group_by(gironi) %>% 
    mutate(team = paste(g1,g2,sep = " & "))
  
  b = tibble(g1 = "a",g2="a",g3="a",g4="a")
  for(i in 1:4){
    
    sgir = gir %>% filter(gironi == i)
    a = combinations(n = 4, r = 2, v =sgir$team, repeats.allowed = F)  %>% as_tibble %>% create_players
    b = b %>% add_row(a) %>% filter(g1 != "a")
    
  }
  
  last <-
    suppressMessages( 
      insert_elo(b, elo_ratings,plus = 5) %>% 
        rob %>% 
        mutate(
          w1 = rbinom(1,1,expA),
          w2 = 1-w1,
          p = case_when(
            w1 == 1 ~rnbinom(1,21,expA)+21,
            TRUE ~rnbinom(1,21,1-expA)+21
          ),
          p1 = case_when(
            w1 == 1 ~21,
            TRUE ~p-21
          ),
          p2 = case_when(
            w2 == 1 ~21,
            TRUE ~p-21
          )      ) %>% 
        select(-game:-pot2) %>% 
        ungroup %>%  
        mutate(gir = rep(c(1:4),each = 4)) %>% 
        mutate(
          t1 =  paste(g1,g2,sep = " & "),
          t2 =  paste(g3,g4,sep = " & "),
        ) %>% 
        select(t1,t2,w1,w2,p1,p2) %>% 
        pivot_longer(cols = t1:t2) %>% 
        mutate(w = case_when(
          name == "t1" & w1 == 0 ~0,
          name == "t1" & w1 == 1 ~1,
          name == "t2" & w1 == 1 ~0,
          name == "t2" & w1 == 0 ~1
        ),
        diff = case_when(
          name == "t1" ~ p1-p2,
          TRUE ~p2-p1
        )) %>% 
        select(value, w,diff) %>% 
        group_by(value) %>% 
        summarize(wins = sum(w),
                  diff = sum(diff)) %>% 
        arrange(-wins,-diff) %>% 
        mutate(rank = 1:16)
    )
  
  br2 = create_bracket(last)
  br3 = create_bracket(br2,g1 = c(1,4,2,3),g2 = c(8,5,7,6),rank = c(1,4,2,3))
  br4 = create_bracket(br3,g1 = c(1,4),g2 = c(2,3),rank = c(1,2))
  br5 = create_bracket(br4,g1 = c(1),g2 = c(2),rank = c(1))
  
  winner = br5$value
  rm(elo_ratings)
  
  return(winner)
}

# GLICKO FUNCTIONS ####
# PlayerRatings::glicko
weeks_passed <- function(year, month, day) {
  input_date <- as.Date(paste(year, month, day, sep = "-"))
  reference_date <- as.Date("2023-04-15")
  weeks <- round(as.numeric(difftime(input_date, reference_date, units = "weeks")),0)
  return(weeks)
}

from_xl_to_glicko <- function(data, year, month, day){
  
  modified_data <-  
    data %>% 
    mutate(
      Play1 = paste(g1,"&",g2),
      Play2 = paste(g3,"&",g4),
      Time = weeks_passed(year,month,day),
      tot_set = w1+w2,
      Score = case_when(
             w1/tot_set == 2/3 ~ 0.75,
             w1/tot_set == 1/3 ~ 0.25,
             TRUE ~ w1/tot_set),
      Weight = case_when(
             # tot_set == 1 ~.75,
             tot_set == 1 ~.5,
             TRUE ~1),
      # Weight = ifelse(Round == "P9",.5, Weight)) %>% 
      Weight = ifelse(Round == "P9",.25, Weight)) %>% 
    select(cat,Time, Play1,Play2,Score,Weight) %>% 
    as.data.frame()
  
  
  return(modified_data)
  
}

remove_third_player <- function(file= NULL, x){
  
  x <- read.csv(file)
  
  if("Team.A...Player.3" %in% names(x)){
    x[x$Team.A...Player.3 != "",]$Team.A...Player.2 <-  x[x$Team.A...Player.3 != "",]$Team.A...Player.3
    x <- x[,-which(names(x) == "Team.A...Player.3")]
  }
  if("Team.B...Player.3" %in% names(x)){
    x[x$Team.B...Player.3 != "",]$Team.B...Player.2 <-  x[x$Team.B...Player.3 != "",]$Team.B...Player.3
    x <- x[,-which(names(x) == "Team.B...Player.3")]
  }
  
  if(sum(x$Team.A...Player.1 == x$Team.A...Player.2)>0) x[which(x$Team.A...Player.1 == x$Team.A...Player.2),]$Team.A...Player.2 <- paste(x[which(x$Team.A...Player.1 == x$Team.A...Player.2),]$Team.A...Player.2, "bis", sep = ".")
  if(sum(x$Team.B...Player.1 == x$Team.B...Player.2)>0) x[which(x$Team.B...Player.1 == x$Team.B...Player.2),]$Team.B...Player.2 <- paste(x[which(x$Team.B...Player.1 == x$Team.B...Player.2),]$Team.B...Player.2, "bis", sep = ".")
  
  return(x)
}

from_csv_to_glicko <- function(file, year, month, day , mixed = F){

  x <- remove_third_player(file)
  sets <- matrix(c(x$Game.1...Score.team.A,x$Game.2...Score.team.A,x$Game.3...Score.team.A),ncol = 3, nrow = nrow(x))
  setwin <- matrix(c(ifelse(x$Game.1...Score.team.A>x$Game.1...Score.team.B,1,0),ifelse(x$Game.2...Score.team.A>x$Game.2...Score.team.B,1,0),ifelse(x$Game.3...Score.team.A>x$Game.3...Score.team.B,1,0)),ncol = 3, nrow = nrow(x))
  
  df <- data.frame(
    cat = x$Division,
    g1 = x$Team.A...Player.1,
    g2 = x$Team.A...Player.2,
    g3 = x$Team.B...Player.1,
    g4 = x$Team.B...Player.2,
    Score = ifelse(x$Winning.team == x$Team.A,1,0),
    sets = apply(sets, 1, function(x) sum(!is.na(x))),
    placement = ifelse(str_detect(x$Round, "(P)[0-9]*"),1,0), # Placement
    Awins = apply(setwin,1, function(x) sum(x,na.rm = T)),
    Division1 = ifelse(str_detect(x$Group.Bracket,"(Division 1)|(Bracket A)"),1,0),
    Division2 = ifelse(str_detect(x$Group.Bracket,"(Division 2)|(Division 3)|(Division 4)|(Bracket B)|(Bracket C)|(Bracket D)"),1,0)
  )
  
  df$Weight <- ifelse(df$sets==1, .5,1)
  # df$Weight <- ifelse(df$sets==1, .75,1)
  df$Weight[df$placement==1] <- .25 # Placement
  # df$Weight[df$placement==1] <- 0.5 # Placement
  df$Score <- (df$Awins)/df$sets
  df$Score[df$Score == 2/3] <- 0.75
  df$Score[df$Score == 1/3] <- 0.25
  df <- df[,-which(names(df) %in% c("sets","Awins"))]
  
  dfout <- data.frame(
    cat = df$cat,
    Time = weeks_passed(year,month,day),
    Play1 = paste(df$g1,"&",df$g2),
    Play2 = paste(df$g3,"&",df$g4),
    Score = df$Score,
    Weight = df$Weight
  )
  

  if(mixed == F){
    dfout <- dfout[!str_detect(dfout$cat, "Mix"),]
  }else{
    dfout <- dfout[str_detect(dfout$cat, "Mix"),]
  }
  
  return(dfout)
}

glicko_c <- function(numplay, numrow, white1, white2, black1, black2, score, cdevsteams,
                     cratsteams,dscore = double(2*numplay),weights,bval) { # Capire perchè 2*np
  escore <- numeric(numplay)
  ascore <- numeric(numplay)
  dval <- numeric(numplay)
  ptsupd <- numeric(numplay)
  l1t <- numeric(numplay)
  escorek <- 0
  qv <- (log(10)/400)
  qv2 <- (log(10)/400)^2
  qip3 <- 3 * (qv/pi)^2
  
  # Unique Team's gDevs
  gdevs <- 1/sqrt(1 + qip3 * cdevsteams)
  
  # Create Empty Vectors
  for (k in 1:numplay) {
    escore[k] <- 0
    ascore[k] <- 0
    dval[k] <- 0
    ptsupd[k] <- 0
    l1t[k] <- 0
  }
  
  for (k in 1:numrow) {
    # Cumulative Scores for each player
    ascore[white1[k]] <- ascore[white1[k]] + score[k]
    ascore[white2[k]] <- ascore[white2[k]] + score[k] 
    ascore[black1[k]] <- ascore[black1[k]] + 1 - score[k]
    ascore[black2[k]] <- ascore[black2[k]] + 1 - score[k] 
    
    ## WHITE 
    # E(s|r,rj,RDj) - expected score
    escorek <- 1 / (1 + 10^(-(gdevs[white1[k]] * (cratsteams[white1[k]] - cratsteams[black1[k]] ))/400))
    escore[white1[k]] <- escore[white1[k]] + escorek
    escore[white2[k]] <- escore[white2[k]] + escorek
    
    # 1/d^2 - dval
    dval[white1[k]] <- dval[white1[k]] + qv2 * gdevs[black1[k]]^2 * escorek * (1 - escorek)
    dval[white2[k]] <- dval[white2[k]] + qv2 * gdevs[black1[k]]^2 * escorek * (1 - escorek)
    
    # \sum g(RD_j)(s_j-E(s|r,rj,RDj)) - Exp gain
    dscore[white1[k]] <- dscore[white1[k]] +  gdevs[black1[k]] * weights[k] * (score[k] + bval - escorek)
    dscore[white2[k]] <- dscore[white2[k]] +  gdevs[black1[k]] * weights[k] * (score[k] + bval - escorek)
    
    # r-r_j
    l1t[white1[k]] <- l1t[white1[k]] + cratsteams[black1[k]] - cratsteams[white1[k]]
    l1t[white2[k]] <- l1t[white2[k]] + cratsteams[black1[k]] - cratsteams[white1[k]]
    
    ## BLACK
    # E(s|r,rj,RDj) - expected score
    escorek <- 1 / (1 + 10^(-(gdevs[black1[k]] * (cratsteams[black1[k]] - cratsteams[white1[k]]))/400))
    escore[black1[k]] <- escore[black1[k]] + escorek
    escore[black2[k]] <- escore[black2[k]] + escorek
    
    # 1/d^2
    dval[black1[k]] <- dval[black1[k]] + qv2 * gdevs[white1[k]]^2 * escorek * (1 - escorek)
    dval[black2[k]] <- dval[black2[k]] + qv2 * gdevs[white1[k]]^2 * escorek * (1 - escorek)
    
    # \sum g(RD_j)(s_j-E(s|r,rj,RDj))
    dscore[black1[k]] <- dscore[black1[k]] +  gdevs[white1[k]] * weights[k] * (1 - score[k] + bval - escorek)
    dscore[black2[k]] <- dscore[black2[k]] +  gdevs[white1[k]] * weights[k] * (1 - score[k] + bval - escorek)
    
    # r-r_j
    l1t[black1[k]] <- l1t[black1[k]] + cratsteams[white1[k]] - cratsteams[black1[k]]
    l1t[black2[k]] <- l1t[black2[k]] + cratsteams[white1[k]] - cratsteams[black2[k]]
    
    
  }
  
  dscore[(numplay+ 1):(2 * numplay)] <- dval
  
  lout <- list(dscore,ascore, l1t)
  return(lout)
}

glick_elo <- function(x, status = NULL, init = c(1200,200),rdmax = 250, cval = 16, whichtourn = 0){
  
  x = separate(x, Play1, into = c("Giocatore1", "Giocatore2"), sep = " & ",remove = T)
  x = separate(x, Play2, into = c("Giocatore3", "Giocatore4"), sep = " & ",remove = T)
  
  time = unique(x$Time)
  giocatori = sort(unique(c(x$Giocatore1, x$Giocatore2, x$Giocatore3, x$Giocatore4)))
  
  ng <- length(giocatori)
  np <- nrow(x)
  
  x$Giocatore1 <- match(x$Giocatore1, giocatori)
  x$Giocatore2 <- match(x$Giocatore2, giocatori)
  x$Giocatore3 <- match(x$Giocatore3, giocatori)
  x$Giocatore4 <- match(x$Giocatore4, giocatori)
  
  if (!is.null(status)) {
    npadd <- giocatori[!(giocatori %in% status$giocatore)]
    zv <- rep(0, length(npadd))
    npstatus <- data.frame(giocatore = npadd,
                           Rating = rep(init[1], length(npadd)),
                           Deviation = rep(init[2], length(npadd)), 
                           Games = zv, Win = zv,  Loss = zv,nTourn = zv,
                           LastTourn = rep(time,length(npadd)))
    if (!("Games" %in% names(status))) 
      status <- cbind(status, Games = 0)
    if (!("Win" %in% names(status))) 
      status <- cbind(status, Win = 0)
    if (!("Loss" %in% names(status))) 
      status <- cbind(status, Loss = 0)
    if (!("nTourn" %in% names(status))) 
      status <- cbind(status, nTourn = 0)
    if (!("LastTourn" %in% names(status))) 
      status <- cbind(status, LastTourn = time)
    
    status <- rbind(status[, c("giocatore", "Rating", "Deviation", 
                               "Games", "Win", "Loss", "nTourn","LastTourn")], npstatus)
    rinit <- ifelse(status[[2]]<900,900,status[[2]])
    dinit <- status[[3]]
    ngames <- status[[4]]
    nwin <- status[[5]]
    nloss <- status[[6]]
    ntourn <- status[[7]]
    nlasttourn <- status[[8]]
    names(rinit) <- names(dinit) <- names(ngames) <- status$giocatore
  }else {
    rinit <- rep(init[1], length.out = ng)
    dinit <- rep(init[2], length.out = ng)
    ngames <- nwin <- nloss <- rep(0, length.out = ng)
    ntourn <- rep(0, ng)
    nlasttourn <- rep(time,ng)
    names(rinit) <- names(dinit) <- names(ngames) <- names(ntourn) <- names(nlasttourn) <- names(ngames) <- names(nwin) <- names(nloss)<- giocatori
    
  }
  
  curplay <- match(giocatori, names(rinit))
  
  orats <- rinit[-curplay]
  odevs <- dinit[-curplay]
  ongames <- ngames[-curplay]
  onwin <- nwin[-curplay]
  onloss <- nloss[-curplay]
  ontourn <- ntourn[-curplay]
  olasttourn <- nlasttourn[-curplay]
  
  crats <- rinit[curplay]
  cdevs <- dinit[curplay]
  ngames <- ngames[curplay]
  nwin <- nwin[curplay]
  nloss <- nloss[curplay]
  ntourn <- ntourn[curplay]
  ntourn <- ntourn +1 
  nlasttourn <- nlasttourn[curplay]
  
  histry <- array(NA,
                  dim = c(ng, 6),
                  dimnames = list(giocatori,
                                  c("Rating", "Deviation", "Games","Wins","WeightedWins","WhichT")))
  
  cdevs <- pmin(sqrt(cdevs^2+cval^2*(time - nlasttourn+1)),rdmax)
  
  nlasttourn <- rep(time,length(nlasttourn))
  
  qv <-  log(10)/400
  qip3 <- 3 * (qv/pi)^2

  dscores <- glicko_c(numplay = ng, numrow = np, white1 = x$Giocatore1, white2 = x$Giocatore2,
           black1 = x$Giocatore3,black2 = x$Giocatore4, score = x$Score,
           cdevsteams  = cdevs, crats = crats,weights=x$Weight, bval = 1/20)
  
  dval <- dscores[[1]][(ng + 1):(2 * ng)] # 1/d^2
  dscore <- dscores[[1]][1:ng] # \sum g(RD_j)(s_j-E(s|r,rj,RDj))
  ascore <- dscores[[2]]
  names(ascore) <- giocatori
  
  curupt <- qv/(1/cdevs^2+ dval) * dscore
  crats <- crats + curupt
  cdevs <- sqrt((1/cdevs^2 + dval)^(-1))
  
  ngamesi <- tabulate(c(x$Giocatore1,x$Giocatore2,x$Giocatore3,x$Giocatore4), ng)
  trainiplw <- c(x$Giocatore1[x$Score > .5], x$Giocatore2[x$Score > .5],x$Giocatore3[x$Score < .5],x$Giocatore4[x$Score < .5])
  trainipll <- c(x$Giocatore1[x$Score < .5], x$Giocatore2[x$Score < .5],x$Giocatore3[x$Score > .5],x$Giocatore4[x$Score > .5])
  ngames <- ngames + ngamesi
  nwin <- nwin + tabulate(trainiplw, ng)
  nloss <- nloss + tabulate(trainipll, ng)
  
  history  <-  data.frame(Rating = crats, Deviation = cdevs)
  history$Rating <-  ifelse(history$Rating<750, 750,history$Rating)
  history$Games <- ngames
  history$Win <-  nwin
  history$Loss <-  nloss
  
  player <- suppressWarnings(as.numeric(names(c(crats, orats))))
  if (any(is.na(player))) 
    player <- names(c(crats, orats))
    dfout <- data.frame(
                      giocatore = player,
                      Rating = c(history$Rating, orats), 
                      Deviation = c(history$Deviation, odevs),
                      Games = c(history$Games, ongames),
                      Win = c(history$Win, onwin),
                      Loss = c(history$Loss, onloss),
                      nTourn = c(ntourn, ontourn),
                      LastTourn = c(nlasttourn,olasttourn),
                      stringsAsFactors = FALSE)
  
  dfout <- dfout[order(dfout$Rating, decreasing = TRUE), ]
  

  histry[,1] <- round(curupt,0)
  histry[,2] <- round(cdevs,0)
  histry[,3] <- ngamesi
  histry[,4] <- tabulate(trainiplw, ng)
  histry[,5] <- ascore
  histry[,6] <- rep(whichtourn, length(curupt))
  
  lout <- list(ratings = dfout, history = histry,type = "Glicko")
  class(lout) <- "rating"
  return(lout)
}

glicko_ets <- function(x, status = NULL,rdmax = 200, cval = 16, whichtourn = 0, ita = T, lambda = 1/20){
  
  x = separate(x, Play1, into = c("Giocatore1", "Giocatore2"), sep = " & ",remove = T)
  x = separate(x, Play2, into = c("Giocatore3", "Giocatore4"), sep = " & ",remove = T)
  
  time = unique(x$Time)
  giocatori = sort(unique(c(x$Giocatore1, x$Giocatore2, x$Giocatore3, x$Giocatore4)))
  
  np <- length(giocatori)
  nm <- length(unique(x$cat))
  
  cat_values <- NULL
  for (i in seq_along(giocatori)) {
    giocatorei <- giocatori[i]
    cat_values[i] <- unique(x$cat[which(giocatorei == x$Giocatore1 | giocatorei == x$Giocatore2 | giocatorei == x$Giocatore3 | giocatorei == x$Giocatore4)])
  }
  names(cat_values) <- giocatori
  
  x$Giocatore1 <- match(x$Giocatore1, giocatori)
  x$Giocatore2 <- match(x$Giocatore2, giocatori)
  x$Giocatore3 <- match(x$Giocatore3, giocatori)
  x$Giocatore4 <- match(x$Giocatore4, giocatori)
  
  if(!exists("initval")) load("D:/Personal Statistics/rcb/Ranking/Tables/initval.rda")
  
  if (!is.null(status)) {
    npadd <- giocatori[!(giocatori %in% status$giocatore)]
    zv <- rep(0, length(npadd))
    npstatus <- data.frame(giocatore = npadd,
                           # Rating = rep(init[1], length(npadd)),
                           Rating = as.numeric(initval$initrats[match(cat_values[names(cat_values) %in% npadd],initval$categories)]),
                           # Deviation = rep(init[2], length(npadd)), 
                           Deviation = as.numeric(initval$initdev[match(cat_values[names(cat_values) %in% npadd],initval$categories)]),
                           Games = zv, Win = zv,  Loss = zv,nTourn = zv,
                           LastTourn = rep(time,length(npadd)))
    if (!("Games" %in% names(status))) 
      status <- cbind(status, Games = 0)
    if (!("Win" %in% names(status))) 
      status <- cbind(status, Win = 0)
    if (!("Loss" %in% names(status))) 
      status <- cbind(status, Loss = 0)
    if (!("nTourn" %in% names(status))) 
      status <- cbind(status, nTourn = 0)
    if (!("LastTourn" %in% names(status))) 
      status <- cbind(status, LastTourn = time)
    
    status <- rbind(status[, c("giocatore", "Rating", "Deviation", 
                               "Games", "Win", "Loss", "nTourn","LastTourn")], npstatus)
    
    rinit <- ifelse(status[[2]]<900,900,status[[2]])
    dinit <- status[[3]]
    ngames <- status[[4]]
    nwin <- status[[5]]
    nloss <- status[[6]]
    ntourn <- status[[7]]
    nlasttourn <- status[[8]]
    names(rinit) <- names(dinit) <- names(ngames) <- status$giocatore
  }else {
    # rinit <- rep(init[1], length.out = np)
    # dinit <- rep(init[2], length.out = np)
    rinit <- as.numeric(initval$initrats[match(cat_values,initval$categories)])
    dinit <- as.numeric(initval$initdev[match(cat_values,initval$categories)])
    ngames <- nwin <- nloss <- rep(0, length.out = np)
    ntourn <- rep(0, np)
    nlasttourn <- rep(time,np)
    names(rinit) <- names(dinit) <- names(ngames) <- names(ntourn) <- names(nlasttourn) <- names(ngames) <- names(nwin) <- names(nloss)<- giocatori
    
    
  }
  
  curplay <- match(giocatori, names(rinit))
  
  orats <- rinit[-curplay]
  odevs <- dinit[-curplay]
  ongames <- ngames[-curplay]
  onwin <- nwin[-curplay]
  onloss <- nloss[-curplay]
  ontourn <- ntourn[-curplay]
  olasttourn <- nlasttourn[-curplay]
  
  crats <- rinit[curplay]
  cdevs <- dinit[curplay]
  ngames <- ngames[curplay]
  nwin <- nwin[curplay]
  nloss <- nloss[curplay]
  ntourn <- ntourn[curplay]
  ntourn <- ntourn +1 
  nlasttourn <- nlasttourn[curplay]
  
  
  histry <- array(NA,
                  dim = c(np, 6),
                  dimnames = list(giocatori, 
                                  c("Rating", "Deviation", "Games", "Wins","CatRating","Category")
                                  ))
  histry<- as.data.frame(histry)
  
  qv <-  log(10)/400
  qip3 <- 3 * (qv/pi)^2
  
  nameteams <- data.frame(G1 = c(x$Giocatore1,x$Giocatore3),G2 = c(x$Giocatore2,x$Giocatore4))
  sq <- unique(paste(nameteams$G1,nameteams$G2,sep = " "))
  split_cols <- strsplit(sq, " ")
  g1 <- as.numeric(sapply(split_cols, `[`, 1))
  g2 <- as.numeric(sapply(split_cols, `[`, 2))
  
  # Unique Team's Rating
  cratsteams <- c((crats[g1]+crats[g2])/2,(crats[g2]+crats[g1])/2)
  cratsteams <- cratsteams[sort(names(cratsteams))]
  
  # Unique Team's Deviation
  # cdevs <- pmin(sqrt(cdevs^2+cval^2*(time - nlasttourn+1)),rdmax)
  cdevs <- pmin(sqrt(cdevs^2+cval^2*(time - nlasttourn+1)),rdmax)
  
  cdevsteams <- c(sqrt((cdevs[g1]^2+cdevs[g2]^2)/2),sqrt((cdevs[g2]^2+cdevs[g1]^2)/2))
  cdevsteams <- cdevsteams[sort(names(cdevsteams))]
  
  x <- split(x, x$cat)
  players <- lapply(x, function(y) unique(c(y$Giocatore1, y$Giocatore2, y$Giocatore3, y$Giocatore4)))
  
  for (i in 1:nm) {
    traini <- x[[i]]
    nr <- nrow(traini)
    
    playi <- players[[i]]

    ngamesi <- tabulate(c(traini$Giocatore1, traini$Giocatore2,traini$Giocatore3, traini$Giocatore4), np)
    
    dscores <- glicko_c(numplay = np, numrow = nr, white1 = traini$Giocatore1, white2 = traini$Giocatore2,
                        black1 = traini$Giocatore3,black2 = traini$Giocatore4, score = traini$Score,
                        cdevsteams = cdevsteams, cratsteams =  cratsteams,weights=traini$Weight, bval = 1/20)
    
    cdscores <- dscores[[1]][playi]
    cdval <- dscores[[1]][(np + 1):(2 * np)][playi] # 1/d^2
    ascore <- dscores[[2]][playi]
    l1t <- dscores[[3]][playi]
    names(ascore) <- names(cdval) <- names(cdscores) <- giocatori[playi]
    
    curupt <- qv/(1/cdevs[playi]^2+ cdval) * cdscores + lambda * l1t/ngamesi[playi]
    crats[playi] <- crats[playi] + curupt 
    
    cdevs[playi] <- sqrt((1/cdevs[playi]^2 + cdval)^(-1))
    
    ngamesi <- tabulate(c(traini$Giocatore1,traini$Giocatore2,traini$Giocatore3,traini$Giocatore4), np)
    trainiplw <- c(traini$Giocatore1[traini$Score > .5], traini$Giocatore2[traini$Score > .5],traini$Giocatore3[traini$Score < .5],traini$Giocatore4[traini$Score < .5])
    trainipll <- c(traini$Giocatore1[traini$Score < .5], traini$Giocatore2[traini$Score < .5],traini$Giocatore3[traini$Score > .5],traini$Giocatore4[traini$Score > .5])
    ngames <- ngames + ngamesi
    nwin <- nwin + tabulate(trainiplw, np)
    nloss <- nloss + tabulate(trainipll, np)

    nlasttourn <- rep(time,length(nlasttourn))

        
    histry[playi, 1] <- round(curupt)
    histry[, 2] <- round(cdevs)
    histry[playi, 3] <- ngamesi[playi]
    histry[playi, 4] <- tabulate(trainiplw, np)[playi]
    # histry[playi, 5] <- round(ascore[!is.na(names(ascore))],1)
    histry[playi, 5] <- round(lambda * l1t/ngamesi[playi])
    histry[playi, 6] <- rep(unique(x[[i]]$cat), length(playi))
  }

  
  player <- suppressWarnings(as.numeric(names(c(crats, orats))))
  if (any(is.na(player))) 
    player <- names(c(crats, orats))
  
  dfout <- data.frame(
    giocatore = player,
    Rating = round(c(crats, orats)), 
    Deviation = round(c(cdevs, odevs),0),
    Games = c(ngames, ongames),
    Win = c(nwin, onwin),
    Loss = c(nloss, onloss),
    nTourn = c(ntourn, ontourn),
    LastTourn = c(nlasttourn,olasttourn),
    stringsAsFactors = FALSE)
  rownames(dfout) <- NULL
  
  dfout <- dfout[order(dfout$Rating, decreasing = TRUE), ]
  rownames(dfout) <- 1:nrow(dfout)
  
  if(ita){
    load("D:/Personal Statistics/rcb/Ranking/Tables/ita_players.rda")
    itadfout <- dfout[dfout$giocatore %in% ita_players,]
    rownames(itadfout) <- 1:nrow(itadfout)
    itahistry <- histry[rownames(histry) %in% ita_players,]
    ita.lout <- list(ratings = itadfout, history = itahistry)
    lout <- list(ratings = dfout, history = histry, ita = ita.lout, type = "Glicko")
  }else{
    lout <- list(ratings = dfout, history = histry, type = "Glicko")
    
  }
  class(lout) <- "rating"
  return(lout)
}

view.ita <- function(rating) {
  load("D:/Personal Statistics/rcb/Ranking/Tables/ita_players.rda")
  rating$ratings[rating$ratings$giocatore %in% ita_players,]
  }
view.ita.history <- function(rating){
  load("D:/Personal Statistics/rcb/Ranking/Tables/ita_players.rda")
  rating$history[rownames(rating$history) %in% ita_players,]
} 
  

## GLICKO VIZ ####

histry_merge <- function(list, giocatori, nmin = 4){
  giocatori = sort(unique(giocatori))
  ng = length(giocatori)
  nt = ncol(sapply(list,dim))
  
  histry =
    array(NA,
          dim = c(ng,nt+1, 3),
          dimnames = list(giocatori,1:(nt+1),
                          c("Rating", "Deviation","Games")))
  
  
  playfilt = list[[nt]]$giocatore[list[[nt]]$nTourn>=nmin]
  playfilt = playfilt[order(playfilt)]
  
  for(i in 2:(nt+1)){
    dflisti = as.data.frame(list[i-1])
    
    
    playmatch = match(dflisti$giocatore,giocatori)
    histry[, i, 1][playmatch] <- dflisti$Rating
    histry[, i, 2][playmatch] <- dflisti$Deviation
    histry[, i, 3][playmatch] <- dflisti$Games
  }
  
  histryfilt =
    array(NA,
          dim = c(length(playfilt),nt+1, 3),
          dimnames = list(playfilt,1:(nt+1),
                          c("Rating", "Deviation","Games")))
  
  histryfilt[,,"Rating"] = histry[,,"Rating"][match(playfilt,rownames(histry[,,"Rating"])),]
  histryfilt[,,"Deviation"] = histry[,,"Deviation"][match(playfilt,rownames(histry[,,"Deviation"])),]
  histryfilt[,,"Games"] = histry[,,"Games"][match(playfilt,rownames(histry[,,"Games"])),]
  histryfilt[,,"Games"] = ifelse(is.na(histryfilt[,,"Games"]),0,histryfilt[,,"Games"])
  histryfilt[,,"Rating"] = ifelse(is.na(histryfilt[,,"Rating"]),1350,histryfilt[,,"Rating"])
  
  
  return(histryfilt)
  
}

plot.ratings <- function(histr, x= 13, y = 1900, which = "Rating", lwd = 1){
  par(xpd=TRUE)
  plot(1:ncol(histr[,,which]), rep(0,ncol(histr[,,which])), type="l",lty = "dashed", ylim = c(min(histr[,,which],na.rm = T)-50,max(histr[,,which],na.rm = T)+50), ylab = which,xlab = "Tournament")
  type = c("dotdash","dotted","solid")
  for(i in 1:nrow(histr[,,"Rating"])){
    lines(1:ncol(histr[,,"Rating"]),  histr[,,which][i,], col=i,type="l",lty = type[i%%3 +1],lwd = lwd)
  }
  legend(x = x, y = y, legend=rownames(histr[,,which]), col=1:nrow(histr[,,which]), lty=type[(1:nrow(histr[,,which])%%3 +1)], cex=0.8)
}

pred_discr <- function(x, status = NULL, init = c(1200,200),rdmax = 250, cval = 50,whichtourn = 0){
  
  x = separate(x, Play1, into = c("Giocatore1", "Giocatore2"), sep = " & ",remove = T)
  x = separate(x, Play2, into = c("Giocatore3", "Giocatore4"), sep = " & ",remove = T)
  
  time = unique(x$Time)
  giocatori = sort(unique(c(x$Giocatore1, x$Giocatore2, x$Giocatore3, x$Giocatore4)))
  
  ng <- length(giocatori)
  np <- nrow(x)
  
  
  x$Giocatore1 <- match(x$Giocatore1, giocatori)
  x$Giocatore2 <- match(x$Giocatore2, giocatori)
  x$Giocatore3 <- match(x$Giocatore3, giocatori)
  x$Giocatore4 <- match(x$Giocatore4, giocatori)
  
  if (!is.null(status)) {
    npadd <- giocatori[!(giocatori %in% status$giocatore)]
    zv <- rep(0, length(npadd))
    npstatus <- data.frame(giocatore = npadd,
                           Rating = rep(init[1], length(npadd)),
                           Deviation = rep(init[2], length(npadd)), 
                           Games = zv, Win = zv,  Loss = zv,nTourn = zv,
                           LastTourn = rep(time,length(npadd)))
    if (!("Games" %in% names(status))) 
      status <- cbind(status, Games = 0)
    if (!("Win" %in% names(status))) 
      status <- cbind(status, Win = 0)
    if (!("Loss" %in% names(status))) 
      status <- cbind(status, Loss = 0)
    if (!("nTourn" %in% names(status))) 
      status <- cbind(status, nTourn = 0)
    if (!("LastTourn" %in% names(status))) 
      status <- cbind(status, LastTourn = time)
    
    status <- rbind(status[, c("giocatore", "Rating", "Deviation", 
                               "Games", "Win", "Loss", "nTourn","LastTourn")], npstatus)
    rinit <- ifelse(status[[2]]<900,900,status[[2]])
    dinit <- status[[3]]
    ngames <- status[[4]]
    nwin <- status[[5]]
    nloss <- status[[6]]
    ntourn <- status[[7]]
    nlasttourn <- status[[8]]
    names(rinit) <- names(dinit) <- names(ngames) <- status$giocatore
  }else {
    rinit <- rep(init[1], length.out = ng)
    dinit <- rep(init[2], length.out = ng)
    ngames <- nwin <- nloss <- rep(0, length.out = ng)
    ntourn <- rep(0, ng)
    nlasttourn <- rep(time,ng)
    names(rinit) <- names(dinit) <- names(ngames) <- names(ntourn) <- names(nlasttourn) <- names(ngames) <- names(nwin) <- names(nloss)<- giocatori
    
  }
  
  curplay <- match(giocatori, names(rinit))
  
  orats <- rinit[-curplay]
  odevs <- dinit[-curplay]
  ongames <- ngames[-curplay]
  onwin <- nwin[-curplay]
  onloss <- nloss[-curplay]
  ontourn <- ntourn[-curplay]
  olasttourn <- nlasttourn[-curplay]
  
  crats <- rinit[curplay]
  cdevs <- dinit[curplay]
  ngames <- ngames[curplay]
  nwin <- nwin[curplay]
  nloss <- nloss[curplay]
  ntourn <- ntourn[curplay]
  ntourn <- ntourn +1
  nlasttourn <- nlasttourn[curplay]
  
  histry <- array(NA,
                  dim = c(ng, 5),
                  dimnames = list(giocatori,
                                  c("Rating", "Deviation", "Games","Wins","WhichT")))
  
  # This needs to be adapted for having a status
  
  x$elo1 = crats[x$Giocatore1]
  x$elo2 = crats[x$Giocatore2]
  x$elo3 = crats[x$Giocatore3]
  x$elo4 = crats[x$Giocatore4]
  x$dev1 = pmin(sqrt(cdevs[x$Giocatore1]^2+cval^2*(time - nlasttourn[x$Giocatore1]+1)),rdmax)
  x$dev2 = pmin(sqrt(cdevs[x$Giocatore2]^2+cval^2*(time - nlasttourn[x$Giocatore2]+1)),rdmax)
  x$dev3 = pmin(sqrt(cdevs[x$Giocatore3]^2+cval^2*(time - nlasttourn[x$Giocatore3]+1)),rdmax)
  x$dev4 = pmin(sqrt(cdevs[x$Giocatore4]^2+cval^2*(time - nlasttourn[x$Giocatore4]+1)),rdmax)
  x$telo1 = (x$elo1+x$elo2)/2
  x$telo2 = (x$elo3+x$elo4)/2
  x$tdev1 = (x$dev1+x$dev2)/2
  x$tdev2 = (x$dev3+x$dev4)/2
  
  nlasttourn <- rep(time,length(nlasttourn))
  
  qv = log(10)/400
  x$gdev1 =  1/sqrt(1+3*(qv/pi)^2 * x$tdev1^2)
  x$gdev2 =  1/sqrt(1+3*(qv/pi)^2 * x$tdev2^2)
  x$exp1 = 1/(1+10^(-x$gdev1*(x$telo1-x$telo2)/400))
  x$exp2 = 1-x$exp1
  
  x$gdev =  1/sqrt(1+3*(qv/pi)^2 * (x$tdev1^2+x$tdev2^2)) 
  
  x$pij = 1/(1+10^(-x$gdev*(x$telo1-x$telo2)/400))
  x$disc = -x$Score*log(x$pij)-(1-x$Score)*log(1-x$pij)
  
  discr = sum(x$disc)
  
  
  dscores = 
    x %>% 
    as_tibble %>% 
    pivot_longer(cols = Giocatore1:Giocatore4, names_to = "giocatore") %>% 
    group_by(value) %>% 
    mutate( dscores = case_when(
      giocatore == "Giocatore1" | giocatore == "Giocatore2" ~  gdev2^2 * exp1 * exp2,
      giocatore == "Giocatore3" | giocatore == "Giocatore4" ~  gdev1^2 * exp1 * exp2
    )) %>% 
    summarize(
      dscore = (q^2 * sum(dscores))^(-1)
    ) %>% 
    as.data.frame()
  
  for(i in 1:nrow(x)){
    for(j in 1:4){
      numero_giocatore <- x[i,j+1]
      index <- which(dscores[,1] == numero_giocatore)
      
      x[i,j+23] <- dscores$dscore[index] 
      colnames(x)[j+23] <- paste("dscore",j,sep = "")
      
    }
  }
  
  x$pts1 = x$gdev2*x$Weight*(x$Score - x$exp1) * q/((x$dev1^2)^(-1)+(x$dscore1)^(-1))
  x$pts2 = x$gdev2*x$Weight*(x$Score - x$exp1) * q/((x$dev2^2)^(-1)+(x$dscore2)^(-1))
  x$pts3 = x$gdev1*x$Weight*((1-x$Score) - x$exp2) * q/((x$dev3^2)^(-1)+(x$dscore3)^(-1))
  x$pts4 = x$gdev1*x$Weight*((1-x$Score) - x$exp2) * q/((x$dev4^2)^(-1)+(x$dscore4)^(-1))
  
  
  points = 
    x %>% 
    as_tibble %>% 
    pivot_longer(cols = Giocatore1:Giocatore4, names_to = "giocatore") %>% 
    gather("pts_ref","pts",pts1:pts4) %>% 
    gather("elo_ref","elo",elo1:elo4) %>% 
    gather("dev_ref","dev",dev1:dev4) %>% 
    group_by(value) %>% 
    filter(
      giocatore  =="Giocatore1" & elo_ref =="elo1" & pts_ref =="pts1" & dev_ref == "dev1" |
        giocatore  =="Giocatore2" & elo_ref =="elo2" & pts_ref =="pts2" & dev_ref == "dev2" |
        giocatore  =="Giocatore3" & elo_ref =="elo3" & pts_ref =="pts3" & dev_ref == "dev3" |
        giocatore  =="Giocatore4" & elo_ref =="elo4" & pts_ref =="pts4" & dev_ref == "dev4" 
    ) %>% 
    summarize(new_pts = sum(pts)) %>% 
    as.data.frame()
  
  
  rownames(points) = giocatori[points$value]
  y = merge(rinit,points,by = "row.names")
  y$elo = round(y$x + y$new_pts,0)
  y = y[,c(1,5)]
  names(y) <- c("giocatore","Rating")
  
  
  rownames(dscores) = giocatori[dscores$value]
  z = merge(dinit,dscores,by = "row.names")
  z$rd = round(sqrt((1/z$x^2+1/z$dscore)^(-1)),0)
  z = z[,c(1,5)]
  names(z) <- c("giocatore","Deviation")
  
  history  = merge(y,z)
  history = history[order(history$giocatore,decreasing = F),]
  history$Games = ngames
  history$Win = nwin
  history$Loss = nloss
  
  player <- suppressWarnings(as.numeric(names(c(crats, orats))))
  if (any(is.na(player))) 
    player <- names(c(crats, orats))
  dfout <- data.frame(giocatore = player,
                      Rating = c(history$Rating, orats), 
                      Deviation = c(history$Deviation, odevs),
                      LastTourn = c(nlasttourn,olasttourn),
                      stringsAsFactors = FALSE)
  
  dfout <- dfout[order(dfout$Rating, decreasing = TRUE), ]
  
  row.names(dfout) <- 1:nrow(dfout)
  
  
  lout <- list(ratings = dfout, discr = discr, type = "Glicko")
  class(lout) <- "rating"
  
  return(lout)
}

load.datasets <- function(dataset = c("elo","glicko")){
  library(tidyverse)
  library(readxl)
  
  if(dataset == "elo"){
    elo_sesso <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/genere_elo_ir.xlsx") 
  # pd_open_gir <<- read_excel("D:/Personal Statistics/rcb/pd_open.xlsx") %>% filter(w1+w2 == 1)
  # pd_open_bra <<- read_excel("D:/Personal Statistics/rcb/pd_open.xlsx") %>% filter(w1+w2 != 1)
  # pd_femm <<- read_excel("D:/Personal Statistics/rcb/pd_femm.xlsx")
  # pd_mix <<- read_excel("D:/Personal Statistics/rcb/pd_mix.xlsx")
  ir_open_gir <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/ir_pt.xlsx", sheet = "open_ir") %>% filter(cat == "gir") %>% select(-cat)
  ir_open_bra <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/ir_pt.xlsx", sheet = "open_ir") %>% filter(cat == "bra") %>% select(-cat)
  ir_femm_gir <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/ir_pt.xlsx", sheet = "femminile_ir") %>% filter(cat == "gir") %>% select(-cat)
  ir_femm_bra <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/ir_pt.xlsx", sheet = "femminile_ir") %>% filter(cat == "bra") %>% select(-cat)
  ir_mixed <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/ir_pt.xlsx", sheet = "misto_ir") 
  hot_mil_open <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/hot_mil.xlsx", sheet = "open_mil") 
  hot_mil_femm <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/hot_mil.xlsx", sheet = "femminile") 
  hot_mil_mix <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/hot_mil.xlsx", sheet = "misto") 
  nat_forli_pow <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/Nationals.xlsx", sheet = "nomi_giusti") %>% filter(sit == "pow") %>% select(-sit)
  nat_forli_ene <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/Nationals.xlsx", sheet = "nomi_giusti") %>% filter(sit == "ene") %>% select(-sit)
  nat_forli_bra <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/Nationals.xlsx", sheet = "nomi_giusti") %>% filter(sit == "bra") %>% select(-sit)
  nat_forli_mixed_pow <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/Nationals.xlsx", sheet = "misto")%>% filter(sit == "pow") %>% select(-sit)
  nat_forli_mixed_ene <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/Nationals.xlsx", sheet = "misto")%>% filter(sit == "ene") %>% select(-sit)
  nat_forli_mixed_bra <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/Nationals.xlsx", sheet = "misto")%>% filter(sit == "bra") %>% select(-sit)
  open_fw <<-  read_excel("D:/Personal Statistics/rcb/Ranking/data/fwango_ws.xlsx", sheet = "nomi_giusti") %>% filter(cat == "Open") %>% select(-cat)
  femm_xl <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/femminile.xlsx", sheet = "nomi_giusti") %>% select(-P1,-P2)
  mixed_fw <<-  read_excel("D:/Personal Statistics/rcb/Ranking/data/fwango_ws.xlsx", sheet = "nomi_giusti") %>% filter(cat == "mixed") %>% select(-cat)
  }else if(dataset == "glicko"){
  source("D:/Personal Statistics/rcb/Ranking/glicko_function.R")
  
  data1 <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/ir_pt.xlsx", sheet = "open_ir")  %>% select(-cat) %>% from_xl_to_glicko(2023,5,28)
  data2 <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/ir_pt.xlsx", sheet = "femminile_ir")  %>% select(-cat) %>% from_xl_to_glicko(2023,5,28)
  data3 <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/ir_pt.xlsx", sheet = "misto_ir")  %>% from_xl_to_glicko(2023,5,28) %>% mutate(Time = Time+.1)
  
  data4 <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/hot_mil.xlsx", sheet = "open_mil") %>% from_xl_to_glicko(2023,7,14)
  data5 <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/hot_mil.xlsx", sheet = "femminile")  %>% from_xl_to_glicko(2023,7,14)
  data6 <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/hot_mil.xlsx", sheet = "misto")  %>% from_xl_to_glicko(2023,7,14) %>% mutate(Time = Time+.1)
  
  data7 <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/Nationals.xlsx", sheet = "nomi_giusti") %>% select(-sit) %>% from_xl_to_glicko(2023,9,30)
  data8 <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/Nationals.xlsx", sheet = "misto") %>% select(-sit) %>%  from_xl_to_glicko(2023,9,30) %>% mutate(Time = Time+.1)
  
  data9 <<-  read_excel("D:/Personal Statistics/rcb/Ranking/data/fwango_ws.xlsx", sheet = "nomi_giusti") %>% filter(cat == "Open") %>% select(-cat) %>%  from_xl_to_glicko(2023,11,4)
  data10 <<- read_excel("D:/Personal Statistics/rcb/Ranking/data/femminile.xlsx", sheet = "nomi_giusti") %>% select(-P1,-P2)%>%  from_xl_to_glicko(2023,11,4)
  data11 <<-  read_excel("D:/Personal Statistics/rcb/Ranking/data/fwango_ws.xlsx", sheet = "nomi_giusti") %>% filter(cat == "mixed") %>% select(-cat)%>%  from_xl_to_glicko(2023,11,4) %>% mutate(Time = Time+.1)
  }else{print("Wrong dataset specified")}
  
  
}
