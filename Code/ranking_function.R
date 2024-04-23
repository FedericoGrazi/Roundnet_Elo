# ELO FUNCTIONS ####


# Please take a look at elo_ets before
elo_c <- function(numplay, numrow, white1, white2, black1, black2, score, cdevsteams,
                     cratsteams,dscore = double(2*numplay),weights,ntourn) { # Capire perchè 2*np
  escore <- numeric(numplay)
  ascore <- numeric(numplay)
  escorek <- 0
  K = 20   # K factor
  Kn = 40 # K factor for new players
  
  Kpl <- ifelse(ntourn >4, Kn, K) # New players are players who have played less than 4 tournaments
  
  # Create Empty Vectors
  for (k in 1:numplay) {
    escore[k] <- 0
    ascore[k] <- 0
  }
  
  for (k in 1:numrow) {
    # Cumulative Scores (games won) for each player
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
  
  giocatori = sort(unique(c(x$Giocatore1, x$Giocatore2, x$Giocatore3, x$Giocatore4))) # Unique players
  
  np <- length(giocatori) # Total number of players
  nm <- length(unique(x$cat)) # Number of categories in the Tournaments

  # Assigns the category in which every player has played
  # PLEASE NOTE THAT MIXED SHOULD NOT BE IN THE SAME DATAFRAME
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

  # Calls Initialisation values for each category
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
    # Status binding
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
    # Players in a category
    playi <- players[[i]]
    # Games Played
    ngamesi <- tabulate(c(traini$Giocatore1, traini$Giocatore2,traini$Giocatore3, traini$Giocatore4), np)
    # see elo_c
    dscores <- elo_c(numplay = np, numrow = nr, white1 = traini$Giocatore1, white2 = traini$Giocatore2,
                        black1 = traini$Giocatore3,black2 = traini$Giocatore4, score = traini$Score,
                         cratsteams =  cratsteams,weights=traini$Weight,ntourn = ntourn)
    
    cdscores <- dscores[playi]
    # ascore <- dscores[np:2*np][playi]
    # names(ascore) <- giocatori[playi]
    
    # Updates score
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
  # Creates a df with only italian players
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

                    
# GLICKO FUNCTIONS ####
# PlayerRatings::glicko

# Obtain week passed from first tournament
weeks_passed <- function(year, month, day) {
  input_date <- as.Date(paste(year, month, day, sep = "-"))
  reference_date <- as.Date("2023-04-15") # ETS PARIS 2023 date as reference
  weeks <- round(as.numeric(difftime(input_date, reference_date, units = "weeks")),0)
  return(weeks)
}

 
# Removes a third player if fwango shows a third player columns
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
                    
# Transforms the .csv fwango export into a dataframe used in glicko_ets                    
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
  
  df$Weight <- ifelse(df$sets==1, .5,1) # Group Stage weigths only 0.5, Bracket 1
  # df$Weight <- ifelse(df$sets==1, .75,1)
  df$Weight[df$placement==1] <- .25 # Placement weigths .25
  # df$Weight[df$placement==1] <- 0.5 # Placement
  df$Score <- (df$Awins)/df$sets
  df$Score[df$Score == 2/3] <- 0.75 # Win 2-1 ==> 0.75
  df$Score[df$Score == 1/3] <- 0.25 # Lose 1-2 ==> 0.25
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
    
    # r-r_j - Rating difference between player and opponents
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

glicko_ets <- function(x, status = NULL,rdmax = 200, cval = 16, whichtourn = 0, ita = T, lambda = 1/20){
  
  x = separate(x, Play1, into = c("Giocatore1", "Giocatore2"), sep = " & ",remove = T)
  x = separate(x, Play2, into = c("Giocatore3", "Giocatore4"), sep = " & ",remove = T)
  
  time = unique(x$Time) # Time of the event in terms of weeks from ETS PARIS 2023
  giocatori = sort(unique(c(x$Giocatore1, x$Giocatore2, x$Giocatore3, x$Giocatore4))) # Unique players in the tournaments
  
  np <- length(giocatori)
  nm <- length(unique(x$cat))

  # Assigns the category in which every player has played
  # PLEASE NOTE THAT MIXED SHOULD NOT BE IN THE SAME DATAFRAME
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
  
  # Calls Initialisation values for each category
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
  # Costants
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
  
  for (i in 1:nm) { # For each division, compute the elo updates
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
    # Updates rating and deviation
    curupt <- qv/(1/cdevs[playi]^2+ cdval) * cdscores + lambda * l1t/ngamesi[playi]
    crats[playi] <- crats[playi] + curupt 
    
    cdevs[playi] <- sqrt((1/cdevs[playi]^2 + cdval)^(-1))
    # General info for each player
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
  
  if(ita){ # Creates new dataframe with only italians
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
