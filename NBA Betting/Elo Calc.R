## Calculate Elo

updateK <- function(MOV, elo_diff) {
  K = 20
  if(MOV > 0) {
    mult = ((MOV+3)^.8)/(7.5+.006*elo_diff)
  } else {
    mult = ((-MOV+3)^.8)/(7.5+.006*-elo_diff)
  }
  return(c(K*mult,K*mult))
}

winLose <- function(homeScore, awayScore) {
  H = 0
  A = 0
  if(homeScore > awayScore) {
    browser()
    H = 1
  } else if(homeScore < awayScore) {
    A = 1
  } else {
    H = .5
    A = .5
  }
  return(c(H,A))
}

predElo <- function(homeElo, awayElo) {
  homeChange = 1/(1+10^((awayElo - homeElo))/400)
  return(homeChange)
}

updateElo <- function(homeScore, awayScore, homeElo, awayElo) {
  HFA = 100
  eloDiff <- homeElo - awayElo
  MOV = homeScore - awayScore
  homeElo <- homeElo + HFA
  homeChange <- predElo(homeElo, awayElo)
  awayChange <- 1-homeChange 
  tmp <- winLose(homeScore, awayScore)
  H <- tmp[1]
  A <- tmp[2]
  tmp2 <- updateK(MOV, eloDiff)
  homeK <- tmp2[1]
  awayK <- tmp2[2]
  return(c(homeK*(H-homeChange),awayK*(A-awayChange)))
}
