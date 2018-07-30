





### set up tournament
 #n_players <- 1501
 #tourn.specs <- list(n_rounds = 15, cut_rounds = 9, cut_wins = 6, mean_skill = 1600, sd_skill = 150)
 #player_skill <- rnorm(n_players, mean = 1600, sd = 150)
 elo_coeff <- 1135.77

# scores <- data.frame(ID = seq_len(n_players), 
#                      skill = player_skill, 
#                      byes = rep(0, n_players), 
#                      R0 = rep(0,n_players))
# opponents <- data.frame(ID = seq_len(n_players))

pairings <- function(scores, rnd = 1){
  df <- scores %>% 
    group_by_(paste0("R", rnd - 1)) %>%
    sample_frac(size = 1) %>% 
    ungroup
  n_matches <- floor(nrow(df)/2)
  opps <- integer(nrow(df)) 
  cbind(df$ID[(2*seq_len(n_matches))], df$ID[(2*seq_len(n_matches)) - 1])
}

new.scores <- function(scores, pairs, rnd = 1, elo_coeff = 1135.77){
  probs <- 1/(1+10^((scores$skill[match(pairs[,2], scores$ID)] - scores$skill[match(pairs[,1], scores$ID)]) /
                      elo_coeff))
  match_results <- rbinom(n = dim(pairs)[1], size = 1, prob = probs) == 1
  winners <- ifelse(match_results, pairs[,1], pairs[,2])
  scores %<>% mutate(temp = 0)
  scores$temp[match(winners, scores$ID)] <- 1
  scores$temp[!(scores$ID %in% pairs)] <- 1
  scores %>% mutate_("temp" = paste0("temp + R", rnd - 1)) %>% 
    rename_(.dots = setNames(list("temp"), paste0("R", rnd)))
}

round.results <- function(scores, rnd, elo_coeff = 1135.77){
  pairs  <- pairings(scores, rnd = rnd)
  res <- new.scores(scores, pairs, rnd = rnd, elo_coeff = elo_coeff)
}

tourn.outcome <- function(scores, elo_coeff = 1135.77, tourn.specs){
  scores <- purrr::reduce(.x = 1:tourn.specs$cut_rounds, .f = round.results, elo_coeff = elo_coeff, .init = scores)
  if(scores[[paste0("R", tourn.specs$cut_rounds)]][1] < tourn.specs$cut_wins){return(scores[[paste0("R", tourn.specs$cut_rounds)]][1])}
  scores %<>% filter_(paste0("R", tourn.specs$cut_rounds," >= ", tourn.specs$cut_wins))
  scores <- purrr::reduce(.x = (tourn.specs$cut_rounds+1):tourn.specs$n_rounds, .f = round.results, elo_coeff = elo_coeff, .init = scores)
  return(scores[[paste0("R", tourn.specs$n_rounds)]][1])
}

whole.tourn.outcome <- function(scores, elo_coeff = 1135.77, tourn.specs){
  d1.scores <- purrr::reduce(.x = 1:tourn.specs$cut_rounds, .f = round.results, elo_coeff = elo_coeff, .init = scores)
  d2.scores <- d1.scores %>% filter_(paste0("R", tourn.specs$cut_rounds," >= ", tourn.specs$cut_wins))
  d2.scores <- purrr::reduce(.x = (tourn.specs$cut_rounds+1):tourn.specs$n_rounds, .f = round.results, elo_coeff = elo_coeff, .init = d2.scores)
  bind_rows(d2.scores, anti_join(d1.scores, d2.scores, by = "ID"))
}

### sample multiple tournaments of one player
tourn.sampling <- function(rating = 1500, elo_coeff = 1135.77, tourn.specs, iter = 50){
  lapply(seq_len(iter), FUN = function(i){
    player_skill <- rnorm(n_players, mean = 1600, sd = 150)
    scores <- data.frame(ID = seq_len(n_players), 
                         skill = player_skill, 
                         byes = rep(0, n_players), 
                         R0 = rep(0,n_players))
    scores$skill[1] <- rating
    tourn.outcome(scores, elo_coeff = elo_coeff, tourn.specs)
  })
}

#tourn.sampling(rating = 1500, elo_coeff = 1000, tourn.specs = tourn.specs, iter = 5)


### compute win percentage}
win.percentage <- function(rating, rating.sample, elo_coeff = 1135.77){
  sapply(rating, FUN = function(x){mean((1+10^((rating.sample - x)/elo_coeff))^(-1))})
}

wp.rating <- function(wp, rating.sample, elo_coeff = 1135.77){
  if(wp > 0.99) stop("You wish!")
  if(wp < 0.01) stop("Ever thought of quitting?")
  uniroot(f = function(rating){wp - mean((1+10^((rating.sample - rating)/elo_coeff))^(-1))},
          interval = c(-600, 3700))$root
}


### skill.distribution
round.player.field <- function(tourn.results, tourn.specs){
  means <- sapply(0:tourn.specs$n_rounds, FUN = function(rnd){
    m.tab <- tourn.results %>% group_by_(paste0("R",rnd)) %>% 
      summarise(m_skill = mean(skill)) %>% filter_(paste0("!is.na(R",rnd,")")) 
    setNames(m.tab$m_skill, m.tab[[as.character(paste0("R",rnd))]])
  })
  names(means) <- as.character(0:tourn.specs$n_rounds)
  
  sds <- sapply(0:tourn.specs$n_rounds, FUN = function(rnd){
    sd.tab <- tourn.results %>% group_by_(paste0("R",rnd)) %>% 
      summarise(sd_skill = sd(skill)) %>% filter_(paste0("!is.na(R",rnd,")")) 
    setNames(sd.tab$sd_skill, sd.tab[[as.character(paste0("R",rnd))]])
  })
  names(sds) <- as.character(0:tourn.specs$n_rounds)
  
  # wp.mean.rate <- lapply(means, FUN = function(m.ratings){
  #   win.percentage(m.ratings, tourn.results$skill)
  # })
  # 
  # mean.wp.rate <- lapply(0:n_rounds, FUN = function(rnd){
  #   res <- sapply(0:rnd, FUN = function(score){
  #     ratings <- tourn.results %>% mutate(temp = score) %>% 
  #       filter_(paste0("R",rnd," == temp")) %>% .[["skill"]]
  #     mean(win.percentage(ratings, tourn.results$skill))
  #   })
  #   setNames(res, as.character(0:rnd))[!is.na(res)]
  # })
  # names(mean.wp.rate) <- as.character(0:n_rounds)
  list("mean" = means, "sd" = sds) #, "wpm" = wp.mean.rate, "mwr" = mean.wp.rate)
}

# round.specs <- round.player.field(res, tourn.specs)

### winprob distribution by round aka the win probability tree
wp.byround <- function(rating, round.specs){
  wpbr <- lapply(seq_len(length(round.specs$mean)), FUN = function(rn){
    rwp <- lapply(seq_len(length(round.specs$mean[[rn]])), FUN = function(score){
      win.percentage(rating, qnorm(p = seq(0.01,0.99, 0.01), round.specs$mean[[rn]][score], round.specs$sd[[rn]][score]), elo_coeff = 1135.77)
    }) %>% unlist
    names(rwp) <- names(round.specs$mean[[rn]])
    rwp
  })
  names(wpbr) <- names(round.specs$mean)
  wpbr
}

### plot of wpt
wpt.plot <- function(wpt, tourn.specs){
  nocut_wpt  <- lapply(seq_len(length(wpt)), FUN = function(rn){
    min_score <- min(as.numeric(names(wpt[[rn]])))
    if(min_score > 0){
      c(setNames(rep(0, min_score), as.character(0:(min_score-1))), wpt[[rn]])
    } else {
      wpt[[rn]]
    }
  })
  
  wpt.df <- lapply(seq_len(length(nocut_wpt)-1), FUN = function(rn){
    data.frame(round = rep(rn, length(nocut_wpt[[rn]])),
               wins = as.numeric(names(nocut_wpt[[rn]])),
               win_prob = round(nocut_wpt[[rn]], digits = 3),
               p_loss = nocut_wpt[[rn+1]][1:rn],
               p_win = nocut_wpt[[rn+1]][2:(rn+1)])
  }) %>% bind_rows
  
  wpt.df <- wpt.df %>% filter(round <= tourn.specs$n_rounds) %>% 
    filter(!(round > tourn.specs$cut_rounds & wins < tourn.specs$cut_wins)) %>% 
    mutate(p_loss = ifelse(round == tourn.specs$n_rounds | (round == tourn.specs$cut_rounds & wins < tourn.specs$cut_wins), NA, p_loss),
           p_win = ifelse(round == tourn.specs$n_rounds | (round == tourn.specs$cut_rounds & wins < tourn.specs$cut_wins - 1), NA, p_win))
  
  p <- ggplot(wpt.df) + 
    geom_segment(aes(x = round, xend = round + 1, y = win_prob, yend = p_win), color = "lightgrey") +
    geom_segment(aes(x = round, xend = round + 1, y = win_prob, yend = p_loss), color = "lightgrey") +
    geom_point(aes(x = round, y = win_prob, color = wins), size = 2) + #, color = "darkred") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = 1:tourn.specs$n_rounds) + 
    labs(title = "Win probability tree", x = "Round", y = "  ")
  ggplotly(p)
}



# wpbr <- wp.byround(1600, round.specs)

score_distribution <- function(wins = 0, losses = 0, draws = 0, wpbr, tourn.specs){
  if(wins + losses + draws > tourn.specs$n_rounds){
    return(c(1, rep(0, tourn.specs$n_rounds)))
  }
  score <- c(rep(0, wins), 1, rep(0, losses + draws))
  if(length(score) < tourn.specs$cut_rounds + 1){
    d1 <- purrr::reduce(.x = wpbr[length(score):tourn.specs$cut_rounds], .f = function(x, p){
      c(x * (1-p), 0) + c(0, x * p)
      }, .init = score)
    score <- d1
  }
  if(length(score) < tourn.specs$n_rounds + 1){
    d2 <- purrr::reduce(.x = wpbr[length(score):tourn.specs$n_rounds], .f = function(x, p){
      c(x * (1-tail(p, length(x))), 0) + c(0, x * tail(p, length(x)))
      }, .init = score[-seq_len(tourn.specs$cut_wins)])
    score <- c(score[seq_len(tourn.specs$cut_wins)], d2)
  }  
  score
}

# score_dist <- score_distribution(0,0,0, wpbr, tourn.specs)

### score standings by round 15 excluding draws
### attention: players with multiple byes are counted for each round separately, i.e. multiply
standings <- function(n_players = 2000, rel.byes = c(0,0,0), abs.byes = NULL, tourn.specs){
  if(is.null(abs.byes)){
    byes <- c(ceiling(rel.byes*n_players), rep(0, tourn.specs$cut_rounds - 3))
  } else {
    byes <- pmin(c(abs.byes, rep(0, tourn.specs$cut_rounds - 3)), n_players)
  }
  day1scores <- purrr::reduce(.x = byes, .f = function(scores, n_byes){
                    index <- union(seq_len(n_byes), seq(1, n_players, 2))
                    scores[index] <- scores[index] + 1
                    sort(scores, decreasing = T)
                  }, .init = rep(0, n_players))
  ###day2
  day2scores <- purrr::reduce(.x = seq_len(tourn.specs$n_rounds - tourn.specs$cut_rounds), .f = function(scores, iter){
                    index <- seq(1, length(scores), 2)
                    scores[index] <- scores[index] + 1
                    sort(scores, decreasing = T)
                  }, .init = day1scores[which(day1scores >= tourn.specs$cut_wins)])
  day2scores
}

# stands <- standings(n_players = 1500, tourn.specs = tourn.specs)


### top 8
### compute the expected skill distribution 
top8skill <- function(stands, round.specs){
   sk.sample <- rnorm(10000, last(round.specs$mean)[as.character(stands[1:8])], last(round.specs$sd)[as.character(stands[1:8])])
   list("mean" = mean(sk.sample), "sd" = sd(sk.sample))
}

# t8s <- top8skill(stands, round.specs)

top8_matchwp <- function(rating, stands, round.specs){
  t8s <- top8skill(stands, round.specs)
  win.percentage(rating = rating, rating.sample = qnorm(p = seq(0.02,0.98, 0.02), t8s$mean, t8s$sd))
}

# t8mwp <- top8_matchwp(1600, stands, round.specs)

### prize money

top8money <- function(tourn = "GP"){
  if(tourn == "GP"){return(c(1500, 2500, 5000, 10000))}
  if(tourn == "PT"){return(c(8150, 13750, 20000, 50000))}
}

top8propoints <- function(tourn = "GP"){
  if(tourn == "GP"){return(c(4, 5, 6, 8))}
  if(tourn == "PT"){return(c(18, 23, 26, 30))}
}

exptop8prize <- function(winprob, prizemoney){
  sum(c(winprob^(0:2)*(1-winprob) * prizemoney[1:3], winprob^3*prizemoney[4]))
}

# et8p <- exptop8prize(winprob = t8mwp, prizemoney = top8money("GP"))

prizemoney <- function(et8p, tourn = "GP", n_players = 2000){
  t8prizes <- rep(et8p, 8)
  if(tourn == "GP"){
    prizes <- c(t8prizes, rep(1000, 8), rep(500, 16), rep(250, 32))
    if(n_players >= 3000){
      prizes <- c(prizes, rep(250, 36), rep(200, 80))
    } 
  }
  if(tourn == "PT"){
    prizes <- c(t8prizes, rep(5000, 8), rep(3000, 8), rep(2000, 8), rep(1500, 16), rep(1000, 16))
  }
  prizes
}

# pmoney <- prizemoney(et8p, tourn = "GP", n_players = 1500)

meanpayout.bywins <- function(stands, pmoney){
  stopifnot(length(stands) >= length(pmoney))
  pmoney <- c(pmoney, rep(0, length(stands) - length(pmoney)))
  df <- data.frame(stands, pmoney)
  payout_nodraw <- data.frame(stands = 0:max(df$stands)) %>% 
    left_join(df %>% group_by(stands) %>% summarise(p = mean(pmoney)), by = "stands")
  for(i in length(payout_nodraw$stands):1){
    if(is.na(payout_nodraw$p[i])){payout_nodraw$p[i] <- payout_nodraw$p[i+1]}
  }
  payout_withdraw <- data.frame(stands = 0:max(df$stands)) %>% 
    left_join(df %>% group_by(stands) %>% summarise(p = max(pmoney)), by = "stands")
  for(i in length(payout_withdraw$stands):1){
    if(is.na(payout_withdraw$p[i])){payout_withdraw$p[i] <- payout_withdraw$p[i+1]}
  }
  list(nodraw = payout_nodraw, plusdraw = payout_withdraw)
}

# t8chance.bywins <- meanpayout.bywins(standings, rep(1,8))

meanpropoints.bywins <- function(tourn = "GP", t8pp, t8chance.bywins){
  if(tourn == "GP"){
    pps <- rep(t8pp, 16) * t8chance.bywins + rev(c(4,4,4,3,2,1,rep(0,10))) * (1-t8chance.bywins)
    return(list(nodraw = pps, plusdraw = pps))
  }
  if(tourn == "PT"){
    pps_nodraw <- rep(t8pp, 17) * t8chance.bywins + rev(c(rep(15, 5), 10, 6, 4, rep(3,9))) * (1-t8chance.bywins)
    pps_draw <- rep(t8pp, 17) * t8chance.bywins + rev(c(rep(15, 5), 11, 7, 5, rep(3,9))) * (1-t8chance.bywins)
    return(list(nodraw = pps_nodraw, plusdraw = pps_draw))
  }
}

# mp <- meanpayout.bywins(stands, pmoney)

exp.payout <- function(scoredistri, payoutbyscore){
  scoredistri %*% payoutbyscore
}

# ev <- exp.payout(score_dist, mp$nodraw$p)

