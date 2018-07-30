
library(dplyr)
library(magrittr)
source('globals.R')

r.sp <- list("GP" = list("dense" = numeric(), "base" = numeric(), "spread" = numeric()),
             "PT" = list("dense" = numeric(), "base" = numeric(), "spread" = numeric()))

## sample one big tournament
n_players <- 10000000
elo_coeff <- 1135.77


### SD specs:
skill_sd <- list("GP" = c("dense" = 66, "base" = 100, "spread" = 200),
                 "PT" = c("dense" = 150, "base" = 200, "spread" = 250))

tourn.specs <- list(n_rounds = 15, cut_rounds = 8, cut_wins = 6, mean_skill = 1554)
for(sd_dens in c("dense", "base", "spread")){
  player_skill <- rnorm(n_players, mean = tourn.specs$mean_skill, sd = skill_sd$GP[[sd_dens]])
  scores <- data.frame(ID = seq_len(n_players), 
                           skill = player_skill, 
                           byes = rep(0, n_players), 
                           R0 = rep(0,n_players))
  res <- whole.tourn.outcome(scores, elo_coeff = elo_coeff, tourn.specs = tourn.specs)
  r.sp$GP[[sd_dens]] <- round.player.field(res, tourn.specs)
}

tourn.specs <- list(n_rounds = 16, cut_rounds = 8, cut_wins = 4, mean_skill = 1778)
for(sd_dens in c("dense", "base", "spread")){
  player_skill <- rnorm(n_players, mean = tourn.specs$mean_skill, sd = skill_sd$PT[[sd_dens]])
  scores <- data.frame(ID = seq_len(n_players), 
                       skill = player_skill, 
                       byes = rep(0, n_players), 
                       R0 = rep(0,n_players))
  res <- whole.tourn.outcome(scores, elo_coeff = elo_coeff, tourn.specs = tourn.specs)
  r.sp$PT[[sd_dens]] <- round.player.field(res, tourn.specs)
}

save(r.sp, file = "roundspecs")

### rating sample plots
## R9
# res$Wins <- as.factor(res$R9)
# ggplot(res, aes(x = skill, fill = Wins)) +
#   geom_density(position = "stack") +
#   labs(title = "", x = "Rating", y = "Density")
