
# libraries ---------------------------------------------------------------

pacman::p_load(tidyverse)


# Pseudocode --------------------------------------------------------------

# At least 1 rare *or better* in every pack
# Average 1 epic in every 5 packs
# Average 1 legendary in every 20
# Epic pity timer: 10
# Legendary pity timer: 40
# Include guaranteed legendary in first 10 packs
# Does a pity timer card take the place of 

# Each pack opens
# 1 card: Draw a rare or greater based on marginal prob of drawing rare or greater
# other 4 cards: Use above described averages and full empirical percentages to
# calculate the probability of a particular rarity
# Add the number of each card rarity to a total counter
# Function takes as input the # of cards in each rarity


# Probability calculations ------------------------------------------------
# Looking at the data on the wiki, the percentage of total for the rarities is
# pretty consistent, so I'll work with the one with most cards the GT

total_cards <- 75545
total_packs <- total_cards / 5
common_cards <- 54271
rare_cards <- 17276
epic_cards <- 3232
legendary_cards <- 766
non_common_total <- sum(c(rare_cards, epic_cards, legendary_cards))

common_perc <- common_cards / total_cards
rare_perc <- rare_cards / total_cards
epic_perc <- epic_cards / total_cards
legendary_perc <- legendary_cards / total_cards

marg_rare_perc <- rare_cards / non_common_total
marg_epic_perc <- epic_cards / non_common_total
marg_legendary_perc <- legendary_cards / non_common_total

# Functions ---------------------------------------------------------------
openPack <- function(epic.pity.timer,
                     legendary.pity.timer) {
  dedicated_good_drop <- rmultinom(1, 1, prob = c(marg_rare_perc,
                                                  marg_epic_perc,
                                                  marg_legendary_perc))
  if (epic.pity.timer == 10) {
    
  }
  
  if (legendary.pity.timer == 40) {
    
  }
}