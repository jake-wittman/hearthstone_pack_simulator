
# libraries ---------------------------------------------------------------

pacman::p_load(tidyverse,
               furrr,
               patchwork,
               zoo)


# Pseudocode --------------------------------------------------------------

# At least 1 rare *or better* in every pack
# Average 1 epic in every 5 packs
# Average 1 legendary in every 20
# Epic pity timer: 10
# Legendary pity timer: 40
# Include guaranteed legendary in first 10 packs
# How does the pity timer card replace one of the other cards?

# Each pack opens
# 1 card: Draw a rare or greater based on marginal prob of drawing rare or greater
# other 4 cards: Use above described averages and full empirical percentages to
# calculate the probability of a particular rarity
# Add the number of each card rarity to a total counter
# Function takes as input the # of cards in each rarity

# Make a data frame that has a row for every pack opened and a column for the number
# of cards in each rarity. Use to make plotts and such

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
# Much faster pack opening function. The code is probably a bit harder to understand
# and will take more effort to dig into, but should produce qualitatively similar
# results as the much slower version.

# Set the number of packs to open > 500 (probably 1000 for a normal set).
openPack <- function(num.commons,
                     num.rares,
                     num.epics,
                     num.legendaries,
                     num.packs) {
  
  # Data frame to hold data
  df <- as.data.frame(matrix(NA, ncol = 4, nrow = num.packs))
  names(df) <- c("commons",
                 "rares",
                 "epics",
                 "legendaries")
  
  
  # Get full number in set
  num.set <- num.commons + num.rares + num.epics + num.legendaries
  
  all_cards <- rmultinom(num.packs,
                         5,
                         prob = c(common_perc,
                                  rare_perc,
                                  epic_perc,
                                  legendary_perc))
  
  if (any(all_cards[2, ] == 0 &
          all_cards[3, ] == 0 &
          all_cards[4, ] == 0)) {
    
    dedicated_good_drop <- rmultinom(sum(all_cards[2,  ] == 0 &
                                           all_cards[3,  ] == 0 &
                                           all_cards[4,  ] == 0), 1, prob = c(0,
                                                                              marg_rare_perc,
                                                                              marg_epic_perc,
                                                                              marg_legendary_perc))
    
    dedicated_good_drop[1, ] <- -1
    
    all_cards[, which(all_cards[2,  ] == 0 &
                        all_cards[3,  ] == 0 &
                        all_cards[4,  ] == 0)] <-
      all_cards[, which(all_cards[2,  ] == 0 &
                          all_cards[3,  ] == 0 &
                          all_cards[4,  ] == 0)] + dedicated_good_drop
  }
  
  # Check for guaranteed legendary in first 10 packs
  if (all(all_cards[4, 1:10] == 0)) {
    
    # Want to replaced one of the common or rare cards (assumed behavior)
    num_rare_common <- all_cards[1, 10] + all_cards[2, 10]
    # Pick whether common or rare is replaced
    replace <- rmultinom(1, 1, prob = c(all_cards[1, 10] / num_rare_common,
                                        all_cards[2, 10] / num_rare_common,
                                        0,
                                        0))
    # Subtract the replaced card
    all_cards[, 10] <- all_cards[, 10] + -replace
    # Add legendary
    all_cards[4, 10] <- all_cards[4, 10] + 1
    
  }
  
  # Legendary pity timer - if there are any sequences of no legendaries >= 40,
  # trigger this
  while (any(rle(all_cards[4,] == 0)$lengths >= 40)) {
    
    
    # Look for sequences of 0 (no legendaries) and how long those sequencies are
    legendary_check <- rle(all_cards[4,] == 0)
    
    # where do the sequences occur
    where_bad_luck <- rep(ifelse(legendary_check$values, legendary_check$lengths, 0),
                          times = legendary_check$lengths)
    # Get rid of sequences of 0 less than 40
    where_bad_luck <- ifelse(where_bad_luck > 39, where_bad_luck, 0)
    
    # First instance of bad luck streak
    first_bad_luck <-  which(where_bad_luck > 39)[40]
    
    # Want to replace one of the common or rare cards (assumed behavior)
    num_rare_common <- all_cards[1, first_bad_luck] + all_cards[2, first_bad_luck]
    # Pick whether common or rare is replaced
    replace <- rmultinom(1, 1, prob = c(all_cards[1, first_bad_luck] / num_rare_common,
                                        all_cards[2, first_bad_luck] / num_rare_common,
                                        0,
                                        0))
    # Subtract the replaced card
    all_cards[, first_bad_luck] <- all_cards[, first_bad_luck] + -replace
    # Add legendary
    all_cards[4, first_bad_luck] <- all_cards[4, first_bad_luck] + 1
    
  }
  
  
  # Epic pity timer - if there are any sequencies of no epics >= 10, trigger this
  while (any(rle(all_cards[3,] == 0)$lengths >= 10)) {
    
    
    # Look for sequences of 0 (no legendaries) and how long those sequencies are
    epic_check <- rle(all_cards[3,] == 0)
    
    # where do the sequences occur
    where_bad_luck <- rep(ifelse( epic_check$values,  epic_check$lengths, 0),
                          times =  epic_check$lengths)
    # Get rid of sequences of 0 less than 40
    where_bad_luck <- ifelse(where_bad_luck > 9, where_bad_luck, 0)
    
    # First instance of bad luck streak
    first_bad_luck <-  which(where_bad_luck > 9)[10]
    
    # Want to replace one of the common or rare cards (assumed behavior)
    num_rare_common <- all_cards[1, first_bad_luck] + all_cards[2, first_bad_luck]
    # Pick whether common or rare is replaced
    replace <- rmultinom(1, 1, prob = c(all_cards[1, first_bad_luck] / num_rare_common,
                                        all_cards[2, first_bad_luck] / num_rare_common,
                                        0,
                                        0))
    # Subtract the replaced card
    all_cards[, first_bad_luck] <- all_cards[, first_bad_luck] + -replace
    # Add legendary
    all_cards[3, first_bad_luck] <- all_cards[3, first_bad_luck] + 1
    
  }
  
  # Add to data frame
  df[, 1:4] <- t(all_cards)
  df <- df %>% 
    mutate(common_cumsum = accumulate(commons, sum),
           rare_cumsum  = accumulate(rares, sum),
           epic_cumsum = accumulate(epics, sum),
           legendary_cumsum = accumulate(legendaries, sum))
  
  df <- df %>% 
    mutate(common_perc = common_cumsum / num.commons,
           rare_perc = rare_cumsum / num.rares,
           epic_perc = epic_cumsum / num.epics,
           legendary_perc = legendary_cumsum / num.legendaries) %>% 
    
    mutate(common_perc = ifelse(common_perc > 1, 1, common_perc),
           rare_perc = ifelse(rare_perc > 1, 1, rare_perc),
           epic_perc = ifelse(epic_perc > 1, 1, epic_perc),
           legendary_perc = ifelse(legendary_perc > 1, 1, legendary_perc),
           set_perc = ((common_perc * num.commons) +
                         (rare_perc * num.rares) +
                         (epic_perc * num.epics) +
                         (legendary_perc * num.legendaries)) / num.set,
           excess_commons = common_cumsum - num.commons,
           excess_rares = rare_cumsum - num.rares,
           excess_epics = epic_cumsum - num.epics,
           excess_legendaries = legendary_cumsum - num.legendaries) %>% 
    
    mutate(excess_commons = ifelse(excess_commons < 0, 0, excess_commons),
           excess_rares = ifelse(excess_rares < 0, 0, excess_rares),
           excess_epics = ifelse(excess_epics < 0, 0, excess_epics),
           excess_legendaries = ifelse(excess_legendaries < 0, 0, excess_legendaries)) %>% 
    
    mutate(total_dust =  excess_commons * 5 +
             excess_rares * 20 +
             excess_epics * 100 +
             excess_legendaries * 400,
           num_packs = 1:nrow(.)) %>% 
    
    select(-contains("excess"))
  
  
  return(df)
} #end function

# Slower alternative function - code is more logical to interpret I think but is MUCH SLOWER
# Like, stupidly slower. I think the probability differences between the two are
# negligible.
openPackAlt <- function(num.commons,
                     num.rares,
                     num.epics,
                     num.legendaries) {
  
  # Data frame to hold data
  df <- data.frame(commons = NA,
                   rares = NA,
                   epics = NA,
                   legendaries = NA,
                   common_cumsum = NA,
                   rare_cumsum = NA,
                   epic_cumsum = NA,
                   legendary_cumsum = NA)
  
  # Get full number in set
  num.set <- num.commons + num.rares + num.epics + num.legendaries
  # Set timers
  epic.pity.timer <- 0
  legendary.pity.timer <- 30
  i <- 1
  # Loop for pack opening
  while (sum(df$commons, na.rm = TRUE) < num.commons |
         sum(df$rares, na.rm = TRUE) < num.rares |
         sum(df$epics, na.rm = TRUE) < num.epics |
         sum(df$legendaries, na.rm = TRUE) < num.legendaries) {
    
    all_cards <- rmultinom(1, 5, prob = c(common_perc,
                                      rare_perc,
                                      epic_perc,
                                      legendary_perc))
    # If all cards are common generate better card
    if (all_cards[2,1] == 0 &
        all_cards[3, 1] == 0 &
        all_cards[4, 1] == 0) {
      
      dedicated_good_drop <- rmultinom(1, 1, prob = c(0,
                                                      marg_rare_perc,
                                                      marg_epic_perc,
                                                      marg_legendary_perc))
      
      dedicated_good_drop[1, 1] <- -1
      
      all_cards <- dedicated_good_drop + all_cards
    }

    
    
    
    # Increment pity timer
    if (all_cards[3, 1] == 0) {
      epic.pity.timer <- epic.pity.timer + 1
    } else {
      epic.pity.timer <- 0
    }
    
    if (all_cards[4, 1] == 0) {
      legendary.pity.timer <- legendary.pity.timer + 1
    } else {
      legendary.pity.timer <- 0
    }
    
    # Activate pity timer
    if (epic.pity.timer == 10) {
      # Want to replaced one of the common or rare cards (assumed behavior)
      num_rare_common <- all_cards[1, 1] + all_cards[2, 1]
      # Pick whether common or rare is replaced
      replace <- rmultinom(1, 1, prob = c(all_cards[1, 1] / num_rare_common,
                                          all_cards[2, 1] / num_rare_common,
                                          0,
                                          0))
      # Subtract the replaced card
      all_cards <- all_cards + -replace
      
      all_cards[3, 1] <- all_cards[3, 1] + 1
      
      # Reset timer
      epic.pity.timer <- 0
    } # End epic pity timer
    
    if (legendary.pity.timer == 40) {
      
      # Want to replaced one of the common or rare cards (assumed behavior)
      num_rare_common <- all_cards[1, 1] + all_cards[2, 1]
      # Pick whether common or rare is replaced
      replace <- rmultinom(1, 1, prob = c(all_cards[1, 1] / num_rare_common,
                                          all_cards[2, 1] / num_rare_common,
                                          0,
                                          0))
      # Subtract the replaced card
      all_cards <- all_cards + -replace
      # Add legendary
      all_cards[4, 1] <- all_cards[4, 1] + 1
      #Reset timer
      legendary.pity.timer <- 0
    } # End legendary pity timer
    
    # Add to data frame
    df[i, 1:4] <- t(all_cards)
    df <- df %>% 
      mutate(common_cumsum = accumulate(commons, sum),
             rare_cumsum  = accumulate(rares, sum),
             epic_cumsum = accumulate(epics, sum),
             legendary_cumsum = accumulate(legendaries, sum))
    i <- i + 1

  } # End pack opening loop
  
  df <- df %>% 
    mutate(common_perc = common_cumsum / num.commons,
           rare_perc = rare_cumsum / num.rares,
           epic_perc = epic_cumsum / num.epics,
           legendary_perc = legendary_cumsum / num.legendaries) %>% 
    
    mutate(common_perc = ifelse(common_perc > 1, 1, common_perc),
           rare_perc = ifelse(rare_perc > 1, 1, rare_perc),
           epic_perc = ifelse(epic_perc > 1, 1, epic_perc),
           legendary_perc = ifelse(legendary_perc > 1, 1, legendary_perc),
           set_perc = ((common_perc * num.commons) +
             (rare_perc * num.rares) +
             (epic_perc * num.epics) +
             (legendary_perc * num.legendaries)) / num.set,
           excess_commons = common_cumsum - num.commons,
           excess_rares = rare_cumsum - num.rares,
           excess_epics = epic_cumsum - num.epics,
           excess_legendaries = legendary_cumsum - num.legendaries) %>% 
    
    mutate(excess_commons = ifelse(excess_commons < 0, 0, excess_commons),
           excess_rares = ifelse(excess_rares < 0, 0, excess_rares),
           excess_epics = ifelse(excess_epics < 0, 0, excess_epics),
           excess_legendaries = ifelse(excess_legendaries < 0, 0, excess_legendaries)) %>% 
    
    mutate(total_dust =  excess_commons * 5 +
                         excess_rares * 20 +
                         excess_epics * 100 +
                         excess_legendaries * 400,
           num_packs = 1:nrow(.)) %>% 
    
    select(-contains("excess"))
  
  return(df)
} #end function


# Function to get the average number of packs you need to open to get a certain 
# percentage. 
avgPacksForPerc <- function(list.out,
                            perc) {
  out <- list.out %>% 
    map(function(.x) {
      list.x <- .x[1:which(.x$set_perc >= perc)[1], ]
    }) %>% 
    bind_rows(.id = "sim") %>% 
    mutate(sim = as.numeric(sim))
  
  total_packs_opened <- out %>%
    group_by(sim) %>% 
    count()
  return(mean(total_packs_opened$n))
}

#  Simulate ---------------------------------------------------------------

# Number of cards in Descent of Dragons
num_commons <- 98
num_rares <- 72
num_epics <- 54
num_legendaries <- 28
num_packs <- 5000

set_nums <- c(num_commons,
              num_rares,
              num_epics,
              num_legendaries,
              num_packs)

num_sims <- 50
sim_list <- rep(list(set_nums), num_sims)

# Set up parallel simulations
plan(multiprocess)

microbenchmark(future_map(sim_list, function(.x) {
  out <- openPack(
    num.commons = .x[1],
    num.rares = .x[2],
    num.epics = .x[3],
    num.legendaries = .x[4],
    num.packs = .x[5]
  )
  return(out)
},
.progress = TRUE),
map(sim_list, function(.x) {
  out <- openPack(
    num.commons = .x[1],
    num.rares = .x[2],
    num.epics = .x[3],
    num.legendaries = .x[4],
    num.packs = .x[5]
  )
  return(out)
}), times = 10)

# Simulate pack opening until set is complete
out <- future_map(sim_list, function(.x) {
  out <- openPack(
    num.commons = .x[1],
    num.rares = .x[2],
    num.epics = .x[3],
    num.legendaries = .x[4],
    num.packs = .x[5]
  )
  return(out)
},
.progress = TRUE) 



# Summaries ---------------------------------------------------------------
plot_out <- out %>% 
  bind_rows(.id = "sim") %>% 
  mutate(sim = as.numeric(sim))
# Average number of packs till set completion
set_perc_plot <- ggplot(plot_out, aes(x = num_packs, y = set_perc)) +
  geom_line(aes(group = sim), alpha = 0.25, colour = "grey") +
  geom_smooth(se = TRUE) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 250, 25),
                     labels = seq(0, 250, 25),
                     limits = c(0, 250)) +
  geom_hline(yintercept = c(0.25, 0.5, 0.75, 1.0), linetype = "dashed") +
  geom_vline(xintercept = c(25, 50, 75, 100), linetype = "dashed") +
  labs(x = "Number of packs opened", y = "Percent of set obtained")


dust_plot <- ggplot(plot_out, aes(x = num_packs, y = total_dust)) +
  geom_line(aes(group = sim), alpha = 0.25, colour = "grey") +
  geom_smooth(se = TRUE) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 10000, 2500),
                     labels = seq(0, 10000, 2500),
                     limits = c(0, 10000)) +
  scale_x_continuous(breaks = seq(0, 200, 25),
                     labels = seq(0, 200, 25),
                     limits = c(0, 200)) +
  labs(x = "Number of packs opened", y = "Dust from disenchanting duplicates")


p <- set_perc_plot + dust_plot

p



# Average number of packs to reach X% -------------------------------------

percs_test <- c(0.1, 0.25, 0.5, 0.75, 1.0)

perc_summary <- sapply(percs_test,
                       FUN = avgPacksForPerc,
                       list.out = out)
names(perc_summary) <- percs_test
perc_summary

avgPacksForPerc(out, 0.50)


test %>% 
  summarise(common_prob = sum(commons) / sum(commons, rares, epics, legendaries),
            rare_prob = sum(rares) / sum(commons, rares, epics, legendaries),
            epic_prob = sum(epics) / sum(commons, rares, epics, legendaries),
            legendary_prob = sum(legendaries) / sum(commons, rares, epics, legendaries))

