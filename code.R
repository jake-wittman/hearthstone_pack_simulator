
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

# Originally assumed the pity timer replaced a common or a rare card, but my %s were off 
# so I'm going to try just replacing a common card when the pity timer
# activates. 

# After ^ That didn't seem to help I think the issue may be when a
# card pack is all commons and how I replace one common with a non-commmon card.
# Need to think about how that process actually occurs



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
  
  # Probability calculations ------------------------------------------------
  # Looking at the data on the wiki, the percentage of total for the rarities is
  # pretty consistent. I'll just use the totals
  
  total_cards <- 163485
  total_packs <- total_cards / 5
  common_cards <- 117090
  rare_cards <- 37355
  epic_cards <- 7245
  legendary_cards <- 1795
  non_common_total <- sum(c(rare_cards, epic_cards, legendary_cards))
  
  common_perc <- common_cards / total_cards
  rare_perc <- rare_cards / total_cards
  epic_perc <- epic_cards / total_cards
  legendary_perc <- legendary_cards / total_cards
  
  
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
    
    dedicated_good_drop <- rmultinom(
      sum(all_cards[2,] == 0 &
            all_cards[3,] == 0 &
            all_cards[4,] == 0),
      1,
      prob = c(0,
               rare_perc + common_perc,
               epic_perc,
               legendary_perc)
    )
    
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
    # Add legendary
    all_cards[, 10] <- t(c(-1, 0, 0, 1))
    
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
  
    # Add legendary
    all_cards[, first_bad_luck] <- c(-1, 0, 0, 1)
    
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
    
    # Add epic
    all_cards[, first_bad_luck] <- c(-1, 0, 1, 0)
    
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

# Number of cards in Ashes of Outland
num_commons <- 104
num_rares <- 70
num_epics <- 46
num_legendaries <- 25
num_packs <- 1000

set_nums <- c(num_commons,
              num_rares,
              num_epics,
              num_legendaries,
              num_packs)

num_sims <- 100
sim_list <- rep(list(set_nums), num_sims)

# Set up parallel simulations
plan(multiprocess)

# microbenchmark(future_map(sim_list, function(.x) {
#   out <- openPack(
#     num.commons = .x[1],
#     num.rares = .x[2],
#     num.epics = .x[3],
#     num.legendaries = .x[4],
#     num.packs = .x[5]
#   )
#   return(out)
# },
# .progress = TRUE),
# map(sim_list, function(.x) {
#   out <- openPack(
#     num.commons = .x[1],
#     num.rares = .x[2],
#     num.epics = .x[3],
#     num.legendaries = .x[4],
#     num.packs = .x[5]
#   )
#   return(out)
# }), times = 10)

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
  geom_smooth(se = TRUE, colour = "green") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 300, 25),
                     labels = seq(0, 300, 25),
                     limits = c(0, 300)) +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = c(0.25, 0.5, 0.75, 1.0), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(25, 50, 75, 100), linetype = "dashed", alpha = 0.5) +
  labs(x = "Number of packs opened",
       y = "% of set obtained",
       title = "# of packs to open for a (nearly) complete set")


dust_plot <- ggplot(plot_out, aes(x = num_packs, y = total_dust)) +
  geom_line(aes(group = sim), alpha = 0.25, colour = "grey") +
  geom_smooth(se = TRUE, size = 1.05, colour = "green") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 15000, 2500),
                     labels = seq(0, 15000, 2500),
                     limits = c(0, 15000)) +
  scale_x_continuous(breaks = seq(0, 300, 25),
                     labels = seq(0, 300, 25),
                     limits = c(0, 300)) +
  labs(x = "Number of packs opened",
       y = "Dust ",
       title = "Estimated amount of dust from duplicates \nfor opening packs from a set")


p1 <- set_perc_plot / dust_plot

# Convert wide to long for facetting
plot_out_long <- plot_out %>% 
  select(sim, num_packs, common_perc:legendary_perc) %>% 
  pivot_longer(cols = common_perc:legendary_perc,
               names_to = c("type", ".value"),
               names_sep = "_") %>% 
  mutate(type = factor(type,
                       levels = c("common",
                                        "rare",
                                        "epic",
                                        "legendary"),
                       labels = c("Common",
                                  "Rare",
                                  "Epic",
                                  "Legendary")))


 
card_perc_plot <- ggplot(plot_out_long, aes(x = num_packs, y = perc)) +
  geom_line(aes(group = sim), colour = "grey", alpha = 0.35) +
  geom_smooth(aes(colour = type),
              size = 1.05,
              se = TRUE,
              method = "glm",
              method.args = list(family = "binomial")) +
  scale_colour_manual(values = c("Common" = "black",
                                 "Rare" = "blue",
                                 "Epic" = "purple",
                                 "Legendary" = "orange")) +
  geom_vline(xintercept = c(50, 100, 200, 300, 400, 500),
             linetype = "dashed",
             alpha = 0.5) +
  geom_hline(yintercept = c(0.25, 0.5, 0.75, 1.0),
             linetype = "dashed",
             alpha = 0.5) +
  facet_wrap(~ type) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Number of packs opened",
       y = "% of card type obtained",
       title = "# of packs to open to collect all cards \nfor a given rarity") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(limits = c(0, 500),
                     breaks = seq(0, 500, 100),
                     labels = seq(0, 500, 100))

p2 <- (p1) | card_perc_plot + 
  labs(caption = "\n Results based on a Hearthstone card ack opening simulator created by reddit user BagofAedeagi \n")

ggsave(filename = "simulation_summary.png",
       plot = p2,
       dpi = "retina",
       units = "in",
       height = 7,
       width = 10)

# Average number of packs to reach X% -------------------------------------

percs_test <- c(0.1, 0.25, 0.5, 0.75, 1.0)

perc_summary <- sapply(percs_test,
                       FUN = avgPacksForPerc,
                       list.out = out)
names(perc_summary) <- percs_test
perc_summary

avgPacksForPerc(out, 0.50)


# Check with reported %s --------------------------------------------------


# Should see about 0.05 epics and 0.01 prob of legendary

# Probability a single card will be of a given rarity
# Seems like I'm underestimating the amounts of commons and overestimating the others,
# although most of the over estimation is from rares
# From wiki it should be 71.6%, 22.85%, 4.43%, 1.10% for asecending rarity
out %>% 
  bind_rows(.id = "sim") %>% 
  group_by(sim) %>% 
  summarise(common_prob = sum(commons) / sum(commons, rares, epics, legendaries),
            rare_prob = sum(rares) / sum(commons, rares, epics, legendaries),
            epic_prob = sum(epics) / sum(commons, rares, epics, legendaries),
            legendary_prob = sum(legendaries) / sum(commons, rares, epics, legendaries)) %>% 
  summarise_all(mean, na.rm = T) %>% 
  select(-sim) %>% 
  rbind(., c(0.716, 0.2285, 0.043, 0.011)) %>% 
  as.data.frame(.) %>% 
  round(., 4) %>% 
  set_rownames(c("Obs", "Exp")) %>% 
  knitr::kable()

# Probability of getting at least one card of a given rarity in a single pack
# Rare prob is low (should be ~ 95.66) while epic and legendary are too high
# They should be 20.56 and 5.13 respectively
out %>% 
  bind_rows(.id = "sim") %>% 
  summarise(common_prob = sum(commons >= 1) / n(),
            rare_prob = sum(rares >= 1) / n(),
            epic_prob = sum(epics >= 1) / n(),
            legendary_prob = sum(legendaries >= 1) / n()) %>% 
  rbind(., c(0.9997, 0.9566, 0.2056, 0.0513)) %>% 
  as.data.frame(.) %>% 
  round(., 4) %>% 
  set_rownames(c("Obs", "Exp"))
