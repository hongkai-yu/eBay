# Author : Hongkai Yu
# Date   : 8 June, 2020
# install.packages("tidyverse")
library(tidyverse)
camera <- read_tsv("camera.txt", col_names = FALSE, col_types = 
                     cols(
                       X1 = col_integer(),
                       X2 = col_integer(),
                       X3 = col_character(),
                       X4 = col_character(),
                       X5 = col_double(),
                       X6 = col_character(),
                       X7 = col_double(),
                       X8 = col_integer(),
                       X9 = col_character(),
                       X10 = col_double(),
                       X11 = col_integer(),
                       X12 = col_logical(),
                       X13 = col_logical(),
                       X14 = col_logical(),
                       # X15 = col_datetime(format = '%Y%m%d%H%M%S'),
                       X15 = col_character(), # some error for parsing as datetime 
                       X16 = col_datetime(format = '%Y%m%d%H%M%S'),
                       X17 = col_datetime(format = '%Y%m%d%H%M%S')
                     ))

camera_named <- camera %>% # names with "_maybe" are guesses 
  mutate(X15 = parse_datetime(X15, format ='%Y%m%d%H%M%S', na = c("200012:0000000", "20000."))) %>%
  rename(bid_id = X1, 
         item_id = X2, 
         product = X3, 
         model = X4, 
         trade_price_maybe = X5,    # related to the highest bid, but not always the same
         seller = X6, 
         reserve_price = X7, 
         X8 = X8, 
         bidder = X9, 
         bid_price = X10, 
         X11 = X11,
         X12 = X12,
         X13 = X13,
         X14 = X14,
         bid_time = X15,
         post_time = X16,
         complete_time = X17)
View(camera_named)

# X1, X2, X3, X4, X6, X9, X10 are clear

# --- evidence related to X15, X16, X17 (datetime) --- #
table(camera_named$bid_time > camera_named$post_time) # always true
table(camera_named$bid_time > camera_named$complete_time) # always false

# --- evidence related to X5, X7 (prices) --- #
table(camera_named$bid_price >= camera_named$reserve_price) # not always true, but all falses are due to invalid bids
camera_named %>% filter(bid_price < reserve_price) %>% View

summary <- camera_named %>%
  group_by(product) %>%
  summarise(highest_bid = max(bid_price),
            trade_price = mean(trade_price_maybe))
table(summary$trade_price == summary$highest_bid) # not always true
summary %>% filter(trade_price != highest_bid) %>% View 
# some are zeros (could be due to invalid bids), but for other inequalities it could be higher or lower (so they aren't due to second auction mechanism), which are confusing

  
# --- evidence related to X11 - X14 (logical values) --- #
# it seems that X11, X12, X13, X14 are attributes to the product, since the values are the same for each product.

table(camera_named$X12) 
table(camera_named$X13) 
table(camera_named$X14) 

table(camera_named$X11) # not only has 0 and 1, can be as high as 30

camera_named %>% # investigate the 30 see if there is anything weird about it
  filter(X11 == 30) %>%
  select(seller)  # sam@xprt.net

camera_named %>%
  filter(seller == 'sam@xprt.net') %>%
  View # nothing very peculiar about it
