# Author : Hongkai Yu
# Date   : 6 June, 2020
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
                       X8 = col_double(),
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

camera_named <- camera %>%
  mutate(X15 = parse_datetime(X15, format ='%Y%m%d%H%M%S', na = c("200012:0000000", "20000."))) %>%
  rename(bid_id = X1, 
         item_id = X2, 
         product = X3, 
         model = X4, 
         trade_price = X5, 
         seller = X6, 
         X7 = X7, 
         X8 = X8, 
         bidder = X9, 
         bid_price = X10, 
         bid_time = X15,
         post_time = X16,
         complete_time = X17)
View(camera_named)

camera_named %>%
  group_by()
  summarise(X11_x = sum(), group_by(product))

camera_named %>%
  filter(X11 == 30) %>%
  select(seller)

camera_named %>%
  filter(seller == 'sam@xprt.net') %>%
  View()

