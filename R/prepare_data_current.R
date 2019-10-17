rm(list = ls())
source("R/ultility_functions.R")
library(rio)
library(magrittr)
library(tidyverse)
library(arules)
product <- import("inst/extdata/b_mba_current.csv")

product %<>% rename_all(tolower)
#==============================================================================
## Cleanning
#==============================================================================

# Loại những quan sát missing
product %<>% filter(!(is.na(age) | is.na(gender)))

product %<>% select(-customer)
# Thau chi
# Dữ liệu với cán bộ nhân viên SeAbank
product_in <- product %>% filter(bi_sea_employee == 1)

# Dữ liệu với khách hàng ngoài SeAbank
product_out <- product %>% filter(bi_sea_employee == 0)

ph <- c(
  #'loan_khac',
  'kd_ngan_han',
  'kd_trung_dai_han',
  #'sea_land',
  'sea_home',
  'sea_car',
  'thau_chi',
  'the_chap_khac',
  'tin_chap_mon',
  'sea_value',
  'internet_banking',
  'sms',
  'tktt',
  'tiet_kiem_ckh_online',
  'tiet_kiem_ckh_quay',
  #'deposit_khac',
  'tiet_kiem_kkh',
  'credit_card',
  'debit_card',
  'defer_card',
  'baohiem_aia',
  'seapay'
)

#library(market.basket.analysis)
trans_all <- df_to_transaction(product, ph, "customer_id")
trans_out1 <- df_to_transaction(product_out, ph, "customer_id")
trans_in1 <- df_to_transaction(product_in, ph, "customer_id")


#
ph_select <- ph %>% setdiff(c("sms"))

trans_out <- df_to_transaction(product_out, ph_select, "customer_id")
trans_in <- df_to_transaction(product_in, ph_select, "customer_id")

save(
  product,
  ph,
  ph_select,
  trans_all,
  trans_out1,
  trans_in1,
  trans_out,
  trans_in,
  file = "vignettes/product_current.RData"
)
