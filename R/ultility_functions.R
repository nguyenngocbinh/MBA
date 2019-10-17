# library(arules) library(arulesViz) library(lubridate) library(DataExplorer) library(purrr) library(tidyr)
# library(readr) library(readxl) library(gridExtra) library(grid) library(ggplot2) library(tibble) library(dplyr)
# library(magrittr)

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))

# Convert data.frame to transaction form
#' @title Convert data.frame to transaction form
#' @description Convert data.frame to transaction form
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom dplyr mutate mutate_at select filter
#' @importFrom tidyr gather replace_na
#' @importFrom magrittr %>%
#' @importFrom readr write_csv
#' @importFrom arules read.transactions
#' @importFrom utils write.table
#' @export df_to_transaction
#' @param df data.frame
#' @param ph variable names or product name will be kept
#' @param id id variable, should be unique
#' @examples
#' data('product_holding')
#' ph_names <- c('credit_card','tktt','baohiem_aia')
#' transaction <- df_to_transaction(product_holding, ph = ph_names, id = 'customer_id')

df_to_transaction <- function(df, ph, id) {
    dat <- df %>% select(id, ph) %>% mutate_at(ph, replace_na, 0) %>% mutate_at(ph, function(x) ifelse(x != 0, 1,
        0))

    # Tinh tong so san pham
    dat_clean <- dat %>% mutate(nbr_products = rowSums(select(dat, ph)))

    # Chuyển dạng dữ liệu
    products_gather <- dat_clean %>% # Giữ những khách hàng có từ 1 sản phẩm trở lên
    filter(nbr_products > 1) %>% select(-nbr_products) %>% gather(key = "item", value = "value", -id) %>% filter(value !=
        0)

    # write data to read again to transaction data write_csv(products_gather, path = 'transaction.csv', col_names =
    # FALSE)

    # transaction <- read.transactions(file = 'transaction.csv', format = 'single', cols = c(1,2), sep=',',
    # rm.duplicates=TRUE)

    # 25-Sep-2019 ------------------------------ see detail: ?transactions-class transaction1 <-
    # as(split(products_gather[,'item'], products_gather[,id]), 'transactions') quicker than transaction1
    write.table(products_gather[, c(id, "item")], file = tmp <- file(), row.names = FALSE)
    transaction <- read.transactions(tmp, format = "single", header = TRUE, cols = c(id, "item"))
    close(tmp)

    return(transaction)
}

# Plot frequency of customer
#' @title Plot frequency of customer
#' @description Plot frequency of customer
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom dplyr mutate_if
#' @importFrom ggplot2 ggplot aes geom_label labs geom_col scale_x_reverse
#' @importFrom purrr quietly
#' @importFrom magrittr %>%
#' @importFrom arules summary
#' @importFrom extrafont loadfonts
#' @importFrom hrbrthemes theme_ipsum_rc scale_y_comma update_geom_font_defaults font_rc_light
#' @export plot_customer_frequency
#' @param transaction transaction data
#' @examples
#' data('product_holding')
#' ph_names <- c('credit_card','tktt','baohiem_aia')
#' transaction <- df_to_transaction(product_holding, ph = ph_names, id = 'customer_id')
#' plot_customer_frequency(transaction)

plot_customer_frequency <- function(transaction) {

    summary1 <- summary(transaction)

    len <- summary1@lengths %>% as.data.frame(stringsAsFactors = FALSE) %>% mutate_if(is.character, as.numeric)

    max_len <- max(len$sizes)
    # using for Roboto font
    loadfonts("pdf", quiet = TRUE)
    loadfonts("postscript", quiet = TRUE)
    if (.Platform$OS.type == "windows") {
        loadfonts("win", quiet = TRUE)
    }

    update_geom_font_defaults(family = font_rc_light)
    p <- len %>% ggplot(aes(x = sizes, y = Freq)) + geom_col(fill = "#208B88") + # scale_x_reverse(breaks = seq(max_len, 2, by = -1))+
    scale_x_continuous(breaks = seq(max_len, 2, by = -1)) + scale_y_comma() + # geom_label(aes(label= Freq))+
    labs(title = "Products holding", subtitle = "Calculate with customers have more than 2 products", x = "No of products",
        y = "No of customers") + theme_ipsum_rc(grid = "Y")

    return(p)

}


# Plot frequency of products
#' @title Plot frequency of products
#' @description Plot frequency of products
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom tibble rownames_to_column
#' @importFrom purrr quietly
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom arules itemFrequency
#' @importFrom hrbrthemes theme_ipsum_rc update_geom_font_defaults font_rc_light
#' @importFrom stats reorder
#' @importFrom utils tail
#' @importFrom scales comma
#' @importFrom extrafont loadfonts
#' @export plot_item_frequency
#' @param transaction transaction data
#' @param topN how many products will show (prioritize larger size)
#' @param type relative or absolute
#' @examples
#' data('product_holding')
#' ph_names <- c('credit_card','tktt','baohiem_aia')
#' transaction <- df_to_transaction(product_holding, ph = ph_names, id = 'customer_id')
#' plot_item_frequency(transaction)

plot_item_frequency <- function(transaction, topN = 20, type = "relative") {
    # using for Roboto font
    loadfonts("pdf", quiet = TRUE)
    loadfonts("postscript", quiet = TRUE)
    if (.Platform$OS.type == "windows") {
        loadfonts("win", quiet = TRUE)
    }
    update_geom_font_defaults(family = font_rc_light)
    p <- transaction %>% itemFrequency(type = type) %>% sort %>% tail(topN) %>% as.data.frame %>% rownames_to_column() %>%
        ggplot(aes(x = reorder(rowname, .), y = .)) + # ggplot(aes(x = reorder(rowname, -`.`), y =`.`)) +
    geom_col(fill = "#617A89") + # geom_text(aes(label=comma(`.`,accuracy = .01))) +
    coord_flip() + labs(x = NULL, y = NULL, title = paste("Item frequency,", type)) + theme_ipsum_rc(grid = "X")

    return(p)
}


# ============================================================================= Hàm tìm số lượng rules
# theo support và confidence
#' @title Grid search parameters support and confidence
#' @description Grid search suitable parameters (support, confidence) upon number of found rules
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom purrr pmap_dbl
#' @importFrom dplyr mutate
#' @importFrom magrittr %>% set_names
#' @importFrom arules apriori
#' @export grid_search_sup_conf
#' @param transaction transaction data
#' @examples
#' data('product_holding')
#' ph_names <- c('credit_card','tktt','baohiem_aia')
#' transaction <- df_to_transaction(product_holding, ph = ph_names, id = 'customer_id')
#' sup_conf_rules <- grid_search_sup_conf(transaction)
grid_search_sup_conf <- function(transaction) {
    support_levels <- c(0.1, 0.05, 0.01, 0.005)
    confidence_levels <- seq(from = 0.1, to = 0.9, by = 0.1)
    # Tao grid data.frame
    grid_param <- expand.grid(support_levels, confidence_levels) %>% set_names(c("supp", "confi"))

    # Viet ham de chay grid
    f_length_ph <- function(supp, confi) {
        rules_sup <- apriori(transaction, parameter = list(sup = supp, conf = confi, target = "rules"))
        n <- length(rules_sup)
        return(n)
    }

    # Chay ham grid
    grid_param_rules <- grid_param %>% mutate(rules = pmap_dbl(grid_param, f_length_ph))

    return(grid_param_rules)

}


# Vẽ số lượng rule và confidence, grid theo support
#' @title Plot no of rules by support and confidence
#' @description Using plot to grid search suitable parameters (support, confidence) upon number of found rules
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom dplyr filter
#' @import ggplot2
#' @importFrom purrr map
#' @importFrom gridExtra marrangeGrob
#' @importFrom magrittr %>%
#' @export plot_grid_sup_conf
#' @param df_sup_conf_rules data.frame include 3 columns: support, confidence, no of rules
#' @examples
#' data('product_holding')
#' ph_names <- c('credit_card','tktt','baohiem_aia')
#' transaction <- df_to_transaction(product_holding, ph = ph_names, id = 'customer_id')
#' sup_conf_rules <- grid_search_sup_conf(transaction)
#' plot_grid_sup_conf(sup_conf_rules)
plot_grid_sup_conf <- function(df_sup_conf_rules) {

    f_plot_ph_by_support <- function(su) {
        df_sup_conf_rules %>% filter(supp == su) %>% ggplot(aes(x = confi, y = rules)) + geom_point() + geom_line() +
            labs(title = paste0("Apriori with a support level of ", su), y = "Number of rules found", x = "Confidence level")
    }

    supp1 <- df_sup_conf_rules$supp %>% unique()
    nr <- ceiling(length(supp1)/2)

    # Tạo form graph grid.arrange(rectGrob(), rectGrob())

    # Vẽ các đồ thị
    all_graph <- map(supp1, f_plot_ph_by_support)

    # Ghép vào 1 graph chung
    ml <- marrangeGrob(all_graph, nrow = nr, ncol = 2)

    return(ml)
}


# Tìm kiếm rules theo minlen, maxlen
# Hàm xuất kết quả các rules theo các tham số đầu vào
#' @title Grid search rules upon input parameters (min_len, max_len, support, confidence)
#' @description Mine frequent itemsets, association rules or association hyperedges using the Apriori algorithm
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom magrittr %>%
#' @importFrom arules apriori is.redundant sort subset DATAFRAME
#' @export search_rules
#' @param transaction transaction data
#' @param min_len minimum products in rules (notes: lhs is null also consider is a product)
#' @param max_len maximum products in rules (notes: lhs is null also consider is a product)
#' @param sup support of rule
#' @param conf confidence of rule
#' @examples
#' data('product_holding')
#' ph_names <- c('credit_card','tktt','baohiem_aia')
#' transaction <- df_to_transaction(product_holding, ph = ph_names, id = 'customer_id')
#' ph_rules <- search_rules(transaction, 2, 2)

search_rules <- function(transaction, min_len = NULL, max_len = NULL, sup = 5e-04, conf = 0.1) {

    rules1 <- apriori(transaction, parameter = list(minlen = min_len, maxlen = max_len, sup = sup, conf = conf, target = "rules"))

    # Sắp xếp theo lift giảm dần (lift càng cao kết hợp giữa lhs và rhs càng chặt)
    rules_sorted <- sort(rules1, by = "lift")

    # Loai nhung rules thua (Pruning Redundant Rules) Redundant là những rules đã có thông tin nằm trong
    # Rules khác.
    rules_pruned <- rules_sorted[!is.redundant(rules_sorted)] %>% subset(lift > 1)
    # inspect(rules_pruned)

    return(rules_pruned)
}

# Tìm sản phẩm phù hợp với khách hàng thứ i
#' @title Match found rules with transaction
#' @description Match found rules with transaction.
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom magrittr %>%
#' @importFrom arules is.subset inspect sort
#' @importFrom purrr map_dfr
#' @export predict_rule
#' @param transaction transaction data
#' @param i
#' @param rules rules found
#' @param measure "lift" is default. Also choose: "confidence", "support" ...
#' @references https://stackoverflow.com/questions/40833925/applying-rules-generated-from-arules-in-r-to-new-transactions
#' @examples
#' data('product_holding')
#' ph_names <- c('credit_card','tktt','baohiem_aia', 'tktt', 'debit_card', 'sms', 'seapay')
#' transaction <- df_to_transaction(product_holding, ph = ph_names, id = 'customer_id')
#' ph_rules <- search_rules(transaction, 1, 5)
#' predict_rule(transaction, 2, ph_rules, "lift")

predict_rule <- function(transaction, rules, measure = "lift"){

  if(nrow(rules@quality) < 1){
    stop("Have no rules in input")
  }

  # sort rules by measure
  rules <- sort(rules, decreasing=TRUE, by=measure)

  # number of transaction
  n <- transaction %>% DATAFRAME() %>% nrow()

  df_match_all <- map_dfr(1:n,
  function(i){
    # find all rules whose lhs matches the training example
    rulesMatch <- is.subset(rules@lhs, transaction[i], sparse = FALSE)

    # subset all applicable rules
    applicable <- rules[rulesMatch==TRUE]

    # the first rule has the best measure since they are sorted
    # prediction <- applicable[1:5]
    prediction <- applicable[1]

    # Match vs transaction
    df_match <- DATAFRAME(transaction[i]) %>%
      #slice(rep(row_number(), 5)) %>%
      cbind(DATAFRAME(prediction))

    return(df_match)
  }
  ) # end loop

  return(df_match_all)
}


