#' Prepare and encode raw fashion product data into a feature matrix
#'
#' Cleans a raw product data frame and encodes all relevant columns into a
#' numeric feature matrix suitable for analysis with
#' \code{\link{analyse}}. The function automatically detects price, volume,
#' color, material, gender, category and binary columns; it applies
#' log-scaling to price, min-max scaling to continuous variables and
#' frequency/ordinal encoding to categorical variables. Columns with zero
#' variance are dropped automatically.
#'
#' @param data A \code{data.frame} (or any object that can be converted to one) of raw 
#'   fashion product records. Every row is one product.
#' @param price_col Character. The name of the column containing product prices.
#'   If \code{NULL} (the default), the function searches column names for
#'   common price related keywords (\code{"price"}, \code{"cost"}, \code{"mrp"},
#'   \code{"inr"}, etc.). An error is shown if no match is found.
#' @param volume_col Character or \code{NULL}. The name of the column containing
#'   sales volume (and related). If \code{NULL} (the default) volume is
#'   not used. If given but not found, a warning is issued and is skipped.
#'
#' @return A named list with one element:
#'   \describe{
#'     \item{\code{features}}{A numeric matrix with one row per product and one
#'       column per encoded feature. Column names describe the feature (e.g.
#'       \code{"price"}, \code{"color"}, \code{"luxury"}).}
#'   }
#'
#' @details
#' The following encoding steps are applied in order:
#' \enumerate{
#'   \item \strong{Price}: non-numeric characters are stripped; missing values
#'     are imputed with the column median; log1p transform followed by
#'     min-max scaling to \eqn{[0,1]}.
#'   \item \strong{Volume} (optional): min-max scaled to \eqn{[0,1]}.
#'   \item \strong{Binary flags}: columns with exactly two unique values that
#'     map cleanly to TRUE/FALSE, yes/no, 1/0 are one-hot encoded.
#'   \item \strong{Product name}: the last word of each product name is used as
#'     a broad type label; frequency-encoded as the proportion of products
#'     sharing that type.
#'   \item \strong{Colour}: mapped to 11 colour families (black, white, grey,
#'     brown, blue, green, red, pink, purple, yellow, orange) and normalised
#'     to \eqn{[0,1]}.
#'   \item \strong{Gender / Category}: inferred from the main category code;
#'     encoded as 0 (female), 0.5 (unisex/unknown), or 1 (male). Category
#'     hierarchy depth is also min-max scaled.
#'   \item \strong{Premium flag}: derived from the word count of the description
#'     column (\eqn{>30} words → premium).
#'   \item \strong{Materials}: luxury score,
#'     blend count and main-fibre purity percentage are all normalised.
#' }
#'
#' @examples
#' \dontrun{
#' raw <- read.csv("fashion_products.csv", stringsAsFactors = FALSE)
#' prepped <- prep_data(raw)
#' dim(prepped$features)           # no of products × encoded features
#' colnames(prepped$features)      # encoded feature names 
#'
#' # Explicitly naming the price column
#' prepped <- prep_data(raw, price_col = "selling_price")
#'
#' # Including volume(if it is present in the dataset)
#' prepped <- prep_data(raw, price_col = "mrp", volume_col = "units_sold")
#' }
#'
#' @seealso \code{\link{analyse}}, \code{\link{recommend}}
#' @export
prep_data <- function(data, price_col = NULL, volume_col = NULL) {
  
  df         <- data.frame(data, stringsAsFactors = FALSE)
  n_products <- nrow(df)
  col_names  <- colnames(df)
  
  
  if (is.null(price_col)) {
    price_keywords <- c(
      "price", "selling price", "sale price", "list price", "mrp",
      "cost", "unit cost", "cost price", "purchase cost", "amount", "amt", "value",
      "charge", "fee", "payment", "rate", "unit price", "price per unit",
      "sp", "cp", "usd", "inr", "rs"
    )
    for (word in price_keywords) {
      idx <- grep(word, col_names, ignore.case = TRUE)
      if (length(idx) > 0) { price_col <- col_names[idx[1]]; break }
    }
    if (is.null(price_col)) stop("No price column found!")
  }
  
  price_raw    <- as.numeric(gsub("[^0-9.]", "", df[[price_col]]))
  price_raw[is.na(price_raw)] <- median(price_raw, na.rm = TRUE)
  price_log    <- log1p(price_raw)
  price_min    <- min(price_log, na.rm = TRUE)
  price_max    <- max(price_log, na.rm = TRUE)
  price_scaled <- (price_log - price_min) / (price_max - price_min)
  
  volume_scaled <- NULL
  
  #checking and working with volume column
  if (is.null(volume_col)) {
    volume_keywords <- c("sales.volume", "salesvolume", "sales_volume",
                         "volume", "vol", "units_sold", "units sold",
                         "sold", "quantity", "qty", "demand",
                         "sales", "orders", "order_count")
    for (word in volume_keywords) {
      idx <- grep(paste0("^", word, "$"), tolower(col_names), ignore.case = TRUE)
      if (length(idx) > 0) { volume_col <- col_names[idx[1]]; break }
    }
  }
  
  if (!is.null(volume_col)) {
    if (!volume_col %in% col_names) {
      warning(paste("Volume column", volume_col, "not found - skipping"))
    } else {
      volume_raw <- as.numeric(gsub("[^0-9.]", "", df[[volume_col]]))
      vol_min    <- min(volume_raw, na.rm = TRUE)
      vol_max    <- max(volume_raw, na.rm = TRUE)
      if (vol_max - vol_min == 0) {
        warning("Volume column has zero variance - skipping")
      } else {
        volume_scaled <- (volume_raw - vol_min) / (vol_max - vol_min)
      }
    }
  }
  
  #dropping statistically insignificant columns 
  always_drop <- c("Unnamed..0", "Unnamed..0.1", "index", "productId",
                   "product_id", "id", "url", "link",
                   "stockState", "stock_state",
                   "comingSoon", "coming_soon",
                   "isOnline",   "is_online",
                   "brand", "brandName")
  df        <- df[, !colnames(df) %in% intersect(colnames(df), always_drop), drop = FALSE]
  col_names <- colnames(df)
  
  #dropping zero variance columns
  drop_list <- c()
  for (col in col_names) {
    if (length(unique(df[[col]])) <= 1) drop_list <- c(drop_list, col)
  }
  df        <- df[, !colnames(df) %in% drop_list, drop = FALSE]
  col_names <- colnames(df)
  
  encoded_list <- list()
  
  #scaling price
  encoded_list$price <- as.numeric(price_scaled)
  
  #scaling volume(if present)
  if (!is.null(volume_scaled) && length(volume_scaled) == n_products)
    encoded_list$volume <- as.numeric(volume_scaled)
  
  #checking fir binary columns 
  for (col in col_names) {
    char_vals <- as.character(df[[col]])
    n_unique  <- length(unique(char_vals))
    if (n_unique == 2) {
      true_count  <- sum(tolower(char_vals) %in% c("true", "yes", "1", "y"))
      false_count <- sum(tolower(char_vals) %in% c("false", "no", "0", "n"))
      if ((true_count + false_count) / n_products > 0.8) {
        feat <- as.numeric(tolower(char_vals) %in% c("true", "yes", "1", "y"))
        if (sd(feat) > 0)
          encoded_list[[paste0(col, "_bin")]] <- feat
      }
    }
  }
  
  #frequency encoding the product name
  prod_cols <- c("productName", "product_name", "name", "article")
  for (pc in prod_cols) {
    if (pc %in% col_names) {
      prod_names <- tolower(df[[pc]])
      prod_names <- gsub("\\s+(in|with|&|-)\\s+\\S+$|\\s+\\(.*\\)$|\\s+\\d+\\s*(pack|piece|pk)$", "", prod_names)
      broad_types     <- sapply(strsplit(prod_names, " "), function(x) {
        x <- x[x != ""]
        if (length(x) == 0) return("unknown")
        return(x[length(x)])
      })
      type_freq_table   <- table(broad_types)
      freq_encoded      <- as.numeric(type_freq_table[broad_types] / n_products)
      freq_min          <- min(freq_encoded)
      freq_max          <- max(freq_encoded)
      encoded_list$product_type <- if (freq_max - freq_min == 0) rep(0, n_products) else
        (freq_encoded - freq_min) / (freq_max - freq_min)
      break
    }
  }
  
  #defining group of colours 
  color_cols <- c("colorName", "color_name", "colour", "color", "colour_name", "colourname", "color.name","colour.name")
  for (c in color_cols) {
    if (c %in% col_names) {
      colors <- tolower(df[[c]])
      colors[is.na(colors)] <- "unknown"
      if (mean(grepl("^[0-9a-f]{5,6}$", colors)) > 0.5) next
      color_family <- rep(11, length(colors))
      color_family[grepl("black|charcoal",              colors)] <- 0
      color_family[grepl("white|cream|off-white|ivory", colors)] <- 1
      color_family[grepl("gray|grey|silver|ash",        colors)] <- 2
      color_family[grepl("brown|beige|tan|camel|khaki", colors)] <- 3
      color_family[grepl("blue|navy|cobalt|indigo",     colors)] <- 4
      color_family[grepl("green|olive|emerald|forest",  colors)] <- 5
      color_family[grepl("red|burgundy|crimson|maroon", colors)] <- 6
      color_family[grepl("pink|rose|blush",             colors)] <- 7
      color_family[grepl("purple|violet|lavender",      colors)] <- 8
      color_family[grepl("yellow|gold|mustard",         colors)] <- 9
      color_family[grepl("orange|rust|terracotta",      colors)] <- 10
      encoded_list$color <- as.numeric(color_family) / 11
      break
    }
  }
  
  
  #checking the gender 
  cat_cols <- c("mainCatCode","catcode", "category", "cat_code","cat.code", "category_code", "gender", "category.code")
  for (c in cat_cols) {
    if (c %in% col_names) {
      cats   <- tolower(df[[c]])
      
      median_char  <- median(nchar(cats), na.rm = TRUE)
      pct_unique   <- length(unique(cats)) / length(cats)
      is_code_col  <- (median_char <= 30) && (pct_unique < 0.05)
      
      if (!is_code_col) next  
      
      gender <- rep(0.5, length(cats))
      gender[grepl("^men|^mens|\\bmen\\b", cats)]            <- 1
      gender[grepl("women|womens|ladies|girl|female", cats)] <- 0
      encoded_list$gender <- as.numeric(gender)
      cat_depth <- lengths(strsplit(cats, "_"))
      depth_min <- min(cat_depth); depth_max <- max(cat_depth)
      if (depth_max > depth_min)
        encoded_list$cat_depth <- (cat_depth - depth_min) / (depth_max - depth_min)
      break
    }
  }
  
  #checking details to create a premium feature 
  detail_cols <- c("details", "description", "desc", "detail")
  for (d in detail_cols) {
    if (d %in% col_names) {
      text       <- as.character(df[[d]])
      text[is.na(text)] <- ""
      word_count <- lengths(strsplit(text, "\\s+"))
      feat       <- as.numeric(word_count > 30)
      if (sd(feat) > 0) encoded_list$premium <- feat
      break
    }
  }
  
  #checking for materials column
  mat_cols <- c("materials", "material", "composition", "fabric")
  for (mat_col in mat_cols) {
    if (mat_col %in% col_names) {
      materials_text  <- tolower(as.character(df[[mat_col]]))
      luxury_score    <- numeric(n_products)
      blend_count     <- numeric(n_products)
      main_fiber_pt   <- numeric(n_products)
      premium_fabrics <- c("wool","silk","cotton","linen","cashmere",
                           "leather","alpaca","lyocell")
      
      for (i in seq_len(n_products)) {
        raw_text <- materials_text[i]
        if (is.na(raw_text) || raw_text == "") next
        just_nums <- gsub("[^0-9]", " ", raw_text)
        all_nums  <- as.numeric(unlist(strsplit(just_nums, "\\s+")))
        all_nums  <- all_nums[!is.na(all_nums) & all_nums <= 100 & all_nums > 0]
        if (length(all_nums) == 0 && nchar(raw_text) > 0) {
          main_fiber_pt[i] <- 50; blend_count[i] <- 1
        } else if (length(all_nums) > 0) {
          main_fiber_pt[i] <- max(all_nums)
          blend_count[i]   <- length(all_nums)
          temp_lux <- 0
          for (fab in premium_fabrics)
            if (grepl(fab, raw_text)) temp_lux <- temp_lux + all_nums[1]
          luxury_score[i] <- min(temp_lux, 100)
        }
      }
      
      encoded_list$luxury <- as.numeric(luxury_score > 0)
      blend_range  <- max(blend_count,   na.rm = TRUE) - min(blend_count,   na.rm = TRUE)
      purity_range <- max(main_fiber_pt, na.rm = TRUE) - min(main_fiber_pt, na.rm = TRUE)
      encoded_list$material_blend  <- if (blend_range  == 0) rep(0, n_products) else
        (blend_count   - min(blend_count,   na.rm = TRUE)) / blend_range
      encoded_list$material_purity <- if (purity_range == 0) rep(0, n_products) else
        (main_fiber_pt - min(main_fiber_pt, na.rm = TRUE)) / purity_range
      break
    }
  }
  
  #returning the encoded feature matrix ready for feeding it to the analyse function
  ready_to_use_df <- as.data.frame(encoded_list)
  ready_to_use_df <- ready_to_use_df[, sapply(ready_to_use_df, function(x) sd(x, na.rm = TRUE) > 0),
                                     drop = FALSE]
  
  if (ncol(ready_to_use_df) == 0)
    stop("prep_data produced 0 features. Check your data has a price column and non-constant columns.")
  
  return(list(features = as.matrix(ready_to_use_df)))
}
