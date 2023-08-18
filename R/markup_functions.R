library(dplyr)
library(fixest)
library(collapse)
library(magrittr)

# TODO: deflate input prices if this is not already done.
# TODO: I get out one number for the output elasticity. Should it not be more?
# TODO: What are the wage fixed effects? Are they just time fixed effects?
# TODO: Why are the two revenue measures different?
    # Theory: One is IVP specific revenue, the other contains data not in IVP

get_markup <- function(o_elas, revenue, var_cost) {
    #' Compute markup for each firm-year.
    #' This follows the production approach to markups from Hall (1986, 1988).
    #' The variable input is assumed to be competitive, hence t-subscript on W.
    #' @param o_elas : output elasticity of the variable input.
    #' @param revenue : total revenue, P_(i,t) * Y_(i,t).
    #' @param var_cost : variable input total expenditure, W_t * V_(i,t).
    #' @return a vector of markups.
    return(o_elas * revenue / var_cost)
}

get_output_elasticity <- function(df_fy) {
    #' Compute the output elasticity of a competitive variable input
    #' This follows the procedure in De Ridder, Grassi, Morzenti (2022) sec. 3.
    #' TODO: adapt to case where p is not present.
    #' @param df : dataframe unit of observation = (firm, year).
    #' This dataframe should contain the following columns:
    #' - firm_id: Firm identifier.
    #' - year: Year.
    #' - p: Price.
    #' - s: market share.
    #' - w: price of the variable input. TODO how to handle this? Just subtract a mean?
    #' - v: quantity of the variable input.
    #' - v_lag: quantity of the variable input in the previous period.
    #' - y: output in physical units.
    # These variables all need to be log deviations from their time-mean.

    # First, quantity is purged from measurement error in a regression
        # TODO: this can be a more flexible prediction.
    m1 <- feols(y ~ v + p + s | w, data = df_fy)
    df_fy$true_output <- predict(m1)
    # Second, the fitted values of output, are then used to construct mgf
    m_iv <- feols(true_output ~ 0 | v ~ v_lag,
        panel.id = ~ firm_id + year,
        data = df_fy)
    output_elasticity <- m_iv[["coefficients"]][[1]]
    return(output_elasticity)
}

log_dev_from_mean <- function(var) {
    #' Compute log deviation from mean of a variable.
    #' This follows the procedure in De Ridder, Grassi, Morzenti (2022)
    #' sec. 3.1. footnote 10.
    #' @param var : (str) variable to be demeaned.
    #' @return a variable : log deviation from mean of var.
    return(log(var) - mean(log(var)))
}

get_market_share <- function(df, rev_var, sector) {
    #' Compute market share for each firm-year.
    #' @param df : dataframe unit of observation = (firm, year).
    #' This dataframe should contain the following columns:
    #' - firm_id: Firm identifier.
    #' - year: Year.
    #' - rev_var: Revenue variable.
    #' - sector: Sector variable.
    #' @return a vector of market shares.
    industry_revenue <- df |>
        fgroup_by(get(sector), year) |>
        fselect(rev_var) |>
        fsum()
    names(industry_revenue) <- c(sector, "year", "industry_revenue")
    df <- merge(df, industry_revenue)
    shares <- df$revenue / df$industry_revenue
    return(shares)
}

standardize_price <- function(df_fgy, good) {
    #' Standardize prices following De Ridder, Grassi, Morzenti (2022).
    #' The Hitchhiker’s Guide to Markup Estimation, section 4 "Data".
    #' Definition of a good/product: Combination of a code and unit of account.
    #' @param: df_fgy. Dataframe with firm-year-product observations.
    #' This dataframe should contain the following columns:
    #' - firm_id: Firm identifier.
    #' - year: Year.
    #' - revenue: Revenue.
    #' - q: Quantity.
    #' - p: Price.
    #' @param: good. Standardize prices at the level defined by good.
    #' @return: df_fy, Dataframe with standardized prices id by firm and year.
    #'
    good_mean_price <- df_fgy |>
        fgroup_by(get(good), year) |>
        fselect(p) |>
        fmean()

    names(good_mean_price) <- c(good, "year", "good_mean_price")

    df_fgy <- merge(df_fgy, good_mean_price)

    df_fgy$p <- df_fgy$p / df_fgy$good_mean_price

    firm_revenue <- df_fgy |>
        fgroup_by(firm_id, year) |>
        fselect(revenue) |>
        fsum()
    names(firm_revenue) <- c("firm_id", "year", "firm_revenue")

    df_fy <- df_fgy |>
        fgroup_by(firm_id, year) |>
        fselect(p) |>
        fmean(w = df_fgy$revenue)

    df_fy <- merge(df_fy, firm_revenue, by = c("firm_id", "year"))

    df_fy$q <- df_fy$firm_revenue / df_fy$p

    return(df_fy)
}

drop_weird_obs <- function(df_fgy) {
    #' Drop observations with weird values.
    #' @param df_fgy : dataframe unit of observation = (firm, good, year).
    #' This dataframe should contain the following columns:
    #' - q: Quantity.
    #' - p: Price.
    #' @return a dataframe with weird observations dropped.
    df_fgy <- df_fgy |>
        dplyr::filter(p > 0 & !is.na(p)) |>
        dplyr::filter(q > 0 & !is.na(q))
    return(df_fgy)
}

winsorize_vars <- function(.df, .var_list, ...) {
    #' Winsorize variables.
    #' @param .df : dataframe.
    #' @param .var_list : list of variables to winsorize.
    #' @param ... : grouping variables. dRGM (2022) group by 2 digit industry.
    #' @return a dataframe with winsorized variables.
    df <- .df  |> group_by(...) |>
        mutate(
            across(all_of(.var_list),
                ~DescTools::Winsorize(.x, probs = c(0.01, 0.99))
                )
            ) |>
        ungroup()
    return(df)
}
