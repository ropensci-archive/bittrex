#' @title Client for the Bittrex Crypto-Currency Exchange
#' @name bittrex-package
#' @aliases bittrex-package bittrex
#' @docType package
#' @references \url{https://bittrex.com},
#'   \url{https://github.com/kaneplusplus/bittrex}
#' @description
#' This software is in no way affiliated, endorsed, or approved by
#' the Bittrex crypto-currency exchange or any of its affiliates. It comes with
#' absolutely no warranty and should not be used in actual trading
#' unless the user can read and understand the source and knows what they are
#' doing.
#'
#' The `bittrex` package is an R implementation of the REST interface used
#' by the Bittrex crypto-currency exchange (\url{https://bittrex.com}). It
#' provides functions for all endpoints currently (as of May 16, 2017)
#' supported by the exchange. This includes the ability to retrieve price,
#' volume, and orderbook information as well as the ability to trade
#' crypto-currencies.
#'
#' Calls to the exchange are categorized as either public, which includes
#' requests for price, volume, and order book information, and private, which
#' includes all requests requiring an account including placing buy or sell
#' orders. Public calls can be used immediately after installing the package.
#' Private calls require creating an account at \url{https://bittrex.com} and
#' creating API and secret keys with appropriate permissions.
#'
#' Private calls retrieve the API and secret key using the BITTREX_API_KEY and
#' BITTREX_SECRET_KEY environment variables. These may be set by the user
#' before opening the R session or, they can be set using the
#' [bt_authenticate()] function.
#'
#' @section Public Function Calls:
#' \itemize{
#' \item{[bt_api_check()]  }{check if the bittrex REST API is working}
#' \item{[bt_getcurrencies()]  }{all supported currencies at Bittrex along with other meta data}
#' \item{[bt_getmarkethistory()]  }{the latest trades that have occurred for a specified market}
#' \item{[bt_getmarkets()]  }{the open and available trading markets at Bittrex along with other meta data}
#' \item{[bt_getmarketsummary()]  }{the last 24 hours' summary of a specific market}
#' \item{[bt_getmarketsummaries()]  }{the last 24 hours' summary of all active markets}
#' \item{[bt_getorderbook()]  }{the orderbook for a given market}
#' \item{[bt_getticker()]  }{the current tick values for a market}
#' }
#'
#' @section Private Function Calls:
#' \itemize{
#' \item{[bt_authenticate()]  }{provide user authentication data}
#' \item{[bt_buy()]  }{place a buy limit order}
#' \item{[bt_cancel()]  }{cancel a buy or sell order}
#' \item{[bt_getbalances()]  }{account balances for all currencies}
#' \item{[bt_getbalance()]  }{account balance for a specified currency}
#' \item{[bt_getdepositaddress()]  }{retrieve or generate an address for a specified
#'  currency}
#' \item{[bt_getdeposithistory()]  }{retrieve your deposit history}
#' \item{[bt_getopenorders()]  }{retrieve data for all open orders}
#' \item{[bt_getorder()]  }{retrieve a single order by uuid}
#' \item{[bt_getorderhistory()]  }{recent order history for an account }
#' \item{[bt_getwithdrawalhistory()]  }{retrieve your withdrawal history}
#' \item{[bt_sell()]  }{place a sell limit order}
#' \item{[bt_withdraw()]  }{withdraw funds from your account}
#' }
NULL
