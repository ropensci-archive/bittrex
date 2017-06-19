library(testthat)

context('Bittrex Public Calls')

test_that('The "getmarkets" function works.', {
  resp = getmarkets()
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "bittrex_api_check" function works.', {
  resp = bittrex_api_check()
  expect_is(resp, "logical")
  Sys.sleep(2)
})

test_that('The "getmarketsummaries" function works.', {
  resp = getmarketsummaries()
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "getcurrencies" function works.', {
  resp = getcurrencies()
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "getticker" function works.', {
  resp = getticker("btc-ltc")
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "getmarketsummaries" function works.', {
  resp = getmarketsummaries()
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "getmarketsummary" function works.', {
  resp = getmarketsummary("btc-ltc")
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "getorderbook" function works.', {
  resp = getorderbook("btc-ltc")
  expect_true(resp$success)
  expect_is(resp$result, 'list')
  expect_equal(names(resp$result), c("buy", "sell"))
  expect_is(resp$result$buy, 'data.frame')
  expect_is(resp$result$sell, 'data.frame')
  Sys.sleep(2)
})

test_that('The "getmarkethistory" function works with a market.', {
  resp = getmarkethistory("btc-ltc")
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "getmarkethistory" function works without a valid market', {
  resp = getmarkethistory("bunk")
  expect_false(resp$success)
  Sys.sleep(2)
})

