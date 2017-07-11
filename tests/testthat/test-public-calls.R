library(testthat)

context('Bittrex Public Calls')

test_that('The "bt_getmarkets" function works.', {
  resp = bt_getmarkets()
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "bt_api_check" function works.', {
  resp = bt_api_check()
  expect_is(resp, "logical")
  Sys.sleep(2)
})

test_that('The "bt_getmarketsummaries" function works.', {
  resp = bt_getmarketsummaries()
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "bt_getcurrencies" function works.', {
  resp = bt_getcurrencies()
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "bt_getticker" function works.', {
  resp = bt_getticker("btc-ltc")
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "bt_getmarketsummaries" function works.', {
  resp = bt_getmarketsummaries()
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "bt_getmarketsummary" function works.', {
  resp = bt_getmarketsummary("btc-ltc")
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "bt_getorderbook" function works.', {
  resp = bt_getorderbook("btc-ltc")
  expect_true(resp$success)
  expect_is(resp$result, 'list')
  expect_equal(names(resp$result), c("buy", "sell"))
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "bt_getmarkethistory" function works with a market.', {
  resp = bt_getmarkethistory("btc-ltc")
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "bt_getmarkethistory" function works without a valid market', {
  resp = bt_getmarkethistory("bunk")
  expect_false(resp$success)
  Sys.sleep(2)
})

