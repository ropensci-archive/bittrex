context('C-Cex Public Calls')

test_that('the "coinnames" function works.', {
  resp = coinnames()
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "getbalancedistribution" function works.', {
  resp = getbalancedistribution("trump")
  expect_true(resp$success)
  expect_is(resp$result, 'numeric')
  Sys.sleep(2)
})

test_that('The "getmarkethistory" function works.', {
  resp = getmarkethistory("usd-btc")
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "getmarkets" function works.', {
  resp = getmarkets()
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

test_that('The "getorderbook" function works.', {
  resp = getorderbook("usd-btc")
  expect_true(resp$success)
  expect_is(resp$result, 'list')
  expect_is(resp$result[[1]], 'data.frame')
  expect_is(resp$result[[2]], 'data.frame')
  Sys.sleep(2)
})

test_that('The "pairs" function works.', {
  resp = pairs()
  expect_true(resp$success)
  expect_is(resp$result, 'character')
  Sys.sleep(2)
})

test_that('The "prices" function works.',{
  resp = prices()
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "volume" function works.', {
  resp = volume("btc")
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

