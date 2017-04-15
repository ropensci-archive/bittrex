context("C-Cex Private Calls")

# A test account has been created to see if the private calls work.
# The key does not allow you to create orders, and you can'T withdraw.
# You may deposit :-)

api_key = "35A4395A6CAC941352D6CA8FD2BF6BFB"
secret_key = "0517E002A5EBBA00B4C1447FC3A32A27"

test_that('The "buylimit" function works.', {
  ccex_authenticate(api_key, secret_key)
  resp = buylimit("btc-usd", 100, 1)
  expect_false(resp$success)
  expect_equal(resp$message, 'ACCESS_DENIED')
  Sys.sleep(2)
})

test_that('The "cancel" function works.', {
  ccex_authenticate(api_key, secret_key)
  resp = cancel("1234")
  expect_false(resp$success)
  expect_equal(resp$message, 'ACCESS_DENIED')
  Sys.sleep(2)
})

test_that('The "getbalance" function works.', {
  ccex_authenticate(api_key, secret_key)
  resp = getbalance("btc")
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "getbalances" function works.', {
  ccex_authenticate(api_key, secret_key)
  resp = getbalances()
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

test_that('The "getopenorders" function works.', {
  ccex_authenticate(api_key, secret_key)
  resp = getopenorders()
  expect_true(resp$success)
  Sys.sleep(2)
})

test_that('The "getorder" function works.', {
  ccex_authenticate(api_key, secret_key)
  resp = getorder("1234")
  expect_false(resp$success)
  expect_equal(resp$message, 'UUID_INVALID')
  Sys.sleep(2)
})

test_that('The "getorderhistory" function works.', {
  ccex_authenticate(api_key, secret_key)
  resp = getorderhistory()
  expect_true(resp$success)
  Sys.sleep(2)
})

test_that('The "mytrades" function works.', {
  ccex_authenticate(api_key, secret_key)
  resp = mytrades("usd-btc")
  expect_true(resp$success)
  Sys.sleep(2)
})

test_that('The "sellimit" function works.', {
  ccex_authenticate(api_key, secret_key)
  resp = selllimit("usd-btc", 100, 1)
  expect_false(resp$success)
  expect_equal(resp$message, 'ACCESS_DENIED')
  Sys.sleep(2)
})

