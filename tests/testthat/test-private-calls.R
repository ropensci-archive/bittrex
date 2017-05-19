context("Bittrex Private Calls")

# A test account has been created to see if the private calls work.
# The key does not allow you to create orders, and you can't withdraw.
# You may deposit :-)

api_key = "bunk"
secret_key = "bunk"

test_that('The "buy" and "cancel" functions work.', {
  bittrex_authenticate(api_key, secret_key)
  resp = buy("btc-ltc", 100, 0.0001)
  cancel(resp$result$uuid)
  Sys.sleep(2)
})

test_that('The "sell" and "cancel" function work.', {
  bittrex_authenticate(api_key, secret_key)
  resp = cancel("1234")
  expect_false(resp$success)
  expect_equal(resp$message, 'ACCESS_DENIED')
  Sys.sleep(2)
})

test_that('The "getbalances" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getbalances()
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})

# START HERE.
test_that('The "getbalance" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getbalance("btc")
  expect_true(resp$success)
  expect_is(resp$result, 'data.frame')
  Sys.sleep(2)
})


test_that('The "getopenorders" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getopenorders()
  expect_true(resp$success)
  Sys.sleep(2)
})

test_that('The "getorder" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getorder("1234")
  expect_false(resp$success)
  expect_equal(resp$message, 'UUID_INVALID')
  Sys.sleep(2)
})

test_that('The "getorderhistory" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getorderhistory()
  expect_true(resp$success)
  Sys.sleep(2)
})

test_that('The "mytrades" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = mytrades("usd-btc")
  expect_true(resp$success)
  Sys.sleep(2)
})

test_that('The "sellimit" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = selllimit("usd-btc", 100, 1)
  expect_false(resp$success)
  expect_equal(resp$message, 'ACCESS_DENIED')
  Sys.sleep(2)
})

