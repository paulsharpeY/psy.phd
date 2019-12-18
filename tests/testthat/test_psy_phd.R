context('PhD')

fixture_dir <- '../fixtures'

rsvp         <- read.csv(paste(fixture_dir, 'rsvp.csv', sep='/'))
participants <- read.csv(paste(fixture_dir, 'p.csv', sep='/'))
test_that("rsvp_accuracy() works", {
  expect_is(rsvp_accuracy(rsvp, participants), 'data.frame')
  })
