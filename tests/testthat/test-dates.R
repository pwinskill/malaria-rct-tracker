test_that("parse_pubmed_date handles month names, numbers, seasons and gaps", {
  expect_equal(parse_pubmed_date("2024", "Mar", "5"), "2024-03-05")
  expect_equal(parse_pubmed_date("2024", "03", ""), "2024-03-01")
  expect_equal(parse_pubmed_date("2024", "3", "7"), "2024-03-07")
  expect_equal(parse_pubmed_date("2024", "Winter", ""), "2024-01-01")  # season -> Jan
  expect_equal(parse_pubmed_date("2024", "", ""), "2024-01-01")
  expect_equal(parse_pubmed_date("", "Mar", "5"), "")                  # no year -> empty
})
