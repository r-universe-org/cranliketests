# copy packages from r-universe because they contain metadata
pkgs <- sort(c("dplyr", "curl", "ggplot2", "jsonlite"))
packages <- db_packages()
files <- db_files()

test_that("CRUD operations", {
  # Wipe the DB
  #packages$drop()
  #files$drop()

  # Upload some stuff from CRAN
  pkgfiles <<- lapply(pkgs, mirror_package, to = 'localhost')
  total <- sum(sapply(pkgfiles, length))
  expect_equal(packages$count(), total)
  expect_equal(nrow(files$find()), total)

  # Test posting failure
  post_failure('curl', version = '9.8000', user = 'localhost')
  post_failure('curl', version = '9.9000', user = 'localhost')
  expect_equal(packages$count(), total + 1)
  expect_equal(nrow(files$find()), total)

  # Overwrite earlier failure
  mirror_package('curl', to = 'localhost')
  expect_equal(packages$count(), total)
  expect_equal(nrow(files$find()), total)

  # Test same files, different user
  copies <- mirror_package('curl', to = 'user2')
  expect_equal(packages$count(), total + length(copies))
  expect_equal(nrow(files$find()), total)

  # The copy
  delete_package('curl', user = 'user2')
  expect_equal(packages$count(), total)
  expect_equal(nrow(files$find()), total)

  # Delete completely
  out <- delete_package('curl', user = 'localhost')
  expect_equal(packages$count(), total - nrow(out))
  expect_equal(nrow(files$find()), total - nrow(out))

  # Restore again
  mirror_package('curl', to = 'localhost')
  expect_equal(packages$count(), total)
  expect_equal(nrow(files$find()), total)
})

test_that("APIs works",{
  # Quick ls api
  apipkgs <- jsonlite::fromJSON('http://localhost:3000/api/ls')
  expect_setequal(pkgs, apipkgs)

  # General packages API
  apipkgs <- jsonlite::fromJSON('http://localhost:3000/api/packages')
  expect_setequal(pkgs, apipkgs$Package)
  lapply(apipkgs[['_assets']], function(assets){
    expect_in(c("extra/citation.html", "extra/NEWS.html", "manual.pdf"), assets)
  })

  # Sysdeps API
  apisysdeps <- jsonlite::fromJSON('http://localhost:3000/api/sysdeps')
  expect_in('curl', apisysdeps$library)

  # Maintainers API
  maintainers <- jsonlite::fromJSON('http://localhost:3000/api/maintainers')
  expect_in(c("jeroen", "hadley"), maintainers$login)

})

test_that("XML feed", {
  doc <- xml2::read_xml('http://localhost:3000/feed.xml')
  xmlpkgs <- xml2::xml_find_all(doc, '//item//r:package') |> xml2::xml_text()
  expect_setequal(pkgs, xmlpkgs)
})

test_that("Test some errors", {

  # Test that we have all the info
  info <- jsonlite::fromJSON('https://cran.dev/curl/json', simplifyVector = FALSE)
  out <- upload_package(info, 'jeroen')
  content_fields <- c("_status", "_maintainer", "_assets", "_cranurl", "_exports", "_stars", "_contributors", "_help", "_readme", "_rundeps", "_vignettes")
  expect_in(content_fields, names(out))

  # Test some required fields
  expect_error(upload_package(modifyList(info, list('_jobs' = NULL)), 'jeroen'), 'jobs')
  expect_error(upload_package(modifyList(info, list('_commit' = NULL)), 'jeroen'), 'commit')
  expect_error(upload_package(modifyList(info, list('_maintainer' = NULL)), 'jeroen'), 'maintainer')
  expect_error(upload_package(modifyList(info, list('Package' = 'yolo')), 'jeroen'), 'package')
  expect_error(upload_package(modifyList(info, list('Version' = 'foo')), 'jeroen'), 'version')
})

test_that("Installing packages", {
  # So we dont need dependencies
  smallpkgs <- c("curl", "jsonlite")

  # Installing binary packages
  dst <- tempfile()
  dir.create(dst)
  install.packages(smallpkgs, dst, repos = 'http://localhost:3000', quiet = TRUE)
  expect_true(all(file.exists(file.path(dst, smallpkgs))))

  # Test installing source packages
  dst <- tempfile()
  dir.create(dst)
  install.packages(smallpkgs, dst, repos = 'http://localhost:3000', type = 'source', quiet = TRUE)
  expect_true(all(file.exists(file.path(dst, smallpkgs))))

})


test_that("Extra jsonlite files can be downloaded", {
  test_get <- function(api, type){
    req <- curl::curl_fetch_memory(paste0('http://localhost:3000', api))
    expect_equal(req$status_code, 200)
    expect_equal(req$type, type)
    expect_gt(length(req$content), 100)
  }
  test_get('/jsonlite/doc/manual.html', 'text/html; charset=utf-8')
  test_get('/jsonlite/doc/page/fromJSON', 'text/html; charset=utf-8')
  test_get('/jsonlite/doc/readme', 'text/html; charset=utf-8')
  test_get('/jsonlite/doc/readme?highlight=hljs', 'text/html; charset=utf-8')
  test_get('/jsonlite/doc/README.md', 'text/plain; charset=utf-8')
  test_get('/jsonlite/doc/json-mapping.pdf', 'application/pdf')
  test_get('/jsonlite/doc/json-apis.html', 'text/html; charset=utf-8')
  test_get('/jsonlite/citation.cff', 'text/plain; charset=utf-8')
  test_get('/jsonlite/citation.txt', 'text/plain; charset=utf-8')
  test_get('/jsonlite/citation.json', 'application/json; charset=utf-8')
  test_get('/jsonlite/citation.html', 'text/html; charset=utf-8')
  test_get('/jsonlite/citation.html', 'text/html; charset=utf-8')
  test_get('/feed.xml', "application/xml; charset=utf-8")
  #test_get('/api/snapshot', "application/zip")
})

test_that("Badges work", {
  test_badge <- function(name){
    req <- curl::curl_fetch_memory(paste0('http://localhost:3000/badges/', name))
    expect_equal(req$status_code, 200)
    expect_equal(req$type, 'image/svg+xml; charset=utf-8')
    expect_gt(length(req$content), 100)
  }
  test_badge('jsonlite')
  test_badge('curl')
  test_badge(':name')
  test_badge(':datasets')
  test_badge(':articles')
  test_badge(':packages')
})

test_that("Datasets can be loaded work", {
  skip_if(TRUE)
  test_get <- function(api, type){
    req <- curl::curl_fetch_memory(paste0('http://localhost:3000/jeroen', api))
    expect_equal(req$status_code, 200)
    expect_equal(req$type, type)
    expect_gt(length(req$content), 100)
  }

  test_get("/ggplot2/data/diamonds/xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  test_get("/ggplot2/data/diamonds/csv", "text/csv; charset=utf-8")
  test_get("/ggplot2/data/diamonds/json", "application/json; charset=utf-8")
  test_get("/ggplot2/data/diamonds/ndjson", "text/plain; charset=utf-8")
  test_get("/ggplot2/data/diamonds/rds", "application/octet-stream")
  test_get("/ggplot2/data/diamonds/rda", "application/octet-stream")
})
