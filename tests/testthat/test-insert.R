# copy packages from r-universe because they contain metadata
pkgs <- sort(c("dplyr", "curl", "ggplot2", "jsonlite"))
options(repos = c('https://jeroen.r-universe.dev', 'https://tidyverse.r-universe.dev'))
packages <- db_packages()
files <- db_files()
pkgfiles <- NULL

test_that("Sync with CRAN works works", {
  # Wipe the DB
  packages$drop()
  files$drop()

  # Upload some stuff from CRAN
  pkgfiles <<- sync_with_cran(pkgs, user = 'jeroen')

  # Test overwriting
  sync_with_cran(pkgs[1], user = 'jeroen')
  expect_equal(nrow(packages$find()), length(pkgs) * 5)
  expect_equal(nrow(files$find()), length(pkgs) * 5)

  # Test same files, different user
  sync_with_cran(pkgs[2], user = 'user2')
  expect_equal(nrow(packages$find()), (1+length(pkgs)) * 5)
  expect_equal(nrow(files$find()), length(pkgs) * 5)

  # Test posting failure
  post_failure(pkgs[2], '1.0.0', user = 'user2')
  post_failure(pkgs[2], '1.0.1', user = 'user2')
  expect_equal(nrow(packages$find()), (1+length(pkgs)) * 5 + 1)
  expect_equal(nrow(files$find()), length(pkgs) * 5)

  # A source package upload should remove the failure so we're back at the original count
  sync_with_cran(pkgs[2], user = 'user2')
  expect_equal(nrow(packages$find()), (1+length(pkgs)) * 5)
  expect_equal(nrow(files$find()), length(pkgs) * 5)
})

test_that("Error handling for PUT/POST", {
  for (submit in list(put_package, post_package)){
    for(type in c('src', 'win', 'mac')){
      path <- pkgfiles[[type]][1,1]
      name <- pkgfiles[[type]][1,2]
      version <- pkgfiles[[type]][1,3]
      if(type == 'src'){
        post_failure(package = name, version = version, user = 'user3')
      }
      out <- submit(path = path, package = name, version = version, type = type, user = 'user3')
      expect_equal(out$Package, name)
      expect_equal(out$Version, version)
      expect_equal(out$MD5sum, unname(tools::md5sum(path)))
      expect_true(all(c("_status", "_maintainer") %in% names(out)))
      if(type == 'src'){
        content_fields <- c("_assets", "_cranurl", "_exports", "_stars", "_contributions", "_help", "_readme", "_rundeps", "_vignettes")
        expect_true(all(content_fields %in% names(out)))
      }
      out <- delete_package(package = name, version = version, type = type, user = 'user3')
      expect_equal(out$Package, name)
      expect_equal(out$Version, version)
      wrongtype <- switch(type, win = 'mac', mac = 'src', src = 'win')
      expect_error(submit(path = path, package = name, version = version, type = wrongtype))
      expect_error(submit(path = path, package = name, version = '999', type = type))
      expect_error(submit(path = path, package = 'bla', version = version, type = type))
      expect_error(submit(path = system.file('DESCRIPTION'), package = name, version = version, type = type))
    }
  }
})

# 3 different types of repos
for(type in c('src', 'win', 'mac')){
  test_that(paste(type, 'repository'), {
    paths <- pkgfiles[[type]][,1]
    pkgs <- pkgfiles[[type]][,2]
    md5s <- unname(tools::md5sum(paths))
    hashes <- shasum(paths)

    # Compare data from DB
    query <- if(type == 'src'){
      sprintf('{"_type": "%s", "_user": "jeroen"}', type)
    } else {
      r_current <- substring(getRversion(), 0, 3)
      sprintf('{"_type": "%s", "_user": "jeroen", "Built.R" : {"$regex": "^%s", "$options" : "i"}}', type, r_current)
    }
    pkgdata <- packages$find(query)
    filedata <- files$find()
    expect_equal(sort(unique(pkgdata$Package)), pkgs)
    expect_equal(unique(sort(pkgdata$MD5sum)), sort(md5s))
    expect_true(all(hashes %in% filedata$id))

    # Compare data from cran-like API
    repo_src <- available.packages(repos = 'http://localhost:3000/jeroen', type = crantype(type))
    repo_df <- as.data.frame(repo_src, row.names = FALSE, stringsAsFactors = FALSE)
    expect_equal(sort(row.names(repo_src)), pkgs)
    expect_equal(unname(repo_src[pkgs,'MD5sum']), md5s)

    # Eror handling
    url <- contrib.url('http://localhost:3000/jeroen', crantype(type))
    df <- jsonlite::stream_in(curl::curl(url), verbose = FALSE)
    fields <- c("Package", "Version", "MD5sum", "NeedsCompilation")
    expect_equal(df[fields], repo_df[fields])

    # Check with correct file
    file <- basename(pkgfiles[[type]][1,1])
    name <- pkgfiles[[type]][1,2]
    req <- curl::curl_fetch_memory(paste0(url, "/", file))
    expect_equal(req$status, 200)

    # Wrong pkg name:
    wrongfile <- sub(name, 'wrong', file, fixed = TRUE)
    req <- curl::curl_fetch_memory(paste0(url, "/", wrongfile))
    expect_equal(req$status, 404)
    expect_match(rawToChar(req$content), 'Package not found')

    # Wrong pkg version:
    wrongfile <- sub("_", "_9", file, fixed = TRUE)
    req <- curl::curl_fetch_memory(paste0(url, "/", wrongfile))
    expect_equal(req$status, 404)
    expect_match(rawToChar(req$content), 'Package not found')

    # Wrong pkg extension:
    wrongfile <- paste0(file, '.bla')
    req <- curl::curl_fetch_memory(paste0(url, "/", wrongfile))
    expect_equal(req$status, 404)
  })
}

test_that("Installing packages", {
  # Installing source packages
  dst <- tempfile()
  dir.create(dst)
  install.packages(pkgs, dst, repos = 'http://localhost:3000/jeroen', type = 'source', quiet = TRUE)
  expect_true(all(file.exists(file.path(dst, pkgs))))

  # Installing binary packages
  dst <- tempfile()
  dir.create(dst)
  if(Sys.info()[['sysname']] == 'Darwin'){
    install.packages(pkgs, dst, repos = 'http://localhost:3000/jeroen', type = crantype('mac'), quiet = TRUE)
    expect_true(all(file.exists(file.path(dst, pkgs))))
  } else if(Sys.info()[['sysname']] == 'Windows'){
    install.packages(pkgs, dst, repos = 'http://localhost:3000/jeroen', type = crantype('win'), quiet = TRUE)
    expect_true(all(file.exists(file.path(dst, pkgs))))
  }
})

test_that("Deleting packages",{
  # Delete a package unique to jeroen
  delete_package(pkgs[1], user = 'jeroen')
  expect_equal(nrow(packages$find()), (1+length(pkgs[-1])) * 5)
  expect_equal(nrow(files$find()), length(pkgs[-1]) * 5)

  # Delete a package from jeroen (but user2 still has it)
  delete_package(pkgs[2], user = 'jeroen')
  expect_equal(nrow(packages$find()), (1+length(pkgs[-c(1:2)])) * 5)
  expect_equal(nrow(files$find()), length(pkgs[-1]) * 5)
})

test_that("APIs works",{
  pkgs <- jsonlite::fromJSON('http://localhost:3000/jeroen/api/ls')
  expect_equal(sort(pkgs), c("ggplot2", "jsonlite"))

  pkginfo <- jsonlite::fromJSON('http://localhost:3000/jeroen/api/packages/jsonlite')
  expect_s3_class(pkginfo[['_dependencies']], 'data.frame')
  expect_type(pkginfo[['_assets']], 'character')

})

test_that("Extra jsonlite files can be downloaded", {
  test_get <- function(api, type){
    req <- curl::curl_fetch_memory(paste0('http://localhost:3000/jeroen', api))
    expect_equal(req$status_code, 200)
    expect_equal(req$type, type)
    expect_gt(length(req$content), 100)
  }
  test_get('/jsonlite/doc/manual.html', 'text/html; charset=utf-8')
  test_get('/jsonlite/doc/page/fromJSON', 'text/html; charset=utf-8')
  test_get('/jsonlite/doc/readme', 'text/html; charset=utf-8')
  test_get('/jsonlite/doc/readme?highlight=hljs', 'text/html; charset=utf-8')
  test_get('/jsonlite/doc/README.md', 'text/markdown; charset=utf-8')
  test_get('/jsonlite/doc/json-mapping.pdf', 'application/pdf')
  test_get('/jsonlite/doc/json-apis.html', 'text/html; charset=utf-8')
  test_get('/jsonlite/citation.cff', 'text/plain; charset=utf-8')
  test_get('/jsonlite/citation.txt', 'text/plain; charset=utf-8')
  test_get('/jsonlite/citation.json', 'application/json; charset=utf-8')
  test_get('/jsonlite/citation.html', 'text/html; charset=utf-8')
  test_get('/jsonlite/citation.html', 'text/html; charset=utf-8')
  test_get('/feed.xml', "application/xml")
  test_get('/api/snapshot', "application/zip")

})

test_that("Badges work", {
  test_badge <- function(name){
    req <- curl::curl_fetch_memory(paste0('http://localhost:3000/jeroen/badges/', name))
    expect_equal(req$status_code, 200)
    expect_equal(req$type, 'image/svg+xml; charset=utf-8')
    expect_gt(length(req$content), 100)
  }
  test_badge('jsonlite')
  test_badge('vctrs')
  test_badge(':name')
  test_badge(':registry')
  test_badge(':datasets')
  test_badge(':articles')
  test_badge(':packages')
})

test_that("Datasets can be loaded work", {
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
