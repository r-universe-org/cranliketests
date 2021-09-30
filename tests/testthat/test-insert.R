pkgfiles <- NULL
pkgs <- sort(c("Rcpp", "curl", "jose", "openssl"))
packages <- db_packages()
files <- db_files()

test_that("Sync with CRAN works works", {
  # Wipe the DB
  packages$drop()
  files$drop()

  # Upload some stuff from CRAN
  pkgfiles <<- sync_with_cran(pkgs, user = 'user1')

  # Test overwriting
  sync_with_cran(pkgs[1], user = 'user1')
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
      expect_true(all(c("status", "url", "sysdeps") %in% names(out[['_builder']])))
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
    hashes <- unname(tools::md5sum(paths))

    # Compare data from DB
    query <- if(type == 'src'){
      sprintf('{"_type": "%s", "_user": "user1"}', type)
    } else {
      r_current <- substring(getRversion(), 0, 3)
      sprintf('{"_type": "%s", "_user": "user1", "Built.R" : {"$regex": "^%s", "$options" : "i"}}', type, r_current)
    }
    pkgdata <- packages$find(query)
    filedata <- files$find()
    expect_equal(sort(unique(pkgdata$Package)), pkgs)
    expect_equal(unique(sort(pkgdata$MD5sum)), sort(hashes))
    expect_true(all(hashes %in% filedata$id))

    # Compare data from cran-like API
    repo_src <- available.packages(repos = 'http://localhost:3000/user1', type = crantype(type))
    repo_df <- as.data.frame(repo_src, row.names = FALSE, stringsAsFactors = FALSE)
    expect_equal(sort(row.names(repo_src)), pkgs)
    expect_equal(unname(repo_src[pkgs,'MD5sum']), hashes)

    # Eror handling
    url <- contrib.url('http://localhost:3000/user1', crantype(type))
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
  install.packages(pkgs, dst, repos = 'http://localhost:3000/user1', type = 'source', quiet = TRUE)
  expect_true(all(file.exists(file.path(dst, pkgs))))

  # Installing binary packages
  dst <- tempfile()
  dir.create(dst)
  if(Sys.info()[['sysname']] == 'Darwin'){
    install.packages(pkgs, dst, repos = 'http://localhost:3000/user1', type = crantype('mac'), quiet = TRUE)
    expect_true(all(file.exists(file.path(dst, pkgs))))
  } else if(Sys.info()[['sysname']] == 'Windows'){
    install.packages(pkgs, dst, repos = 'http://localhost:3000/user1', type = crantype('win'), quiet = TRUE)
    expect_true(all(file.exists(file.path(dst, pkgs))))
  }
})

test_that("Deleting packages",{
  # Delete a package unique to user1
  delete_package(pkgs[1], user = 'user1')
  expect_equal(nrow(packages$find()), (1+length(pkgs[-1])) * 5)
  expect_equal(nrow(files$find()), length(pkgs[-1]) * 5)

  # Delete a package from user1 (but user2 still has it)
  delete_package(pkgs[2], user = 'user1')
  expect_equal(nrow(packages$find()), (1+length(pkgs[-c(1:2)])) * 5)
  expect_equal(nrow(files$find()), length(pkgs[-1]) * 5)
})
