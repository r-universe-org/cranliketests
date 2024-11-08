test_repo_hashes <- function(repos = 'https://bioc.r-universe.dev/bin/linux/noble/4.5'){
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  db <- available.packages(repos = repos, type = 'source')
  download.packages(row.names(db), db = db, destdir = tmp, type = 'source', repos = repos)
  sums1 <- sort(unname(tools::md5sum(list.files(tmp, full.names = TRUE))))
  sums2 <- sort(unname(db[,'MD5sum']))
  stopifnot(all.equal(sums1, sums2))
}

test_repo_mixed <- function(repos = 'https://bioc.r-universe.dev'){
  read_packages <- function(what){
    read.dcf(url(file.path(repos, what)))
  }
  df1 <- read_packages('bin/linux/noble/4.4/src/contrib/PACKAGES')
  stopifnot(all(df1[,'Platform'] == 'x86_64-pc-linux-gnu'))
  stopifnot(all(grepl("unix", df1[,'Built'])))

  df3 <- read_packages('bin/linux/koekjes/4.4/src/contrib/PACKAGES')
  df4 <- read_packages('bin/linux/noble/4.9/src/contrib/PACKAGES')
  df5 <- read_packages('src/contrib/PACKAGES')
  stopifnot(all.equal(df3, df4))
  stopifnot(all.equal(df3, df5))
}


