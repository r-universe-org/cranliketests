#' cranlike tools
#'
#' Utilities for testing the CRANlike API.
#'
#' @export
#' @rdname cranlike
#' @param pkgs a vector of packages
#' @param types one or more of src, win, mac
#' @param dependencies also sync hard dependencies of pkgs
#' @param user name of the user or organization to publish
sync_with_cran <- function(pkgs, types = c('src', 'win', 'mac', 'oldwin', 'oldmac'),
                           dependencies = FALSE, user = 'cran'){
  repos <- 'https://cloud.r-project.org'
  if(isTRUE(dependencies)){
    deps <- tools::package_dependencies(pkgs, recursive = TRUE)
    pkgs <- c(pkgs, unlist(unname(deps)))
  }
  types <- match.arg(types, several.ok = TRUE)
  out <- lapply(types, function(type){
    dir.create(destdir <- file.path(tempdir(), type), showWarnings = FALSE)
    files <- utils::download.packages(pkgs, destdir = destdir, type = crantype(type),
                               quiet = TRUE, contriburl = get_contrib_url(repos = repos, type = type))
    t(apply(files, 1, function(row){
      path <- row[2]
      package <- row[1]
      version <- sub(".*_(.*)\\.(tar.gz|tgz|zip)", "\\1", basename(path))
      print(system.time(put_package(path, package, version = version, type = sub("old", "", type), user = user)))
      c(path, package, version)
    }))
  })
  structure(out, names = types)
}

#' @export
#' @rdname cranlike
#' @param package name of a package
#' @param version optional string with version
#' @param type one of src, win, mac
delete_package <- function(package, version = NULL, type = c('src', 'win', 'mac'), user = 'cran'){
  h <- curl::new_handle(customrequest = 'DELETE')
  url <- sprintf("http://localhost:3000/%s/packages/%s", user, package);
  if(length(version)){
    url <- paste0(url, "/", version)
    if(length(type)){
      type <- match.arg(type)
      url <- paste0(url, "/", type)
    }
  }
  res <- curl::curl_fetch_memory(url, handle = h)
  out <- parse_res(res)
  stopifnot(out$Package == package)
  return(out)
}

#' @export
#' @rdname cranlike
post_package <- function(path, package, version, type = c('src', 'win', 'mac'), user = 'cran'){
  type <- match.arg(type)
  h <- curl::new_handle()
  buildfields = list('Builder-Status' = "OK", 'Builder-URL' = "http://localhost/test",
                     'Builder-Sysdeps' = 'libfoobar (1.2.3)')
  curl::handle_setform(h, file = curl::form_file(path), .list = buildfields)
  url <- sprintf('http://localhost:3000/%s/packages/%s/%s/%s', user, package, version, type)
  res <- curl::curl_fetch_memory(url, handle = h)
  out <- parse_res(res)
  stopifnot(out$Package == package, out$Version == version)
  return(out)
}

#' @export
#' @rdname cranlike
#' @param path full path to file to upload
put_package <- function(path, package, version, type = c('src', 'win', 'mac'), user = 'cran'){
  type <- match.arg(type)
  md5 <- unname(tools::md5sum(path))
  url <- sprintf('http://localhost:3000/%s/packages/%s/%s/%s/%s', user, package, version, type, md5)
  buildheaders <- c("Builder-Status: OK", paste0("Builder-URL: http://localhost/test/", type),
                    "Builder-Sysdeps: libfoobar (1.2.3)")
  res <- curl::curl_upload(path, url, verbose = FALSE, httpheader = buildheaders)
  out <- parse_res(res)
  stopifnot(out$Package == package, out$Version == version)
  return(out)
}

#' @export
#' @rdname cranlike
crantype <- function(type = c('src', 'win', 'mac', 'oldwin', 'oldmac')){
  switch(match.arg(type),
         src = 'source',
         win = 'win.binary',
         mac = 'mac.binary',
         oldwin = 'win.binary',
         oldmac = 'mac.binary.el-capitan')
}

#' @export
#' @rdname cranlike
db_packages <- function(){
  mongolite::mongo('packages', db = 'cranlike', url = 'mongodb://localhost')
}

#' @export
#' @rdname cranlike
db_files <- function(){
  mongolite::gridfs(db = 'cranlike', prefix = 'files', url = 'mongodb://localhost')
}

parse_res <- function(res){
  text <- rawToChar(res$content)
  if(res$status >= 400)
    stop(text)
  jsonlite::fromJSON(text)
}

get_contrib_url <- function(type, repos = 'https://cloud.r-project.org'){
  url <- contrib.url(repos = repos, type = crantype(type))
  if(grepl("old", type))
    url <- file.path(dirname(url), '3.6')
  return(url)
}
