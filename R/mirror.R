#' Mirrors full universe
#'
#' Download and reupload all the files.
#'
#' @export
mirror_universe <- function(from, to = 'localhost'){
  pkglist <- jsonlite::fromJSON(sprintf('https://%s.r-universe.dev/api/packages', from))
  packages <- pkglist$Package[pkglist$`_registered`]
  for(pkg in packages){
    mirror_package(pkg, from, to)
  }
}

#' @export
mirror_package <- function(package, from = NULL, to = 'localhost'){
  endpoint <- if(length(from)){
    sprintf('https://%s.r-universe.dev/%s/files', from, package)
  } else {
    sprintf('https://cran.dev/%s/files', package)
  }
  files <- jsonlite::fromJSON(endpoint, simplifyVector = FALSE)
  for(info in files){
    upload_package(info, to)
  }
  return(files)
}

#' @export
upload_package <- function(info, universe){
  package <- info$Package
  version <- info$Version
  type <- info$`_type`
  sha <- info$`_fileid`
  path <- tempfile()
  on.exit(unlink(path))
  curl::curl_download(paste0('https://cdn.r-universe.dev/', sha), path)
  url <- sprintf('http://%s.r-universe.dev/api/packages/%s/%s/%s/%s', universe, package, version, type, sha)
  headers <- c("Builder-Status: OK",
                    'Builder-Registered: true',
                    paste('Builder-Registered:', info$`_registered`),
                    paste('Builder-Distro:', info$`_distro`),
                    paste('Builder-Status:', info$`_status`),
                    paste('Builder-Check:', info$`_check`),
                    paste('Builder-Buildurl:', info$`_buildurl`),
                    paste('Builder-Maintainer:', gzjson_b64(info$`_maintainer`)),
                    paste('Builder-Commit:', gzjson_b64(info$`_commit`)),
                    paste('Builder-Jobs:', gzjson_b64(info$`_jobs`)),
                    paste('Builder-Upstream:', info$`_upstream`))
  target <- if(length(info$Built$Platform) && nchar(info$Built$Platform)) {
    paste(info$Built$R, info$Built$Platform)
  } else if(length(info$Built$R)) {
    paste(info$Built$R, info[['_type']])
  } else {
    "source"
  }
  message(sprintf('OK: %s %s (%s)', package, version, target))
  res <- curl::curl_upload(path, url, verbose = FALSE, httpheader = headers, connect_to="::localhost:3000")
  out <- parse_res(res)
  stopifnot(out$Package == package, out$Version == version)
  return(out)
}

gzjson_b64 <- function(x){
  if(!length(x)) return(NULL)
  gsub("\n", "", jsonlite::as_gzjson_b64(x, auto_unbox=TRUE), fixed = TRUE)
}
