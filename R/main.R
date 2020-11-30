
#' @describeIn read_boulder parses strings in the Boulder-IO format, with each string representing a line.
#'
#' @export
parse_boulder <- function(strings, single=FALSE) {
  records <- list()
  current <- list()
  for (line in strings) {
    if (line == "=") {
      # end of record; push it
      records[[length(records)+1]] <- current
      current <- list()
      next
    }

    result <- stringr::str_split(line, stringr::fixed("="))
    tag <- result[[1]][1]
    value <- result[[1]][2]

    if (tag %in% names(current)) {
      current[[tag]] <- c(current[[tag]], value)
    } else {
      current[[tag]] <- value
    }
  }

  if (length(current) > 0) {
    warning("parsing ended without a record terminator ('=')")
  }

  if (single) {
    if (length(records) < 1) {
      return(list())
    } else {
      return(records[[1]])
    }
  }

  return(records)
}

#' Read Boulder-IO formatted files.
#'
#' @describeIn read_boulder reads a text file containing
#' one or more Boulder-IO records.
#'
#' @param strings Character vector of lines from a Boulder-IO file.
#' @param file File connection object or filename.
#' @param single By default, a Boulder-IO file contains multiple records, so a list of
#' records is returned. If \code{single=TRUE} only a single record object (the first) is
#' returned.
#'
#' @return A list of records. Each record is also a list of tags (names) and values. If
#' The Boulder-IO record contained multiple lines with the same tag, the values are
#' joined into a single vector in the list. All values are character strings.
#'
#' @export
read_boulder <- function(file, ...) {
  parse_boulder(readLines(file), ...)
}

#' Write a list of records to a Boulder-IO text file.
#'
#' @param records A list of records. Each record is also a list of tags (names) and
#' values. All values are converted with \code{as.character}. Tags with a vector of
#' values are written on separate lines with the same tag.
#' @param file A filename or a connection object. Use "" for STDOUT.
#' @param single If \code{TRUE}, the input \code{records} is only a single record,
#' not a list of records.
#'
#' @export
write_boulder <- function(records, file="", single=FALSE) {
  if (single) {
    records <- list(records)
  }

  lines <- c()
  for (record in records) {
    for (key in names(record)) {
      for (value in record[[key]]) {
        lines[length(lines)+1] <- paste(key, as.character(value), sep="=")
      }
    }
    lines[length(lines)+1] <- "="
  }

  if (file == "") {
    file <- stdout()
  }

  writeLines(lines, con=file)
}

