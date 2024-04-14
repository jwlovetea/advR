# escaping ----------------------------------------------------------------
#' @export
html <- function(x) structure(x, class = "advr_html")

#' @export
print.advr_html <- function(x, ...) {
  out <- paste0("<HTML>", x)
  cat(paste(strwrap(out), collapse = "\n"), "\n", sep = "")
}

escape <- function(x) UseMethod("escape")

#' @export
escape.character <- function(x) {
  x <- gsub("&", "&amp;", x)
  x <- gsub("<", "&lt;", x)
  x <- gsub(">", "&gt;", x)

  html(x)
}

#' @export
escape.advr_html <- function(x) x

dots_partition <- function(...) {
  dots <- rlang::list2(...)

  if (is.null(names(dots))) {
    is_named <- rep(FALSE, length(dots))
  } else {
    is_named <- names(dots) != ""
  }

  list(
    named = dots[is_named],
    unnamed = dots[!is_named]
  )
}

html_attributes <- function(list) {
  if (length(list) == 0) {
    return("")
  }

  attr <- purrr::map2_chr(names(list), list, html_attribute)
  paste0(" ", unlist(attr), collapse = "")
}

html_attribute <- function(name, value = NULL) {
  if (length(value) == 0) {
    return(name)
  } # for attributes with no value
  if (length(value) != 1) stop("`value` must be NULL or length 1")

  if (is.logical(value)) {
    # Convert T and F to true and false
    value <- tolower(value)
  } else {
    value <- escape_attr(value)
  }
  paste0(name, "='", value, "'")
}

escape_attr <- function(x) {
  x <- escape.character(x)
  x <- gsub("\'", "&#39;", x)
  x <- gsub("\"", "&quot;", x)
  x <- gsub("\r", "&#13;", x)
  x <- gsub("\n", "&#10;", x)
  x
}


tag <- function(tag) {
  rlang::new_function(
    rlang::exprs(... = ),
    rlang::expr({
      dots <- dots_partition(...)
      attribs <- html_attributes(dots$named)
      children <- purrr::map_chr(dots$unnamed, escape)

      html(paste0(
        !!paste0("<", tag), attribs, ">",
        paste(children, collapse = ""),
        !!paste0("</", tag, ">")
      ))
    }),
    rlang::caller_env()
  )
}

# void_tag ----------------------------------------------------------------

void_tag <- function(tag) {
  rlang::new_function(
    rlang::exprs(... = ),
    rlang::expr({
      dots <- dots_partition(...)
      if (length(dots$unnamed) > 0) {
        rlang::abort(!!paste0("<", tag, "> must not have unnamed arguments"))
      }
      attribs <- html_attributes(dots$named)
      html(paste0(!!paste0("<", tag), attribs, " />"))
    }),
    rlang::caller_env()
  )
}


# tags --------------------------------------------------------------------

tags <- c(
  "a", "abbr", "address", "article", "aside", "audio",
  "b", "bdi", "bdo", "blockquote", "body", "button", "canvas",
  "caption", "cite", "code", "colgroup", "data", "datalist",
  "dd", "del", "details", "dfn", "div", "dl", "dt", "em",
  "eventsource", "fieldset", "figcaption", "figure", "footer",
  "form", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header",
  "hgroup", "html", "i", "iframe", "ins", "kbd", "label",
  "legend", "li", "mark", "map", "menu", "meter", "nav",
  "noscript", "object", "ol", "optgroup", "option", "output",
  "p", "pre", "progress", "q", "ruby", "rp", "rt", "s", "samp",
  "script", "section", "select", "small", "span", "strong",
  "style", "sub", "summary", "sup", "table", "tbody", "td",
  "textarea", "tfoot", "th", "thead", "time", "title", "tr",
  "u", "ul", "var", "video"
)

void_tags <- c(
  "area", "base", "br", "col", "command", "embed",
  "hr", "img", "input", "keygen", "link", "meta", "param",
  "source", "track", "wbr"
)


html_tags <- c(
  tags %>% rlang::set_names() %>% purrr::map(tag),
  void_tags %>% rlang::set_names() %>% purrr::map(void_tag)
)


with_html <- function(code) {
  code <- rlang::enquo(code)
  rlang::eval_tidy(code, html_tags)
}
