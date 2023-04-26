# https://canvas.instructure.com/doc/api/pages.html
#

#' Show Front Page
#'
#' @param course_id a valid course id
#' @return data.frame with front page information
#' @export
#'
#' @examples
#' show_page_front(34232)
show_wpage_front <- function(course_id) {
  # GET /api/v1/courses/:course_id/front_page
  url <- make_canvas_url("courses", course_id, "front_page")
  resp <- process_response(url, args = list())
  return(resp)
}


#' Duplicate course page
#'
#' @param course_id a valid course id
#' @param page_url name of the page url. Just the final portion.
#'
#' @return invisible
#' @export
#'
duplicate_wpage <- function(course_id, page_url){
  # POST /api/v1/courses/:course_id/pages/:url/duplicate
  url <- make_canvas_url("courses", course_id, "pages", page_url, "duplicate")
  resp <- httr::POST(url,
                     httr::user_agent("rcanvas - https://github.com/daranzolin/rcanvas"),
                     httr::add_headers(Authorization = paste("Bearer", check_token())),
                     body = list()
  )
  httr::stop_for_status(resp)
  return(resp)
}



#' Get data frame of pages in course
#'
#' @param course_id a valid course id
#' @param sort_type a string, Sort results by this field.  Allowed values: 'title',
#' 'created_at', and 'updated_at'
#' @param order_type a string. The sorting order. Defaults to 'asc'. Allowed values: asc, desc
#' @param search a string. The partial title of the pages to match and return.
#' @param published a boolean. If true, include only published paqes. If false, exclude published pages. If not present, do not filter on published status. Defaults to not present.
#'
#' @return a data.frame with all pages from course.
#' @export
#'
get_wpages_list <- function(course_id, sort_type = c("title", "created_at", "updated_at")[1],
                            order_type = "asc", search = NULL, published = NULL){
  # GET /api/v1/courses/:course_id/pages
  url <- make_canvas_url("courses", course_id, "pages")
  args_list <- list(sort = sort_type, order = order_type)
  if (!is.null(search)) args_list <- c(args_list, search_term = search)
  if (!is.null(published)) args_list <- c(args_list, published = published)
  args <- sc(args_list)

  resp <- process_response(url, args = args)
  return(resp)
}

#' Get data frame of a page in course
#'
#' @param course_id a valid course id
#' @param page_url a valid page url.
#'
#' @return a data.frame with all page from course.
#' @export
get_wpage <- function(course_id, page_url){
  # GET /api/v1/courses/:course_id/pages/:url
  url <- make_canvas_url("courses", course_id, "pages", page_url)
  resp <- process_response(url, args = NULL)
  return(resp)
}


#' Create page in course
#'
#' @param course_id a valid course id
#' @param title a string.  The title for the new page.
#' @param body a string	The content for the new page.
#' @param editing_roles a string.  Which user roles are allowed to edit this page. Any combination of these roles is allowed (separated by commas).  Allowed values: teachers, students, members, public
#' @param published a boolean.	Whether the page is published (true) or draft state (false).
#'
#' @return empty
#' @export
#'
create_wpage <- function(course_id, title, body, editing_roles = "teachers", published = FALSE) {
  # POST /api/v1/courses/:course_id/pages
  url <- make_canvas_url("courses", course_id, "pages")

  args <- list(wiki_page = sc(list(title = title,
                                   body = body,
                                   editing_roles = editing_roles,
                                   notify_of_update = FALSE,
                                   published = published,
                                   front_page = FALSE
  )))

  json_data <- jsonlite::toJSON(args, auto_unbox = TRUE)

  resp <- httr::POST(url,
                     httr::user_agent("rcanvas - https://github.com/daranzolin/rcanvas"),
                     httr::add_headers(Authorization = paste("Bearer", check_token()), 'Content-Type' = 'application/json'),
                     body = json_data, encode = "json")

  httr::stop_for_status(resp)
  message(sprintf("Page '%s' created", title))
  return(resp)
}


#' Update/Create page in course
#'
#' @param course_id a valid course id
#' @param page_url a valid page url.
#' @param title a string.  The title for the new page. NOTE: changing a page's title will change its url. The updated url will be returned in the result.
#' @param body a string	The content for the new page.
#' @param editing_roles a string.  Which user roles are allowed to edit this page. Any combination of these roles is allowed (separated by commas).  Allowed values: teachers, students, members, public
#' @param published a boolean.	Whether the page is published (true) or draft state (false).
#' @param notify a boolean. Whether participants should be notified when this page changes.
#'
#' @return empty
#' @export
#'
update_wpage <- function(course_id, page_url, title = NULL, body = NULL, editing_roles = "teachers", published = FALSE, notify = FALSE) {
  # PUT /api/v1/courses/:course_id/pages/:url
  url <- make_canvas_url("courses", course_id, "pages", page_url)

  args_list <- list(editing_roles = editing_roles,
                    published = published,
                    notify_of_update = notify,
                    front_page = FALSE)
  if (!is.null(title)) args_list <- c(args_list, title = title)
  if (!is.null(body)) args_list <- c(args_list, body = body)

  args <- list(wiki_page = sc(args_list))
  json_data <- jsonlite::toJSON(args, auto_unbox = TRUE)

  resp <- httr::PUT(url,
                    httr::user_agent("rcanvas - https://github.com/daranzolin/rcanvas"),
                    httr::add_headers(Authorization = paste("Bearer", check_token()), 'Content-Type' = 'application/json'),
                    body = json_data, encode = "json")

  httr::stop_for_status(resp)
  message(sprintf("Page '%s' updated", title))
  return(resp)

}

#' Delete page from course
#'
#' @param course_id a valid course id
#' @param page_url a valid page url.
#'
#' @return invisible
#' @export
#'
delete_wpage <- function(course_id, page_url){
  # DELETE /api/v1/courses/:course_id/pages/:url
  url <- paste0(canvas_url(), file.path("courses", course_id, "pages", page_url))
  resp <- canvas_query(url, type = "DELETE")

  httr::stop_for_status(resp)
  message(sprintf("Page '%s' deleted", page_url))
  return(resp)

}
