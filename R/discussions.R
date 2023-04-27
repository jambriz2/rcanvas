#' @title Discussion functions
#'
#' @description A group of functions which deal with [discussion topics](https://canvas.instructure.com/doc/api/discussion_topics.html).
#'
#' * `get_discussions_context`: get all discussions belonging to a course or group. Note that theoretically this should include announcements, as they are technically discussions, but does not. Use `get_announcements` instead.
#'
#' @param object_id course or group id
#' @param object_type "courses" or "groups"
#' @param include If "all_dates" is passed, all dates associated with graded discussions' assignments will be included.
#'
#' @return discussions belonging to requested context
#' @export
#' @md
#'
#' @examples
#' get_discussions_context(4371405)
get_discussions_context <- function(object_id, object_type = "courses",
                                    include = NULL) {
  stopifnot(object_type %in% c("courses", "groups"))
  url <- make_canvas_url(object_type, object_id, "discussion_topics")
  args <- list(per_page = 100)
  include <- iter_args_list(include, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  dat
}

#' @title Get single discussion by id
#'
#' @param discussion_id  specific id of discussion to get/update
#' @rdname get_discussions_context
#' @return single discussion
#' @export
#' @md
#'
#' @examples
#' get_discussion_id(4371405, 1350207)
get_discussion_id <- function(discussion_id, object_id, object_type = "courses") {
  stopifnot(object_type %in% c("courses", "groups"))
  url <- make_canvas_url(object_type, object_id, "discussion_topics", discussion_id, "view")
  args <- list(per_page = 100)
  include <- iter_args_list(NULL, "include[]")
  args <- c(args, include)
  raw_data <- canvas_query(url, args, "GET") %>% httr::content("text")
  data <- jsonlite::fromJSON(raw_data, flatten = TRUE)
  return(data)
}

#' @title Update discussion by id
#'
#' @param message new body of discussion id
#'
#' @rdname get_discussions_context
#' @return silently sends put request and updates
#' @export
#' @md
#'
#' @examples
#' update_discussion_id(4371405, 1350207, newtext)
update_discussion_id <- function(discussion_id, object_id, message,
                                 object_type = "courses") {
  stopifnot(object_type %in% c("courses", "groups"))
  url <- make_canvas_url(object_type, object_id, "discussion_topics", discussion_id)
  args <- list(access_token = check_token(),
               message = message,
               per_page = 100)
  canvas_query(url, args, "PUT")
}

#' @title Get discussion entries by user
#'
#' @param course_id The course ID.
#' @param discussion_id The discussion ID.
#' @return A data frame containing discussion entries for the specified user.
#' @export
get_discussion_entries <- function(course_id, discussion_id) {
  # Construct the API endpoint for the discussion entries
  endpoint <- make_canvas_url("courses", course_id, "discussion_topics", discussion_id, "entries")

  # Get the first page of discussion entries
  response <- canvas_query(paste0(endpoint, "?per_page=100"))
  discussion_entries <- httr::content(response)

  # Get all the pages of discussion entries
  while (length(response$headers$link) > 0 && grepl("rel=\"next\"", response$headers$link)) {
    next_page_url <- sub(".*<(.*?)>; rel=\"next\".*", "\\1", response$headers$link)
    response <- canvas_query(next_page_url)
    discussion_entries <- rbind(discussion_entries, httr::content(response))
  }

  # Convert the list of discussion entries to a data frame
  entries_df <- map_df(discussion_entries, ~ as.data.frame(t(unlist(.x)))) %>% select(1:13)

  # Parse the HTML code in each message
  messages <- entries_df$message
  messages_parsed <- lapply(messages, function(x) {
    html_text(read_html(x))
  })
  entries_df$message <- messages_parsed
  return(entries_df)
}

#' @title Get discussion entries by user
#'
#' @param course_id The course ID.
#' @param discussion_id The discussion ID.
#' @param user_id The user ID.
#' @return A data frame containing discussion entries for the specified user.
#' @export
get_discussion_entries_by_user <- function(course_id, discussion_id, user_id) {
  # Construct the API endpoint for the discussion entries
  endpoint <- make_canvas_url("courses", course_id, "discussion_topics", discussion_id, "entries")

  # Get the first page of discussion entries
  response <- canvas_query(paste0(endpoint, "?per_page=100"))
  discussion_entries <- httr::content(response)

  # Get all the pages of discussion entries
  while (length(response$headers$link) > 0 && grepl("rel=\"next\"", response$headers$link)) {
    next_page_url <- sub(".*<(.*?)>; rel=\"next\".*", "\\1", response$headers$link)
    response <- canvas_query(next_page_url)
    discussion_entries <- rbind(discussion_entries, httr::content(response))
  }

  # Convert the list of discussion entries to a data frame
  entries_df <- map_df(discussion_entries, ~ as.data.frame(t(unlist(.x)))) %>% select(1:13)

  # Parse the HTML code in each message
  messages <- entries_df$message
  messages_parsed <- lapply(messages, function(x) {
    html_text(read_html(x))
  })
  entries_df$message <- messages_parsed
  return(entries_df)
}

