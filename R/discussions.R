#' @title Discussion functions
#'
#' @description This group of functions deals with [discussion topics](https://canvas.instructure.com/doc/api/discussion_topics.html). This retrieves all discussions belonging to a course or group. Note that theoretically this should include announcements, as they are technically discussions, but it does not. Use `get_announcements` instead.
#'
#' @param course_id The ID of the course for which you want to get permission information. (Integer)
#' @param object_type The type of object. Should be either "courses" or "groups". (String)
#' @param include If "all_dates" is passed, all dates associated with graded discussions' assignments will be included. (String)
#'
#' @return A data frame containing discussions belonging to the requested context.
#' @export
#'
#' @examples
#' get_discussions_context(object_id = 123)
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

#' @title Get single discussion by ID
#'
#' @description Retrieves a single discussion topic by its ID.
#'
#' @param discussion_id The specific ID of the discussion topic to get. (Integer)
#' @param object_id The ID of the course or group. (Integer)
#' @param object_type The type of object. Should be either "courses" or "groups". (String)
#'
#' @return A data frame containing the details of the single discussion topic.
#' @export
#'
#' @examples
#' get_discussion_id(discussion_id = 123, object_id = 123)
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

#' @title Update discussion by ID
#'
#' @description Updates the body of a discussion topic by its ID.
#'
#' @param discussion_id The specific ID of the discussion topic to update. (Integer)
#' @param object_id The ID of the course or group. (Integer)
#' @param message The new body of the discussion topic. (String)
#' @param object_type The type of object. Should be either "courses" or "groups". (String)
#'
#' @return This function silently sends a PUT request to update the discussion topic.
#' @export
#'
#' @examples
#' update_discussion_id(discussion_id = 123, object_id = 123, "Hello")
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
#' @param course_id The ID of the course for which you want to get permission information. (Integer)
#' @param discussion_id The specific ID of the discussion topic to get. (Integer)
#'
#' @return A data frame containing discussion entries for the specified user.
#' @export
#'
#' @examples
#' get_discussion_entries(course_id = 123, discussion_id = 123)
get_discussion_entries <- function(course_id, discussion_id) {
  endpoint <- make_canvas_url("courses", course_id, "discussion_topics", discussion_id, "entries")
  response <- canvas_query(paste0(endpoint, "?per_page=100"))
  discussion_entries <- httr::content(response)

  # Get all the pages of discussion entries
  while (length(response$headers$link) > 0 && grepl("rel=\"next\"", response$headers$link)) {
    next_page_url <- sub(".*<(.*?)>; rel=\"next\".*", "\\1", response$headers$link)
    response <- canvas_query(next_page_url)
    discussion_entries <- rbind(discussion_entries, httr::content(response))
  }

  # Parse the HTML code in each message
  entries_df <- map_df(discussion_entries, ~ as.data.frame(t(unlist(.x)))) %>% select(1:13)
  messages <- entries_df$message
  messages_parsed <- lapply(messages, function(x) {
    html_text(read_html(x))
  })
  entries_df$message <- messages_parsed
  return(entries_df)
}

#' @title Get discussion entries by user
#'
#' @description Retrieves all discussion entries for a specific user in a course and discussion topic.
#'
#' @param course_id The ID of the course for which you want to get permission information. (Integer)
#' @param discussion_id The specific ID of the discussion topic to get. (Integer)
#' @param user_id A valid user id. (Integer)
#'
#' @return A data frame containing discussion entries for the specified user.
#' @export
#'
#' @examples
#' get_discussion_entries_by_user(course_id = 123, discussion_id = 123, user_id = 123)
get_discussion_entries_by_user <- function(course_id, discussion_id, user_id) {
  endpoint <- make_canvas_url("courses", course_id, "discussion_topics", discussion_id, "entries")
  response <- canvas_query(paste0(endpoint, "?per_page=100"))
  discussion_entries <- httr::content(response)

  # Get all the pages of discussion entries
  while (length(response$headers$link) > 0 && grepl("rel=\"next\"", response$headers$link)) {
    next_page_url <- sub(".*<(.*?)>; rel=\"next\".*", "\\1", response$headers$link)
    response <- canvas_query(next_page_url)
    discussion_entries <- rbind(discussion_entries, httr::content(response))
  }

  # Parse the HTML code in each message
  entries_df <- map_df(discussion_entries, ~ as.data.frame(t(unlist(.x)))) %>% select(1:13)
  messages <- entries_df$message
  messages_parsed <- lapply(messages, function(x) {
    html_text(read_html(x))
  })
  entries_df$message <- messages_parsed
  return(entries_df)
}

