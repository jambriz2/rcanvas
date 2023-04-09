
#' Get discussion entries by user
#'
#' @param course_id The course ID.
#' @param user_id The user ID.
#' @return A data frame containing discussion entries for the specified user.
#' @export
get_discussion_entries_by_user <- function(course_id, user_id) {
  # Construct the API endpoint for the course discussions
  endpoint <- make_canvas_url("courses", course_id, "discussion_topics")

  # Get the discussion topics
  response <- canvas_query(endpoint)
  discussion_topics <- content(response, "parsed")

  # Function to get entries for a specific discussion topic
  get_topic_entries <- function(topic_id) {
    topic_endpoint <- make_canvas_url("courses", course_id, "discussion_topics", topic_id, "entries")
    topic_response <- canvas_query(topic_endpoint)
    content(topic_response, "parsed")
  }

  # Get all entries for each discussion topic
  all_entries <- map(discussion_topics$id, get_topic_entries)

  # Combine all entries into a single data frame
  all_entries_df <- bind_rows(all_entries)

  # Filter entries by user_id
  user_entries <- filter(all_entries_df, user_id == user_id)

  return(user_entries)
}
