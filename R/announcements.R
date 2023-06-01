#' @title Get announcements from a Canvas course
#'
#' @param course_id The ID of the course for which you want to get permission information. (Integer)
#' @param start_date Only return announcements posted since the start_date. The defaults to 14 days ago from the current date. The value should be formatted as: yyyy-mm-dd or ISO 8601 YYYY-MM-DDTHH:MM:SSZ. (String Vector)
#' @param end_date Only return announcements posted before the end_date. The defaults to 28 days from start_date. The value should be formatted as: yyyy-mm-dd or ISO 8601 YYYY-MM-DDTHH:MM:SSZ. Announcements scheduled for future posting will only be returned to course administrators. (String Vector)
#' @param active_only Only return active announcements that have been published. Applies only to requesting users that have permission to view unpublished items. Defaults to false for users with access to view unpublished items, otherwise true and unmodifiable. (Boolean)
#'
#' @return A data frame with the details of announcements for a given course.
#' @export
#
#' @examples
#' get_announcements(course_id = 123)
#' get_announcements(course_id = 123, start_date = "2020-01-01", end_date = "2023-04-22")
get_announcements <- function(course_id, start_date = NULL, end_date = NULL, active_only = FALSE) {
  course_context_code <- ifelse(grepl(pattern = "course", x = course_id), course_id, paste0("course_", course_id))
  url <- make_canvas_url("announcements")
  args <- list(per_page = 100,
               `context_codes[]` = course_context_code,
               start_date = start_date,
               end_date = end_date,
               active_only = active_only)
  args <- sc(args)
  dat <- process_response(url, args)
  dat
}

