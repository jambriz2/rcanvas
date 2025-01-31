#' @title Get all assignments
#'
#' @param course_id The ID of the course for which you want to get permission information. (Integer)
#'
#' @return A data frame with the details of all assignments in the course.
#' @export
#' @examples
#' get_assignment_list(course_id = 123)
get_assignment_list <- function(course_id = NULL) {
  stopifnot(!is.null(course_id))
  url <- make_canvas_url("courses", course_id, "assignments")
  args <- list(per_page = 100)
  process_response(url, args)

}

#' @title Get a single assignment
#'
#' @param course_id The ID of the course for which you want to get permission information. (Integer)
#' @param assignment_id The ID of the assignment. (Integer)
#' @param include The associations to include with the assignment. (String Vector)
#' @param override_assignment_dates Whether to apply assignment overrides to the assignment. (Boolean)
#' @param needs_grading_count_by_section Whether to split up "needs_grading_count" by sections. (Boolean)
#' @param all_dates Whether to include all dates associated with the assignment. (Boolean)
#'
#' @return A data frame with the details of the single assignment.
#' @export
#' get_single_assignment(course_id = 123, assignment_id = 12345)
get_single_assignment <- function(course_id,
                                  assignment_id,
                                  include = NULL,
                                  override_assignment_dates = TRUE,
                                  needs_grading_count_by_section = FALSE,
                                  all_dates = FALSE) {

  # Validate include parameter
  if (!is.null(include)) {
    allowed_include_values <- c("submission", "assignment_visibility", "overrides", "observed_users", "can_edit", "score_statistics")
    if (!all(include %in% allowed_include_values)) {
      stop("Invalid include value(s). Allowed values are: ", paste(allowed_include_values, collapse = ", "))
    }
  }

  # Prepare the endpoint and parameters
  endpoint <- make_canvas_url("courses", course_id, "assignments", assignment_id)

  args <- list(override_assignment_dates = override_assignment_dates,
               needs_grading_count_by_section = needs_grading_count_by_section,
               all_dates = all_dates)

  # Prepare include arguments as separate list elements using iter_args_list if include is not NULL
  if (!is.null(include)) {
    include <- iter_args_list(include, "include[]")
    args <- c(args, include)
  }

  # Make the API request and process the response
  assignment_details <- do_query(endpoint, args)

  # If there is a rubric in the response, process it
  if (!is.null(assignment_details$rubric)) {
    rubric_data <- assignment_details$rubric %>%
      as.data.frame() %>%
      tibble::rowid_to_column("criterion_id") %>%
      dplyr::select(criterion_id, id, description, points)
  } else {
    rubric_data <- NULL
  }

  # Add rubric data to the assignment details
  assignment_details_processed <- list(assignment = assignment_details, rubric = rubric_data)

  return(assignment_details_processed)
}

