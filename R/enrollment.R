#' Enroll a user into the course
#'
#' @param course_id ID of the course (or section, see below)
#' @param user_id ID of the user
#' @param type enrollment type
#' @param state enrollment state
#' @param section if TRUE, course_id should be a section_id instead of a course_id and students will be enrolled into this section
#' @param ... any other arguments passed to the API
add_enrollment <- function(course_id, user_id, type, state, section=F, ...) {
  url <- paste0(canvas_url(),
                paste(ifelse(section, "sections", "courses"), course_id, "enrollments", sep="/"))
  args <- list("enrollment[user_id]" = user_id, "enrollment[type]"=type, "enrollment[enrollment_state]"=state, ...)
  canvas_query(url, args, "POST")
}

#' Enroll user(s) into a course (or multiple courses)
#'
#' Enrolls the given user(s) into the course. If multiple course_ids are given, it should be of the same length as user_id,
#' and each user will be enrolled into the corresponding course.
#'
#' @param course_id ID of the course (or section, see below)
#' @param user_id ID of the user
#' @param type enrollment type
#' @param state enrollment state
#' @param section if TRUE, course_id should be a section_id instead of a course_id and students will be enrolled into this section
#' @param ... any other arguments passed to the API
#' @rdname enrollment
#' @export
add_enrollments <- function(course_id, user_ids, type=c("StudentEnrollment", "TeacherEnrollment", "TaEnrollment", "ObserverEnrollment", "DesignerEnrollment"),
                            state=c("invited", "active", "inactive"), section=F, ...) {
  type <- match.arg(type)
  state <- match.arg(state)
  invisible(purrr::map2(course_id, user_ids, add_enrollment, type=type, state=state, section=section, ...))
}


#' Get course permissions for the authenticated user
#'
#' This function returns permission information for the calling user in the given course.
#'
#' @param course_id The ID of the course for which you want to get permission information.
#' @param permissions A vector of permission names to check against the authenticated user.
#'                    Permission names are documented in the Create a role endpoint.
#'
#' @return A named list with permission names and their respective boolean values (TRUE/FALSE).
#' @import httr
#' @export
get_course_permissions <- function(course_id, permissions = NULL) {
  url <- make_canvas_url("courses", course_id, "permissions")

  # Prepare the permissions parameter
  if (!is.null(permissions)) {
    permissions_arg <- paste0("permissions[]=", permissions, collapse = "&")
  } else {
    permissions_arg <- NULL
  }

  args <- list(permissions = permissions_arg)
  args <- sc(args)

  dat <- process_response(url, args)
  dat
}

