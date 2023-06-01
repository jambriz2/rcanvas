#' @title Enroll a user into the course
#'
#' @param course_id The ID of the course for which you want to get permission information. (Integer)
#' @param user_id The valid ID of the user to be enrolled in the course. (Integer)
#' @param type 	Enroll the user as a student, teacher, TA, observer, or designer. Options: 'StudentEnrollment', 'TeacherEnrollment', 'TaEnrollment', 'ObserverEnrollment', 'DesignerEnrollment' (String)
#' @param state If set to ‘active,’ student will be immediately enrolled in the course. Otherwise they will be required to accept a course invitation. Option: 'active', 'invited', 'inactive' (String)
#' @param section The ID of the course section to enroll the student in. (Integer)
#' @param ... any other arguments passed to the API
#'
#' @export
add_enrollment <- function(course_id, user_id, type, state, section=F, ...) {
  url <- paste0(canvas_url(),
                paste(ifelse(section, "sections", "courses"), course_id, "enrollments", sep="/"))
  args <- list("enrollment[user_id]" = user_id, "enrollment[type]"=type, "enrollment[enrollment_state]"=state, ...)
  canvas_query(url, args, "POST")
}

#' @title Enroll user(s) into a course (or multiple courses)
#'
#' Enrolls the given user(s) into the course. If multiple course_ids are given, it should be of the same length as user_id,
#' and each user will be enrolled into the corresponding course.
#'
#' @param course_id The ID of the course for which you want to get permission information. (Integer)
#' @param user_id The valid ID of the user to be enrolled in the course. (Integer)
#' @param type 	Enroll the user as a student, teacher, TA, observer, or designer. Options: 'StudentEnrollment', 'TeacherEnrollment', 'TaEnrollment', 'ObserverEnrollment', 'DesignerEnrollment' (String)
#' @param state If set to ‘active,’ student will be immediately enrolled in the course. Otherwise they will be required to accept a course invitation. Option: 'active', 'invited', 'inactive' (String)
#' @param section The ID of the course section to enroll the student in. (Integer)
#' @param ... any other arguments passed to the API
#' @rdname enrollment
#'
#' @export
add_enrollments <- function(course_id, user_ids, type=c("StudentEnrollment", "TeacherEnrollment", "TaEnrollment", "ObserverEnrollment", "DesignerEnrollment"),
                            state=c("invited", "active", "inactive"), section=F, ...) {
  type <- match.arg(type)
  state <- match.arg(state)
  invisible(purrr::map2(course_id, user_ids, add_enrollment, type=type, state=state, section=section, ...))
}
