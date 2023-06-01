#' @title Create a course
#'
#' @description This function creates a new course with the specified parameters.
#'
#' @param account_id The ID of the account in which to create the course. (Integer)
#' @param name The name of the course. If omitted, the course will be named "Unnamed Course". (String)
#' @param course_code The course code for the course. (String)
#' @param start_at The course start date in ISO8601 format, e.g., 2011-01-01T01:00Z (String)
#' @param end_at The course end date in ISO8601 format, e.g., 2011-01-01T01:00Z (String)
#' @param license The name of the licensing. Should be one of the following abbreviations:
#'   'private' - Private Copyrighted,
#'   'cc_by_nc_nd' - CC Attribution Non-Commercial No Derivatives,
#'   'cc_by_nc_sa' - CC Attribution Non-Commercial Share Alike,
#'   'cc_by_nc' - CC Attribution Non-Commercial,
#'   'cc_by_nd' - CC Attribution No Derivatives,
#'   'cc_by_sa' - CC Attribution Share Alike,
#'   'cc_by' - CC Attribution,
#'   'public_domain' - Public Domain (String)
#' @param is_public Set to TRUE if the course is public to both authenticated and unauthenticated users. (Boolean)
#' @param is_public_to_auth_users Set to TRUE if the course is public only to authenticated users. (Boolean)
#' @param public_syllabus Set to TRUE to make the course syllabus public. (Boolean)
#' @param public_syllabus_to_auth Set to TRUE to make the course syllabus public for authenticated users. (Boolean)
#' @param public_description A publicly visible description of the course. (String)
#' @param allow_student_wiki_edits If TRUE, students will be able to modify the course wiki. (Boolean)
#' @param allow_wiki_comments If TRUE, course members will be able to comment on wiki pages. (Boolean)
#' @param allow_student_forum_attachments If TRUE, students can attach files to forum posts. (Boolean)
#' @param open_enrollment Set to TRUE if the course is open enrollment. (Boolean)
#' @param self_enrollment Set to TRUE if the course is self enrollment. (Boolean)
#' @param restrict_enrollments_to_course_dates Set to TRUE to restrict user enrollments to the start and end dates of the course. (Boolean)
#' @param term_id The unique ID of the term in which to create the course. (Integer)
#' @param sis_course_id The unique SIS identifier. (String)
#' @param integration_id The unique Integration identifier. (String)
#' @param hide_final_grades If TRUE, the totals in the student grades summary will be hidden. (Boolean)
#' @param apply_assignment_group_weights Set to TRUE to weight final grade based on assignment groups percentages. (Boolean)
#' @param time_zone The time zone for the course. Allowed time zones are IANA time zones or friendlier Ruby on Rails time zones. (String)
#' @param offer If TRUE, the course will be available to students immediately. (Boolean)
#' @param enroll_me If TRUE, the current user will be enrolled as the teacher. (Boolean)
#' @param default_view The type of page that users will see when they first visit the course. Options:
#'   'feed', 'wiki', 'modules', 'syllabus', 'assignments' (String)
#' @param syllabus_body The syllabus body for the course. (String)
#' @param grading_standard_id The grading standard ID to set for the course. If no value is provided, the current grading_standard will be un-set from this course. (String)
#' @param course_format Specifies the format of the course. Should be one of the following:
#'   'on_campus', 'online', or 'blended'. (String)
#' @param enable_sis_activation When TRUE, will first try to re-activate a deleted course with a matching sis_course_id if possible. (Boolean)
#'
#' @return Invisible
#' @export
#'
#' @examples
#' create_canvas_course(account_id = 123, name = "David Ranzolin")
create_canvas_course <- function(account_id,
                                 name = NULL,
                                 course_code = NULL,
                                 start_at = NULL,
                                 end_at = NULL,
                                 license = NULL,
                                 is_public = NULL,
                                 is_public_to_auth_users = NULL,
                                 public_syllabus = NULL,
                                 public_syllabus_to_auth = NULL,
                                 public_description = NULL,
                                 allow_student_wiki_edits = NULL,
                                 allow_wiki_comments = NULL,
                                 allow_student_forum_attachments = NULL,
                                 open_enrollment = NULL,
                                 self_enrollment = NULL,
                                 restrict_enrollments_to_course_dates = NULL,
                                 term_id = NULL,
                                 sis_course_id = NULL,
                                 integration_id = NULL,
                                 hide_final_grades = NULL,
                                 apply_assignment_group_weights = NULL,
                                 time_zone = NULL,
                                 offer = NULL,
                                 enroll_me = NULL,
                                 default_view = NULL,
                                 syllabus_body = NULL,
                                 grading_standard_id = NULL,
                                 course_format = NULL,
                                 enable_sis_activation = NULL) {
  url <- make_canvas_url("accounts", account_id, "courses")
  course_args <- sc(list(
    name = name,
    course_code = course_code,
    start_at = start_at,
    end_at = end_at,
    license = license,
    is_public = is_public,
    is_public_to_auth_users = is_public_to_auth_users,
    public_syllabus = public_syllabus,
    public_syllabus_to_auth = public_syllabus_to_auth,
    public_description = public_description,
    allow_student_wiki_edits = allow_student_wiki_edits,
    allow_wiki_comments = allow_wiki_comments,
    allow_student_forum_attachments = allow_student_forum_attachments,
    open_enrollment = open_enrollment,
    self_enrollment = self_enrollment,
    restrict_enrollments_to_course_dates = restrict_enrollments_to_course_dates,
    term_id = term_id,
    sis_course_id = sis_course_id,
    integration_id = integration_id,
    hide_final_grades = hide_final_grades,
    apply_assignment_group_weights = apply_assignment_group_weights,
    time_zone = time_zone,
    default_view = default_view,
    syllabus_body = syllabus_body,
    grading_standard_id = grading_standard_id,
    course_format = course_format)
  )
  names(course_args) <- sprintf("course[%s]", names(course_args))
  args <- list(course_args) %>%
    purrr::discard(is_empty) %>%
    purrr::flatten() %>%
    append(list(offer = offer, enroll_me = enroll_me, enable_sis_activation = enable_sis_activation)) %>%
    purrr::discard(is.null)
  invisible(canvas_query(url, args, "POST"))
  m <- paste("Course", ifelse(is.null(name), "'Unnamed Course'", name), "created.")
  message(m)
}
