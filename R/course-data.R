#' @importFrom magrittr %>%
#'
#' @title Get all courses
#'
#' @description This function retrieves a list of all courses in Canvas.
#'
#' @param user_id Optional argument to specify courses for a specific user ID. (Integer)
#' @param include Optional argument to specify additional information to include, such as "teachers", "total_students", etc. (String)
#'
#' @return A data frame with the details of all courses.
#' @export
#'
#' @examples
#' get_course_list()
#' get_course_list(user_id = 123)
#' get_course_list(include = c("teachers", "total_students"))
get_course_list <- function(user_id = NULL, include = NULL) {
  if (!is.null(user_id)) {
    url <- make_canvas_url("users", user_id, "courses")
  } else {
    url <- make_canvas_url("courses")
  }
  args <- list(per_page = 100, user_id = user_id)

  include <- iter_args_list(include, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  return(unique(dat))
}

#' @title Get courses for a specific account or all courses
#'
#' @description This function retrieves a list of courses for a specific account or all courses in Canvas.
#'
#' @param account_id Optional argument to specify courses for a specific account ID. (Integer).
#' @param include Optional argument to specify additional information to include. (String).
#'
#' @return A data frame with the details of the courses.
#' @export
#'
#' @examples
#' get_account_course_list()
#' get_account_course_list(account_id = 123)
get_account_course_list <- function(account_id = NULL, include = NULL) {
  if (!is.null(account_id)) {
    url <- make_canvas_url("accounts", account_id, "courses")
  } else {
    url <- make_canvas_url("courses")
  }

  args <- list(per_page = 100)

  include <- iter_args_list(include, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  return(unique(dat))
}

#' @title Get course analytics data
#'
#' @description This function retrieves course analytics data and returns a data frame or a list if an individual's user_id is specified.
#'
#' @param course_id The ID of the course for which you want to get permission information. (Integer)
#' @param type One of "assignments", "activity", or "student_summaries". (String)
#' @param user_id Optional argument to specify courses for a specific user ID. (Integer)
#'
#' @return A data frame or a list if user_id is specified.
#' @export
#'
#' @examples
#' #' get_course_analytics_data(course_id = 123)
#' #' get_course_analytics_data(course_id = 123, type = "activity")
#' #' get_course_analytics_data(course_id = 123, type = "student_summaries", user_id = 123)
get_course_analytics_data <- function(course_id, type = "assignments", user_id = NULL) {
  if (!is.null(user_id)) {
    url <- make_canvas_url("courses", course_id, "analytics/users", user_id, type)
  } else {
    url <- make_canvas_url("courses", course_id, "analytics", type)
  }
  if (type == "communication" & is.null(user_id)) {
    stop("user_id must be specified for communication data")
  }
  args <- list(
    per_page = 100,
    user_id = user_id
  )
  resp <- canvas_query(url, args)
  json <- httr::content(resp, "text")
  if (json == "[]") stop("Nothing available for this course.")
  jsonlite::fromJSON(json, flatten = TRUE)
}

#' @title Get various course items
#'
#' @description Returns a data.frame of various course items. See "item" argument below. Omitting the "item argument
#' returns a course object.
#'
#' @param course_id The ID of the course for which you want to get permission information. (Integer)
#' @param item Optional argument to specify list "settings", "discussion_topics", "todo", "enrollments", "features", "files", "modules", "front_page", "pages", "quizzes", "folders". (String)
#' @param include Optional argument to specify additional information to include. (String)
#'
#' @return A data frame containing the requested course items.
#' @export
#'
#' @examples
#' #' get_course_items(course_id = 123, item = "settings")
#' #' get_course_items(course_id = 123, item = "enrollments")
#' #' get_course_items(course_id = 123, item = "users", include = "email")
get_course_items <- function(course_id, item, include = NULL) {
  valid_items <- c("settings", "discussion_topics", "todo", "enrollments", "users", "students",
                   "features", "assignments", "files", "modules", "front_page", "pages", "quizzes",
                   "folders", "assignment_groups")
  if (!missing(item) && !item %in% valid_items) {
    stop(paste("item argument must be one of:", paste(valid_items, collapse = ", ")))
  }
  if (!missing(item)) {
    url <- make_canvas_url("courses", course_id, item)
  } else {
    #Omitting the item argument will return general information about the course
    url <- make_canvas_url("courses", course_id)
  }
  args <- list(per_page = 100)
  include <- iter_args_list(include, "include[]")
  args <- c(args, include)
  process_response(url, args) %>%
    dplyr::mutate(course_id = course_id)
}

#' @title Get course permissions for the authenticated user
#'
#' @description This function returns permission information for the calling user in the given course.
#'
#' @param course_id The ID of the course for which you want to get permission information. (Integer)
#' @param permissions A vector of permission names to check against the authenticated user. Permission names are documented in the Create a role endpoint. (String)
#'
#' @return A named list with permission names and their respective boolean values.
#' @import httr
#' @export
#'
#' @examples
#' get_course_permissions(course_id = 123)
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

#' @title Get course outcome results
#'
#' @description This function retrieves course outcome results and returns them as a data frame.
#'
#' @param course_id The ID of the course for which you want to get permission information. (Integer)
#'
#' @return A data frame containing course outcome results.
#' @export
#'
#' @examples
#' get_outcome_results(course_id = 123)
get_outcome_results <- function(course_id) {
  url <- make_canvas_url("courses", course_id, "outcome_results")
  args <- list(per_page = 100)
  dat <- process_response(url, args)

  return(unique(dat))
}

#' @title Search all courses
#'
#' @description This function searches for all public courses and returns the results as a data frame.
#'
#' @param search An optional search keyword to filter the courses (String)
#' @return A data frame containing the search results
#' @export
#'
#' @examples
#' search_courses()
#' search_courses(search = "big data")
search_courses <- function(search = NULL) {
  url <- make_canvas_url("search", "all_courses")
  args = list(per_page = 100)
  if (!is.null(search)) args["search"] = search

  resp <- list()

  has_more_results <- TRUE
  page <- 1

  while (has_more_results) {
    args["page"] = page
    page_resp <- canvas_query(url, args, "GET") %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten = TRUE)

    if (length(page_resp) == 0) {
      has_more_results <- FALSE
    } else {
      if (is.null(nrow(resp))) {
        resp <- page_resp
      } else {
        columns_to_add <- setdiff(names(page_resp), names(resp))
        for (column in columns_to_add) {
          resp[[column]] <- NA
        }
        columns_to_add <- setdiff(names(resp), names(page_resp))
        for (column in columns_to_add) {
          page_resp[[column]] <- NA
        }
        resp <- rbind(resp, page_resp)
      }
    }
    page <- page + 1
  }

  return(if (is.null(nrow(resp))) data.frame() else resp)
}
