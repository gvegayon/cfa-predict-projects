#' Get stats for a GitHub repository using
#' the GitHub API.
#' @param repo A string of the form "OWNER/REPO"
#' @details 
#' For more details, see
#' <https://docs.github.com/en/rest/metrics/statistics?apiVersion=2022-11-28#get-all-contributor-commit-activity>
get_stats <- function(repo, verbose=FALSE) {

  cmd <- sprintf(
    paste(
      'gh api',
      '-H "Accept: application/vnd.github+json"',
      '-H "X-GitHub-Api-Version: 2022-11-28"',
      'repos/%s/stats/contributors'
    ),
    repo
  )

  if (verbose || interactive()) {
    message(cmd)
  }
    
  system(cmd, intern = TRUE) |>
    jsonlite::fromJSON()
}

#' Get activity for a GitHub repository using
#' the GitHub API.
#' @param repo A string of the form "OWNER/REPO"
#' @details
#' For more details, see
#' <https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#list-repository-activities>
get_activity <- function(repo, verbose=FALSE) {
  
  cmd <- sprintf(
    paste(
      'gh api',
      '-H "Accept: application/vnd.github+json"',
      '-H "X-GitHub-Api-Version: 2022-11-28"',
      'repos/%s/activity?per_page=100'
    ),
    repo
  )

  if (verbose || interactive()) {
    message(cmd)
  }
    
  system(cmd, intern = TRUE) |>
    jsonlite::fromJSON()
}

#' Get contributors for a GitHub repository using
#' the GitHub API.
#' @param repo A string of the form "OWNER/REPO"
#' @details
#' For more details, see
#' <https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#list-repository-contributors>
get_contributors <- function(repo, verbose=FALSE) {
  
  cmd <- sprintf(
    paste(
      'gh api',
      '-H "Accept: application/vnd.github+json"',
      '-H "X-GitHub-Api-Version: 2022-11-28"',
      'repos/%s/contributors?per_page=100'
    ),
    repo
  )

  if (verbose || interactive()) {
    message(cmd)
  }
    
  system(cmd, intern = TRUE) |>
    jsonlite::fromJSON()
}

#' Generate nice gravatar icons for authors
#' 
#' @param url,avatar_url Character vectors of the same length return
#' by the `gh` api call to the repos/{owner}/{repo}/stats/contributors
#' endpoint.
#' @param size A string representing the size of the avatar
#' in pixels. Default is '25px'.
gen_author_icons <- function(
  url,
  avatar_url,
  size='25px'
) {

  sprintf(
    '<a href="%s" target="_blank"><img style="border-radius: 50%%; width:%s" src="%s"/></a>',
    url,
    size,
    avatar_url
    ) |> paste(collapse = " ")


}