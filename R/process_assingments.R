#' @title Process assignments create with edu and completed by students
#' @description Function processes number of completed assignments and outputs a single R-script with the question and all students answers AND correct answeers
#' @param folder Absolute path to the folder containing the completed assignments as R.scripts ending either with .R or .r
#' @param outfile Name of the the output
#' @author Markus Kainu <markus.kainu@kela.fi>
#' @return A R-script
#' @examples
#'  \dontrun{
#'  process_assignments(path = "~/rcourse/exercises/import/", outfile = "import_summary")
#'  }
#'
#' @rdname process_assignments
#' @export
process_assignments <- function(path,outfile){

  flies <- list.files(path = path, full.names = , pattern = ".R$|.r$")
  return(flies)

}
