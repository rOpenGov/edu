#' @title Process assignments create with edu and completed by students
#' @description Function processes number of completed assignments and outputs a single R-script with the question and all students answers AND correct answeers
#' @param path_assignments Absolute path to the folder containing the completed assignments as R.scripts ending either with .R or .r
#' @param outfile_name Name of the the output script
#' @param outfile_path Absolute path to the folder where scipt should be written
#' @author Markus Kainu <markus.kainu@kela.fi>
#' @return A R-script
#' @examples
#'  \dontrun{
#'  process_assignments(path_assignments = "~/btsync/workspace/ropengov/edutest/basics",
#'                    outfile_name = "basics_summary",
#'                    outfile_path = "~/btsync/workspace/ropengov/edutest/")
#'  }
#'
#' @rdname process_assignments
#' @export
#'
process_assignments <- function(path_assignments = "~/btsync/workspace/ropengov/edutest/basics",
                                outfile_name = "basics_summary",
                                outfile_path = "~/btsync/workspace/ropengov/edutest/"){

  flies <- list.files(path = path_assignments, pattern = ".R$|.r$", full.names = TRUE)


  # Check if all assignments have same number of questions AND chunk titles
  checkdata <- data_frame()
  for (i in 1:length(flies)){
    lines <- readLines(flies[i], encoding = "UTF-8")
    new_row <- data_frame(filename = flies[i],
                          n_questions = length(lines[grepl("^#' \\*", lines)]),
                          n_chunks = length(lines[grepl("^#\\+ ", lines)]))
    checkdata <- bind_rows(checkdata, new_row)
  }
  if (unique(checkdata$n_questions) != unique(checkdata$n_chunks)) stop("Assignment do not have identical questions or chunk titles. Please edit the files!")

  # if the each assignment do have the same number of question and chunks
  # we assume that it is safe to go on

  # Lets get the question AND chunk titles from the first assignment
  lines <- readLines(flies[1], encoding = "UTF-8")
  question_lines <- lines[grepl("^#' \\*", lines)]
  chunk_title_lines <- lines[grepl("^#\\+ ", lines)]
  # chunk_ids <- gsub("^#\\+ |, eval = FALSE", "", chunk_title_lines)

  # Then lets loop through each assignment and extract the answer lines
  # AND create and single R-script on the fly


  # First lets create the summary script and write the yaml-block
  outfile <- paste0(outfile_path,"/",outfile_name,".R")
  file.create(outfile, showWarnings = FALSE)

  cat(paste0(
"#' ---\n",
"#' title: ",outfile_name,"\n",
"#' author: ",Sys.info()[['user']],"\n",
"#' date: '`r Sys.time()`'\n",
"#' output:\n",
"#'   html_document:\n",
"#'     theme: united\n",
"#'     toc: true\n",
"#'     toc_float: true\n",
"#'     number_sections: yes\n",
"#'     code_folding: show\n",
"#' ---\n",
"#' \n",
# "#' # Exercises\n",
"#' ******\n\n")
, file = outfile, append = TRUE)

  # get the lines AFTER each chunc_title_lines until next question_lines
  for (i in 1:length(question_lines)){
    # lets go through each assignment for this question
    answers <- vector()
    for (nro in 1:length(flies)){
      tmplines <- readLines(flies[nro], encoding = "UTF-8")
      index_begin <- match(chunk_title_lines[i], tmplines)
      # In the case of last question and end of question will be set and the last line of assignment
      if (i == length(question_lines)){
        index_end <- length(tmplines)
      } else index_end <- match(question_lines[i+1], tmplines)
      answer_lines <- tmplines[index_begin:index_end]
      answer_lines <- answer_lines[!grepl("^#\\+ |#'", answer_lines)]
      answer_lines <- answer_lines[nchar(answer_lines) > 0]
      answers <- c(answers,answer_lines)
    }

    # write question AND chunk title
    cat(paste0(
      "\n\n\n\n",
      question_lines[i],
      "\n",
      chunk_title_lines[i],
      "\n",
      "\n"), file = outfile, append = TRUE
    )
    # Write all answers
    cat(paste0(answers, collapse = "\n\n###\n\n"), file = outfile, append = TRUE)
    # Finally get the correct answers from the yaml files by matching the chunk ids
    chunk_id <- gsub("^#\\+ |, eval = FALSE", "", chunk_title_lines[i])
    # scan assignments
    assign_ymls <- list.files(system.file(package = "edu", ... = "data/"),pattern = ".yml", full.names = TRUE)
    all_assignments <- data_frame()
    for (i in assign_ymls){
      tmp <- yaml::yaml.load_file(i)
      tmp_d <- plyr::ldply(tmp, data.frame, stringsAsFactors=FALSE)
      all_assignments <- bind_rows(all_assignments,tmp_d)
    }
    correct <- all_assignments[all_assignments$id == chunk_id, ]$ans
    # write the correct as last one
    cat(paste0(
      "\n\n",
      "# correct answer begins\n",
      correct,
      "\n# correct answer ends\n\n"),
      file = outfile, append = TRUE
    )
  }
}








