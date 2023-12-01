#' @title aqs_qa_annualpeferomanceeval_by_site
#' @description \lifecycle{deprecated}
#'              aqs_qa_annualpeferomanceeval_by* functions have been renamed to
#'              aqs_qa_annualperformance* functions, please use these
#'              functions instead.
#'
#' @family deprecated RAQSAPI functions
#' @importFrom rlang abort
#' @export
#' @rdname deprecated
aqs_qa_annualpeferomanceeval_by_site <- function()
{
  abort(message = "aqs_qa_annualpeferomanceeval_by* functions have been renamed
                   to aqs_qa_annualperformance* functions, please use these
                   functions instead."
        )
}


#' @title aqs_qa_annualpeferomanceeval_by_county
#' @description \lifecycle{deprecated}
#'              aqs_qa_annualpeferomanceeval_by* functions have been renamed to
#'              aqs_qa_annualperformance* functions, please use these
#'              functions instead.
#'
#' @family deprecated RAQSAPI functions
#' @importFrom rlang abort
#' @export
#' @rdname deprecated
aqs_qa_annualpeferomanceeval_by_county <- function()
{
  abort(message = "aqs_qa_annualpeferomanceeval_by* functions have been renamed
                   to aqs_qa_annualperformance* functions, please use these
                   functions instead."
        )
}

#' @title aqs_qa_annualpeferomanceeval_by_state
#' @description \lifecycle{deprecated}
#'              aqs_qa_annualpeferomanceeval_by* functions have been renamed to
#'              aqs_qa_annualperformance* functions, please use these functions
#'              instead.
#'
#' @family deprecated RAQSAPI functions
#' @importFrom rlang abort
#' @export
#' @rdname deprecated
aqs_qa_annualpeferomanceeval_by_state <- function()
{
  abort(message = "aqs_qa_annualpeferomanceeval_by* functions have been renamed
                   to aqs_qa_annualperformance_by* functions, please use these
                   functions instead."
        )
}

#' @title aqs_qa_annualpeferomanceeval_by_pqao
#' @description \lifecycle{deprecated}
#'              aqs_qa_annualpeferomanceeval_by* functions have been renamed to
#'              aqs_qa_annualperformance_by* functions, please use these
#'              functions instead.
#'
#' @family deprecated RAQSAPI functions
#' @importFrom rlang abort
#' @export
#' @rdname deprecated
aqs_qa_annualpeferomanceeval_by_pqao <- function()
{
  abort(message = "aqs_qa_annualpeferomanceeval_by* functions have been renamed
                   to aqs_qa_annualperformance_by* functions, please use these
                   functions instead."
        )
}


#' @title aqs_qa_annualpeferomanceeval_by_MA
#' @description \lifecycle{deprecated}
#'              aqs_qa_annualpeferomanceeval_by* functions have been renamed
#'              to
#'              aqs_qa_annualperformance_by* functions, please use these
#'              functions instead.
#'
#' @family deprecated RAQSAPI functions
#' @importFrom rlang abort
#' @export
#' @rdname deprecated
aqs_qa_annualpeferomanceeval_by_MA <- function()
{
   abort(message = "aqs_qa_annualpeferomanceeval_by* functions have been
                  renamed to aqs_qa_annualperformance* functions, please use
                    these functions instead."
         )
}

#' @title aqs_qa_annualpeferomanceevaltransaction_by_site
#' @description \lifecycle{deprecated}
#'              aqs_qa_annualpeferomanceeval_by* functions have been renamed to
#'              aqs_qa_annualperformance_by* functions, please use these
#'              functions instead.
#'
#' @family deprecated RAQSAPI functions
#' @importFrom rlang abort
#' @export
#' @rdname deprecated
aqs_qa_annualpeferomanceevaltransaction_by_site <- function()
{
  abort(message = "aqs_qa_annualpeferomanceevaltransaction_by* functions have
                   been renamed to aqs_qa_annualperformancetransaction_by*
                   functions, please use these functions instead.")
}


#' @title aqs_qa_annualpeferomanceevaltransaction_by_county
#' @description \lifecycle{deprecated}
#'              aqs_qa_annualpeferomanceeval_by* functions have been renamed to
#'              aqs_qa_annualperformance_by* functions, please use these
#'              functions instead.
#'
#' @family deprecated RAQSAPI functions
#' @importFrom rlang abort
#' @export
#' @rdname deprecated
aqs_qa_annualpeferomanceevaltransaction_by_county <- function()
{
  abort(message = "aqs_qa_annualpeferomanceevaltransaction_by* functions have
                   been renamed to aqs_qa_annualperformancetransaction_by*
                   functions, please use these functions instead.")
}


#' @title aqs_qa_annualpeferomanceevaltransaction_by_pqao
#' @description \lifecycle{deprecated}
#'              aqs_qa_annualpeferomanceeval_by* functions have been renamed to
#'              aqs_qa_annualperformance_by* functions, please use these
#'              functions instead.
#'
#' @family deprecated RAQSAPI functions
#' @importFrom rlang abort
#' @export
#' @rdname deprecated
aqs_qa_annualpeferomanceevaltransaction_by_pqao <- function()
{
  abort(message = "aqs_qa_annualpeferomanceevaltransaction_by* functions have
                   been renamed to aqs_qa_annualperformancetransaction_by*
                   functions, please use these functions instead.")
}

#' @title aqs_qa_annualpeferomanceevaltransaction_by_MA
#' @description \lifecycle{deprecated}
#'              aqs_qa_annualpeferomanceeval_by* functions have been renamed to
#'              aqs_qa_annualperformance_by* functions, please use these
#'              functions instead.
#'
#' @family deprecated RAQSAPI functions
#' @importFrom rlang abort
#' @export
#' @rdname deprecated
aqs_qa_annualpeferomanceevaltransaction_by_MA <- function()
{
  abort(message = "aqs_qa_annualpeferomanceevaltransaction_by* functions have
                   been renamed to aqs_qa_annualperformancetransaction_by*
                   functions, please use these functions instead.")
}


#' @title aqs_ratelimit
#' @description \lifecycle{depricated}
#' @description a helper function that should not be called externally, used
#'                 as a primitive rate limit function for aqs.
#' @param waittime the number of seconds, encoded as a numeric, that the API
#'                     should wait after performing a API query
#'                     (defaults to 5 seconds, as recommended by the AQS team).
#' @note  Although this function is designed to prevent users from exceeding
#'        allowed data limits, it can not garuntee that the user exceed rate
#'        limits. Users are advised to monitor their own usage to ensure that
#'        data limits are not exceeded. Use of this package is at the users own
#'        risk. The maintainers of this code assume no responsibility due to
#'        anything that may happen as a result of using this code.
#' @return NULL
#' @noRd
aqs_ratelimit <- function(waittime=5L)
{
  Sys.sleep(waittime)
}
