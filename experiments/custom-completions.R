
## Proof-of-concept implementation of completion for NSE functions.
## source() this file to use.

custom.argument.completer <- function(fun, text, ...)
{
    ans <- NSE1CompleterPipe(text)
    if (!length(ans)) ans <- NSE1CompleterStandard(text)
    ans
}

custom.quote.completer <- function(fullToken, ...)
{
    ## str(list(fullToken = fullToken, ...))
    ## TODO
    character(0)
}


rc.options(custom.quote.completer = custom.quote.completer,
           custom.argument.completer = custom.argument.completer)

if (FALSE)
{
    ## with(mtcars, mean(dis<TAB>)) # should work
    ## with(mtcars, mean(1:10, na.rm = dis<TAB>)) # should also work, though not argument
    ## with(subset(mtcars, cyl == 10), dis) # ??
    fm <- lm(disp ~ mpg, mtcars)
    ## with(fm, mean(xle))
}


## Add some cache-ing: the same string should not be evaluated
## consecutively.

.lastEvalAttempt <- new.env(parent = emptyenv())
.lastEvalAttempt$text <- ""

tryToEval <- function(s)
{
    if (s != .lastEvalAttempt$text) {
        .lastEvalAttempt$text <- s
        .lastEvalAttempt$object <-
            tryCatch(eval(str2expression(s),
                          envir = .GlobalEnv), error = function(e) NULL)
    }
    return(.lastEvalAttempt$object)
}

findCompletions <- function(token, values)
{
    ## should eventually export this from utils. Takes care of 'fuzzy'
    ## matching if enabled.
    utils:::findMatches(sprintf("^%s", utils:::makeRegexpSafe(token)), values)
}

## Do something if we are inside with() etc. We may be deep inside, so
## without a parse tree, we can only try to do string matching. The
## actual function we are immediately inside is not relevant.

## List of "known" NSE functions where evaluating inside the _first_
## argument is appropriate. Should match with how breakRE is defined
## below.

NSE1 <- c("with", "within", "subset", "transform", "mutate", "filter",
          "select",
          ## "dplyr::mutate", "dplyr::filter", "dplyr::select",
          "xyplot")

## NOTE: xyplot() shouln't really be on this list, but allows us to
## complete xyplot(data = faithful, eruptions ~ waiting), although it
## also allows NSE completions in other arguments, which it
## shouldn't. Would be nice to be able to handle formula-interface
## functions in a systematic manner.



NSE1CompleterPipe <- function(token)
{
    rcs <- rc.status()
    if (rcs$start == 0) return(character(0))
    linebuffer <- with(rcs, substring(linebuffer, 1, start)) # ignore anything afterwards
    .lastEvalAttempt$linebuffer <- linebuffer
    ## break up by pipeline operators: "|>" and "%>%" for now
    mpipe.words <- strsplit(linebuffer, split = "%>%", fixed = TRUE)[[1]]
    bpipe.words <- strsplit(linebuffer, split = "|>", fixed = TRUE)[[1]]
    ## str(list(m = mpipe.words, b = bpipe.words))
    if (length(mpipe.words) == 1 && length(bpipe.words) == 1) ## no pipeline
        return(character(0))
    if (length(mpipe.words) == 1 || nchar(tail(mpipe.words, 1)) > nchar(tail(bpipe.words, 1)))
    {
        pipe <- "|>"
        words <- bpipe.words
    }
    else 
    {
        pipe <- "%>%"
        words <- mpipe.words
    }

    ## We should actually check if we are inside a NSE function. This
    ## is simple: break the last part using comma, and take the first
    ## part
    FUN <- trimws(strsplit(tail(words, 1), split = "(", fixed = TRUE)[[1]][1])
    if (FUN %in% NSE1)
    {
        ## Everything before last occurrence of pipe represents object.
        object <- tryToEval(paste(head(words, -1), collapse = pipe))
        validNames <- .DollarNames(object)
        return(findCompletions(token, validNames))
    }
    else return(character(0))
}


callingFunctions <- function(line, cursor)
{
    ## Want to analyze linebuffer upto cursor position, and figure out
    ## which functions we are currently inside. This is similar to
    ## utils:::inFunction(), which only gives the immediate calling
    ## function. This is a simplified version, and recursively calls
    ## itself to get a list of all calling functions

    ## Are we inside a function? Yes if the number of ( encountered
    ## going backwards exceeds number of ).  In that case, we would
    ## also like to know what functions we are currently inside

    parens <-
        sapply(c("(", ")"),
               function(s) gregexpr(s, substr(line, 1L, cursor), fixed = TRUE)[[1L]],
               simplify = FALSE)
    ## remove -1's
    parens <- lapply(parens, function(x) x[x > 0])

    ## The naive algo is as follows: set counter = 0; go backwards
    ## from cursor, set counter-- when a ) is encountered, and
    ## counter++ when a ( is encountered.  We are inside a function
    ## that starts at the first ( with counter > 0.

    temp <-
        data.frame(i = c(parens[["("]], parens[[")"]]),
                   c = rep.int(c(1, -1), lengths(parens)))
    if (nrow(temp) == 0) return(character())
    temp <- temp[order(-temp$i), , drop = FALSE] ## order backwards
    wp <- which(cumsum(temp$c) > 0)
    if (length(wp)) # inside a function
    {
        breakRE <- "[^\\.\\w]" # to identify function names
        index <- temp$i[wp[1L]]
        prefix <- substr(line, 1L, index - 1L)
        suffix <- substr(line, index + 1L, nchar(line)) # useful later
        ## guess function name
        possible <- suppressWarnings(strsplit(prefix, breakRE, perl = TRUE))[[1L]]
        if (length(possible) == 0)
            NULL
        else
        {
            possible <- tail(possible[nzchar(possible)], 1)
            ## recursively add further calling functions by shortening cursor
            rbind(data.frame(fun = possible, suffix = suffix),
                  callingFunctions(line, index - 1L))
        }
    }
    else NULL
}


firstObjectInString <- function(s)
{
    ## We want to use this function to identify the 'object' that
    ## immediately follows the name of an NSE function (if we are
    ## completing the first argument, then there is nothing to be
    ## done). The input is the part of the input line following the
    ## the NSE function and upto the start of the token.  For a
    ## non-trivial object to look inside to be present, we must have
    ## at least one comma in the string, and

    ## Algorithm: look forward for comma after the NSE
    ## function. Choose the first occurrence which successfully yields
    ## an evaluated object.

    commas <- gregexpr(",", s, fixed = TRUE)[[1L]]
    if (commas[1] == -1) return(NULL)
    for (i in commas)
    {
        object <- tryToEval(substr(s, 1L, i - 1L))
        if (!is.null(object)) return(object)
    }
    return(NULL)
}


NSE1CompleterStandard <- function(token)
{
    rcs <- rc.status()
    if (rcs$start == 0) return(character(0))
    linebuffer <- with(rcs, substring(linebuffer, 1, start)) # ignore anything afterwards
    .lastEvalAttempt$linebuffer <- linebuffer
    calling.funs <- callingFunctions(linebuffer, rcs$start)
    if (length(ww <- which(calling.funs$fun %in% NSE1)))
    {
        ## It's possible that there are multiple NSE functions in the
        ## calling chain. We loop through all of them and accumulate,
        ## because it's not difficult to do it, although it's unlikely
        ## to be very useful in practice.

        ans <- character(0)
        for (w in ww) # or ww[1] for just most recent one
        {
            FUN <- calling.funs[w, "fun"]       # NSE function
            SUFFIX <- calling.funs[w, "suffix"] # part after the NSE function
            object <- firstObjectInString(SUFFIX)
            validNames <- .DollarNames(object)
            ans <- c(ans, findCompletions(token, validNames))
        }
        return(ans)
    }
    else return(character(0))
}


## complete.partial <- function(.CompletionEnv)
## {
##     text <- .CompletionEnv[["token"]]
##     prefix <- substring(text, 1, .CompletionEnv$BEGIN-1)
##     text <- substring(text, .CompletionEnv$BEGIN)
##     comps <- apropos(text)
##     .CompletionEnv[["comps"]] <- paste0(prefix, comps)
## }

## complete.partial <- function(.CompletionEnv)
## {
##     .CompletionEnv[["comps"]] <- paste0("foo", sample(month.name, 5))
## }

## complete.partial <- function(.CompletionEnv)
## {
##     text <- .CompletionEnv[["token"]]
##     comps <- apropos(text)
##     .CompletionEnv[["comps"]] <- comps
## }

## rc.options(custom.completer = complete.partial)


