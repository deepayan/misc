

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
    str(list(fullToken = fullToken, ...))
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

tryToEval <- function(s)
{
    if (s == .lastEvalAttempt$text) return(.lastEvalAttempt$object)
    .lastEvalAttempt$text <- s
    .lastEvalAttempt$object <- tryCatch(eval(str2expression(s), envir = .GlobalEnv), error = function(e) NULL)
    .lastEvalAttempt$object
}

findCompletions <- function(token, values) # should eventually export this from utils
{
    utils:::findMatches(sprintf("^%s", utils:::makeRegexpSafe(token)), values)
}

## Do something if we are inside with() etc. We may be deep inside, so
## without a parse tree, we can only try to do string matching. The
## actual function we are immediately inside is not relevant.

## List of known NSE functions where evaluating inside the first
## argument is appropriate

NSE1 <- c("with", "within", "subset", "transform", "mutate",
          "dplyr::mutate", "filter", "dplyr::filter", "select",
          "dplyr::select")

NSE1CompleterPipe <- function(token)
{
    rcs <- rc.status()
    if (rcs$start == 0) return(character(0))
    linebuffer <- with(rcs, substring(linebuffer, 1, start)) # ignore anything afterwards
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


NSE1CompleterStandard <- function(token)
{
    rcs <- rc.status()
    if (rcs$start == 0) return(character(0))
    linebuffer <- with(rcs, substring(linebuffer, 1, start)) # ignore anything afterwards
    ## simple breakup: just { whitespace, '(', ',' }
    words <- strsplit(linebuffer, split = "[[:space:],(]+")[[1]]
    if (length(ww <- which(words %in% NSE1)))
    {
        ## We want to find completions in the 'object' that
        ## immediately follows the NSE function. If there are multiple
        ## such functions, we will use the last one.  We could
        ## alternatively loop through all occurrences and accumulate,
        ## but that's unlikely to be very useful.

        w <- ww[length(ww)]
        if (w == length(words))
        {
            object <- NULL
        }
        else
        {
            object <- tryToEval(words[w + 1])
            ## This will fail if the actual object (first argument of
            ## NSE function) is a complex expression (like a function
            ## call) that was broken up by strsplit() above. A more
            ## sophisticated option could be to look forward to first
            ## comma, second comma, ... until we find something that
            ## evaluates to non-NULL
        }
        validNames <- .DollarNames(object)
        return(findCompletions(token, validNames))
    }
    else return(character(0))
}




