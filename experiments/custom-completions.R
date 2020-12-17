

## Proof-of-concept implementation of completion for NSE functions.
## source() this file to use.

custom.argument.completer <- function(fun, text, ...)
{
    c(NSE1Completer(text))
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
    
tryToEval <- function(s)
{
    tryCatch(eval(str2expression(s), envir = .GlobalEnv), error = function(e) NULL)
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

NSE1 <- c("with", "within", "subset", "transform", "mutate", "dplyr::mutate", "filter", "dplyr::filter")

NSE1Completer <- function(token)
{
    linebuffer <- with(rc.status(), substring(linebuffer, 1, end)) # ignore anything afterwards
    ## simple breakup: just { whitespace, '(', ',' }
    words <- strsplit(linebuffer, split = "[[:space:],(]+")[[1]]
    if (length(ww <- which(words %in% NSE1)))
    {
        ## Normally, we would want to find completions in the 'object'
        ## that immediately follows the NSE function. If there are
        ## multiple such functions, we will use the last one.  We
        ## could alternatively loop through all occurrences and
        ## accumulate, but that's unlikely to be very useful.

        ## However, if the NSE function is itself the last word, or if
        ## we are currently somewhere inside the first argument after
        ## it, then it's possible that we are using a pipeline
        ## operator. Checking that we are in the first argument is
        ## more complicated (go upto next comma and try to evaluate?
        ## and do that for all commas?), but we can easily handle the
        ## simple case

        w <- ww[length(ww)]
        if (w == length(words) || words[w + 1] == token)
        {
            ## Possibly pipeline. Verify first:
            if (words[w-1] %in% c("|>", "%>%"))
            {
                ## Need to get linebuffer upto pipeline operator.
                ## Split on it and join again leaving out last
                pipeOp <- words[w-1]
                pipeComps <- strsplit(linebuffer, split = pipeOp, fixed = TRUE)[[1]]
                object <- tryToEval( paste(pipeComps[-length(pipeComps)], collapse = pipeOp) )
            }
            else
                object <- NULL
        }
        else
        {
            ## This will fail if the actual object (first argument of NSE
            ## function) is a complex expression (like a function call)
            ## that was broken up by strsplit() above
            object <- tryToEval(words[w + 1])
        }
        validNames <- .DollarNames(object)
        return(findCompletions(token, validNames))
    }
    else return(character(0))
}



