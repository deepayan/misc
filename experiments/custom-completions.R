

## Proof-of-concept implementation of completion for NSE functions.


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
    linebuffer <- rc.status()$linebuffer
    ## simple breakup: just { whitespace, '(', ',' }
    words <- strsplit(linebuffer, split = "[[:space:],(]+")[[1]]
    if (length(wwith <- which(words %in% NSE1)))
    {
        ## Find completions in 'object' immediately following the NSE
        ## function (use last one if many). We could alternatively
        ## loop through all occurrences and accumulate, but that's
        ## unlikely to be very useful.

        ## This will fail if the actual object (first argument of NSE
        ## function) is a complex expression (like a function call)
        ## that was broken up by strsplit() above
        withWhat <- tryToEval(words[wwith[length(wwith)] + 1])
        validNames <- .DollarNames(withWhat)
        return(findCompletions(token, validNames))
    }
    else return(character(0))
}

## Supporting pipelines would need more work, but should be possible
## if the complete LHS is available and can be evaluated. The NSE1
## function should then be immediately preceded by the pipeline
## operator, and everything to the left of that should be evaluated to
## get the object to look in.



