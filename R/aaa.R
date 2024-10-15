
ttx_state <- new.env()

get <- function(name) {
    base::get0(name, envir=ttx_state, inherits=FALSE)
}

mget <- function(names) {
    base::mget(names, envir=ttx_state, inherits=FALSE)
}

set <- function(name, value) {
    assign(name, value, envir=ttx_state)
}




