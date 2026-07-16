# Validaciones generales de parámetros
# Proyecto: temporal-discounting-lab
#
# Estas funciones no calculan modelos. Únicamente detienen la ejecución
# cuando una entrada no cumple las condiciones matemáticas esperadas.

assert_numeric_finite <- function(x, name = deparse(substitute(x))) {
  if (!is.numeric(x) || length(x) == 0L) {
    stop(sprintf("`%s` debe ser un vector numérico no vacío.", name),
         call. = FALSE)
  }

  if (anyNA(x) || any(!is.finite(x))) {
    stop(sprintf("`%s` no puede contener NA, NaN o valores infinitos.", name),
         call. = FALSE)
  }

  invisible(TRUE)
}

validate_nonnegative <- function(x, name = deparse(substitute(x))) {
  assert_numeric_finite(x, name)

  if (any(x < 0)) {
    stop(sprintf("`%s` debe contener valores mayores o iguales que 0.", name),
         call. = FALSE)
  }

  invisible(TRUE)
}

validate_positive <- function(x, name = deparse(substitute(x))) {
  assert_numeric_finite(x, name)

  if (any(x <= 0)) {
    stop(sprintf("`%s` debe contener valores estrictamente mayores que 0.", name),
         call. = FALSE)
  }

  invisible(TRUE)
}

validate_unit_interval <- function(
    x,
    name = deparse(substitute(x)),
    include_zero = TRUE,
    include_one = TRUE
) {
  assert_numeric_finite(x, name)

  lower_invalid <- if (include_zero) x < 0 else x <= 0
  upper_invalid <- if (include_one) x > 1 else x >= 1

  if (any(lower_invalid | upper_invalid)) {
    left_bracket <- if (include_zero) "[" else "("
    right_bracket <- if (include_one) "]" else ")"

    stop(
      sprintf(
        "`%s` debe pertenecer al intervalo %s0, 1%s.",
        name,
        left_bracket,
        right_bracket
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_time <- function(time, name = "time") {
  validate_nonnegative(time, name)
  invisible(TRUE)
}

validate_delta <- function(delta, name = "delta") {
  if (length(delta) != 1L) {
    stop(sprintf("`%s` debe ser un único valor.", name), call. = FALSE)
  }

  validate_unit_interval(
    delta,
    name = name,
    include_zero = FALSE,
    include_one = TRUE
  )

  invisible(TRUE)
}

validate_beta <- function(beta, name = "beta") {
  if (length(beta) != 1L) {
    stop(sprintf("`%s` debe ser un único valor.", name), call. = FALSE)
  }

  validate_unit_interval(
    beta,
    name = name,
    include_zero = FALSE,
    include_one = TRUE
  )

  invisible(TRUE)
}

validate_k <- function(k, name = "k") {
  if (length(k) != 1L) {
    stop(sprintf("`%s` debe ser un único valor.", name), call. = FALSE)
  }

  validate_nonnegative(k, name)
  invisible(TRUE)
}

validate_amount <- function(amount, name = "amount") {
  validate_nonnegative(amount, name)
  invisible(TRUE)
}
