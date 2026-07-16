# Funciones de descuento temporal
# Proyecto: temporal-discounting-lab
#
# Requisito:
# source("R/utilities/validacion_parametro.R")
#
# Convención:
# - `time` se expresa en una sola unidad consistente: días, semanas, meses, etc.
# - `delta` es el factor de descuento por esa misma unidad.
# - `k` es la tasa hiperbólica por esa misma unidad.
# - Las funciones están vectorizadas: `time` puede ser un escalar o un vector.

discount_exponential <- function(time, delta) {
  validate_time(time)
  validate_delta(delta)

  delta ^ time
}

discount_hyperbolic <- function(time, k) {
  validate_time(time)
  validate_k(k)

  1 / (1 + k * time)
}

discount_quasi_hyperbolic <- function(time, beta, delta) {
  validate_time(time)
  validate_beta(beta)
  validate_delta(delta)

  ifelse(
    time == 0,
    1,
    beta * (delta ^ time)
  )
}

# Núcleo temporal del modelo CTB propuesto en la App A.
#
# En esta primera app no incorpora efecto de magnitud ni contraste.
# Por ello, su componente temporal coincide intencionalmente con el
# descuento cuasi-hiperbólico. La diferencia del modelo completo aparecerá
# al combinarlo con utilidad CRRA, restricción presupuestaria y una regla
# probabilística de elección.
discount_proposed_model <- function(time, beta, delta) {
  discount_quasi_hyperbolic(
    time = time,
    beta = beta,
    delta = delta
  )
}

# Factores temporales para las dos entregas de una decisión CTB.
#
# `start_day` es la fecha de la entrega temprana.
# `delay` es la distancia entre la entrega temprana y la tardía.
#
# Ejemplos:
# start_day = 0,  delay = 35  -> fechas 0 y 35
# start_day = 35, delay = 63  -> fechas 35 y 98
ctb_discount_weights <- function(start_day, delay, beta, delta) {
  validate_time(start_day, "start_day")
  validate_time(delay, "delay")
  validate_beta(beta)
  validate_delta(delta)

  if (length(start_day) != 1L || length(delay) != 1L) {
    stop(
      "`start_day` y `delay` deben ser valores escalares.",
      call. = FALSE
    )
  }

  early_time <- start_day
  later_time <- start_day + delay
  times <- c(early_time, later_time)

  factors <- discount_proposed_model(
    time = times,
    beta = beta,
    delta = delta
  )

  data.frame(
    moment = c("early", "later"),
    time = times,
    discount_factor = factors,
    stringsAsFactors = FALSE
  )
}

# Valor subjetivo bajo utilidad monetaria lineal.
#
# Esta función es útil para presentar los modelos clásicos antes de
# introducir una función de utilidad no lineal.
subjective_value_linear <- function(amount, discount_factor) {
  validate_amount(amount)
  validate_unit_interval(
    discount_factor,
    name = "discount_factor",
    include_zero = TRUE,
    include_one = TRUE
  )

  compatible_lengths <- (
    length(amount) == length(discount_factor) ||
      length(amount) == 1L ||
      length(discount_factor) == 1L
  )

  if (!compatible_lengths) {
    stop(
      "`amount` y `discount_factor` deben tener la misma longitud, o uno debe ser escalar.",
      call. = FALSE
    )
  }

  amount * discount_factor
}

# Construye una tabla comparativa lista para graficar.
discount_curve_data <- function(
    horizon,
    delta,
    k,
    beta,
    step = 1
) {
  validate_time(horizon, "horizon")
  validate_positive(step, "step")
  validate_delta(delta)
  validate_k(k)
  validate_beta(beta)

  if (length(horizon) != 1L || length(step) != 1L) {
    stop("`horizon` y `step` deben ser escalares.", call. = FALSE)
  }

  time <- seq(from = 0, to = horizon, by = step)

  data.frame(
    time = rep(time, times = 4L),
    model = factor(
      rep(
        c(
          "Exponencial",
          "Hiperbólico",
          "Cuasi-hiperbólico",
          "Modelo CTB propuesto: núcleo temporal"
        ),
        each = length(time)
      ),
      levels = c(
        "Exponencial",
        "Hiperbólico",
        "Cuasi-hiperbólico",
        "Modelo CTB propuesto: núcleo temporal"
      )
    ),
    discount_factor = c(
      discount_exponential(time, delta),
      discount_hyperbolic(time, k),
      discount_quasi_hyperbolic(time, beta, delta),
      discount_proposed_model(time, beta, delta)
    )
  )
}
