# Pruebas unitarias de los modelos de descuento
#
# Ejecutar desde la raíz del repositorio:
# testthat::test_file("tests/unit/test-discount-models.R")

source(file.path("R", "utilities", "validacion_parametro.R"))
source(file.path("R", "discount", "discount_models.R"))

testthat::test_that("todos los modelos clásicos valen 1 en t = 0", {
  testthat::expect_equal(
    discount_exponential(time = 0, delta = 0.95),
    1
  )

  testthat::expect_equal(
    discount_hyperbolic(time = 0, k = 0.10),
    1
  )

  testthat::expect_equal(
    discount_quasi_hyperbolic(time = 0, beta = 0.80, delta = 0.95),
    1
  )
})

testthat::test_that("el modelo exponencial mantiene una razón constante", {
  delta <- 0.95
  time <- 0:10
  factors <- discount_exponential(time, delta)

  ratios <- factors[-1] / factors[-length(factors)]

  testthat::expect_equal(
    ratios,
    rep(delta, length(ratios))
  )
})

testthat::test_that("el modelo hiperbólico no mantiene una razón constante", {
  time <- 0:10
  factors <- discount_hyperbolic(time, k = 0.10)

  ratios <- factors[-1] / factors[-length(factors)]

  testthat::expect_false(
    isTRUE(all.equal(ratios, rep(ratios[1], length(ratios))))
  )
})

testthat::test_that("el cuasi-hiperbólico aplica beta solamente fuera del presente", {
  result <- discount_quasi_hyperbolic(
    time = c(0, 1, 2),
    beta = 0.80,
    delta = 0.95
  )

  expected <- c(
    1,
    0.80 * 0.95,
    0.80 * 0.95^2
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("el núcleo CTB coincide con el cuasi-hiperbólico en App A", {
  time <- 0:20

  testthat::expect_equal(
    discount_proposed_model(time, beta = 0.80, delta = 0.95),
    discount_quasi_hyperbolic(time, beta = 0.80, delta = 0.95)
  )
})

testthat::test_that("las fechas CTB se construyen con start_day más delay", {
  result <- ctb_discount_weights(
    start_day = 35,
    delay = 63,
    beta = 0.80,
    delta = 0.995
  )

  testthat::expect_equal(result$time, c(35, 98))
  testthat::expect_equal(nrow(result), 2L)
})

testthat::test_that("el valor subjetivo lineal multiplica monto por descuento", {
  testthat::expect_equal(
    subjective_value_linear(
      amount = 1200,
      discount_factor = 0.80
    ),
    960
  )
})

testthat::test_that("las validaciones rechazan parámetros inválidos", {
  testthat::expect_error(
    discount_exponential(time = -1, delta = 0.95)
  )

  testthat::expect_error(
    discount_exponential(time = 1, delta = 1.10)
  )

  testthat::expect_error(
    discount_hyperbolic(time = 1, k = -0.10)
  )

  testthat::expect_error(
    discount_quasi_hyperbolic(time = 1, beta = 0, delta = 0.95)
  )
})
