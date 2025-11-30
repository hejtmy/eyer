context("Preprocessing")

obj <- eyer_data

test_that("Tests changing resolution", {
  expect_silent(obj_normal <- change_resolution(obj, resolution = obj$info$resolution, target = list(width = 1, height = 1)))
  # TODO - more validations
})

test_that("Tests removing out of bounds data", {

})

test_that("Downsampling works", {
  eyer_d <- downsample(obj, 10)
  original_n <- nrow(obj$data$gaze)
  expect_equal(nrow(eyer_d$data$gaze), original_n / 10 - original_n %% 10)
})

test_that("flipping axis works", {
  expect_warning(obj_prep <- flip_axis(obj, "z", 5))
  expect_null(obj_prep)
  expect_silent(obj_prep <- flip_axis(obj, "y", 1080))
  expect_s3_class(obj_prep, "eyer")
  expect_silent(obj_prep <- flip_axis(obj, "x", 1920))
  expect_s3_class(obj_prep, "eyer")
})

test_that("recalibrating data works", {
  expect_silent(obj_prep <- recalibrate_eye_data(obj, c(100, 100)))
  expect_s3_class(obj_prep, "eyer")
  expect_gt(mean(obj$data$gaze$x, na.rm = TRUE), mean(obj_prep$data$gaze$x, na.rm = TRUE))
  expect_gt(mean(obj$data$gaze$y, na.rm = TRUE), mean(obj_prep$data$gaze$y, na.rm = TRUE))
  expect_silent(obj_prep <- recalibrate_eye_data(obj, c(-100, -100)))
  expect_s3_class(obj_prep, "eyer")
  expect_lt(mean(obj$data$gaze$x, na.rm = TRUE), mean(obj_prep$data$gaze$x, na.rm = TRUE))
  expect_lt(mean(obj$data$gaze$y, na.rm = TRUE), mean(obj_prep$data$gaze$y, na.rm = TRUE))

  expect_silent(obj_prep <- recalibrate_eye_data(obj, c(10, 10), times = c(0, 50)))
  expect_s3_class(obj_prep, "eyer")
  expect_gt(mean(obj$data$gaze$x, na.rm = TRUE), mean(obj_prep$data$gaze$x, na.rm = TRUE))
  expect_silent(obj_prep2 <- recalibrate_eye_data(obj, c(10, 10),
    times = c(0, 50),
    raw_times = F
  ))
  expect_equal(obj_prep, obj_prep2)
  expect_silent(obj_prep2 <- recalibrate_eye_data(obj, c(10, 10),
    times = c(
      obj$info$start_time,
      obj$info$start_time + 50
    ),
    raw_times = T
  ))
  expect_equal(obj_prep, obj_prep2)
})
