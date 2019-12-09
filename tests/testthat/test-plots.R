context("plot output functions")

library('ggplot2')

# clear plots from last run
for (suffix in c('.eps', '.svg', '.png', '.pdf')) {
  if (file.exists(paste0('plot1', suffix))) {
    file.remove(paste0('plot1', suffix))
  }
}

plot_list <- list()
for (index in 1:3) {
  plot_list <- add_to_plot_list(plot_list,
                                ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)),
                                filename = paste0('plot', index, '.eps'))
}
for (index in 1:3) {
  plot_list <- add_to_plot_list(plot_list,
                                ggplot(data = mpg) + geom_point(mapping = aes(x = drv, y = hwy)),
                                filename = paste0('plot', index, '.svg'))
}
for (index in 1:3) {
  plot_list <- add_to_plot_list(plot_list,
                                ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = cty)),
                                filename = paste0('plot', index, '.png'))
}
for (index in 1:3) {
  plot_list <- add_to_plot_list(plot_list,
                                ggplot(data = mpg) + geom_point(mapping = aes(x = year, y = cty)),
                                filename = paste0('plot', index, '.pdf'))
}
test_that("add_to_plot_list works", {
  expect_length(plot_list, 12)
  expect_equal(plot_list[[1]]$filename, 'plot1.eps')
  expect_equal(plot_list[[4]]$filename, 'plot1.svg')
  expect_equal(plot_list[[7]]$filename, 'plot1.png')
  expect_equal(plot_list[[10]]$filename, 'plot1.pdf')
})

for(index in c(1,4,7,10)) {
  output_plot(plot_list[[index]])
}
test_that("output_plot works", {
  expect_true(file.exists('plot1.eps'))
  expect_true(file.exists('plot1.svg'))
  expect_true(file.exists('plot1.png'))
  expect_true(file.exists('plot1.pdf'))
})

