const gulp = require('gulp')
const shell = require('gulp-shell')

gulp.task('eunit', shell.task([
  'rebar3 eunit'
]))

gulp.task('watch', function () {
  gulp.watch('./src/**/*.erl', ['eunit'])
  gulp.watch('./src/**/*.hrl', ['eunit'])
  gulp.watch('./test/**/*.erl', ['eunit'])
})
