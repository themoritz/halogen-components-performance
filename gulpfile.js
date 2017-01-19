var gulp            = require("gulp");
var purescript      = require("gulp-purescript");
var browserify      = require("browserify");
var vinyl           = require("vinyl-source-stream")

// Purescript

var sources = [
  "src/**/*.purs",
  "test/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs",
];

var foreigns = [
  "src/**/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("make", function () {
  return purescript.psc({
    src: sources,
    ffi: foreigns,
    output: "output",
    verboseErrors: false
  });
});

gulp.task("bundle", ["make"], function () {
  return purescript.pscBundle({
    src: "output/**/*.js",
    output: "dist/main.js",
    main: "Main"
  });
});

gulp.task("browserify", ["bundle"], function () {
  return browserify("dist/main.js")
    .bundle()
    .pipe(vinyl("main.js"))
    .pipe(gulp.dest("public/js"));
});

// Main tasks

gulp.task("default", ["browserify"]);
