//
// Gulp is a task runner, it helps you automate things.
// In this case, we're going to use it so it can automatically
// update your JavaScript js files when you edit your TypeScript ts files.
// Read more about Gulp: http://gulpjs.com/
//
var gulp = require('gulp');

// This is a Gulp Plugin for TypeScript.
var typescript = require('gulp-tsc');

// This is the task for compiling our TypeScript source files and outputting them.
gulp.task('compile-typescript', function() {
	var typescriptPaths = {
		src: [
			'typescript/scripts/*.ts',
			'typescript/scripts/host/*.ts',
			'typescript/scripts/os/*.ts'
		],
		dest: 'dist/scripts/'
	};

	return gulp.src(typescriptPaths.src)
        .pipe(typescript({
        	emitError: false
        }))
        .pipe(gulp.dest(typescriptPaths.dest));
});

// This is the task for copying over our CSS to the dist directory.
// It really doesn't do much, but if we were to use a CSS preprocesser (like LESS/SASS),
// then we would build our LESS/SASS files and copy the resulting CSS to the dist folder.
// This is just to show some good front-end web development techniques.
gulp.task('copy-css', function() {
	var cssPaths = {
		src: ['typescript/styles/*.css'],
		dest: 'dist/styles/'
	};

	return gulp.src(cssPaths.src)
		.pipe(gulp.dest(cssPaths.dest));
});

// This is the default task that will run when we run `gulp` at the command line.
gulp.task('default', function() {
	gulp.watch('typescript/scripts/*.ts',      ['compile-typescript']);
	gulp.watch('typescript/scripts/host/*.ts', ['compile-typescript']);
	gulp.watch('typescript/scripts/os/*.ts',   ['compile-typescript']);
	gulp.watch('typescript/styles/*.css',      ['copy-css']);
});
