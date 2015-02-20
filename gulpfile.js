// Generated on 2015-02-18 using generator-jekyllized 0.7.1
'use strict';

var gulp = require('gulp');
// Loads the plugins without having to list all of them, but you need
// to call them as $.pluginname
var $ = require('gulp-load-plugins')();
// 'fs' is used to work with files and such
var fs = require('fs');
// 'path' is used to work with filenames and such
var path = require('path');
// 'del' is used to clean out directories and such
var del = require('del');
// BrowserSync isn't a gulp package, and needs to be loaded manually
var browserSync = require('browser-sync');
// merge is used to merge the output from two different streams into the same stream
var merge = require('merge-stream');
// Need a command for reloading webpages using BrowserSync
var reload = browserSync.reload;
// And define a variable that BrowserSync uses in it's function
var bs;

// Deletes the directory that is used to serve the site during development
gulp.task('clean:dev', del.bind(null, ['serve']));

// Deletes the directory that the optimized site is output to
gulp.task('clean:prod', del.bind(null, ['site']));

// Runs the bundler update command to update Jekyll
gulp.task('update:jekyll', $.shell.task('bundle update'));

// Runs the npm update command to update required npm packages
gulp.task('update:npm', $.shell.task('npm update'));

// Runs the bower update command to update required Bower packages
gulp.task('update:bower', ['update:npm'], $.shell.task('bower update'));

// TODO: Write some tasks to quickly create/undraft/publish/delete posts
// Creates a new draft post and promts you for a title and optional cantegories
gulp.task('jekyll:post', function () {
  var title = null,
      titleSlug = null,
      date = new Date(),
      dateSlug = [
        date.getUTCFullYear(),
        (date.getUTCMonth() + 1) < 10 ? '0' + (date.getUTCMonth() + 1) : (date.getUTCMonth() + 1),
        date.getDate() < 10 ? '0' + date.getDate() : date.getDate()],
      categoriesType = 'category',
      categories = '';

  // Prompt for the new post's title and categories
  return gulp.src('templates/post.md')
    .pipe($.prompt.prompt({
      type: 'input',
      name: 'title',
      message: 'Post title:'
    }, function (response) {
      // Sanitize title
      title = response.title.trim();
      titleSlug = title.toLowerCase()
        //.replace(/[\.,\/#!$%\^&\*;:{}\[\]=_`~()]/g, '')
        .replace(/\s+/g, '-').trim();
    }))
    // Prompt for the new post's category/categories
    .pipe($.prompt.prompt({
      type: 'input',
      name: 'categories',
      message: 'Post category or categories (space or comma separated):'
    }, function (response) {
      // Sanitize categories
      categories = response.categories.trim();
      if (categories.indexOf(',') !== -1) {
        categoriesType = 'categories';
        categories = categories.replace(/\s+,\s+/g, ' ');
      }
      if (categories.indexOf('[') !== -1 || categories.indexOf(']') !== -1) {
        categoriesType = 'categories';
        categories = categories.replace(/\s+[\[\]]\s+/g, '');
      }
      categories = categories.trim();

      if (categories.length === 0) {
        categories = 'uncategorized';
      }

      console.log(title + ' (' + titleSlug + ')');

      // Render the new post template
      gulp.src('templates/post.md')
      .pipe($.mustache({
        title: title,
        titleSlug: titleSlug,
        date: dateSlug.join('-'),
        categoriesType: categoriesType,
        categories: categories
      }))
      .pipe($.rename(dateSlug.join('-') + '-' + titleSlug + '.md'))
      .pipe(gulp.dest('src/_drafts/'));
    }));
});

// Moves a post chosen by you to _posts, thereby publishing it
gulp.task('jekyll:publish', function () {
  var posts = [],
      draftsDir = 'src/_drafts/',
      titleRegEx = /title:\s+(.*)\s+/;

  // Get metadata for all draft posts
  fs.readdirSync(draftsDir).forEach(function (post) {
    var title = titleRegEx.exec(fs.readFileSync(path.join(draftsDir, post)))[1];
    posts.push({
      path: post,
      title: (title === undefined) ? path.basename(post) : title
    });
  });
  if (posts.length === 0) {
    console.log('There are no drafted posts to publish.');
    return;
  }

  return gulp.src(draftsDir + '*')
    .pipe($.prompt.prompt({
        type: 'list',
        name: 'post',
        message: 'Select a post to publish:',
        choices: posts.map(function (post, index) { return {name: post.title, value: index}; })
    }, function (response) {
      var post = posts[response.post];

      console.log('Publishing ' + post.title + ' (' + post.path + ')');

      // Copy the post to _posts, publishing it
      gulp.src(path.join(draftsDir, post.path))
        .pipe(gulp.dest('src/_posts/'))
        .on('end', function () {
          // Delete draft post
          del([path.join(draftsDir, post.path)]);
        });
    }));
});

// Runs the build command for Jekyll to compile the site locally
// This will build the site with the production settings
gulp.task('jekyll:dev', $.shell.task('jekyll build'));
gulp.task('jekyll-rebuild', ['jekyll:dev'], function () {
  reload();
});

// Almost identical to the above task, but instead we load in the build configuration
// that overwrites some of the settings in the regular configuration so that you
// don't end up publishing your drafts or future posts
gulp.task('jekyll:prod', $.shell.task('jekyll build --config _config.yml,_config.build.yml'));

// Compiles the SASS files and moves them into the 'assets/stylesheets' directory
gulp.task('styles', function () {
  // Looks at the style.scss file for what to include and creates a style.css file
  return gulp.src('src/assets/scss/style.scss')
    .pipe($.sass())
    // AutoPrefix your CSS so it works between browsers
    .pipe($.autoprefixer('last 1 version', { cascade: true }))
    // Directory your CSS file goes to
    .pipe(gulp.dest('src/assets/stylesheets/'))
    .pipe(gulp.dest('serve/assets/stylesheets/'))
    // Outputs the size of the CSS file
    .pipe($.size({title: 'styles'}))
    // Injects the CSS changes to your browser since Jekyll doesn't rebuild the CSS
    .pipe(reload({stream: true}));
});

// Optimizes the images that exist
gulp.task('images', function () {
  return gulp.src('src/assets/images/**')
    .pipe($.changed('site/assets/images'))
    .pipe($.imagemin({
      // Lossless conversion to progressive JPGs
      progressive: true,
      // Interlace GIFs for progressive rendering
      interlaced: true
    }))
    .pipe(gulp.dest('site/assets/images'))
    .pipe($.size({title: 'images'}));
});

// Optimizes the icons that exist
gulp.task('icons', function () {
  return gulp.src('src/assets/icons/**')
    .pipe($.changed('site/assets/icons'))
    .pipe($.imagemin({
      // Lossless conversion to progressive JPGs
      progressive: true,
      // Interlace GIFs for progressive rendering
      interlaced: true
    }))
    .pipe(gulp.dest('site/assets/icons'))
    .pipe($.size({title: 'icons'}));
});

// Copy over fonts to the "site" directory
gulp.task('fonts', function () {
  return gulp.src('src/assets/fonts/**')
    .pipe(gulp.dest('site/assets/fonts'))
    .pipe($.size({ title: 'fonts' }));
});

// Copy special files to the "site" directory
gulp.task('copy', function () {
  return gulp.src(['serve/*.txt', 'serve/*.xml', 'README.md', 'CNAME'])
    .pipe(gulp.dest('site'))
    .pipe($.size({ title: 'special files' }));
});

// Optimizes all the CSS, HTML and concats the JS etc
gulp.task('html', ['styles'], function () {
  var assets = $.useref.assets({searchPath: 'serve'});

  return gulp.src('serve/**/*.html')
    .pipe(assets)
    // Concatenate JavaScript files and preserve important comments
    .pipe($.if('*.js', $.uglify({preserveComments: 'some'})))
    // Minify CSS
    .pipe($.if('*.css', $.minifyCss()))
    // Start cache busting the files
    .pipe($.revAll({ ignore: ['.eot', '.svg', '.ttf', '.woff'] }))
    .pipe(assets.restore())
    // Conctenate your files based on what you specified in _layout/header.html
    .pipe($.useref())
    // Replace the asset names with their cache busted names
    .pipe($.revReplace())
    // Minify HTML
    .pipe($.if('*.html', $.htmlmin({
      removeComments: true,
      removeCommentsFromCDATA: true,
      removeCDATASectionsFromCDATA: true,
      collapseWhitespace: true,
      collapseBooleanAttributes: true,
      removeAttributeQuotes: true,
      removeRedundantAttributes: true
    })))
    // Send the output to the correct folder
    .pipe(gulp.dest('site'))
    .pipe($.size({title: 'optimizations'}));
});


// Task to upload your site to your personal GH Pages repo
gulp.task('deploy', function () {
  // Deploys your optimized site, you can change the settings in the html task if you want to
  return gulp.src('./site/**/*')
    .pipe($.ghPages({
      // Currently only personal GitHub Pages are supported so it will upload to the master
      // branch and automatically overwrite anything that is in the directory
      branch: 'master'
      }));
});

// Run JS Lint against your JS
gulp.task('jslint', function () {
  gulp.src('./serve/assets/javascript/*.js')
    // Checks your JS code quality against your .jshintrc file
    .pipe($.jshint('.jshintrc'))
    .pipe($.jshint.reporter());
});

// Runs "jekyll doctor" on your site to check for errors with your configuration
// and will check for URL errors a well
gulp.task('doctor', $.shell.task('jekyll doctor'));

// BrowserSync will serve our site on a local server for us and other devices to use
// It will also autoreload across all devices as well as keep the viewport synchronized
// between them.
gulp.task('serve:dev', ['styles', 'jekyll:dev'], function () {
  bs = browserSync({
    notify: true,
    // tunnel: '',
    server: {
      baseDir: 'serve'
    }
  });
});

// These tasks will look for files that change while serving and will auto-regenerate or
// reload the website accordingly. Update or add other files you need to be watched.
gulp.task('watch', function () {
  gulp.watch(['_config.*.yml', 'src/**/*.md', 'src/**/*.html', 'src/**/*.xml',
     'src/**/*.txt', 'src/**/*.js', 'src/**/*.yml'], ['jekyll-rebuild']);
  gulp.watch(['serve/assets/stylesheets/*.css'], reload);
  gulp.watch(['src/assets/scss/**/*.scss'], ['styles']);
});

// Serve the site after optimizations to see that everything looks fine
gulp.task('serve:prod', function () {
  bs = browserSync({
    notify: false,
    // tunnel: true,
    server: {
      baseDir: 'site'
    }
  });
});

// Default task, run when just writing "gulp" in the terminal
gulp.task('default', ['serve:dev', 'watch']);

// Perform all updates
gulp.task('update', ['update:jekyll', 'update:npm', 'update:bower']);

// Perform all cleansing
gulp.task('clean', ['clean:dev', 'clean:prod']);

// Checks your CSS, JS and Jekyll for errors
gulp.task('check', ['jslint', 'doctor'], function () {
  // Better hope nothing is wrong.
});

// Builds the site but doesn't serve it to you
gulp.task('build', ['jekyll:prod', 'styles'], function () {});

// Builds your site with the "build" command and then runs all the optimizations on
// it and outputs it to "./site"
gulp.task('publish', ['build'], function () {
  gulp.start('html', 'copy', 'images', 'icons', 'fonts');
});
