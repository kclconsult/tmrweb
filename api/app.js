var createError = require('http-errors');
var express = require('express');
var path = require('path');
var cookieParser = require('cookie-parser');
var logger = require('morgan');

// Environment variables
require('dotenv').config()

var guidelineRouter = require('./routes/guideline');
var drugRouter = require('./routes/drug');
var beliefRouter = require('./routes/belief');
var transitionRouter = require('./routes/transition');
var guidelinesRouter = require('./routes/guidelines');
var drugsRouter = require('./routes/drugs');
var beliefsRouter = require('./routes/beliefs');
var transitionsRouter = require('./routes/transitions');

var app = express();

// view engine setup
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'pug');

app.use(logger('dev'));
app.use(express.json());
app.use(express.urlencoded({ extended: false }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));

app.get('/', function(req, res, next) {

  res.end();

});

app.use('/guideline', guidelineRouter);
app.use('/drug', drugRouter);
app.use('/belief', beliefRouter);
app.use('/transition', transitionRouter);
app.use('/guidelines', guidelinesRouter);
app.use('/drugs', drugsRouter);
app.use('/beliefs', beliefsRouter);
app.use('/transitions', transitionsRouter);

// catch 404 and forward to error handler
app.use(function(req, res, next) {
  next(createError(404));
});

// error handler
app.use(function(err, req, res, next) {
  // set locals, only providing error in development
  res.locals.message = err.message;
  res.locals.error = req.app.get('env') === 'development' ? err : {};

  // render the error page
  res.status(err.status || 500);
  res.render('error');
});

module.exports = app;
