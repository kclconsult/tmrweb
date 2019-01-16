const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

router.post('/get', function(req, res, next) {

  utils.sparqlInstanceOf("transitions", "<http://anonymous.org/vocab/TransitionType>", function(sparqlQuery, error, response, body, uris) {

    res.send(uris);

  });

});

router.post('/situations/get', function(req, res, next) {

  utils.sparqlInstanceOf("transitions", "<http://anonymous.org/vocab/SituationType>", function(sparqlQuery, error, response, body, uris) {

    res.send(uris);

  });

});

module.exports = router;
