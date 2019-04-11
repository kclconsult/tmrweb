const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

router.post('/get', function(req, res, next) {

  utils.sparqlGraphInstanceOf("beliefs", "<http://anonymous.org/vocab/CausationBelief>", function(uris) {

    res.send(uris);

  });

});

module.exports = router;
