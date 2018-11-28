const express = require('express');
const router = express.Router();
const request = require('request');

const utils = require('../lib/utils');

router.post('/effects', function(req, res, next) {

  var postData = require('querystring').stringify({
      'drug_full_id' : req.body.drug_full_id
  });

  utils.callPrologServer("drugeffects", postData, res);

});

module.exports = router;
