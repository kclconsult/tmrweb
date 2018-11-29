const express = require('express');
const router = express.Router();
const request = require('request');

const utils = require('../lib/utils');

router.post('/interactions', function(req, res, next) {

  var postData = require('querystring').stringify({
      'guideline_group_id' : req.body.guideline_group_id,
  });

  utils.callPrologServer("interactions", postData, res);

});

router.post('/drug', function(req, res, next) {

  var postData = require('querystring').stringify({
      'guideline_group_id' : `CIG-` + req.body.guideline_group_id,
      'guideline_id' : req.body.guideline_id
  });

  utils.callPrologServer("drug", postData, res);

});

module.exports = router;
