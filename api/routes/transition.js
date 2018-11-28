const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

function postTransition(transitionData, res) {

  utils.sparqlUpdate('transitions', transitionData, function(sparqlUpdate, error, response, body) {

    if (!error && response.statusCode == 200) {

      console.log(body);

    } else {

      console.log(sparqlUpdate);
      console.log(response.body);

    }

    res.end();

  });

}

router.post('/add', function(req, res, next) {

  const transition = `:Tr` + req.body.transition_id + ` rdf:type vocab:TransitionType, owl:NamedIndividual ;
                  vocab:hasTransformableSituation :Sit` + req.body.prior_situation_id + ` ;
                  vocab:hasExpectedSituation :Sit` + req.body.post_situation_id + ` .`

  postTransition(transition, res);

});

router.post('/situation/add', function(req, res, next) {

  var transitionSituation = `:Sit` + req.body.situation_id + ` rdf:type vocab:SituationType, owl:NamedIndividual;
              rdfs:label "` + req.body.situation_label + `"@en `;

  if ( req.body.umlsCodes ) {

    transitionSituation += `;`;

    req.body.umlsCodes.split(",").forEach(function(code) {

      transitionSituation += `
      vocab:umlsCode   "` + code.trim() + `"^^xsd:string ;`

    });

    transitionSituation = transitionSituation.substring(0, transitionSituation.length - 1);

  }

  transitionSituation += `.`

  postTransition(transitionSituation, res);

});

module.exports = router;
