const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');

function updateDrugs() {

}

router.post('/individual/create', function(req, res, next) {

  // Individual drug format:
  const drug = `:` + req.body.drug_id + ` rdf:type vocab:DrugType, owl:NamedIndividual ;
            rdfs:label "` + req.body.drug_label + `"@en .`

  const drugAdministration = `:` + req.body.drug_administration_id + ` rdf:type vocab:DrugAdministrationType, owl:NamedIndividual ;
    rdfs:label "` + req.body.drug_administration_label + `"@en ;
    vocab:administrationOf :` + req.body.drug_id + ` .`

  res.end();

});

router.post('/group/create', function(req, res, next) {

  // Drug collection format:

});

module.exports = router;
