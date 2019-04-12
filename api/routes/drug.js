const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

function postDrugs(drugData, res, insertOrDelete, callback) {

  utils.sparqlUpdate('drugs', drugData, insertOrDelete, function(status) {

    callback(status);

  });

}

function type(drugOrCategory, id) {

  return drug = `:Drug` + drugOrCategory + id + ` rdf:type vocab:DrugType, owl:NamedIndividual ;
            rdfs:label "` + id + `"@en `

}

function administration(drugOrCategory, id) {

  var drugAdministration = `:ActAdminister` + id + ` rdf:type vocab:DrugAdministrationType, owl:NamedIndividual ;
    rdfs:label "Administer ` + id + `"@en ;
    `
  drugAdministration += `vocab:administrationOf :Drug` + drugOrCategory + id;

  return drugAdministration;

}

// Specifying drug subsumption via the administrationOf triple.
function administrationSubsumption(id) {

  return ` ;
  vocab:subsumes :ActAdminister` + id + ` .`

}

function individualAction(req, res, insertOrDelete, callback) {

  // Individual drug format:
  const drug = type("T", req.body.drug_id) + ` .`

  var drugAdministration = administration("T", req.body.drug_id);

  if ( req.body.subsumed_drug_id ) {

    drugAdministration += administrationSubsumption(req.body.subsumed_drug_id);

  } else {

    drugAdministration += ` .`

  }

  postDrugs(drug + " " + drugAdministration, res, insertOrDelete, callback);

}

router.post('/individual/add', function(req, res, next) {

  individualAction(req, res, config.INSERT, function(status) {

    res.sendStatus(status);

  });

});

router.post('/individual/delete', function(req, res, next) {

  individualAction(req, res, config.DELETE, function(status) {

    res.sendStatus(status);

  });

});

// Specifying drug subsumptions directly via drug definition (typically used for categories).
function directSubsumptions(drugOrCategory, subsumedDrugIds) {

  var drugSubsumption = ` ;
  vocab:subsumes  `;

  subsumedDrugIds.split(",").forEach(function(drugId) {

    drugSubsumption += (`:Drug` + drugOrCategory + drugId.trim() + `, `);

  });

  drugSubsumption = drugSubsumption.substring(0, drugSubsumption.length - 2);

  return drugSubsumption;

}

function groupingCriteria(groupingCriteriaIds) {

  var groupingCriteria = ` ;
  vocab:hasGroupingCriteria  `;

  groupingCriteriaIds.split(",").forEach(function(criteriaId) {

    groupingCriteria += (`:` + criteriaId.trim() + `, `);

  });

  groupingCriteria = groupingCriteria.substring(0, groupingCriteria.length - 2);

  groupingCriteria += ` .`

  return groupingCriteria;

}

function categoryAction(req, res, insertOrDelete, callback) {

  // Drug category format:
  var drugCategory = type("Cat", req.body.drug_category_id)

  if ( req.body.subsumed_drug_ids ) {

    // Assumes IDs are for drugs, thus does not currently allow the specification of categories subsuming categories.
    drugCategory += directSubsumptions("T", req.body.subsumed_drug_ids);

    if ( !req.body.grouping_criteria_ids ) {

      drugCategory += ` .`

    }

  }

  if ( req.body.grouping_criteria_ids ) {

    drugCategory += groupingCriteria(req.body.grouping_criteria_ids);

  }

  if ( !req.body.subsumed_drug_ids && !req.body.grouping_criteria_ids ) {

    drugCategory += ` .`

  }

  const drugCategoryAdministration = administration("Cat", req.body.drug_category_id) + ` .`;

  console.log(drugCategory);
  console.log(drugCategoryAdministration);

  postDrugs(drugCategory + " " + drugCategoryAdministration, res, insertOrDelete, callback);

}

router.post('/category/add', function(req, res, next) {

  categoryAction(req, res, config.INSERT, function(status) {

    res.sendStatus(status);

  });

});

router.post('/category/delete', function(req, res, next) {

  categoryAction(req, res, config.DELETE, function(status) {

    res.sendStatus(status);

  });

});

function situationAction(req, res, insertOrDelete, callback) {

  // Drug situation format:
  const drugSituation = `:Sit` + req.body.drug_situation_id + `rdf:type vocab:SituationType, owl:NamedIndividual ;
                  rdfs:label     "` + req.body.drug_situation_label + `"@en .`;

  postDrugs(drugSituation, res, insertOrDelete, callback);

}

router.post('/situation/add', function(req, res, next) {

  situationAction(req, res, config.INSERT, function(status) {

    res.sendStatus(status);

  });

});

router.post('/situation/delete', function(req, res, next) {

  situationAction(req, res, config.DELETE, function(status) {

    res.sendStatus(status);

  });

});

//

router.post('/effect/get', function(req, res, next) {

  var postData = require('querystring').stringify({
      'drug_full_id' : req.body.drug_full_id
  });

  utils.callPrologServer("drugeffects", postData, res, function(data) {

    res.send(data);

  });

});

router.post('/all/get/', function(req, res, next) {

  utils.sparqlSubject("drugs", req.body.drug_full_id, function(drugData) {

    res.send(drugData);

  });

});

module.exports = router;
