const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

function postDrugs(drugData, res, insertOrDelete) {

  utils.sparqlUpdate('drugs', drugData, insertOrDelete, function(sparqlUpdate, error, response, body) {

    if (!error && response.statusCode == 200) {

      console.log(body);

    } else {

      console.log(sparqlUpdate);
      console.log(response.body);

    }

    res.end();

  });

}

function actionIndividual(req, res, insertOrDelete) {

  // Individual drug format:
  const drug = `:DrugT` + req.body.drug_id + ` rdf:type vocab:DrugType, owl:NamedIndividual ;
            rdfs:label "` + req.body.drug_id + `"@en .`

  var drugAdministration = `:ActAdminister` + req.body.drug_id + ` rdf:type vocab:DrugAdministrationType, owl:NamedIndividual ;
    rdfs:label "Administer ` + req.body.drug_id + `"@en ;
    `

  if ( req.body.drug_category_id ) {

    drugAdministration += `vocab:administrationOf :DrugCat` + req.body.drug_category_id;

  } else {

    drugAdministration += `vocab:administrationOf :DrugT` + req.body.drug_id;

  }

  if ( req.body.subsumed_drug_id ) {

    drugAdministration += ` ;
    vocab:subsumes :ActAdminister` + req.body.subsumed_drug_id + ` .`

  } else {

    drugAdministration += ` .`

  }

  postDrugs(drug + " " + drugAdministration, res, insertOrDelete);

}

router.post('/individual/add', function(req, res, next) {

  individualAction(req, res, config.INSERT)

});

router.post('/individual/delete', function(req, res, next) {

  individualAction(req, res, config.DELETE)

});

function categoryAction(req, res, insertOrDelete) {

  // Drug category format:
  var drugCategory = `:DrugCat` + req.body.drug_category_id + ` rdf:type vocab:DrugType, owl:NamedIndividual ;
    rdfs:label "` + req.body.drug_category_id + `"@en`;

  if ( req.body.subsumed_drug_ids ) {

    drugCategory += ` ;
    vocab:subsumes  `;

    req.body.subsumed_drug_ids.split(",").forEach(function(drug_id) {

      drugCategory += (`:DrugT` + drug_id.trim() + `, `);

    });

    drugCategory = drugCategory.substring(0, drugCategory.length - 2);

    if ( !req.body.grouping_criteria_ids ) {

      drugCategory += ` .`

    }

  }

  if ( req.body.grouping_criteria_ids ) {

    drugCategory += ` ;
    vocab:hasGroupingCriteria  `;

    req.body.grouping_criteria_ids.split(",").forEach(function(criteria_id) {

      drugCategory += (`:` + criteria_id.trim() + `, `);

    });

    drugCategory = drugCategory.substring(0, drugCategory.length - 2);

    drugCategory += ` .`

  }

  if ( !req.body.subsumed_drug_ids && !req.body.grouping_criteria_ids ) {

    drugCategory += ` .`

  }

  postDrugs(drugCategory, res, insertOrDelete);

  res.end();

}

router.post('/category/add', function(req, res, next) {

  categoryAction(req, res, config.INSERT);

});

router.post('/category/delete', function(req, res, next) {

  categoryAction(req, res, config.DELETE);

});

function actionSituation(req, res, insertOrDelete) {

  // Drug situation format:
  const drugSituation = `:Sit` + req.body.drug_situation_id + `rdf:type vocab:SituationType, owl:NamedIndividual ;
                  rdfs:label     "` + req.body.drug_situation_label + `"@en .`;

  postDrugs(drugSituation, res, insertOrDelete);

}

router.post('/situation/add', function(req, res, next) {

  actionSituation(req, res, config.INSERT);

});

router.post('/situation/delete', function(req, res, next) {

  actionSituation(req, res, config.DELETE);

});

module.exports = router;
