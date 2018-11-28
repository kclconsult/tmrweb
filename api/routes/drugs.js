const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

function postDrugs(drugData, res) {

  utils.sparqlUpdate('drugs', drugData, function(sparqlUpdate, error, response, body) {

    if (!error && response.statusCode == 200) {

      console.log(body);

    } else {

      console.log(sparqlUpdate);
      console.log(response.body);

    }

    res.end();

  });

}

router.post('/individual/add', function(req, res, next) {

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

  postDrugs(drugAdministration, res);

});

router.post('/category/add', function(req, res, next) {

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

  postDrugs(drugCategory, res);

  res.end();

});

router.post('/situation/add', function(req, res, next) {

  // Drug situation format:
  const drugSituation = `:Sit` + req.body.drug_situation_id + `rdf:type vocab:SituationType, owl:NamedIndividual ;
                  rdfs:label     "` + req.body.drug_situation_label + `"@en .`;

  postDrugs(drugSituation, res);

});

module.exports = router;
