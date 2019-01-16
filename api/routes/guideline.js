const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

router.post('/create', function(req, res, next) {

  request.post({

      url: 'http://localhost:' + config.JENA_PORT + "/$/datasets?dbType=tdb&dbName=CIG-" + req.body.guideline_group_id,
      headers: {
        Authorization: "Basic " + new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64")
      },

    },

    function (error, response, body) {

      const description = `:CIG-` + req.body.guideline_group_id + ` {
          :CIG-` + req.body.guideline_group_id + ` rdf:type vocab:ClinicalGuideline, owl:NamedIndividual ;
              rdfs:label "` + req.body.description + `"@en .
      }`;

      utils.sparqlUpdate("CIG-" + req.body.guideline_group_id, description, config.INSERT, function(sparqlUpdate, error, response, body) {

        if (!error && response.statusCode == 200) {

          console.log(body);

        } else {

          console.log(sparqlUpdate);
          console.log(response.body);

        }

        res.end();

      });

    }

  );

});

function action(req, res, insertOrDelete) {

  // Guideline format:
  const head = `:Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_head {
    :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_nanopub
            a                           nanopub:Nanopublication ;
            nanopub:hasAssertion        :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + ` ;
            nanopub:hasProvenance       :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_provenance ;
            nanopub:hasPublicationInfo  :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_publicationinfo .
  }`

  const body = `:Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + ` {
    :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `
            a                       vocab:ClinicalRecommendation ;
            rdfs:label              "` + req.body.label  + `"@en ;
            vocab:aboutExecutionOf  :ActAdminister` + req.body.drug_id + ` ;
            vocab:basedOn           :CB` + req.body.belief_id + ` ;
            vocab:partOf            :CIG-` + req.body.guideline_group_id + ` ;
            vocab:strength          "` + req.body.should_or_shouldnot + `" .
  }`

  const provenance = `:Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_provenance {
    :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `
            prov:wasDerivedFrom  <http://hdl.handle.net/10222/43703> .

    :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_provenance
            a             oa:Annotation ;
            oa:hasBody    :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + ` ;
            oa:hasTarget  [ oa:hasSource  <http://hdl.handle.net/10222/43703> ] .
  }`

  const publication = `:Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_publicationinfo {
    :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_nanopub
            prov:generatedAtTime  "1922-12-28"^^xsd:dateTime ;
            prov:wasAttributedTo  :` + req.body.author + ` .
  }`

  utils.sparqlUpdate("CIG-" + req.body.guideline_group_id, head + " " + body + " " + provenance + " " + publication, insertOrDelete, function(sparqlUpdate, error, response, body) {

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

  action(req, res, config.INSERT);

});

router.post('/delete', function(req, res, next) {

  action(req, res, config.DELETE);

});

//

router.post('/drug/get', function(req, res, next) {

  var postData = require('querystring').stringify({
      'guideline_group_id' : `CIG-` + req.body.guideline_group_id,
      'guideline_id' : req.body.guideline_id
  });

  utils.callPrologServer("drug", postData, res);

});

router.post('/all/get/', function(req, res, next) {

  utils.sparqlGraph("CIG-" + req.body.guideline_group_id, req.body.guideline_full_id, function(guidelineData) {

    res.send(guidelineData);

  });

});

module.exports = router;
