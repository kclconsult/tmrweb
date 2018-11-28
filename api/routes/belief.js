const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

router.post('/add', function(req, res, next) {

  // Belief format:
  const head = `data:CB` + req.body.belief_id + `_head {
  	  data:CB` + req.body.belief_id + `_nanopub
          a                             nanopub:Nanopublication ;
          nanopub:hasAssertion          data:CB` + req.body.belief_id + ` ;
          nanopub:hasProvenance         data:CB` + req.body.belief_id + `_provenance ;
  		    nanopub:hasPublicationInfo    data:CB` + req.body.belief_id + `_publicationinfo .
  }`;

  const body = `data:CB` + req.body.belief_id + ` {
      data:ActAdminister` + req.body.drug_cause_id + `
  				vocab:causes 									data:` + req.body.transition_effect_id + ` .
      data:CB` + req.body.belief_id + `
          a                             vocab:CausationBelief ;
          vocab:strength                "` + req.body.strength + `"^^xsd:string;
          vocab:frequency               "` + req.body.frequency + `"^^xsd:string.
  }`;

  const provenance = `data:CB` + req.body.belief_id + `_provenance {
      data:CB` + req.body.belief_id + `_provenance
          a                             oa:Annotation ;
          oa:hasBody                    data:CB` + req.body.belief_id + ` ;
          oa:hasTarget                  [ oa:hasSource <http://hdl.handle.net/10222/43703> ] .
      data:CB` + req.body.belief_id + `
          prov:wasDerivedFrom           <http://hdl.handle.net/10222/43703> .
  }`;

  const publication = `data:CB` + req.body.belief_id + `_publicationinfo {
      data:CB` + req.body.belief_id + `_nanopub
          prov:generatedAtTime          "1922-12-28"^^xsd:dateTime ;
  		    prov:wasAttributedTo          data:` + req.body.author + `.
  }`;

  utils.sparqlUpdate("beliefs", head + " " + body + " " + provenance + " " + publication, function(sparqlUpdate, error, response, body) {

    if (!error && response.statusCode == 200) {

      console.log(body);

    } else {

      console.log(sparqlUpdate);
      console.log(response.body);

    }

    res.end();

  });

});

module.exports = router;
