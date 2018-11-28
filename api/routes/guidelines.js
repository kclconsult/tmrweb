var express = require('express');
var router = express.Router();
var request = require('request');
var N3 = require('n3');
var parser = new N3.Parser();

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');

router.post('/create', function(req, res, next) {

  // Guideline format:
  var head = `:Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_head {
    :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_nanopub
            a                           nanopub:Nanopublication ;
            nanopub:hasAssertion        :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + ` ;
            nanopub:hasProvenance       :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_provenance ;
            nanopub:hasPublicationInfo  :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_publicationinfo .
  }`

  var body = `:Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + ` {
    :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `
            a                       vocab:ClinicalRecommendation ;
            rdfs:label              "` + req.body.label  + `"@en ;
            vocab:aboutExecutionOf  :` + req.body.drug_administration_id + ` ;
            vocab:basedOn           :` + req.body.belief_id + ` ;
            vocab:partOf            :CIG-` + req.body.guideline_group_id + ` ;
            vocab:strength          "` + req.body.should_or_shouldnot + `" .
  }`

  var provenance = `:Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_provenance {
    :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `
            prov:wasDerivedFrom  <http://hdl.handle.net/10222/43703> .

    :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_provenance
            a             oa:Annotation ;
            oa:hasBody    :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + ` ;
            oa:hasTarget  [ oa:hasSource  <http://hdl.handle.net/10222/43703> ] .
  }`

  var publication = `:Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_publicationinfo {
    :Rec` + req.body.guideline_group_id + `-` + req.body.guideline_id + `_nanopub
            prov:generatedAtTime  "1922-12-28"^^xsd:dateTime ;
            prov:wasAttributedTo  :` + req.body.author + ` .
  }`

  sparqlUpdate = ` INSERT DATA {
  `;

  // Parsing allows us to express the guidelines in TRIG (as per the original) work, and then convert them into triples for the SPARQL update. TODO: look at adding the TRIG directly to Jena.
  parser.parse(

    guidelines.PREFIXES + body,

    (error, quad, prefixes) => {

      if (quad) {

        sparqlUpdate += `
        GRAPH ` + (quad.graph.termType == "NamedNode" ? `<` : ``) + quad.graph.id + (quad.graph.termType == "NamedNode" ? `>` : ``) + ` {
          ` +
          (quad.subject.termType == "NamedNode" ? `<` : ``) + quad.subject.id + (quad.subject.termType == "NamedNode" ? `> ` : ` `) + (quad.predicate.termType == "NamedNode" ? `<` : ``) + quad.predicate.id + (quad.predicate.termType == "NamedNode" ? `> ` : ` `) + (quad.object.termType == "NamedNode" ? `<` : ``) + quad.object.id + (quad.object.termType == "NamedNode" ? `>` : ``)
          + `
          }
        `;

      } else {

        sparqlUpdate += `
        }`;
        request.post(

            'http://localhost:' + config.JENA_PORT + '/' + req.body.guideline_group_id + "/update" ,
            { body: guidelines.PREFIXES + sparqlUpdate },

            function (error, response, body) {

                if (!error && response.statusCode == 200) {

                    console.log(body)

                }

                res.end();

            }

        );

      }

    }

  );

});

module.exports = router;
