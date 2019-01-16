const request = require('request');
const parser = new require('n3').Parser();
const xmlReader = require('xml-reader');
const xmlQuery = require('xml-query');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');

class Util {

	static sparqlUpdate(dataset_id, content, insertOrDelete, callback) {

    var sparqlUpdate = ` ` + insertOrDelete + ` DATA {
    `;

    // Parsing allows us to express the guidelines in TRIG (as per the original work), and then convert them into triples for the SPARQL update. TODO: look at adding the TRIG directly to Jena.
    parser.parse(

      guidelines.PREFIXES + content,

      (error, quad, prefixes) => {

        if (quad) {

          // We need angular brackets around named nodes (URIs).

          if ( quad.graph.id ) {

            sparqlUpdate += `
            GRAPH ` + (quad.graph.termType == "NamedNode" ? `<` : ``) + quad.graph.id + (quad.graph.termType == "NamedNode" ? `>` : ``) + ` {
            `;

          } else {

            sparqlUpdate += `
            GRAPH <default> {
            `;

          }

          sparqlUpdate += (quad.subject.termType == "NamedNode" ? `<` : ``) + quad.subject.id + (quad.subject.termType == "NamedNode" ? `> ` : ` `) + (quad.predicate.termType == "NamedNode" ? `<` : ``) + quad.predicate.id + (quad.predicate.termType == "NamedNode" ? `> ` : ` `) + (quad.object.id.indexOf('http') > -1 ? quad.object.id.replace('http', '<http') : quad.object.id) + (quad.object.id.indexOf('http') > -1 ? `>` : ``)
          + `
          }
          `;

        } else {

          sparqlUpdate += `
          }`;
          var prefixAndSparqlUpdate = guidelines.PREFIXES + "\n" + sparqlUpdate

          request.post(

              'http://localhost:' + config.JENA_PORT + '/' + dataset_id + "/update" ,
              { body: prefixAndSparqlUpdate },

              function (error, response, body) {

                  callback(prefixAndSparqlUpdate, error, response, body);

              }

          );

        }

      }

    );

  }

	static sparqlQuery(dataset_id, query, callback) {

		request.get(

			'http://localhost:' + config.JENA_PORT + '/' + dataset_id + "/query?query=" + query,

			function (error, response, body) {

				var data = [];

				var queryContainer = xmlQuery(xmlReader.parseSync(body));

				queryContainer.find('binding').each(function(binding) {

					data.push(binding.children[0].children[0].value);

				});

				callback(query, error, response, body, data);

			}

		);

	}

	static sparqlInstanceOf(dataset_id, instance, callback) {

		var query = `
		SELECT ?s
		WHERE {
		  GRAPH ?g { ?s a ` + instance + ` }
		}
		`;

		this.sparqlQuery(dataset_id, query, callback)

	}

	static sparqlSubject(dataset_id, subject, callback) {

			var query = `
			SELECT ?p ?o
			WHERE {
			  GRAPH ?g { <`+ subject  +`> ?p ?o }
			}
			`;

			this.sparqlQuery(dataset_id, query, function(sparqlQuery, error, response, body, data) {

		    var pairedPredicateObject = [];

		    for ( var i = 0; i < data.length; i+= 2) {

		      pairedPredicateObject.push([data[i], data[i+1]]);

		    }

		    callback(pairedPredicateObject);

			});

	}

	static callPrologServer(path, data, res) {

	  request.post(

      'http://localhost:' + config.PROLOG_PORT + "/" + path,
        { headers: {
          'Content-Type': 'application/x-www-form-urlencoded'
        },
        body: data },

      function (error, response, body) {

          res.send(response.body);

      }

	  );

	}

}

module.exports = Util;
