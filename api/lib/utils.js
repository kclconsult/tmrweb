const request = require('request');
const n3 = require('n3');
const parser = new n3.Parser();
const xmlReader = require('xml-reader');
const xmlQuery = require('xml-query');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const logger = require('../config/winston');

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
					const URL = "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + dataset_id + "/update";

          request.post(

            URL, {
							headers: {
								"Authorization": "Basic " + new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64")
							},
							body: prefixAndSparqlUpdate
						},

            function (error, response, body) {

							if ( !error && response && response.statusCode < 400 ) {

								callback(200);

							} else {

								console.log("SPARQL update failed at: " + URL + " Query: " + prefixAndSparqlUpdate + ". Error: " + ( error ? error : "None" ) + ". Body: " + ( body ? body : "None" ) + ". Status: " + ( ( response && response.statusCode ) ? response.statusCode : "No response." ) + ".");
								callback(400);

							}

            }

          );

        }

      }

    );

  }

	static sparqlQuery(dataset_id, query, callback) {

		request.get(

			"http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + dataset_id + "/query?query=" + query,

			function (error, response, body) {

				if ( !error && response && response.statusCode == 200 ) {

					var data = [];

					var queryContainer = xmlQuery(xmlReader.parseSync(body));

					queryContainer.find('binding').each(function(binding) {

						data.push(binding.children[0].children[0].value);

					});

					callback(data);

				} else {

					console.log("SPARQL query failed: " + query + ". Error: " + error + ". Body: " + body + ". Status: " + ( ( response && response.statusCode ) ? response.statusCode : "No response." ) + ".");
					callback(null);

				}

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

	static nList(list, n) {

		var pairedPredicateObject = [];

		for ( var i = 0; i < list.length; i+= n) {

			var nTuple = [];

			for ( var j = i; j < i + n; j++ ) {

				nTuple.push(list[j]);

			}

			pairedPredicateObject.push(nTuple);

		}

		return pairedPredicateObject;

	}

	static sparqlGraph(dataset_id, graph, callback) {

		var query = `
		SELECT ?s ?p ?o
		WHERE {
			GRAPH <` + graph + `> { ?s ?p ?o }
		}
		`;

		this.sparqlQuery(dataset_id, query, function(data) {

			callback(Util.nList(data, 3));

		});

	}

	static sparqlSubject(dataset_id, subject, callback) {

		var query = `
		SELECT ?p ?o
		WHERE {
		  GRAPH ?g { <`+ subject  +`> ?p ?o }
		}
		`;

		this.sparqlQuery(dataset_id, query, function(data) {

	    callback(Util.nList(data, 2));

		});

	}

	static sparqlGraphInstanceOf(dataset_id, instance, callback) {

		var query = `
		SELECT ?g
		WHERE {
		  GRAPH ?g { ?s a ` + instance + ` }
		}
		`;

		this.sparqlQuery(dataset_id, query, callback)

	}

	static callPrologServer(path, data, res, callback) {

		const URL = "http://" + config.PROLOG_HOST + ":" + config.PROLOG_PORT + "/" + path;

	  request.post(

      	URL, {
				headers: {
					"Authorization": "Basic " + new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64"),
          "Content-Type": "application/x-www-form-urlencoded"
        },
        body: data },

      function (error, response, body) {

				if ( !error && response && response.statusCode < 400 && body ) {

					callback(body);

				} else {

					logger.error("Failed to call prolog server with path: " + path + ". Data: " + data + ". Error: " + error + ". Body: " + ( body ? body : "None" ) + ". Status: " + ( ( response && response.statusCode ) ? response.statusCode : "No response." ) + ".");
					callback(null);

				}

      }

	  );

	}

}

module.exports = Util;
