# Drug Interaction Middleware

## Getting Started

### Prerequisites

docker stop fuseki-data
docker rm fuseki-data
docker run --name fuseki-data -v /fuseki busybox
docker stop fuseki
docker rm fuseki
docker run -e ADMIN_PASSWORD=Izf3TaF0UI8oj7f -d --name fuseki -p 3030:3030 --volumes-from fuseki-data stain/jena-fuseki

docker stop fuseki-data; docker rm fuseki-data; docker run --name fuseki-data -v /fuseki busybox; docker stop fuseki; docker rm fuseki; docker run -e ADMIN_PASSWORD=Izf3TaF0UI8oj7f -d --name fuseki -p 3030:3030 --volumes-from fuseki-data stain/jena-fuseki

### Building (Optional)

## Usage

## Running the tests

## Deployment

## Built With

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/martinchapman/nokia-health/tags).

## Authors

Produced as part of the [CONSULT project](https://consult.kcl.ac.uk/).

![CONSULT project](https://consult.kcl.ac.uk/wp-content/uploads/sites/214/2017/12/overview-consult-768x230.png "CONSULT project")

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments

*
