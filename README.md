# Drug Interaction Middleware

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

Before installing, [download and install Docker](https://www.docker.com/get-started), [Prolog](http://www.swi-prolog.org/Download.html) and [Node.js](https://nodejs.org/en/download/).

## Usage

### Apache Jena Fuseki (Triple Store)

Run main container storage:

```
docker run --name fuseki-data -v /fuseki busybox
```

Run main container, supply password and container store:

```
docker run -e ADMIN_PASSWORD=[Password] -d --name fuseki -p 3030:3030 --volumes-from fuseki-data stain/jena-fuseki
```

Installs through docker:

```
docker run -p 3030:3030 stain/jena-fuseki
```

### TMR reasoner

Clone this repository:

```
git clone https://github.kcl.ac.uk/big/guideline-interaction-tool
```

Change into the backend directory:

```
cd backend
```

Clone the TMR repository:

```
git clone git@github.kcl.ac.uk:big/tmr.git
```

Enter prolog environemtn:

```
swipl
```



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
