# Drug Interaction Middleware

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

Before installing, [download and install Docker](https://www.docker.com/get-started), [Prolog](http://www.swi-prolog.org/Download.html), [python](https://www.python.org/downloads/), [pip](https://packaging.python.org/tutorials/installing-packages/#use-pip-for-installing), [virtualenv](https://virtualenv.pypa.io/en/latest/installation/) and [Node.js](https://nodejs.org/en/download/).

## Running

### Apache Jena Fuseki (Triple Store)

Run main container storage:

```
docker run --name fuseki-data -v /fuseki busybox
```

Run main container, supply port (3030, recommended), password and container store:

```
docker run -e ADMIN_PASSWORD=[Password] -d --name fuseki -p 3030:3030 --volumes-from fuseki-data stain/jena-fuseki
```

Navigate to http://localhost:3030, login with the username `admin` and the password as set above. select Manage Datasets, and create three new (persistent) datasets: drugs, transitions and beliefs.

### TMR reasoner

Clone this repository:

```
git clone https://github.com/consult-kcl/drug-interaction
```

Change into the backend directory:

```
cd backend
```

Clone the TMR repository:

```
git clone https://github.com/consult-kcl/tmr.git
```

Enter Prolog environment:

```
swipl
```

Load server:

```
?- consult('server.pl').
```

Start server on a given port (1234, recommended):

```
?- server(1234).
```

### TMRWeb API

Change to the API folder.

```
cd ../api
```

Create a node virtual environment (within a python virtual environment), and activate it:

```
virtualenv env
. env/bin/activate
pip install nodeenv
nodeenv nenv
. nenv/bin/activate
```

Install dependencies:

```
cat requirements.txt | xargs npm install -g
```

Create an environment file:

```
touch .env
```

Add the following information to this environment file using a text editor:

```
FUSEKI_PASSWORD="[Password]"
```

Where [Password] is the password you created for the triple store earlier.

Run server:

```
npm start
```

The server runs by default on port 3000.

## Usage

See [documentation](api/README.md).

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
