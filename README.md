## package for selection and download of Dutch long term monitoring water data

Water data in the Netherlands are available via Rijkswaterstaat webservices. In short, there are two web services


1.  waterbase - contains surface water monitoring data for a lot of parameters.
2.  data distribution layer service (json for data, wfs for locations) - contains also monitoring data from deeper waters, but limiting number of parameters. In development
3. OpenEarth copy of waterbase at Deltares OPeNDAP server - copy of selected parameters from (1), with additional metadata (international standards)


This package aims to provide functions to easily connect to these services and download. 

It is under development

1.  waterbase functions

2.  data distribution layer services

documentation of services: https://www.rijkswaterstaat.nl/rws/opendata/DistributielaagWebservices-SUM-2v7.pdf

* rws_metadata - gets catalogue of all locations, depths, parameters    
* rws_observations - gets observation data

3.  OpenEarth waterbase copy

The package is maintained by willem.stolte(at)deltares.nl
