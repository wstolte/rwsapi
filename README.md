## package for selection and download of Dutch long term monitoring water data

Water data in the Netherlands are available via Rijkswaterstaat webservices. In short, there are two web services


2.  data distribution layer service (json for data, wfs for locations) - contains also monitoring data from deeper samples.


This package aims to provide functions to easily connect to this service and download water-related data. 

It is under development

2.  data distribution layer services

documentation of services: https://www.rijkswaterstaat.nl/rws/opendata/DistributielaagWebservices-SUM-2v7.pdf

* rws_metadata - gets catalogue of all locations, depths, parameters    
* rws_observations2 - gets observation data

The package is maintained by willem.stolte(at)deltares.nl
