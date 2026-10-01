# RAQSAPI - Objectives of the RAQSAPI package

Abstract

RAQSAPI is an add-on package for the R programming language and
environment (hereafter referred to as simply “R”) that connects R to a
U.S. EPA database for the retrieval of ambient air pollution data. This
document is intended to state the purpose of creating RAQSAPI.

## The Air Quality System (AQS)

The Clean Air act¹ requires all federal, state, local and tribal air
pollution control agencies to monitor ambient air for concentrations of
certain air pollutants. Codified in 40 CFR Part 58 are the statutory
requirements for these monitoring programs, including monitoring network
technical requirements, operating schedules, data certification, data
submittal and archiving requirements. In addition to the required air
pollution and meteorological monitoring, pollution control agencies
often perform additional and/or voluntary air monitoring.

- The three objectives of ambient air monitoring programs as stated in
  40 CFR Part 58 Appendix D.1 are as follows:
  - Provide air pollution data to the public in a timely manner;
  - Support compliance with ambient air quality standards and emissions
    strategy development; and
  - Support for air pollution research studies.

As required by 40 CFR Part 58, air pollution and meteorological data is
submitted to the United States Environmental Protection Agency along
with associated metadata and quality assurance metadata via EPA’s Air
Quality System (AQS)².

### About AQS Data Mart

AQS Data Mart is a publicly accessible mirror of data stored on the AQS
database designed to make air monitoring data more accessible and useful
to the technical community, scientific community and the general public.
Data on AQS is copied to AQS Data Mart once per week and this data is
made available to the public through web-based applications and APIs
(application programming interface)³. RAQSAPI functions use the APIs
provided by AQS Data Mart to retrieve data.

## Benefits of using RAQSAPI

RAQSAPI package for R is an easy to use R package that allows anyone
using the R programming environment to retrieve data from AQS Data Mart
Database via the exposed API interface using a set of functions with
consistent input variables, function names and output structure. RAQSAPI
does not require installation of software or drivers external to R. The
goal of RAQSAPI is to make a package which eases retrieving of ambient
air monitoring data from AQS Data Mart.

- Some possible uses of the RAQSAPI package
  - Can be used as part of a monitoring agency’s data validation process
  - RAQSAPI can be used for data retrieval as part of quality management
    of air data
  - Ambient air monitoring data retrieval for Design value calculations
  - To retrieve ambient air pollution concentrations
    - Useful for retrieving ambient background air pollution data needed
      to perform air quality dispersion modeling
  - Data retrieval for automation of Network Analysis
  - Statistical analysis of ambient air data
  - Compare ambient air pollution concentrations from different air
    monitoring sites.
  - Data retrieval to generate quarterly/audit reports

## References

\(1\)

Overview of the Clean Air Act and Air Pollution, 2021.
<https://www.epa.gov/clean-air-act-overview>

\(2\)

About AQS Data, 2019.
<https://aqs.epa.gov/aqsweb/documents/about_aqs_data.html>

\(3\)

AQS Data Mart Welcome, 2018.
<https://aqs.epa.gov/aqsweb/documents/data_mart_welcome.html>
