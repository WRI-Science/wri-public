=========================================
BUILT ENVIRONMENT STATUS COMPONENT
=========================================

Overview:
---------
The Built Environment status component determines the presence or absence of
buildings in a given area using Microsoft building polygons.

There are **no standalone scripts in infrastructure/status/**: building
footprints are downloaded and processed in the **defensible space** resistance
pipeline (infrastructure/resistance/defensible_space/). Outputs from that
workflow feed infrastructure status and resistance indicators elsewhere in
this domain.

Data Source:
-------------
Building polygon data is provided by Microsoft's Global ML Building Footprints repository:
https://github.com/microsoft/GlobalMLBuildingFootprints

Data Integration:
------------------
Building polygons are incorporated into the defensible-space workflow in the
resistance folder so vegetation-within-buffer metrics align with structure
locations.

Usage:
-------
Use this component conceptually to evaluate areas based on building presence;
operational code paths live under defensible_space/.

=========================================
