This directory contains a variety of inputs used by the TMIP-EMAT
module to manipulate the VE-RSPM model inputs.
last revised: slason (4.19.23)


Lane Miles - folder (LM) 
- the marea_lane_miles file was updated on 13 April by Slason to reflect changes in the RTP '23 that were provided by Thaya Patton on 12 April 23.
- default scaler = 1.  range of 0.9 to 1.2


VMT Taxes (RUC)
- azone_veh_use_taxes
mixture
- 1: Fuel taxes but no RUC
- 2: Fuel taxes with ful RUC (STS Scenario)


Freeway Congestion Charges: (FConP)
marea_congestion_charges
mixture
- 1: Low: Zero charges
- 2: Full Freeway charges


Arterial Congestion Charges: (AConP)
marea_congestion_charges
mixture
- 1: Low: Zero charges
- 2: Full Arterial charges


Transit Powertrain (TP)
marea_transit_powertrain_prop
mixture
- 1: conservative powertrain
- 2: optimistic powertrain


Transit Service (TS) - this input is still slightly under development
marea_transit_service
- Categorical
- 1: Low: RTP18 inputs
- 2: Med: RTP23 Financial constrained
- 3: High: RTP23 strategic scenario - We still don't have this


Teleworking (WFH)
region_telework_input
- mixture
- Low: base pre-pandemic WFH rates
- High: higher full time WFH rates


Parking (P)
bzone_parking
- categorical: 
- 1: low (RTP18)
- 2: higher (RTP23)



