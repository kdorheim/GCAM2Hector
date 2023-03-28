emissions_from_gcam_db <- function(conn, scenario, start_year, end_year) {
    query_emiss <- '<emissionsQueryBuilder title="nonCO2 emissions by region">
    <axis1 name="GHG">GHG</axis1>
    <axis2 name="Year">emissions</axis2>
    <xPath buildList="true" dataName="emissions" group="false" sumAll="false">*[@type = \'sector\' (:collapse:) or @type = \'resource\' (:collapse:)](: / *[@type = \'subresource\' (: collapse :)] :)//
            *[@type = \'GHG\']/emissions/node()</xPath>
    <comments/>
</emissionsQueryBuilder>'
    query_luc <- '<query title="LUC_emissions">
                <axis1 name="region">region</axis1>
                <axis2 name="Year">land-use-change-emission[@year]</axis2>
                <xPath buildList="true" dataName="land-use-change-emission" group="false" sumAll="true">/LandNode[@name=\'root\' or @type=\'LandNode\' (:collapse:)]//
                    LandLeaf[true() or @type=\'LandLeaf\' (: collapse :)]/land-carbon-densities/
                land-use-change-emission[@year&gt;1970]/text()</xPath>
                <comments/>
            </query>'
    gcam_to_hector_emiss_map <- fread("GCAM_hector_emissions_map.csv") %>%
        setkey(gas)

    rgcam::runQuery(conn, query_emiss, scenario, c()) %>%
        setDT() %>%
        setkey(ghg) ->
        emissions
    emissions[gcam_to_hector_emiss_map, nomatch=0][year >= start_year & year <= end_year, .(value = sum(value * unit.conv)), .(hector.name, hector.units, Date = year)] ->
        emissions

    gcam_variables = unique(emissions$hector.name)

    rgcam::runQuery(conn, query_luc, scenario, c('USA')) %>%
        setDT() -> luc_emissions
    luc_emissions$ghg = "CO2NetLandUse"
    luc_emissions %>%
        setkey(ghg)[gcam_to_hector_emiss_map, nomatch=0][year >= start_year & year <= end_year, .(value = sum(value * unit.conv)), .(hector.name, year)] ->
        luc_emissions

   gcam_base_emiss = read.csv("/Users/pralitp/model/gcam-dac/input/climate/gcam_emissions.csv", comment.char=";", stringsAsFactors = FALSE) %>% setDT() %>% melt(id.vars=c("Date"), variable.name="hector.name")
   gcam_base_emiss.nongcam = gcam_base_emiss[!hector.name %in% gcam_variables]
   gcam_base_emiss.gcam = gcam_base_emiss[hector.name %in% gcam_variables][year < start_year, year > end_year]
   emissions_complete = rbind(gcam_base_emiss.nongcam, gcam_base_emiss.gcam, emissions)
