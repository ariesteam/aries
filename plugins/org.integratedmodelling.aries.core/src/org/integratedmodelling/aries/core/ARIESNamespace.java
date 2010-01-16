package org.integratedmodelling.aries.core;

public class ARIESNamespace {

	// properties
	public static final String DEFAULT_ECOSYSTEM_SERVICES_PROPERTY = "aries.default.ecosystemservices";
	public static final String DEFAULT_REGION_OF_INTEREST_PROPERTY = "aries.default.region";
	public static final String MAX_LINEAR_RESOLUTION_PROPERTY = "aries.raster.resolution.linear.max";

	// kbox properties
	public static final String ARIES_DATA_KBOX = "aries.data.kbox";
	public static final String ARIES_DISTRICTS_KBOX = "aries.district.kbox";
	public static final String ARIES_SOURCES_KBOX = "aries.sources.kbox";
	
	// classes
	public static final String BENEFIT_TYPE = "eserv:EcosystemBenefit";
	public static final String PORTFOLIO_TYPE = "aries:Portfolio";
	public static final String BENEFIT_OBSERVATION_TYPE = "aries:BenefitObservation";
	public static final String CASE_STUDY_TYPE = "aries:CaseStudy";

	public static final String BENEFIT_SOURCE_TYPE = "eserv:SourceType";
	public static final String BENEFIT_USE_TYPE = "eserv:UseType";
	public static final String BENEFIT_SINK_TYPE = "eserv:Sink";
	public static final String BENEFIT_FLOW_TYPE = "eserv:FlowType";

	public static final String SOURCE_TRAIT = "eserv:SourceTrait";
	public static final String USE_TRAIT = "eserv:UseTrait";
	public static final String FLOW_TRAIT = "eserv:FlowTrait";
	public static final String SINK_TRAIT = "eserv:SinkTrait";

	// relationships
	public static final String HAS_BENEFIT = "eserv:producesBenefit";
	public static final String HAS_PROVISION = "eserv:hasProvision";
	public static final String HAS_USAGE = "eserv:hasUsage";
	
}
