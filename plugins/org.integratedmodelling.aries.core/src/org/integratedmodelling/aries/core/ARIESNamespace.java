/*
Copyright 2011 The ARIES Consortium (http://www.ariesonline.org)

This file is part of ARIES.

ARIES is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License,
or (at your option) any later version.

ARIES is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with ARIES.  If not, see <http://www.gnu.org/licenses/>.
*/

package org.integratedmodelling.aries.core;

import java.util.HashSet;

import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.utils.Pair;

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

	public static final String SERVICE_OBSERVABLE = "eserv:ServiceObservable";
	// which subsumes...
	public static final String BENEFIT_SOURCE_TYPE = "eserv:Source";
	public static final String BENEFIT_USE_TYPE = "eserv:Use";
	public static final String BENEFIT_SINK_TYPE = "eserv:Sink";
	public static final String BENEFIT_FLOW_TYPE = "eserv:Flow";

	public static final String SOURCE_TRAIT = "eserv:SourceTrait";
	public static final String USE_TRAIT = "eserv:UseTrait";
	public static final String FLOW_TRAIT = "eserv:FlowTrait";
	public static final String SINK_TRAIT = "eserv:SinkTrait";

	// relationships
	public static final String HAS_BENEFIT = "eserv:producesBenefit";
	public static final String HAS_PROVISION = "eserv:hasProvision";
	public static final String HAS_USAGE = "eserv:hasUsage";
	
	// dry flow-related traits
	public static final String THEORETICAL = "eserv:Theoretical";
	public static final String BLOCKED = "eserv:Blocked";
	public static final String ACTUAL = "eserv:Actual";
	public static final String POSSIBLE = "eserv:Possible";
	public static final String INACCESSIBLE = "eserv:Inaccessible";
	
	// namespace of contexts that define coverage for models
	public static final String MODEL_COVERAGE_NAMESPACE = "aries.contexts.model-coverage";
		
	private static HashSet<IConcept> mainTraits = null;
	private static HashSet<IConcept> mainServiceTypes = null;
	static IConcept storyline = null;
	
	
	public static IConcept Storyline() {

		if (storyline == null) {
			storyline = KnowledgeManager.get().retrieveConcept("aries:CaseStudy");
		}
		return storyline;
	}
	
	private static IConcept classifyTrait(IConcept c, IConcept deflt) throws ThinklabException {
		
		IConcept ret = null;
		
		if (c.is("eserv:FlowRelatedTrait")) {
			
			// get the specific kind of trait this is: SourceTrait, FlowTrait, ...
			IConcept pt = null;
			for (IConcept par : deflt.getAllParents()) {
				if (mainTraits.contains(par)) {
					pt = par;
					break;
				}
			}
			
			/* scan direct parents to determine which specific trait we are */
			for (IConcept par : c.getParents()) {
				if (par.is(pt)) {
					ret = par;
					break;
				}
			}
		}
		
		if (ret == null) {
			
			/*
			 * find the theoretical trait for the object, which is the default if we get the
			 * state from a non-flow model.
			 */
			for (IConcept ch : c.getChildren()) {
				if (ch.is(deflt)) {
					ret = ch;
					break;
				}
			}
		}
		
		/*
		 * last resort, better than null, at least we can classify even if we don't have service-specific
		 * labels.
		 */
		if (ret /* still */ == null)
			ret = deflt;
		
		return ret;
	}
	
	
	/**
	 * Classify the observable for a state into its status as source, sink, use, or flow (the first 
	 * concept, matching the appropriate Flow, Sink, Use or Source for the observable without the trait) and its correspondent trait of 
	 * theoretical, blocked, actual, possible, inaccessible (second concept matching one of the ServiceTrait-s). Returns
	 * null if the observable is not service-related.
	 * 
	 * @param c
	 * @return
	 * @throws ThinklabException 
	 */
	public static Pair<IConcept, IConcept> classifyObservable(IConcept c) throws ThinklabException {
		
		IConcept main = c;
		IConcept trai = null;
		
		if (mainTraits == null) {
			mainTraits = new HashSet<IConcept>();
			mainTraits.add(KnowledgeManager.get().requireConcept(SOURCE_TRAIT));
			mainTraits.add(KnowledgeManager.get().requireConcept(FLOW_TRAIT));
			mainTraits.add(KnowledgeManager.get().requireConcept(USE_TRAIT));
			mainTraits.add(KnowledgeManager.get().requireConcept(SINK_TRAIT));
		}
		
		if (mainServiceTypes == null) {
			mainServiceTypes = new HashSet<IConcept>();
			mainServiceTypes.add(KnowledgeManager.get().requireConcept(BENEFIT_SOURCE_TYPE));
			mainServiceTypes.add(KnowledgeManager.get().requireConcept(BENEFIT_SINK_TYPE));
			mainServiceTypes.add(KnowledgeManager.get().requireConcept(BENEFIT_USE_TYPE));
			mainServiceTypes.add(KnowledgeManager.get().requireConcept(BENEFIT_FLOW_TYPE));
		}
		
		IConcept theorSource = KnowledgeManager.get().requireConcept("eserv:TheoreticalSource");
		IConcept theorSink   = KnowledgeManager.get().requireConcept("eserv:TheoreticalSink");
		IConcept theorUse    = KnowledgeManager.get().requireConcept("eserv:TheoreticalUse");
		IConcept theorFlow   = KnowledgeManager.get().requireConcept("eserv:TheoreticalFlow");
		IConcept mainparent = null;
		
		if (c.is(BENEFIT_SOURCE_TYPE)) {
			mainparent = KnowledgeManager.get().requireConcept(BENEFIT_SOURCE_TYPE);
			trai = classifyTrait(c, theorSource);
		} else if (c.is(BENEFIT_SINK_TYPE)) {
			mainparent = KnowledgeManager.get().requireConcept(BENEFIT_SINK_TYPE);
			trai = classifyTrait(c, theorSink);
		} else if (c.is(BENEFIT_USE_TYPE)) {
			mainparent = KnowledgeManager.get().requireConcept(BENEFIT_USE_TYPE);
			trai = classifyTrait(c, theorUse);
		}  else if (c.is(BENEFIT_FLOW_TYPE)) {
			mainparent = KnowledgeManager.get().requireConcept(BENEFIT_FLOW_TYPE);
			trai = classifyTrait(c, theorFlow);			
		} else {
			return null;
		}
		
		/*
		 * determine the "main" parent that is a service concept but not a trait and not an 
		 * abstract general concept (may be self, so this last check is required).
		 */
		for (IConcept cc : c.getParents()) {
			if (cc.is(mainparent) && !cc.is("eserv:FlowRelatedTrait") && !mainServiceTypes.contains(cc)) {
				main = cc; 
				break;
			}
		}
		
 		
		return new Pair<IConcept, IConcept>(main, trai);
	}
}
