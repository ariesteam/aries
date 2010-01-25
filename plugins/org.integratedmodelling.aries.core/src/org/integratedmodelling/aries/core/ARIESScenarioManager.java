package org.integratedmodelling.aries.core;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Stack;

import org.integratedmodelling.corescience.interfaces.IObservation;
import org.integratedmodelling.modelling.Model;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.knowledge.IInstance;
import org.integratedmodelling.thinklab.interfaces.knowledge.IKnowledge;
import org.integratedmodelling.utils.Pair;

/**
 * A container for scenarios
 * @author Ferdinando Villa
 *
 */
public class ARIESScenarioManager {

	public static class Scenario {

		IKnowledge observable;
		String id;
		Model  model;
		Stack<IInstance> resultStack = 
			new Stack<IInstance>();
		HashSet<String> results = new HashSet<String>();
		
	}
		
	HashMap<String, Scenario> scenarios = new HashMap<String, Scenario>();
	/**
	 * what this scenario observes. All scenarios will observe either this
	 * concept (baseline) or a specialized instance of it.
	 */
	IConcept mainObservable;
	
	/**
	 * When a new result is available, put it here and we'll do the rest, including
	 * making and storing maps.
	 * 
	 * @param result
	 * @param scenario
	 */
	public void addResult(IInstance result, String scenario) {
		
	}
	
	/**
	 * Get the observation for a given observable in a given scenario.
	 * 
	 * @param observable
	 * @param scenario
	 * @return
	 */
	public IObservation getResult(IConcept observable, String scenario) {
		return null;
	}
	
	/**
	 * Return the ordered collection of images to show and the corresponding
	 * observables in the given scenario.
	 * 
	 * @param scenario
	 * @return
	 */
	public Collection<Pair<IConcept, String>> getImageUrls(String scenario) {
		return null;
	}
	
	/**
	 * Return the URL of a zip file containing all scenario results for a given
	 * scenario.
	 * 
	 * @param scenario
	 * @return
	 */
	public String exportScenario(String scenario) {
		return null;
	}
	
	/**
	 * Return the URL of a kvm file containing all scenario results for a given
	 * scenario, so that it can be visualized in Google Earth.
	 * 
	 * @param scenario
	 * @return
	 */
	public String exportToKVM(String scenario) {
		return null;
	}
}
