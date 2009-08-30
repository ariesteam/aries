package org.integratedmodelling.aries.core.gssm;

import java.util.Map;

import org.integratedmodelling.thinklab.interfaces.knowledge.IInstance;

/**
 * Used to talk to the GSSM Clojure side. This is used in a proxy form to yield an object that can be used
 * to run GSSM from Java.
 * 
 * @author Ferdinando Villa
 *
 */
public interface GSSMProxy {

	/**
	 * Run a GSSM simulation from four observations of source, sink, use and flow. Choose the flow model 
	 * according to the observable of the flow observation.
	 * 
	 * @param source
	 * @param use
	 * @param sink
	 * @param flow
	 * @return a flow map, from which results can be obtained.
	 */
	public abstract Map<?,?> runGSSM(IInstance source, IInstance use, IInstance sink, IInstance flow);
}
