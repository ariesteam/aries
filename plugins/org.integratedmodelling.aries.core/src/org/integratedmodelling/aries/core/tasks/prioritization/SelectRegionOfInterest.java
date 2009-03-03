package org.integratedmodelling.aries.core.tasks.prioritization;


import org.integratedmodelling.geospace.Geospace;
import org.integratedmodelling.geospace.literals.ShapeValue;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;
import org.integratedmodelling.thinklab.interfaces.annotations.TaskNamespace;

/**
 * This is an obviously interactive task, so what this one should do is to return a random 
 * place from a test list, or one place for demonstration. 
 * 
 * @author Ferdinando
 *
 */
@TaskNamespace(ns = "aries")
public class SelectRegionOfInterest implements ITask {

	static public final String UPPER_EASTERN_USA = "POLYGON((-78.046875 48.515625,-106.875 48.515625,-106.875 31.640625,-78.75 31.640625,-78.75 31.640625,-78.046875 48.515625))";
	static public final String KING_COUNTY_PORTION = "POLYGON((-123.842070 47.723772,-121.619678 47.723772,-121.619678 47.028239,-121.619678 47.723772,-123.842070 47.723772))";
	static public final String KING_COUNTY_SMALL = "POLYGON((-122.2 47.6,-122.1 47.6,-122.1 47.5,-122.1 47.5,-122.2 47.6))";
	ShapeValue chosen = null;
	
	public ShapeValue getRegionOfInterest() {
		return chosen;
	}
	
	@Override
	public void run(ISession session)  throws ThinklabException  {

		// TODO could randomize a region for testing.
		chosen = 
			(ShapeValue) KnowledgeManager.get().validateLiteral(Geospace.get().Polygon(), KING_COUNTY_SMALL);
	}

}
