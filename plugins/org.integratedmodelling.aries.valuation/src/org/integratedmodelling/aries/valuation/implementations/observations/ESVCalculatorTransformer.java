package org.integratedmodelling.aries.valuation.implementations.observations;

import java.util.ArrayList;
import java.util.HashSet;

import org.integratedmodelling.aries.valuation.calculator.ESCalculatorFactory;
import org.integratedmodelling.corescience.CoreScience;
import org.integratedmodelling.corescience.implementations.datasources.MemDoubleContextualizedDatasource;
import org.integratedmodelling.corescience.implementations.observations.Observation;
import org.integratedmodelling.corescience.interfaces.IExtent;
import org.integratedmodelling.corescience.interfaces.IObservationContext;
import org.integratedmodelling.corescience.interfaces.IState;
import org.integratedmodelling.corescience.interfaces.internal.TransformingObservation;
import org.integratedmodelling.corescience.literals.GeneralClassifier;
import org.integratedmodelling.corescience.metadata.Metadata;
import org.integratedmodelling.currency.CurrencyPlugin;
import org.integratedmodelling.geospace.Geospace;
import org.integratedmodelling.geospace.extents.GridExtent;
import org.integratedmodelling.modelling.ObservationFactory;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabValidationException;
import org.integratedmodelling.thinklab.interfaces.annotations.InstanceImplementation;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.knowledge.IInstance;
import org.integratedmodelling.utils.Pair;
import org.integratedmodelling.utils.Polylist;

/**
 * Support for running the Clojure SPAN models and making lazy observation proxies to its
 * results.  
 * @author Ferdinando
 */
@InstanceImplementation(concept="esvaluation:ESVCalculator")
public class ESVCalculatorTransformer 
	extends Observation 
	implements TransformingObservation {
	
	/*
	 * the silly typology of ES we're using in this thing.
	 */
	private static final String LAND_USE_DEPENDENCY = "esclass:HistoricalESLandcover";

	ArrayList<Pair<GeneralClassifier, IConcept>> classifiers = 
		new ArrayList<Pair<GeneralClassifier,IConcept>>();
	
	HashSet<IConcept> outputStates = new HashSet<IConcept>();
	IConcept cSpace = null;
	
	static double SQUARE_METERS_TO_ACRES = 0.000247105381;
	
	@Override
	public IObservationContext getTransformedContext(IObservationContext context)
			throws ThinklabException {
		return context;
	}

	@Override
	public IConcept getTransformedObservationClass() {
		return CoreScience.Observation();
	}
	@Override
	public IInstance transform(IInstance sourceObs, ISession session,
			IObservationContext context) throws ThinklabException {
		
		IConcept landUseConcept = KnowledgeManager.get().requireConcept(LAND_USE_DEPENDENCY);

		/*
		 * find out rows and cols from context and ensure all extents are
		 * OK.
		 */
		IExtent extent = context.getExtent(Geospace.get().SpaceObservable());
		
		if ( extent == null || !(extent instanceof GridExtent) || 
				!(extent.getTotalGranularity() == context.getMultiplicity()))
			throw new ThinklabValidationException("span model run in a non-spatial context or with non-spatial extents");

		int rows = ((GridExtent)extent).getYCells();
		int cols = ((GridExtent)extent).getXCells();
		int size = extent.getTotalGranularity();
		
		// conversion factor for the silly dollars/acre stuff
		double cellAcres = ((GridExtent)extent).getCellAreaMeters() * SQUARE_METERS_TO_ACRES;
		
		/*
		 * create states from closures where the concept space of the observable
		 * contains an observable for the result type.
		 */
		ArrayList<Pair<IConcept, IState>> states = new ArrayList<Pair<IConcept,IState>>();
		
		/*
		 * TODO the observable should be the VALUE OF, not the concept - again we need to
		 * use individuals for observables
		 */
		for (IConcept observable : ESCalculatorFactory.get().getAllESConcepts()) {
			
			IState state = new MemDoubleContextualizedDatasource(observable, size); 
			state.setMetadata(Metadata.UNITS, "USD@2004/year");
			state.setMetadata(Metadata.RANGE_MIN, new double[size]);
			state.setMetadata(Metadata.RANGE_MAX, new double[size]);
			states.add(new Pair<IConcept, IState>(observable, state));	
		} 	
		
		/*
		 * find the state of our very only dependency
		 */
		IState state = ObservationFactory.getStateMap(sourceObs).get(landUseConcept);

		/*
		 * create data
		 */
		for (int i = 0; i < size; i++) {
			
			// total value
			double tmean = 0.0;
			double tmin  = 0.0;
			double tmax  = 0.0;
			
			for (Pair<IConcept, IState> cs : states) {
				
				/*
				 * get silly landcover concept
				 */
				IConcept landcover = (IConcept) state.getValue(i, null);

				double[] mins = (double[]) cs.getSecond().getMetadata(Metadata.RANGE_MIN);
				double[] maxs = (double[]) cs.getSecond().getMetadata(Metadata.RANGE_MAX);
			
				
				if (landcover != null) {
					
					/*
					 * compute silly value for silly ecosystem service
					 */
					Pair<Double, Double> vres = 
						ESCalculatorFactory.get().computeValue(cs.getFirst(), landcover, cellAcres);
				
					double mean = vres.getFirst() + (vres.getSecond()- vres.getFirst())/2; 
					
					/*
					 * add mean, min and max to ds and source
					 */
					mins[i] = vres.getFirst();
					maxs[i] = vres.getSecond();
				
					cs.getSecond().addValue(new Double(mean));

					tmin  += mins[i]; 
					tmax  += maxs[i]; 
					tmean += mean;
					
				} else {
					
					mins[i] = Double.NaN;
					maxs[i] = Double.NaN;
					cs.getSecond().addValue(Double.NaN);
					
				}
			}
		}
		
		/*
		 * prepare new observation
		 */
		Polylist rdef = Polylist.list(
				CoreScience.OBSERVATION,
				Polylist.list(
						CoreScience.HAS_OBSERVABLE, getObservable().toList(null)));
		
		/*
		 * make new extents to match previous
		 */
		for (IConcept ext : context.getDimensions()) {
			rdef = ObservationFactory.addExtent(rdef, context.getExtent(ext).conceptualize());
		}
		
		/*
		 * add all dependents - for now these have means and stds in them, not 
		 * distributions, so a ranking is appropriate.
		 */
		for (Pair<IConcept, IState> st : states) {
			
			Polylist ddef = Polylist.list(
					CurrencyPlugin.MONETARY_VALUE_OBSERVATION,
					Polylist.list(
							CoreScience.HAS_OBSERVABLE, Polylist.list(st.getFirst())),
					Polylist.list(
							CoreScience.HAS_DATASOURCE, 
							st.getSecond().conceptualize()));
			
			rdef = ObservationFactory.addDependency(rdef, ddef);
		}
		
		return session.createObject(rdef);
		
	}
}