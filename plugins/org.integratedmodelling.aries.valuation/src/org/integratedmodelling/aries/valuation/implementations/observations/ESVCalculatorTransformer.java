package org.integratedmodelling.aries.valuation.implementations.observations;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Map;

import org.integratedmodelling.aries.valuation.ARIESValuationPlugin;
import org.integratedmodelling.aries.valuation.calculator.ESCalculatorFactory;
import org.integratedmodelling.aries.valuation.models.ESVCalculatorModel;
import org.integratedmodelling.aries.valuation.models.ESVCalculatorModel.Influence;
import org.integratedmodelling.corescience.CoreScience;
import org.integratedmodelling.corescience.context.ObservationContext;
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
import org.integratedmodelling.utils.Triple;

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

	// reflected, must be public
	public boolean normalize = false;
	public ArrayList<Influence> influences = null;
	
	ArrayList<Pair<GeneralClassifier, IConcept>> classifiers = 
		new ArrayList<Pair<GeneralClassifier,IConcept>>();
	
	HashSet<IConcept> outputStates = new HashSet<IConcept>();
	IConcept cSpace = null;
	
	// if not null, 
	public IConcept service = null;
	
	static double SQUARE_METERS_TO_ACRES = 0.000247105381;

	@Override
	public IConcept getTransformedObservationClass() {
		return CoreScience.Observation();
	}
	
	@Override
	public Polylist transform(IObservationContext sourceCtx, ISession session,
			IObservationContext context) throws ThinklabException {
		
		IConcept landUseConcept = KnowledgeManager.get().requireConcept(LAND_USE_DEPENDENCY);
		
		if (!this.getObservableClass().equals(ARIESValuationPlugin.ESVConcept()))
			this.service = this.getObservableClass();

		/*
		 * find out rows and cols from context and ensure all extents are
		 * OK.
		 */
		IExtent extent = context.getExtent(Geospace.get().SpaceObservable());
		
		if ( extent == null || !(extent instanceof GridExtent) || 
				!(extent.getValueCount() == context.getMultiplicity()))
			throw new ThinklabValidationException("span model run in a non-spatial context or with non-spatial extents");

//		int rows = ((GridExtent)extent).getYCells();
//		int cols = ((GridExtent)extent).getXCells();
		int size = extent.getValueCount();
		
		// conversion factor for the silly dollars/acre stuff
		double cellAcres = ((GridExtent)extent).getCellAreaMeters() * SQUARE_METERS_TO_ACRES;
		
		System.out.println("Trattasi di " + cellAcres/SQUARE_METERS_TO_ACRES + " m2, " + cellAcres + " acri");
		
		/*
		 * create states from closures where the concept space of the observable
		 * contains an observable for the result type.
		 */
		ArrayList<Pair<IConcept, IState>> states = new ArrayList<Pair<IConcept,IState>>();
		
		/*
		 * TODO the observable should be the VALUE OF, not the concept - again we need to
		 * use individuals for observables
		 */
		ArrayList<Triple<Double,Double,Double>> totals = new ArrayList<Triple<Double,Double,Double>>();
		for (IConcept observable : ESCalculatorFactory.get().getAllESConcepts()) {
			
			if (service != null && !observable.is(service))
				continue;
			
			IState state = new MemDoubleContextualizedDatasource(observable, size, 
					(ObservationContext)context); 
			state.getMetadata().put(Metadata.UNITS, "$@2004/ac.yr");
			state.getMetadata().put(Metadata.CONTINUOUS, Boolean.TRUE);
			state.getMetadata().put(Metadata.RANGE_MIN, new double[size]);
			state.getMetadata().put(Metadata.RANGE_MAX, new double[size]);
			// FIXME state.setMetadata(Metadata.DEFINING_MODEL, this);

			states.add(new Pair<IConcept, IState>(observable, state));	
			totals.add(new Triple<Double, Double, Double>(0.0,0.0,0.0));
		} 	
		
		IState state = sourceCtx.getState(landUseConcept);

		/*
		 * create data
		 */
		for (int i = 0; i < size; i++) {
			
			// total value
			double tmean = 0.0;
			double tmin  = 0.0;
			double tmax  = 0.0;
			
			int stt = 0;
			for (Pair<IConcept, IState> cs : states) {
				
				/*
				 * get silly landcover concept
				 */
				IConcept landcover = (IConcept) state.getValue(i);

				double[] mins = (double[]) cs.getSecond().getMetadata().get(Metadata.RANGE_MIN);
				double[] maxs = (double[]) cs.getSecond().getMetadata().get(Metadata.RANGE_MAX);
			
				if (landcover != null) {
					
					/*
					 * compute silly value for silly ecosystem service
					 */
					Pair<Double, Double> vres = 
						ESCalculatorFactory.get().computeValue(cs.getFirst(), landcover, normalize);
				
										
					/*
					 * add mean, min and max to ds and source, memorized as values per acre, not
					 * per pixel.
					 */
					mins[i] = vres.getFirst();
					maxs[i] = vres.getSecond();

					/*
					 * process influences if any
					 */
					if ((influences != null && influences.size() > 0)) {
						
						for (ESVCalculatorModel.Influence inf : influences) {
							
							IState ist = sourceCtx.getState(inf.observable);
							if (ist == null)
								throw new ThinklabValidationException("cannot find state for required influence " + inf.observable);
							double n = ist.getDoubleValue(i);
							if (Double.isNaN(n))
								continue;
							if (n < inf.min) n = inf.min;
							if (n > inf.max) n = inf.max;
							double mul = (n - inf.min)/(inf.max - inf.min);
							mins[i] += mins[i]*mul*inf.influenceFactor;
							maxs[i] += maxs[i]*mul*inf.influenceFactor;
						}
						
					}

					double mean = mins[i] + (maxs[i]- mins[i])/2; 
					
					cs.getSecond().setValue(i, new Double(mean));

					tmin  += mins[i]*cellAcres; 
					tmax  += maxs[i]*cellAcres; 
					tmean += mean*cellAcres;
				
					// total ACTUAL values per pixel cumulated
					Triple<Double,Double,Double> tt = totals.get(stt);
					tt.setFirst(tt.getFirst() + mins[i]);
					tt.setSecond(tt.getSecond() + maxs[i]);
					tt.setThird(tt.getThird() + mean);
					totals.set(stt, tt);

					
				} else {
					
					mins[i] = Double.NaN;
					maxs[i] = Double.NaN;
					cs.getSecond().setValue(i, Double.NaN);
					
				}
				stt ++;
			}
		}
		
	
		/*
		 * set aggregate totals in metadata for display
		 */
		int stt = 0;
		for (Pair<IConcept, IState> cs : states) {
			cs.getSecond().getMetadata().put(Metadata.AGGREGATED_MIN, totals.get(stt).getFirst());
			cs.getSecond().getMetadata().put(Metadata.AGGREGATED_MAX, totals.get(stt).getSecond());
			cs.getSecond().getMetadata().put(Metadata.AGGREGATED_TOTAL, totals.get(stt).getThird());
			stt++;
		}
		
		/*
		 * prepare new observation
		 */
		Polylist rdef = Polylist.list(
				CoreScience.OBSERVATION,
				Polylist.list(
						CoreScience.HAS_OBSERVABLE, getObservable().toList(null)));
		
		
		/*
		 * add all dependents - for now these have means and stds in them, not 
		 * distributions, so a ranking is appropriate.
		 */
		Polylist stdef = null;
		for (Pair<IConcept, IState> st : states) {
			
			Polylist ddef = Polylist.list(
					CurrencyPlugin.MONETARY_VALUE_OBSERVATION,
					Polylist.list(
							CoreScience.HAS_OBSERVABLE, Polylist.list(st.getFirst())),
					Polylist.list(
							CoreScience.HAS_DATASOURCE, 
							st.getSecond().conceptualize()));
			
			rdef = ObservationFactory.addDependency(rdef, stdef = ddef);
		}
		
		/*
		 * return the lone service obs if that's what we asked for.
		 */
		Polylist ret = 
			(service != null && stdef != null) ?
					stdef : rdef;

		/*
		 * make new extents to match previous
		 */
		for (IConcept ext : context.getDimensions()) {
			ret = ObservationFactory.addExtent(ret, context.getExtent(ext).conceptualize());
		}

		return ret;
		
	}
}