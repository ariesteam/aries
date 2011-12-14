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

package org.integratedmodelling.aries.valuation.implementations.observations;

import java.util.ArrayList;
import java.util.HashSet;

import org.integratedmodelling.aries.valuation.ARIESValuationPlugin;
import org.integratedmodelling.aries.valuation.calculator.ESCalculatorFactory;
import org.integratedmodelling.aries.valuation.models.ESVCalculatorModel;
import org.integratedmodelling.aries.valuation.models.ESVCalculatorModel.Influence;
import org.integratedmodelling.corescience.CoreScience;
import org.integratedmodelling.corescience.context.ObservationContext;
import org.integratedmodelling.corescience.implementations.datasources.MemDoubleContextualizedDatasource;
import org.integratedmodelling.corescience.implementations.observations.Observation;
import org.integratedmodelling.corescience.interfaces.IContext;
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
	public IContext transform(IObservationContext sourceCtx, ISession session,
			IContext context) throws ThinklabException {
		
		IConcept landUseConcept = KnowledgeManager.get().requireConcept(LAND_USE_DEPENDENCY);
		
		if (!this.getObservableClass().equals(ARIESValuationPlugin.ESVConcept()))
			this.service = this.getObservableClass();

		/*
		 * find out rows and cols from context and ensure all extents are
		 * OK.
		 */
		IExtent extent = context.getExtent(Geospace.get().SpaceObservable());
		
		if ( extent == null || !(extent instanceof GridExtent) || 
				!(extent.getValueCount() == ((IObservationContext)context).getMultiplicity()))
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
			state.getMetadata().put(Metadata.UNIT_SPECS, "$@2004/ac.yr");
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
	
		
		ObservationContext ret = new ObservationContext(context.getExtents());
		ret.setObservation(this);
		for (Pair<IConcept, IState> st : states) {
			ret.addState(st.getSecond());
		}
		
		return ret;

	}
}