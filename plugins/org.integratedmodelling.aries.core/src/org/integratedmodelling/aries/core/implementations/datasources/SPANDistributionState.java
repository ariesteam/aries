package org.integratedmodelling.aries.core.implementations.datasources;

import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.integratedmodelling.corescience.implementations.datasources.MemDoubleContextualizedDatasource;
import org.integratedmodelling.corescience.interfaces.IState;
import org.integratedmodelling.corescience.metadata.Metadata;
import org.integratedmodelling.thinklab.exception.ThinklabRuntimeException;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;

import clojure.lang.IFn;

public class SPANDistributionState extends MemDoubleContextualizedDatasource
		implements IState {

	// how many times the stdev should encompass the data range before we declare
	// complete uncertainty.
	// TODO this should encompass the POTENTIAL data range, not the actual.
	private static final double STD_UNCERTAINTY_MULTIPLIER = 1.6;
	private IFn closure = null;
	private int rows;
	private int cols;

	// for serializer only
	public SPANDistributionState() {
	}
	
	public SPANDistributionState(IConcept type, int rows, int cols, IFn clojure) {
		super(type, rows*cols);
		this.closure  = clojure;
		this.rows = rows;
		this.cols = cols;

		setMetadata(Metadata.CONTINUOUS, Boolean.TRUE);
		
		// TODO make it lazy by computing values only when getDataAsDoubles is called
		computeValues(clojure);
	}

	private void computeValues(IFn clojure) {
		try {
			
			Map<?,?> map = (Map<?, ?>) closure.invoke();

			/*
			 * we get a different distribution than the one we originally set in,
			 * so we store mean and standard deviation for now. Later we can get
			 * the whole thing and use it for more.
			 */
			if (map != null) {
				
				double mmax = 0.0; double mmin = 0.0;
				double smax = 0.0; double smin = 0.0;
				boolean first = true;
				double[] uncertainty = null;
				
				for (Map.Entry<?,?> e : map.entrySet()) {
				
					// LazilyPersistentVector
					Iterable<?> location = (Iterable<?>) e.getKey();
					// PersistentHashMap
					Map<?,?> distribu = (Map<?, ?>) e.getValue();
				
					Iterator<?> it = location.iterator();
					int x = ((Number)(it.next())).intValue();
					int y = ((Number)(it.next())).intValue();
	
					double mean = 0.0; double std = 0.0;
					for (Entry<?, ?> en : distribu.entrySet()) {
						double v = ((Number)en.getKey()).doubleValue();
						double p = ((Number)en.getValue()).doubleValue();
						mean += v*p;
						std  += v*v*p;
					}
					std -= mean*mean; std = Math.sqrt(std);
				
					/*
					 * set mean and std in proper location. If std == 0 it was
					 * a deterministic source.
					 */
					if (first) {
						first = false;
						mmax = mmin = mean;
					} else {
						if (mean > mmax) mmax = mean;
						if (mean < mmin) mmin = mean;
					}
					
					data[(x*cols) + y] = mean;
					if (Double.compare(std, 0.0) != 0) {
						if (uncertainty == null) {
							uncertainty = new double[this.rows*this.cols];
							smax = smin = std;
						} else {
							if (std > smax) smax = std;
							if (std < smin) smin = std;
						}
						uncertainty[(x*cols) + y] = std;
					}
				}
				
				/*
				 * normalize uncertainty and insert it
				 */
				if (uncertainty != null) {
					double range = (mmax - mmin) * STD_UNCERTAINTY_MULTIPLIER; 
					for (int i = 0; i < uncertainty.length; i++) {
						uncertainty[i] = uncertainty[i]/range;
						if (uncertainty[i] > 1.0)
							uncertainty[i] = 1.0;
					}
					setMetadata(Metadata.UNCERTAINTY, uncertainty);
				}
				
			}
			
		} catch (Exception e) {
			throw new ThinklabRuntimeException(e);
		}
	}

}
