package org.integratedmodelling.aries.core.implementations.datasources;

import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.integratedmodelling.corescience.implementations.datasources.MemDoubleContextualizedDatasource;
import org.integratedmodelling.corescience.interfaces.IState;
import org.integratedmodelling.thinklab.exception.ThinklabRuntimeException;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;

import clojure.lang.IFn;
import clojure.lang.LazilyPersistentVector;
import clojure.lang.Ratio;

public class SPANDistributionState extends MemDoubleContextualizedDatasource
		implements IState {

	private IFn closure = null;

	public SPANDistributionState(IConcept type, int rows, int cols, IFn clojure) {
		super(type, rows*cols);
		this.closure  = clojure;
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
			for (Map.Entry<?,?> e : map.entrySet()) {
				
				// LazilyPersistentVector
				Iterable<?> location = (Iterable<?>) e.getKey();
				// PersistentHashMap
				Map<?,?> distribu = (Map<?, ?>) e.getValue();
				
				Iterator<?> it = location.iterator();
				int x = ((Number)(it.next())).intValue();
				int y = ((Number)(it.next())).intValue();
	
				double mean = 0.0;
				for (Entry<?, ?> en : distribu.entrySet()) {
					double v = ((Number)en.getKey()).doubleValue();
					double p = ((Number)en.getValue()).doubleValue()/100.0;
					mean += v*p;
				}
				
				/*
				 * TODO set mean in proper location
				 */
			}
			
		} catch (Exception e) {
			throw new ThinklabRuntimeException(e);
		}
	}

}
