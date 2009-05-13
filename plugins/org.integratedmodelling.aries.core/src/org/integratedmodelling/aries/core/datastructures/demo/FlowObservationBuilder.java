package org.integratedmodelling.aries.core.datastructures.demo;

import java.util.Collection;

import org.integratedmodelling.modelling.visualization.NetCDFArchive;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.knowledge.IInstance;

public class FlowObservationBuilder {
	
	static String[] flowObservables = {
		"TheoreticalSource",
		"TheoreticalSink",
		"TheoreticalUse",
		"InaccessibleSource",
		"InaccessibleSink",
		"InaccessibleUse",
		"PossibleFlow",
		"PossibleSource",
		"PossibleInflow",
		"PossibleSink",
		"PossibleUse",
		"PossibleOutflow",
		"BlockedFlow",
		"BlockedSource",
		"BlockedInflow",
		"BlockedSink",
		"BlockedUse",
		"BlockedOutflow",
		"ActualFlow",
		"ActualSource",
		"ActualInflow",
		"ActualSink",
		"ActualUse",
		"ActualOutflow"
	};
	
	static public void buildObservation(Collection<Object> coll,
			IInstance sourceData, Object output) throws ThinklabException {
		
		NetCDFArchive netcdf = new NetCDFArchive();
		netcdf.setObservation(sourceData);
		
		int i = 0;
		for (Object o : coll) {
			netcdf.addRasterVariable(flowObservables[i++], (double[][])o);
		}
		
		netcdf.write(output.toString());
	}
}
