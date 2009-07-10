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
	
	static public void buildObservation(
			Collection<Object> coll,
			IInstance sourceData, 
			IInstance useData, 
			IInstance sinkData, 
			IInstance flowData, 
			Object output) throws ThinklabException {
		
		NetCDFArchive netcdf = new NetCDFArchive();
		netcdf.setObservation(sourceData);
		
		if (useData != null)
			netcdf.addObservation(useData);
		if (sinkData != null)
			netcdf.addObservation(sinkData);
		if (flowData != null)
			netcdf.addObservation(flowData);
		
		int i = 0;
		for (Object o : coll) {
			netcdf.addRasterVariable(flowObservables[i++], (double[][])o);
			if (i == 24)
				break;
		}
		
		netcdf.write(output.toString());
	}
}
