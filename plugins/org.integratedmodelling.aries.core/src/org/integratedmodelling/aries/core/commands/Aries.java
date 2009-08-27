package org.integratedmodelling.aries.core.commands;

import org.integratedmodelling.aries.core.datastructures.demo.ARIESDemoKbox;
import org.integratedmodelling.aries.core.tasks.prioritization.SelectRegionOfInterest;
import org.integratedmodelling.corescience.contextualization.Compiler;
import org.integratedmodelling.corescience.interfaces.observation.IObservation;
import org.integratedmodelling.geospace.implementations.observations.RasterGrid;
import org.integratedmodelling.modelling.Model;
import org.integratedmodelling.modelling.ModelManager;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.command.Command;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabValidationException;
import org.integratedmodelling.thinklab.interfaces.annotations.ThinklabCommand;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.commands.ICommandHandler;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.knowledge.IInstance;
import org.integratedmodelling.thinklab.interfaces.literals.IValue;
import org.integratedmodelling.thinklab.interfaces.query.IQueryResult;
import org.integratedmodelling.thinklab.interfaces.storage.IKBox;
import org.integratedmodelling.thinklab.literals.ObjectReferenceValue;
import org.integratedmodelling.utils.Polylist;

@ThinklabCommand(
		name="aries",
		argumentNames="command",
		argumentTypes="thinklab-core:Text",
		argumentDescriptions="subtask for ARIES to execute",
		optionalArgumentNames="p1,p2,p3,p4,p5,p6",
		optionalArgumentTypes="thinklab-core:Text,thinklab-core:Text,thinklab-core:Text,thinklab-core:Text,thinklab-core:Text,thinklab-core:Text",
		optionalArgumentDescriptions="p1,p2,p3,p4,p5,p6",
		optionalArgumentDefaultValues="_,_,_,_,_,_",
		optionArgumentLabels="p1,p2,p3,p4,p5,p6"
)
public class Aries implements ICommandHandler {

	private String getParameter(Command command, int n, String label) throws ThinklabValidationException {
		String ret = command.getArgumentAsString("p" + n);
		if (ret == null || ret.equals("_"))
			throw new ThinklabValidationException(n + "-th command argument '" + label + "' required");
		return ret;
 	}
	
	@Override
	public IValue execute(Command command, ISession session)
			throws ThinklabException {

		IValue ret = null;
		String cmd = command.getArgumentAsString("command");
		IKBox kbox = new ARIESDemoKbox();

		SelectRegionOfInterest rtask = new SelectRegionOfInterest();
		rtask.run(session);
		IInstance where = 
			session.createObject(RasterGrid.createRasterGrid(rtask.getRegionOfInterest(), 128));
		
		if (cmd.equals("model")) {
			
			String c = getParameter(command, 1, "concept");
			Model model = ModelManager.get().requireModel(c);
			IQueryResult r = model.observe(kbox, session, where);
					
			if (session.getOutputStream() != null) {
				
				session.getOutputStream().println(
						"query returned " + r.getTotalResultCount() + " results");
			
				if (r.getTotalResultCount() > 0) {	
					Polylist lr = r.getResultAsList(0, null);
					session.getOutputStream().println(Polylist.prettyPrint(lr));
					IInstance obs = session.createObject(lr);
					
					IInstance result = 
						Compiler.contextualize((IObservation)obs.getImplementation(), session);	
					
					ret = new ObjectReferenceValue(result);
				}
			}
		}
		
		return ret;
	}

}
