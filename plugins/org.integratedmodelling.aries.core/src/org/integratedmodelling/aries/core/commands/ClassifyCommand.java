package org.integratedmodelling.aries.core.commands;

import org.integratedmodelling.aries.core.ARIESNamespace;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.command.Command;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.annotations.ThinklabCommand;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.commands.ICommandHandler;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.literals.IValue;

/**
 * TODO
 * testing only, remove
 * @author Ferdinando
 *
 */
@ThinklabCommand(
		name="classify",
		argumentNames="concept",
		argumentDescriptions="concept to classify",
		argumentTypes="thinklab-core:Text")
public class ClassifyCommand implements ICommandHandler {

	@Override
	public IValue execute(Command command, ISession session)
			throws ThinklabException {

		IConcept c = KnowledgeManager.get().requireConcept(command.getArgumentAsString("concept"));
		session.getOutputStream().println(ARIESNamespace.classifyObservable(c));
		
		return null;
	}

}
