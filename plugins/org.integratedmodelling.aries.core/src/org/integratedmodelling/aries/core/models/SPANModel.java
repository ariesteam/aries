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

package org.integratedmodelling.aries.core.models;

import java.util.ArrayList;
import java.util.Collection;

import org.integratedmodelling.corescience.CoreScience;
import org.integratedmodelling.corescience.interfaces.IContext;
import org.integratedmodelling.modelling.interfaces.IModel;
import org.integratedmodelling.modelling.model.DefaultAbstractModel;
import org.integratedmodelling.modelling.model.DefaultStatefulAbstractModel;
import org.integratedmodelling.modelling.model.ModelFactory;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabValidationException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.storage.IKBox;
import org.integratedmodelling.utils.Polylist;

import clojure.lang.Compiler;
import clojure.lang.Keyword;
import clojure.lang.PersistentHashMap;

public class SPANModel extends DefaultAbstractModel {

    public SPANModel(String namespace) {
		super(namespace);
		// TODO Auto-generated constructor stub
	}

	public static final String DOWNSCALE_PROPERTY_PREFIX = "aries.model.downsample.";

    /**
     * these are reclassified in the context of the main observable, so that the
     * corresponding parameters can be communicated to users appropriately.
     */
    private static final String SOURCE_THRESHOLD_CONCEPT = "eserv:SourceThreshold";
    private static final String SINK_THRESHOLD_CONCEPT = "eserv:SinkThreshold";
    private static final String USE_THRESHOLD_CONCEPT = "eserv:UseThreshold";
    private static final String TRANSITION_THRESHOLD_CONCEPT = "eserv:TransitionThreshold";
    private static final String DECAY_RATE_CONCEPT = "eserv:DecayRate";
    
    IConcept sourceObservableId = null;
    IConcept sinkObservableId = null;
    IConcept useObservableId = null;
    IConcept flowObservableId = null;
    Collection<IConcept> flowDataObservableIds = null;
  
    IConcept sourceObservable = null;
    IConcept sinkObservable = null;
    IConcept useObservable = null;
    IConcept flowObservable = null;
    Collection<IConcept> flowDataObservables = new ArrayList<IConcept>();
    private PersistentHashMap flowParams = PersistentHashMap.create(new Object[]{});
    
	ArrayList<IConcept> keepers = new ArrayList<IConcept>();

    
    /*
     * this is the actual way to pass parameters
     */
    ArrayList<Polylist> parameters = new ArrayList<Polylist>();
    
    @Override
    protected void copy(DefaultAbstractModel model) {
        super.copy(model);
        sourceObservableId = ((SPANModel)model).sourceObservableId;
        sinkObservableId = ((SPANModel)model).sinkObservableId;
        useObservableId = ((SPANModel)model).useObservableId;
        flowObservableId = ((SPANModel)model).flowObservableId;
        flowDataObservableIds = ((SPANModel)model).flowDataObservableIds;
        sourceObservable = ((SPANModel)model).sourceObservable;
        sinkObservable = ((SPANModel)model).sinkObservable;
        useObservable = ((SPANModel)model).useObservable;
        flowObservable = ((SPANModel)model).flowObservable;
        flowDataObservables = ((SPANModel)model).flowDataObservables;
        flowParams = ((SPANModel)model).flowParams;
        parameters = ((SPANModel)model).parameters;
//		keeperIds = ((SPANModel)model).keeperIds;
		keepers = ((SPANModel)model).keepers;
    }

    static Keyword downscalingFactor = Keyword.intern(null, "downscaling-factor");

    @Override
    protected void validateMediatedModel(IModel model)
            throws ThinklabValidationException {
        super.validateMediatedModel(model);
    }

    @Override
    public IConcept getCompatibleObservationType(ISession session) {
        return CoreScience.Observation();
    }

    public void setFlowObservables(IConcept source, IConcept use, IConcept sink, IConcept flow, Collection<Object> flowData) throws ThinklabException {
        this.sourceObservable = source;
        this.useObservable = use;
        this.sinkObservable = sink;
        this.flowObservable = flow;
        if (flowData != null) {
        	for (Object o : flowData)
        		this.flowDataObservables.add(
        			ModelFactory.annotateConcept(namespace, o.toString()));
        }
    }
    
    @Override
    public IModel getConfigurableClone() {
        
        SPANModel ret = new SPANModel(namespace);
        ret.copy(this);
        return ret; 
    }

    @Override
    public void applyClause(String keyword, Object argument)
            throws ThinklabException {

        if (keyword.equals(":source-threshold")   ||
            keyword.equals(":sink-threshold")     ||
            keyword.equals(":use-threshold")      ||
            keyword.equals(":trans-threshold")    ||
            keyword.equals(":source-type")        ||
            keyword.equals(":sink-type")          ||
            keyword.equals(":use-type")           ||
            keyword.equals(":benefit-type")       ||
            keyword.equals(":value-type")         ||
            keyword.equals(":rv-max-states")      ||
            keyword.equals(":downscaling-factor") ||
            keyword.equals(":animation?") ||
            keyword.equals(":save-file")) {

            Object evaledArgument;
            try {
                evaledArgument = Compiler.eval(argument);
            } catch (Exception e) {
                evaledArgument = argument;
            }

            // these must be sent back to Clojure, and a normal Map won't do - what a pain
            flowParams =
                (PersistentHashMap)
                    flowParams.assoc(Keyword.intern(null, keyword.substring(1)), evaledArgument);

        }  else if (keyword.equals(":keep")) {
			
			Collection<?> p = (Collection<?>) argument;
			for (Object c : p)
				keepers.add(ModelFactory.annotateConcept(namespace, c.toString()));

		} else {
            super.applyClause(keyword, argument);
        }
    }

    @Override
    public Polylist buildDefinition(IKBox kbox, ISession session, IContext context, int flags) throws ThinklabException {

        ArrayList<Object> arr = new ArrayList<Object>();
        
        // TODO if someone has overridden the downsampling factor for this model, put it in
        // fucking FIXME - the stupid thing still has null name even if we set it everywhere.
//      String pmod = DOWNSCALE_PROPERTY_PREFIX + this.name.replaceAll("\\/", "-");
//      
//      String ns = ARIESCorePlugin.get().getProperties().getProperty(pmod);
//      if (ns != null) {
//          Integer nds = Integer.parseInt(ns);
//          flowParams.put(downscalingFactor, nds);
//      }
            
        arr.add("aries:SPANTransformer");
        arr.add(Polylist.list(
                CoreScience.HAS_OBSERVABLE,
                Polylist.list(getObservableClass())));
        
        PersistentHashMap fp = (PersistentHashMap) PersistentHashMap.create(flowParams);
        
        for (IModel m : dependents) {
            if (m.getObservableClass().is(TRANSITION_THRESHOLD_CONCEPT)) {
                Object st = ((DefaultStatefulAbstractModel)m).getState();
                fp = (PersistentHashMap) fp.assoc(
                        Keyword.intern(null, "trans-threshold"), st);
            } else if (m.getObservableClass().is(SINK_THRESHOLD_CONCEPT)) {
                Object st = ((DefaultStatefulAbstractModel)m).getState();
                fp = (PersistentHashMap) fp.assoc(
                        Keyword.intern(null, "sink-threshold"), st);
            } else if (m.getObservableClass().is(USE_THRESHOLD_CONCEPT)) {
                Object st = ((DefaultStatefulAbstractModel)m).getState();
                fp = (PersistentHashMap) fp.assoc(
                        Keyword.intern(null, "use-threshold"), st);
            } else if (m.getObservableClass().is(SOURCE_THRESHOLD_CONCEPT)) {
                Object st = ((DefaultStatefulAbstractModel)m).getState();
                fp = (PersistentHashMap) fp.assoc(
                        Keyword.intern(null, "source-threshold"), st);
            } else if (m.getObservableClass().is(DECAY_RATE_CONCEPT)) {
                Object st = ((DefaultStatefulAbstractModel)m).getState();
                fp = (PersistentHashMap) fp.assoc(
                        Keyword.intern(null, "decay-rate"), st);                
            }
        }
		
        // add what to keep in result list
        for (int i = 0; i < keepers.size(); i++) {
			arr.add(Polylist.list(
						ModelFactory.RETAINS_STATES, 
						keepers.get(i).toString()));
		}
        
        // set flow parameters directly into instance implementation (SPANTransformer) 
        // using reflection. Non-serializable, but who cares.
        arr.add(Polylist.list(":flowParams",    fp));
        arr.add(Polylist.list(":sourceConcept", sourceObservable));
        arr.add(Polylist.list(":useConcept",    useObservable));
        arr.add(Polylist.list(":sinkConcept",   sinkObservable));
        arr.add(Polylist.list(":flowConcept",   flowObservable));
        arr.add(Polylist.list(":flowDataConcepts",   flowDataObservables));

        return addDefaultFields(Polylist.PolylistFromArrayList(arr));
    }

    @Override
    public Polylist conceptualize() throws ThinklabException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    protected void validateSemantics(ISession session) throws ThinklabException {
        
        // TODO take the concept IDs and annotate them as usual. NOTE: the 
        // concepts themselves should actually be observables of dependents, so
        // the whole thing needs attention.
		
//		for (String s : keeperIds) {
//			keepers.add(annotateConcept(s,session, null));
//		}
    }
    
    @Override
    public String toString() {
    	return "span";
    }

}
