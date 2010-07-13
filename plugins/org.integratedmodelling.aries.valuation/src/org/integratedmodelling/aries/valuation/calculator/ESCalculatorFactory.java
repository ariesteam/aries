package org.integratedmodelling.aries.valuation.calculator;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;

import org.apache.commons.collections.BidiMap;
import org.apache.commons.collections.bidimap.DualHashBidiMap;
import org.integratedmodelling.aries.valuation.ARIESValuationPlugin;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabRuntimeException;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.literals.BooleanValue;
import org.integratedmodelling.utils.Pair;
import org.integratedmodelling.utils.xml.XMLDocument;
import org.w3c.dom.Node;

public class ESCalculatorFactory {

	private static ESCalculatorFactory _this = null;
	
	public static ESCalculatorFactory get() {
		
		if (_this == null) {
			_this = new ESCalculatorFactory();
			try {
				_this.initialize(ARIESValuationPlugin.get().getResourceURL("calculator/ESVData.xml"));
			} catch (Exception e) {
				throw new ThinklabRuntimeException(e);
			}
		}
		return _this;
	}

	BidiMap esMappings = new DualHashBidiMap();
	BidiMap lcMappings = new DualHashBidiMap();
	
	class EcosystemService {
		public String id;
		public String label;
		public String description;
		public String classname;
	}
	
	class LandCoverClass {
		public String label;
		public String description;
	}
	
	private class ESValue {
		
		public double minValue = 0.0;
		public double maxValue = 0.0;
		public double origMinValue = 0.0;
		public double origMaxValue = 0.0;		
	}
		
	private HashMap<String, EcosystemService> ecosystemServices;
	private HashMap<String, LandCoverClass> landCoverClasses;
	private HashMap<String, ESValue> esValues;
	// the key for this one is <EcosystemServiceID>|<LandCoverClassID>; if not present, coefficient is 1.0
	private HashMap<String, Double> esCoefficients;
	
	ArrayList<String> declarationOrderLU;
	private ArrayList<String> sortedEcosystemServiceIDs;
	private ArrayList<IConcept> sortedEcosystemServiceConcepts = new ArrayList<IConcept>();
	private ArrayList<IConcept> sortedLandCoverConcepts = new ArrayList<IConcept>();
	
	/**
	 * Get global minimum value of passed ecosystem service
	 * @param ESid
	 * @return
	 */
	public double getGlobalMinimumESValue(IConcept es) {
		
		ESValue e = esValues.get((String) esMappings.getKey(es));
		return e.minValue;
	}
	
	/**
	 * Get global maximum value of passed ecosystem service
	 * @param ESid
	 * @return
	 */
	public double getGlobalMaximumESValue(IConcept es) {
		ESValue e = esValues.get((String) esMappings.getKey(es));
		return e.maxValue;
	}
	
	/**
	 * Get global minimum value of passed ecosystem service
	 * @param ESid
	 * @return
	 */
	public double getGlobalMinimumESValue(String ESid) {
		
		ESValue es = esValues.get(ESid);
		return es.minValue;
	}
	
	/**
	 * Get global maximum value of passed ecosystem service
	 * @param ESid
	 * @return
	 */
	public double getGlobalMaximumESValue(String ESid) {
		ESValue es = esValues.get(ESid);
		return es.maxValue;
	}
	/**
	 * Value of passed ES in passed LC for passed area in acres.
	 * @param es
	 * @param lc
	 * @param normalize (just conversion coefficient for ES in land use, twice)
	 * @return
	 */
	public Pair<Double, Double> computeValue(IConcept es, IConcept lc, boolean normalize) {
		
		String esid = (String) esMappings.getKey(es);
		String lcid = (String) lcMappings.getKey(lc);
		
		double min = getGlobalMinimumESValue(esid);
		double max = getGlobalMaximumESValue(esid);
		double con = getESConversionCoefficient(esid, lcid);
		
		return normalize ? 
				new Pair<Double, Double>(con, con) :
				new Pair<Double, Double>(min*con, max*con);
	}
	
	public IConcept getConceptForESID(String esid) {
		return (IConcept) esMappings.get(esid);
	}
	
	public String getIDForESConcept(String esid) {
		return (String) esMappings.getKey(esid);
	}
	
	public IConcept getConceptForLCID(String esid) {
		return (IConcept) lcMappings.get(esid);
	}
	
	public String getIDForLCConcept(String esid) {
		return (String) lcMappings.getKey(esid);
	}
	
	/**
	 * Get conversion coefficient for given ecosystem service in given land use
	 * @return
	 */
	public double getESConversionCoefficient(String ESid, String LUid) {
		double ret = 1.0;
		Double d = esCoefficients.get(ESid + "|" + LUid);
		if (d != null)
			ret = d;
		return ret;
	}
	
	/**
	 * Read XML configuration, cross-check all data
	 *
	 */
	public void initialize(URL configfile) throws Exception {
		
		XMLDocument doc = new XMLDocument(configfile);
		
		ecosystemServices = new HashMap<String, EcosystemService>();
		landCoverClasses = new HashMap<String, LandCoverClass>();
		esValues = new HashMap<String, ESValue>();
		esCoefficients = new HashMap<String, Double>();
		declarationOrderLU = new ArrayList<String>();
		
		// Read XML config
		for (Node n = doc.root().getFirstChild(); n != null; n = n.getNextSibling()) {
            
			if (n.getNodeName().equals("concept-mappings")) {
				
				for (Node nn = n.getFirstChild(); nn != null; nn = nn.getNextSibling()) {
					if (nn.getNodeName().equals("ecosystem-service")) {
						
						IConcept concept = 
							KnowledgeManager.get().requireConcept(
									XMLDocument.getAttributeValue(nn, "concept"));
						String id     = XMLDocument.getAttributeValue(nn, "id");
						String active = XMLDocument.getAttributeValue(nn, "active");

						boolean isActive = true;
						if (active != null)
							isActive = BooleanValue.parseBoolean(active); 
						
						if (isActive)
							sortedEcosystemServiceConcepts.add(concept);
						
						esMappings.put(id,concept);
						
					} else if  (nn.getNodeName().equals("land-cover")) {
						
						IConcept concept = 
							KnowledgeManager.get().requireConcept(
									XMLDocument.getAttributeValue(nn, "concept"));
						String id = XMLDocument.getAttributeValue(nn, "id");
						
						sortedLandCoverConcepts.add(concept);
						lcMappings.put(id,concept);
					} 
				}
			} else if (n.getNodeName().equals("ecosystem-services")) {
				
				for (Node nn = n.getFirstChild(); nn != null; nn = nn.getNextSibling()) {
					if (nn.getNodeName().equals("ecosystem-service")) {
						
						EcosystemService e = new EcosystemService();
						
						e.label = XMLDocument.getAttributeValue(nn, "label");
						e.id = XMLDocument.getAttributeValue(nn, "id");
						e.description = XMLDocument.getAttributeValue(nn, "description");
						e.classname = XMLDocument.getAttributeValue(nn, "class");
						ecosystemServices.put(e.id, e);
					}
				}				
				
			} else if (n.getNodeName().equals("land-covers")) {
				
				for (Node nn = n.getFirstChild(); nn != null; nn = nn.getNextSibling()) {
					
					if (nn.getNodeName().equals("land-cover")) {
						
						LandCoverClass lcc = new LandCoverClass();
						lcc.label = XMLDocument.getAttributeValue(nn, "label");
						lcc.description = XMLDocument.getAttributeValue(nn, "description");
						
						String id = XMLDocument.getAttributeValue(nn, "id");
						landCoverClasses.put(id, lcc);
						declarationOrderLU.add(id);
					}
				}
				
			} else if (n.getNodeName().equals("values")) {
				
				for (Node nn = n.getFirstChild(); nn != null; nn = nn.getNextSibling()) {

					if (nn.getNodeName().equals("value")) {
						
						ESValue val = new ESValue();
						
						String o1 = XMLDocument.getAttributeValue(nn, "calculated-min-value");
						
						val.origMinValue = 
							(o1 == null || o1.equals("")) ? 
									0.0 :
									Double.parseDouble(o1);
						
						o1 = XMLDocument.getAttributeValue(nn, "calculated-max-value");
						val.origMaxValue = 
							(o1 == null || o1.equals("")) ? 
									0.0 :
									Double.parseDouble(o1);

						o1 = XMLDocument.getAttributeValue(nn, "min-value");
						val.minValue = 
							(o1 == null || o1.equals("")) ? 
									val.origMinValue :
									Double.parseDouble(o1);

						o1 = XMLDocument.getAttributeValue(nn, "max-value");
						val.maxValue = 
							(o1 == null || o1.equals("")) ? 
									val.origMaxValue :
									Double.parseDouble(o1);

						o1 = XMLDocument.getAttributeValue(nn, "service-id");

						if (this.getEcosystemService(o1) == null)
							throw new IOException("CONFIGURATION ERROR: ecosystem service category " +
									               o1 + " does not exist");
						
						esValues.put(o1, val);
					}
				}
				
			} else if (n.getNodeName().equals("coefficients")) {
				
				for (Node nn = n.getFirstChild(); nn != null; nn = nn.getNextSibling()) {
					
					if (nn.getNodeName().equals("coefficient")) {
			
						String service = XMLDocument.getAttributeValue(nn, "service-id");
						if (this.getEcosystemService(service) == null)
							throw new IOException("CONFIGURATION ERROR: ecosystem service category " +
												   service + " does not exist");

						String cover = XMLDocument.getAttributeValue(nn, "cover-id");
						if (this.getLandCoverClass(cover) == null)
							throw new IOException("CONFIGURATION ERROR: land cover class " +
												   cover + " does not exist");
						
						String o1 = XMLDocument.getAttributeValue(nn, "value");
						
						double d = 
							(o1 == null || o1.equals("")) ? 
									0.0 :
									Double.parseDouble(o1);
						
						esCoefficients.put(service + "|" + cover, new Double(d));
					}
				}	
			}	
		}
		
		
		// create order for ES ids based on alphabetical class and label
		sortedEcosystemServiceIDs = new ArrayList<String>();
		
		String[] ll = new String[ecosystemServices.size()];

		int i = 0;		
		for (EcosystemService e : ecosystemServices.values()) {
			ll[i++] = e.classname + "|" + e.label + "|" + e.id;
		}
		
		Arrays.sort(ll);
		
		for (String s : ll) {
			sortedEcosystemServiceIDs.add(s.substring(s.lastIndexOf("|")+1));
		}
	}

	public Collection<String> getSortedLandUseIDs() {
		return declarationOrderLU;		
	}

	public LandCoverClass getLandCoverClass(String id) {
		return landCoverClasses.get(id);
	}

	public Collection<IConcept> getAllESConcepts() {
		return sortedEcosystemServiceConcepts;
	}
	
	public Collection<IConcept> getAllLCConcepts() {
		return sortedLandCoverConcepts;
	}

	/**
	 * Return a collection of ecosystem service IDs sorted by class+ID
	 * @return
	 */
	public Collection<String> getSortedEcosystemServiceIDs() {
		return sortedEcosystemServiceIDs;
	}

	public EcosystemService getEcosystemService(String s) {
		return ecosystemServices.get(s);
	}
	
}
