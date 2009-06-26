package org.integratedmodelling.aries.valuation.tools;

import java.io.PrintStream;
import java.net.URL;

import org.integratedmodelling.aries.valuation.ARIESValuationPlugin;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.utils.XMLDocument;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

public class ESDTranslator {

	/*
	 *   <esval:value ima:id="esdvor-1">

    <geospace:geospace>
      <geospace:narrative> Whole globe </geospace:narrative>

      <geospace:context>
        <geospace:administrative>
          <geospace:context-type> geospace:world </geospace:context-type>

          <geospace:context-id> World </geospace:context-id>

          <geospace:context-key> geospace:narrative-description
          </geospace:context-key>
        </geospace:administrative>
      </geospace:context>
    </geospace:geospace>

    <ima:classification>
      <documentation:bibliographic-reference ima:references="pg:///esd#esdbr-2" />
    </ima:classification>

    <ima:state ima:initial-value="0.72"
               ima:state-type="ima:long-floating-point"
               ima:unit="USD-2001/(ha yr)" />


  </esval:value>
	 */
	public static void translateValData(URL document) throws ThinklabException {
		
		XMLDocument doc = new XMLDocument(document);
		XMLDocument.NodeIterator it = doc.iterator();
		PrintStream out = System.out;
		
		while (it.hasNext()) {
			
			Node n = it.next();
			if (!n.getNodeName().equals("esval:value"))
				continue;

			Node state = XMLDocument.findNode(n, "ima:state");
			Node biblio = XMLDocument.findNode(n, "documentation:bibliographic-reference");
			Node geosp = XMLDocument.findNode(n, "geospace:geospace");

			String originalValue = XMLDocument.getTextValue((Element) n, "esval:original-value");
			String originalUnits = XMLDocument.getTextValue((Element) n, "esval:original-unit");
			String valMethod = XMLDocument.getTextValue((Element) n, "esval:valuation-method");
			String label = XMLDocument.getTextValue((Element) n, "ima:name");
			String biome = XMLDocument.getTextValue((Element) n, "esval:biome");
			String eservice = XMLDocument.getTextValue((Element) n, "esval:ecosystem-service");
			String comment = XMLDocument.getTextValue((Element) n, "ima:description");
			String keywords = XMLDocument.getTextValue((Element) n, "ima:keywords");

			
			String actualValue = XMLDocument.getAttributeValue(state, "ima:initial-value");
			String currency = XMLDocument.getAttributeValue(state, "ima:state-type");
			String id = XMLDocument.getAttributeValue(n, "ima:id");
			
			String bibref = XMLDocument.getAttributeValue(biblio, "ima:references");
			bibref = bibref.substring(bibref.indexOf("#") + 1);
			
		}
		
		

	}

	public static void translateBibData(URL document) throws ThinklabException {
		
		XMLDocument doc = new XMLDocument(document);
		XMLDocument.NodeIterator it = doc.iterator();
		PrintStream out = System.out;
		
		while (it.hasNext()) {
			
			Node n = it.next();
			if (!n.getNodeName().equals("documentation:bibliographic-reference"))
				continue;

			Node seriesInfo = XMLDocument.findNode(n, "documentation:series-information");

			String originator = XMLDocument.getTextValue((Element) n, "documentation:originator");
			String title = XMLDocument.getTextValue((Element) n, "documentation:title");
			String date = XMLDocument.getTextValue((Element) n, "documentation:publication-date");
			String type = XMLDocument.getTextValue((Element) n, "documentation:type");
			String details = XMLDocument.getTextValue((Element) n, "documentation:details");
			String comment = XMLDocument.getTextValue((Element) n, "rdfs:comment");
			String label = XMLDocument.getTextValue((Element) n, "rdfs:label");
			String keywords = XMLDocument.getTextValue((Element) n, "ima:keywords");

			String where = XMLDocument.getTextValue((Element)seriesInfo, "documentation:series-name");
			String issue = XMLDocument.getTextValue((Element)seriesInfo, "documentation:series-issue");
			String pages = XMLDocument.getTextValue((Element)seriesInfo, "documentation:series-page-range");
/*
			(modelling/object 'bibtex:Article
					"Test article"
					"Description of a test article"			
					(bibtex:hasJournal "Environmental Modelling & Software")
				  (bibtex:hasAuthor "Kollat, JB" "Reed, P")
				  (bibtex:hasTitle "A framework for visually interactive decision-making and design using evolutionary multi-objective optimization (VIDEO)")
		  		(bibtex:hasAffiliation "Penn State Univ, Dept Civil & Environm Engn, University Pk, PA 16802 USA")
		  		(bibtex:hasYear 2007)
			    (bibtex:hasVolume 22)
		  		(bibtex:hasNumber 12)
			    (bibtex:hasPages "1691-1704")
		   		(bibtex:hasKeywords "visualization; decision support; monitoring design; multi-objective optimization; genetic algorithms; kriging")
		  	  (metadata:hasISIClassification "groundwater monitoring design; genetic algorithms; objectives; strategies; management; solve; model")
		   		(bibtex:hasAbstract "This study presents a framework for (V) under bar isually (I) under bar nteractive (D) under bar ecision-making and (D) under bar esign using (E) under bar volutionary Multi-objective Optimization (V (I) under bar DEO). The VI (D) under bar EO framework allows users to visually navigate large multi-objective solution sets while aiding decision makers in identifying one or more optimal designs. Specifically, the interactive visualization framework is intended to provide an innovative exploration tool for high-order Pareto-optimal solution sets (i.e., solution sets for three or more objectives). The framework is demonstrated for a long-term ground-water monitoring (LTM) application in which users can explore and visualize tradeoffs for up to four design objectives, simultaneously. Interactive functionality within the framework allows the user to select solutions within the objective space and visualize the corresponding monitoring plan's perfon-nance in the design space. This functionality provides the user with a holistic picture of the information provided by a particular solution, ultimately allowing them to make a more informed decision. In addition, the ease with which the framework allows users to navigate and compare solutions as well as design tradeoffs leads to a time efficient analysis, even when there are thousands of potential solutions. (c) 2007 Elsevier Ltd. All rights reserved.")
		  		(metadata:hasDownloadLink "articles/08003011183020297.pdf")) 
		  	:id 'KollatRed2007
		  	
		  	*/
			
			/* TODO change pub type */
			String pClass = "bibtex:Article";
			
			
			out.println(
				"(modelling/object '" + pClass  + "\n\t" +
					"\"" + label + "\"\n\t" +
					"\"" + comment + "\"\n\t" +
					"(bibtex:hasAuthor " + originator + ")\n\t" +
					"(bibtex:hasYear " + date + ")\n\t" +
					"(bibtex:hasNumber " + issue + ")\n\t" +
					"(bibtex:hasPages " + pages + ")\n\t" +
					"(bibtex:hasAbstract " + details + ")\n\t" +
					"(bibtex:hasKeywords " + keywords + ")\n\t" +
					")"			
			);
		}
		
		

	}
	
	static public void translate() {
		
		try {
			translateBibData(ARIESValuationPlugin.get().getResourceURL("legacy/esd_bibliodata.xml"));
			translateValData(ARIESValuationPlugin.get().getResourceURL("legacy/esd_valdata.xml"));

		} catch (ThinklabException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		
		
	}
	
}
