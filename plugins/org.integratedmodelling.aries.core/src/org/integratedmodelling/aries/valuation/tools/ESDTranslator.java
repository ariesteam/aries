package org.integratedmodelling.aries.valuation.tools;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.net.URL;

import org.integratedmodelling.aries.valuation.ARIESValuationPlugin;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.utils.Escape;
import org.integratedmodelling.utils.xml.XMLDocument;
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
	
	public static String trim(String s) {
		return Escape.collapseWhitespace(Escape.forDoubleQuotedString(s, true).trim());
	}
	
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

		try {
			FileOutputStream os = new FileOutputStream(new File("biblio.data"));
			out = new PrintStream(os, true);
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		while (it.hasNext()) {
			
			Node n = it.next();
			if (!n.getNodeName().equals("documentation:bibliographic-reference"))
				continue;

			String id = XMLDocument.getAttributeValue(n, "ima:id");
			Node seriesInfo = XMLDocument.findNode(n, "documentation:series-information");

			String originator = XMLDocument.getTextValue((Element) n, "documentation:originator");
			String title = XMLDocument.getTextValue((Element) n, "documentation:title");
			String date = XMLDocument.getTextValue((Element) n, "documentation:publication-date");
			String type = XMLDocument.getTextValue((Element) n, "documentation:type").trim();
			String details = XMLDocument.getTextValue((Element) n, "documentation:details");
			String comment = XMLDocument.getTextValue((Element) n, "rdfs:comment");
			String label = XMLDocument.getTextValue((Element) n, "rdfs:label");
			String keywords = XMLDocument.getTextValue((Element) n, "ima:keywords");

			String where = 
				seriesInfo == null ? 
					null :
					XMLDocument.getTextValue((Element)seriesInfo, "documentation:series-name");
			String issue = 				
				seriesInfo == null ? 
					null :
					XMLDocument.getTextValue((Element)seriesInfo, "documentation:series-issue");
			String pages = 
				seriesInfo == null ? 
					null :
					XMLDocument.getTextValue((Element)seriesInfo, "documentation:series-page-range");
			
			/* TODO change pub type */
			String pClass = "bibtex:Article";
			String whereprop = "bibtex:hasJournal";
			if (type.equals("documentation:report")) {
				pClass = "bibtex:Techreport";
				whereprop = "bibtex:hasBooktitle";
			} else if (type.equals("documentation:book-section")) {
				pClass = "bibtex:Incollection";
				whereprop = "bibtex:hasBooktitle";
			} else if (type.equals("documentation:web-page")) {
				pClass = "bibtex:Techreport";
			} else if (type.equals("documentation:working-paper")) {
				pClass = "bibtex:Techreport";
			} 
				
			/*
			 * class; add where
			 */
			out.println(
				" (modelling/object '" + pClass  + "\n\t" +
					(label == null ? "" : ("\"" + trim(label) + "\"\n\t")) +
					(comment == null ? "" : ("\"" + trim(comment) + "\"\n\t")) +
					(where == null ? "" : ("(" + whereprop + " \"" + trim(originator) + "\")\n\t")) +
					(originator == null ? "" : ("(bibtex:hasAuthor \"" + trim(originator) + "\")\n\t")) +
					(title == null ? "" : ("(bibtex:hasTitle \"" + trim(title) + "\")\n\t")) +
					(date == null ? "" : ("(bibtex:hasYear " + trim(date) + ")\n\t")) +
					(issue == null ? "" : ("(bibtex:hasNumber " + trim(issue) + ")\n\t")) +
					(pages == null ? "" : ("(bibtex:hasPages \"" + trim(pages) + "\")\n\t")) +
					(details == null ? "" : ("(bibtex:hasAbstract \"" + trim(details) + "\")\n\t")) +
					(keywords == null ? "" : ("(bibtex:hasKeywords \"" + trim(keywords) + "\")\n\t")) +
					") :id " + id			
			);
		}
		
		out.close();
		
	}
	
	static public void translate() {
		
		try {
			translateBibData(ARIESValuationPlugin.get().getResourceURL("legacy/esd_bibliodata.xml"));
			// translateValData(ARIESValuationPlugin.get().getResourceURL("legacy/esd_valdata.xml"));

		} catch (ThinklabException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		
		
	}
	
}
