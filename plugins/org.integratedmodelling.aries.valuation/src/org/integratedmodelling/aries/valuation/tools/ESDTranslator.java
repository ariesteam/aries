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
	
	static public void main(String[] args) {
		
		try {
			translateValData(ARIESValuationPlugin.get().getResourceURL("legacy/esd_valdata.xml"));
			
			
			
			
		} catch (ThinklabException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		
		
	}
	
}
