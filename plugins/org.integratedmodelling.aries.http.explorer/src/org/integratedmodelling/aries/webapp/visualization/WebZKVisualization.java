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

package org.integratedmodelling.aries.webapp.visualization;

import java.io.File;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;

import org.integratedmodelling.aries.webapp.view.STYLE;
import org.integratedmodelling.corescience.context.DatasourceStateAdapter;
import org.integratedmodelling.corescience.interfaces.IContext;
import org.integratedmodelling.corescience.interfaces.IState;
import org.integratedmodelling.corescience.literals.IndexedCategoricalDistribution;
import org.integratedmodelling.corescience.metadata.Metadata;
import org.integratedmodelling.modelling.data.CategoricalDistributionDatasource;
import org.integratedmodelling.modelling.data.CategoricalDistributionDatasource.DistributionParameters;
import org.integratedmodelling.modelling.visualization.VisualizationFactory;
import org.integratedmodelling.modelling.visualization.WebVisualization;
import org.integratedmodelling.modelling.visualization.knowledge.TypeManager;
import org.integratedmodelling.modelling.visualization.knowledge.VisualConcept;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.webapp.ZK;
import org.integratedmodelling.thinklab.webapp.ZK.ZKComponent;
import org.integratedmodelling.utils.MiscUtilities;
import org.integratedmodelling.utils.Pair;
import org.integratedmodelling.utils.image.ColorMap;

public class WebZKVisualization extends WebVisualization {

	HashMap<IConcept, ZK.ZKComponent> _legends = 
		new HashMap<IConcept, ZK.ZKComponent>();
	
	public WebZKVisualization(IContext context, String directory,
			String urlPrefix) throws ThinklabException {
		super(context, directory, urlPrefix);
	}

	public ZK.ZKComponent getLegend(IConcept concept, String units, String currentPlotType) throws ThinklabException {
		

		IState state = context.getState(concept);
		
		if (units == null)
			units = (String) state.getMetadata().get(Metadata.UNIT_SPECS);
		
		String filename = getStateDirectory(concept) + File.separator + "legend.png";
		Pair<File[], String[]> legend = null;
		
		if (currentPlotType.equals(VisualizationFactory.PLOT_GEOCONTOUR_2D) ||
				currentPlotType.equals(VisualizationFactory.PLOT_GEOCONTOUR_2D)) {
			/*
			 * just so
			 */
			legend =
				VisualizationFactory.get().getLegend(ColorMap.jet(254), 254, 8, "", filename);
			
		} else {

			if (_legends.containsKey(concept))
				return _legends.get(concept);

			legend = VisualizationFactory.get().getLegend(state, 256, 8, filename);
		}
		
		if (legend == null)
			return null;

		ZK.ZKComponent[] comps = new ZK.ZKComponent[legend.getFirst().length + 1];
		int n = 0;
		for (File f : legend.getFirst()) {
			String url = 
				getStateUrl(concept) + 
				"/" +
				MiscUtilities.getFileName(f.toString());
			comps[n] = 
				ZK.image(url).tooltip(legend.getSecond()[n]);
			n++;
		}
		
		// TODO this is min-max, not
		String aggrdesc = null /* VisualizationFactory.getAggregatedDescription(state) */;
		
		comps[n] =
			aggrdesc == null ?
					null :
					ZK.hbox(
						ZK.separator(false).width(4),
						ZK.label(aggrdesc).sclass(STYLE.TEXT_SMALLER_BRIGHT));
		
		ZK.ZKComponent ret = ZK.hbox(comps).spacing(0);
		
		if (units != null)
			ret =
				ZK.vbox(ret, ZK.label("All values in " + units).sclass(STYLE.TEXT_VERYSMALL));
		
		_legends.put(concept, ret);
		
		return ret;
	}
	
	public ZK.ZKComponent getVerticalLegend(Pair<File[],String[]> legend, String baseFile) {
		
		String baseUrl = MiscUtilities.getFilePath(baseFile.toString());
		ZK.ZKComponent[] comps = new ZK.ZKComponent[legend.getFirst().length];
		int n = 0;
		for (File f : legend.getFirst()) {
			String url = 
				baseUrl + 
				"/" +
				MiscUtilities.getFileName(f.toString());
			comps[n] = 
				ZK.hbox(
					ZK.image(url),
					ZK.label(legend.getSecond()[n])
				);
			n++;
		}
		return ZK.vbox(comps);
	}
	
	public ZK.ZKComponent getStateDescriptionAt(IConcept concept, int imgX, int imgY, String units) throws ThinklabException {
		
		System.out.println("---------------------------------------------------------------------------");
		System.out.println("You asked for: " + concept + ". Available states are: ");
		for (IState s : context.getStates()) {
			System.out.println(s.getObservableClass() + " " + (s instanceof CategoricalDistributionDatasource ? "WITH distribution info" : "WITHOUT distribution info"));
		}
		System.out.println("Make sure the state you want to show is in the storyline with the intended concept");
		System.out.println("---------------------------------------------------------------------------");
		
		Object o = getDataAt(concept, imgX, imgY);
		DecimalFormat df = new DecimalFormat("#.####");
		DecimalFormat df3 = new DecimalFormat("#.###");
		DecimalFormat df1 = new DecimalFormat("#.#");
		IState state = context.getState(concept);
		if (state instanceof DatasourceStateAdapter)
			state = ((DatasourceStateAdapter)state).getOriginalState();

		if (units == null)
			units = (String) state.getMetadata().get(Metadata.UNIT_SPECS);
		
		if (units == null)
			units = "";
		else
			units = " " + units;

		ZK.ZKComponent dvis = null;
		
		if (o == null) {
			
			dvis = 
				ZK.vbox(
					ZK.spacer(20),
					ZK.label("[no data]").sclass(STYLE.TEXT_SMALL));
			
		} else if (o instanceof Double) {
			
			dvis = 
				ZK.vbox(
					ZK.spacer(20),
					ZK.label(df.format((Double)o) + units).sclass(STYLE.TEXT_SMALL));
			
		} else if (o instanceof IndexedCategoricalDistribution && !(state instanceof CategoricalDistributionDatasource)) {
			
			o = new Double(((IndexedCategoricalDistribution)o).getMean());
			
			dvis = 
				ZK.vbox(
					ZK.spacer(20),
					ZK.label(df.format((Double)o) + units).sclass(STYLE.TEXT_SMALL));
			
		}else if (o instanceof IConcept) {
			
			VisualConcept vc = TypeManager.get().getVisualConcept((IConcept)o);
			HashMap<IConcept, String> leg = VisualizationFactory.get().getClassLegend(state);

			if (leg != null && leg.containsKey((IConcept)o)) {
				dvis = 
					ZK.vbox(
						ZK.spacer(10),
						ZK.label(vc.getLabel()).height(16),
						ZK.label(leg.get((IConcept)o)).sclass(STYLE.TEXT_VERYSMALL));
			} else {
				dvis = 	
					ZK.vbox(
						ZK.spacer(10),
						ZK.label(vc.getLabel()));
			}
		} else if (o instanceof String) {

			dvis = 
				ZK.vbox(
					ZK.spacer(20),
					ZK.label((String)o).sclass(STYLE.TEXT_SMALL));

		} else if (o instanceof IndexedCategoricalDistribution && state instanceof CategoricalDistributionDatasource) {
			
			IndexedCategoricalDistribution dist = (IndexedCategoricalDistribution) o;
			CategoricalDistributionDatasource z = (CategoricalDistributionDatasource)state;
			DistributionParameters stats = z.getDistribution(dist);
			VisualConcept vc;
			
			if (stats.isBinary) {
				vc = TypeManager.get().getVisualConcept(stats.categories[1]);
				dvis = ZK.text(
						"P(" +
						vc.getLabel() + 
						"): " +
						df.format(stats.probabilities[1]));
			} else {
				
				double h = 36;
				int    w = 48/dist.data.length;
				ArrayList<ZKComponent> bars = new ArrayList<ZK.ZKComponent>();
				for (int i = 0; i < dist.data.length; i++) {
				
					String spr = "";
					if (stats.categories != null) {
						vc = TypeManager.get().getVisualConcept(stats.categories[i]);
						spr = 
							vc.getLabel() + 
							(stats.min_values == null ? 
									"" :
									(" [" + stats.min_values[i] + " - " + stats.max_values[i] + "]")) +
							"\n";
					}
					
					/*
					 * distribution toolbar
					 */
					int hbar = (int)(h * dist.data[i]);
					bars.add(
							ZK.vbox(
									ZK.div().width(w).height(36 - hbar),
									ZK.div().width(w).height(hbar).bgcolor("#cccccc")).
									tooltip(spr + "p = " + df.format(dist.data[i])).
									height(36));
				}
				
				dvis = 
					ZK.hbox(bars.toArray(new ZKComponent[bars.size()])).
						spacing(1).
						height(48);
				
				if (stats.min_values != null) {
					dvis = 
						ZK.hbox(
							ZK.vbox(
								ZK.spacer(12),
								ZK.label("\u03BC = " + df3.format(stats.mean)).height(24).sclass(STYLE.TEXT_VERYSMALL), 
								ZK.label("\u00B1 = " + df1.format(stats.cv*100.0) + "%").height(24).sclass(STYLE.TEXT_VERYSMALL)),
							dvis);
				}
			}
		}
		
		return dvis;
	}
	
	public ZK.ZKComponent getLatLonDescriptionAt(int imgX, int imgY) {
		
		Pair<Double, Double> lonlat = this.getGeoCoordinates(imgX, imgY);
		DecimalFormat df = new DecimalFormat("#.####");

		if (lonlat == null)
			return null;

		return 
			ZK.vbox(
				ZK.label("LON: " + df.format(lonlat.getFirst())).sclass(STYLE.TEXT_VERYSMALL),	
				ZK.label("LAT: " + df.format(lonlat.getSecond())).sclass(STYLE.TEXT_VERYSMALL)	
			);
	}

	public String getAggregatedDescription(IConcept concept, String units) {
		
		String ret = "";
		
		DecimalFormat df1 = new DecimalFormat("#.#");
		DecimalFormat df2 = new DecimalFormat("#.##");
		IState state = context.getState(concept);
		
		if (units == null)
			units = (String) state.getMetadata().get(Metadata.UNIT_SPECS);
		
		if (state.isSpatiallyDistributed() && !state.isTemporallyDistributed()) {
			
		}
		
		// TODO use aggregator
		Double tot = null;
		if (tot != null) {
			ret = " Total: " + df1.format(tot);
			Double cv = null;
			if (cv != null) {
				ret += " \u00b1 " + df2.format(cv*100.0) + "%";
			}
			if (units != null) {
				ret += " " + units;
			}
		}

		return ret;
	}
}
