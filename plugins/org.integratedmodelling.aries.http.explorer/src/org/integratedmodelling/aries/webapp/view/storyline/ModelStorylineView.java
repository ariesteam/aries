package org.integratedmodelling.aries.webapp.view.storyline;

import java.awt.image.BufferedImage;
import java.util.ArrayList;

import org.apache.commons.lang.StringUtils;
import org.integratedmodelling.aries.webapp.view.AriesBrowser;
import org.integratedmodelling.aries.webapp.view.STYLE;
import org.integratedmodelling.aries.webapp.visualization.WebZKVisualization;
import org.integratedmodelling.corescience.interfaces.IObservationContext;
import org.integratedmodelling.modelling.context.Context;
import org.integratedmodelling.modelling.storyline.ModelStoryline;
import org.integratedmodelling.modelling.storyline.Storyline;
import org.integratedmodelling.modelling.visualization.VisualizationFactory;
import org.integratedmodelling.modelling.visualization.WebVisualization;
import org.integratedmodelling.modelling.visualization.storyline.StorylineTemplate;
import org.integratedmodelling.modelling.visualization.storyline.StorylineTemplate.Page;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabRuntimeException;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.webapp.ZK;
import org.integratedmodelling.thinklab.webapp.ZK.ZKComponent;
import org.integratedmodelling.utils.Pair;
import org.integratedmodelling.utils.image.ImageUtil;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.EventListener;
import org.zkoss.zk.ui.event.MouseEvent;
import org.zkoss.zul.Div;

/**
 * The view that allows data display for a computed visualization, or information display for one still to be
 * computed. Should have methods to interact with the storyline (e.g. to start the computation) and if the
 * storyline has any children (scenarios) it should allow comparison of the computed data with its own.
 * 
 * @author Ferdinando
 *
 */
public class ModelStorylineView extends StorylineView {

	private StorylineControlPanel cpanel;
	int patch = -1;
	private String[] currentPlotType;
	private String covUrl = null;

	public ModelStorylineView(Storyline s, AriesBrowser browser, StorylineControlPanel badge) {

		super(s, browser);
		this.cpanel = badge;

		try {
			render();
		} catch (ThinklabException e) {
			throw new ThinklabRuntimeException(e);
		}
	}
	
	public StorylineControlPanel getControlPanel() {
		return cpanel;
	}
	
	public void initPages() throws ThinklabException {

		showSequence();
		currentPlotType = new String[getPageSequence().size()];
		for (int i = 0; i < getPageSequence().size(); i++) {
			currentPlotType[i] = 
				getPageSequence().get(i).getPlotType() + ".png";
		}
	}
	
	private static final long serialVersionUID = -7984552889374835100L;

	protected String getImage() throws ThinklabException {

		String ret = null;

		if (currentPage < 0) {
			
			if (storyline.getStatus() == ModelStoryline.COMPUTING || 
				storyline.getStatus() == ModelStoryline.PENDING) {
				
				ret = "/aries/images/computing.png";
				patch = (VIEWPORT_Y - 2 - 166)/2;
				
			} else if (storyline.getStatus() == ModelStoryline.COMPUTED) {

				ret = "/aries/images/computed.png";
				patch = (VIEWPORT_Y - 2 - 166)/2;
				initPages();
				
			} else if (storyline.getContext() != null){
				
				if (covUrl == null) {
					
					Pair<Integer,Integer> xy = 
						VisualizationFactory.get().getPlotSize(vx-2, vy-2, storyline.getContext());
					patch = (vy - 2 - xy.getSecond())/2;
					
					Pair<String, String> zr =
						browser.getApplication().getNewResourceUrl(".png", browser.getSession());

					/*
					 * create coverage image
					 */
					BufferedImage bim = storyline.getCoverageMap(xy.getFirst(), xy.getSecond());
					if (bim != null) {
						ImageUtil.saveImage(bim, zr.getFirst());
					}
					
					patch = (VIEWPORT_Y - 2 - xy.getSecond())/2;
					covUrl = zr.getSecond();

				}
				ret = covUrl;
			}
			
		} else {
			
			Pair<Integer,Integer> xy = 
				VisualizationFactory.get().getPlotSize(vx-2, vy-2, storyline.getContext());
			
			patch = (vy - 2 - xy.getSecond())/2;
			
			ret = ((WebZKVisualization)(storyline.getVisualization())).getStateUrl(
					getPageSequence().get(currentPage).getConcept(), 
					currentPlotType[currentPage]);
		}
		return ret;
	}


	@Override
	protected boolean isPageEnabled(StorylineTemplate.Page page) {
		return 
			!page.getBoolean("disabled") &&
			Context.getState(page.getConcept(), 
					(IObservationContext) storyline.getContext()) != null;
	}
	
	@Override
	protected ZKComponent getRibbon(int width, int height) throws ThinklabException {
		
		ZKComponent ret = null;
		
		if (storyline.getStatus() == ModelStoryline.IDLE) {
			
			/*
			 * TODO message, launch button
			 */
			ret = 
				ZK.vbox(
					ZK.spacer(28),
					ZK.label("Click the play button in the sidebar to start computing this storyline.")).
					align("center").sclass(STYLE.TEXT_LARGE_BOLD_GREY);
			
		} else if (storyline.getStatus() == ModelStoryline.COMPUTING) {
			
			/*
			 * TODO icon, progress bar etc
			 */
			ret = 
				ZK.vbox(
					ZK.spacer(28),
					ZK.label("The storyline is being computed. Results will appear in this area.")).
				align("center").sclass(STYLE.TEXT_LARGE_BOLD_GREY);
			
		} else if (storyline.getStatus() == ModelStoryline.ERROR) {
			
			/*
			 * TODO "more info" link, icons etc.
			 */
			ret = 
				ZK.vbox(
					ZK.spacer(28),
					ZK.label("The computation ended with errors.")).
				align("center").sclass(STYLE.TEXT_LARGE_BOLD_RED);
			
		}  else if (storyline.getStatus() == ModelStoryline.DISABLED) {
			ret = 
				ZK.vbox(
					ZK.spacer(28),
					ZK.label("Computation of this storyline is not possible in this region.")).
				align("center").sclass(STYLE.TEXT_LARGE_BOLD_GREY);
		} else {
			
			/*
			 * Make the ribbon for the result dataset corresponding to the various pages.
			 * Call the vis first or we won't get our sequence right the first time.
			 */
			WebVisualization visualization = 
				(WebZKVisualization)(storyline.getVisualization());
			
			if (visualization == null)
				return null;
			
			Pair<Integer, Integer> xy = 
				VisualizationFactory.get().getPlotSize(110, 64, storyline.getContext());
			
			ArrayList<ZKComponent> components = new ArrayList<ZK.ZKComponent>();
			
			for (int i = 0; i < getPageSequence().size(); i++) {
				Page p = getPageSequence().get(i);
				final int idx = i;
				String img = visualization.getStateUrl(p.getConcept(), VisualizationFactory.PLOT_GEOSURFACE_2D);
				if (img != null) {
					components.add(
						ZK.div(
							ZK.vbox(
								ZK.image(img).width(xy.getFirst()).height(xy.getSecond()).sclass(currentPage == i ? STYLE.BRIGHT_BORDER : null),
								ZK.label(StringUtils.abbreviate(p.getName(),24)).sclass(STYLE.TEXT_VERYSMALL).align("center").fillx())).
							align("center").
							width(120).
							tooltip(p.getRunningHead()).
							hand().listener("onClick", new EventListener() {
							
							@Override
							public void onEvent(Event arg0) throws Exception {
								switchPage(idx);
							}
						}).tmargin(64 - xy.getSecond()));
				}
			}
			
			ret = 
				ZK.ribbon(
					width, height, 120, 
					components.toArray(new ZKComponent[components.size()])).
				selected(currentPage);
		}
		
		return ret;
	}

	@Override
	protected ZKComponent getImageArea(int width, int height) throws ThinklabException {
		
		String img = getImage();
		
		if (currentPage >= 0) {
			
			String units = getPageSequence().get(currentPage).get("units");
			
			String totals = "";
			
			String statsReq = getPageSequence().get(currentPage).get("statistics");
			if (statsReq != null && statsReq.contains("totals")) {
				totals = 
					" " +
					((WebZKVisualization)(storyline.getVisualization())).getAggregatedDescription(
							getPageSequence().get(currentPage).getConcept(),
							getPageSequence().get(currentPage).get("units"));
				
			} else {
				totals = " click map for values";
			}
			
			/*
			 * list of buttons to switch images among the allowed views.
			 */
			ArrayList<ZKComponent> ibuts = new ArrayList<ZK.ZKComponent>();
			ibuts.add(ZK.spacer(4));
			ibuts.add(
				ZK.imagebutton("/aries/images/" + getPageSequence().get(currentPage).getPlotType() + ".png")
					.tooltip(VisualizationFactory.getPlotDescription(getPageSequence().get(currentPage).getPlotType() + ".png"))
					.listener("onClick", new EventListener() {
						@Override
						public void onEvent(Event arg0) throws Exception {
							// TODO Auto-generated method stub
							currentPlotType[currentPage] = 
								getPageSequence().get(currentPage).
									getWith("plot-type", "default", "true") + ".png";
							render();
						}
					}));
			
			for (String s : getPageSequence().get(currentPage).getAllWithout("plot-type", "default")) {
				final String zing = s;
				ibuts.add(
					ZK.imagebutton("/aries/images/" + s + ".png")
						.tooltip(VisualizationFactory.getPlotDescription(s + ".png"))
						.listener("onClick", new EventListener() {
					
							@Override
							public void onEvent(Event arg0) throws Exception {
								// TODO Auto-generated method stub
								currentPlotType[currentPage] = zing + ".png";
								render();
							}
						}));
			}
			
			return 
				ZK.div(
					ZK.vbox(
						ZK.div(
					
							ZK.hbox(
								ZK.div(
									ZK.vbox(
										ZK.spacer(8),
										ZK.hbox(
											ibuts.toArray(new ZKComponent[ibuts.size()])
										).spacing(4))).align("left"),
								ZK.spacer(6),
								ZK.vbox(
									ZK.spacer(8),
									ZK.label(VisualizationFactory.getPlotDescription(currentPlotType[currentPage]) +
												" :: " +
												totals)
										),
								ZK.div(
										ZK.vbox(
											ZK.spacer(8),
											ZK.hbox(
												ZK.imagebutton("/images/icons/camera_go.png").
													listener("onClick", new EventListener() {
											
														@Override
														public void onEvent(Event arg0) throws Exception {
															//exportImages();
														}
													}).
													tooltip("Export the selected layer as a PNG image"),
												ZK.imagebutton("/images/icons/folder_go.png").
													listener("onClick", new EventListener() {
											
														@Override
														public void onEvent(Event arg0) throws Exception {
															// exportGIS();
														}
													}).
													tooltip("Export the selected layer as a GeoTIFF file, suitable for GIS applications."),
												ZK.imagebutton("/images/icons/google-earth.png").
													listener("onClick", new EventListener() {
														@Override
														public void onEvent(Event arg0) throws Exception {
															// exportKML();
														}
													}).
													tooltip("View the selected layer in Google Earth"),
												ZK.spacer(4)
											).spacing(4))).align("right")
							).fillx()
						
						).sclass(STYLE.GLASSTOP_SMALL).width(vx).height(TOPBAR_H),
						ZK.div(
							ZK.vbox(
								ZK.separator().height(patch),
								ZK.image(img).
									listener("onClick", new EventListener() {
										
										@Override
										public void onEvent(Event arg0) throws Exception {
										
											int x = ((MouseEvent)arg0).getX();
											int y = ((MouseEvent)arg0).getY();
											showDataAt(getPageSequence().get(currentPage).getConcept(), x, y);
										}
									}))).
							align("center").
							sclass(STYLE.TEXTURED).
							width(width).
							height(height - BOTBAR_H - TOPBAR_H),
						ZK.div(
							ZK.hbox(
								ZK.div(
									ZK.vbox(
										ZK.spacer(20),
										ZK.hbox(
											ZK.spacer(16),
											((WebZKVisualization)(storyline.getVisualization())).
												getLegend(getPageSequence().get(currentPage).getConcept(), units, currentPlotType[currentPage])
									))).align("left"),
								ZK.div().align("right").id("datawin")
							).fillx()
							
						).sclass(STYLE.GLASSBOTTOM_LARGE).width(vx).height(BOTBAR_H)).spacing(0)).
					width(width).
					height(height).
					sclass(STYLE.BORDERED_SUNK);
		} 
		
		/*
		 * TODO
		 * no map to show
		 */
		return 
			ZK.div(
				ZK.vbox(
					ZK.separator().height(patch),
					ZK.image(getImage()))).
				align("center").
				sclass(STYLE.TEXTURED_SUNK).
				width(width).
				height(height);
	}
	
	private void showDataAt(IConcept concept, int imgX, int imgY) throws ThinklabException {

		Div div = (Div) ZK.getComponentById(this, "datawin");
		
		ZKComponent content = 
			ZK.vbox(
				ZK.hbox(
						((WebZKVisualization)(storyline.getVisualization())).
						getStateDescriptionAt(
								concept, imgX, imgY,
								getPageSequence().get(currentPage).get("units")),
					ZK.bar(),
					ZK.vbox(
						ZK.spacer(12),
						((WebZKVisualization)(storyline.getVisualization())).
							getLatLonDescriptionAt(imgX, imgY)	
					),
					ZK.spacer(12)
				)
			);
		
		ZK.resetComponent(div);
		div.appendChild(content.get());
	}

	public void initialize(Storyline sl, AriesBrowser browser, StorylineControlPanel badge) {
		
		initialize(sl, browser);
		this.cpanel = badge;
		try {
			render();
		} catch (ThinklabException e) {
			throw new ThinklabRuntimeException(e);
		}
	}
}
