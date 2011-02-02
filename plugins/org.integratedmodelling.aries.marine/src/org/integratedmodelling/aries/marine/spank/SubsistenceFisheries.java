package org.integratedmodelling.aries.marine.spank;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Random;

import org.integratedmodelling.corescience.context.ObservationContext;
import org.integratedmodelling.corescience.implementations.datasources.MemDoubleContextualizedDatasource;
import org.integratedmodelling.corescience.interfaces.IContext;
import org.integratedmodelling.corescience.interfaces.IState;
import org.integratedmodelling.modelling.agents.SPANK;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;

public class SubsistenceFisheries extends SPANK {

	/*
	 * kg/ha/year necessary for an agent to exist. This is tailored to MG data, I have no idea about
	 * how much sense these numbers make.
	 */
	double source_threshold = 70000.0;
	double shore_fishing_range_m = 1400.0;
	double max_distance_traveled = 7000.0;
	double use_threshold = 10.0;
	int max_paths_bifurcations = 24;
	
	private IState flowState;
	private IState usedState;
	private IState satisfiedState;
	private IState unusedState;
	private IState unsatisfiedState;
	
	private IState path;
	private IState pop;

	Random rgen = new Random(123);
	
	double[] need = null, fish = null;
	private int size;
	
	public class SFAgent extends Agent {

		private double stock, distance;
		private List<Integer> accessPoints;
		private HashSet<Integer> beenThere = new HashSet<Integer>();
		
		public SFAgent(int x, int y, List<Integer> accessPoints, double stock) {
			
			super(x, y);
			this.stock = stock;
			this.accessPoints = new ArrayList<Integer>();
			this.distance = Math.sqrt(xmeters*ymeters);
			
			// group access points according to how many neighboring path points
			// they have; take only those that have 0 or 1 so we ensure we are
			// taking single path at a time.
			for (int ap : accessPoints) {
				int[] xy = grid.getXYCoordinates(ap);
				int cp = 0;
				for (int neigh : grid.getMooreNeighbors(xy[0], xy[1])) {
					if (path.getValue(neigh) != null) 
						cp++;
				}
				if (cp > 1)
					this.accessPoints.add(ap);
			}
		}
		
		@Override
		public String toString() {
			return super.toString() + " [" + stock + "/" + getAccessPointCount() + "]";
		}
		
		public int getAccessPointCount() {
			return accessPoints.size();
		}
		
		@Override
		protected void move() throws ThinklabException {

			/*
			 * compute new access points if any stock is left
			 */
			if (stock > 0.0) {
				
				ArrayList<Integer> newPoints = new ArrayList<Integer>();
				for (int oap : sample(accessPoints)) {

					int[] xy = grid.getXYCoordinates(oap);
					for (int neigh : grid.getMooreNeighbors(xy[0], xy[1])) {
						if (path.getValue(neigh) != null && !beenThere.contains(neigh)) {
							newPoints.add(neigh);
							beenThere.add(neigh);
						}
					}
				}
				accessPoints = newPoints;
			}
		}

		private List<Integer> sample(List<Integer> points) {

			List<Integer> ret = points;

			if (points.size() > max_paths_bifurcations) {
				
				ret = new ArrayList<Integer>();
				double cutoff = (double)max_paths_bifurcations/(double)(points.size());
				
				for (int n : points) {
					if (rgen.nextDouble() <= cutoff) {
						ret.add(n);
					}
				}
			}

			return ret;
		}

		@Override
		protected boolean done() {
			
			boolean more = (stock > use_threshold && accessPoints.size() > 0);
			if (accessPoints.size() == 0 && stock > 0) {
				// we're left with fishable fish unfished; update map
			}
			return !more;
		}

		@Override
		protected void takeStock() throws ThinklabException {

			double avstock = stock/accessPoints.size();
			/*
			 * we have stock left in x, y. Find out if any users along the path need it and how much.
			 */
			for (int i = 0; i < accessPoints.size(); i++) {
				
				int apc = accessPoints.get(i);
//				int[] xy = grid.getXYCoordinates(apc);
				
				// ACHTUNG - we should use the need still unmet, but this creates
				// a big order dependency. May want to normalize to total need later.
				double needed = need[apc];
				
				/*
				 * need is per capita;
				 * adjust per population size
				 */
				double psize = pop.getDoubleValue(apc);
				if (Double.isNaN(psize))
					psize = 0.0;
				
				needed *= psize;
				
				if (needed > 0) {
					
					/*
					 * deplete stock proportionally
					 */
					double used  = avstock > needed ? needed : avstock;
					
					avstock -= used;
					stock -= used;
					
					// move used fraction from unused to used
					add(usedState, used, grid.getIndex(xpos, ypos));
					add(satisfiedState, used, apc);
					sub(unusedState, used, apc);
					
				}
				
				// add amount moved to flow density 
				add(flowState, avstock, apc);

			}
			
			/* energy decay in fish by distance having done another step. Stock reduces
			 * for all by same amount. FIXME - linear! This should be gaussian or poisson. 
			 */
			stock -= stock * distance/max_distance_traveled;
			
		}

		private void sub(IState state, double amount, int idx) {
			Double d = (Double)state.getValue(idx);
			if (d != null) {
				state.setValue(idx, new Double(d-amount));
			}
		}

		private void add(IState state, double amount, int idx) {
			Double d = (Double)state.getValue(idx);
			if (d != null) 
				d = 0.0;
			state.setValue(idx, new Double(d+amount));
		}
	}
	
	@Override
	protected void extractStates(IContext context) throws ThinklabException {

		KnowledgeManager km = KnowledgeManager.get();
		
		this.source = context.getState(km.requireConcept("fisheries:TotalSubsistenceHarvest"));
		this.use = context.getState(km.requireConcept("fisheries:SubsistenceUse"));
		this.path = context.getState(km.requireConcept("infrastructure:Path"));
		this.pop = context.getState(km.requireConcept("fisheries:PopulationDensity"));

		this.need = this.use.getDataAsDoubles();
		this.fish = this.source.getDataAsDoubles();
		
		for (int i = 0; i < need.length; i++) {
			need[i] *= pop.getDoubleValue(i);
		}
	}
	
	

	@Override
	protected void setupSpace(IContext context) throws ThinklabException {
		super.setupSpace(context);
		
		this.size = width*height;

		// ensure we have enough fishing range to span at least one cell
		double minsize = Math.max(grid.getCellWidthMeters(), grid.getCellHeightMeters());
		if (shore_fishing_range_m < minsize)
			shore_fishing_range_m = minsize + 1;
	}



	@Override
	protected Collection<Agent> createAgents() throws ThinklabException {

		ArrayList<Agent> agents = new ArrayList<Agent>();
		double[] stock = source.getDataAsDoubles();
		
		for (int x = 0; x < width; x++) {
			for (int y = 0; y < height; y++) {
				double d = stock[grid.getIndex(x, y)];
				
				/*
				 * enough fish?
				 */
				if (Double.isNaN(d) || d < this.source_threshold) {
					continue;
				}
				
				/*
				 * access path within fishing range?
				 */
				ArrayList<Integer> accessPoints = new ArrayList<Integer>();
				for (int idx : grid.getCoordinatesWithinM(x, y, shore_fishing_range_m)) {
					if (path.getValue(idx) != null) {
						accessPoints.add(idx);
					}
				}
				
				/*
				 * select sources in range of a unique path beginning
				 */
				if (accessPoints.size() > 0) {
					SFAgent ag = new SFAgent(x, y, accessPoints, d);
					if (ag.getAccessPointCount() > 0)
						agents.add(ag);
				}
			}
		}
		
		System.out.println("created " + agents.size() + " source agents");
		
		return agents;
	}

	@Override
	protected ArrayList<IState> createStates() throws ThinklabException {
		
		ArrayList<IState> ret = new ArrayList<IState>();
		
		ret.add(this.flowState = 	
			new MemDoubleContextualizedDatasource(
					KnowledgeManager.get().requireConcept("fisheries:SubsistenceFishFlow"),
					this.size,
					(ObservationContext)context));
		
		ret.add(this.usedState = 
			new MemDoubleContextualizedDatasource(
					KnowledgeManager.get().requireConcept("fisheries:UtilizedSubsistenceFish"),
					this.size,
					(ObservationContext)context));
		
		ret.add(this.satisfiedState = 
			new MemDoubleContextualizedDatasource(
					KnowledgeManager.get().requireConcept("fisheries:SatisfiedSubsistenceFishDemand"),
					this.size,
					(ObservationContext)context));

		ret.add(this.unusedState = 
			new MemDoubleContextualizedDatasource(
					KnowledgeManager.get().requireConcept("fisheries:UnutilizedSubsistenceFish"),
					this.size,
					(ObservationContext)context));
	
		ret.add(this.unsatisfiedState = 
			new MemDoubleContextualizedDatasource(
					KnowledgeManager.get().requireConcept("fisheries:UnsatisfiedSubsistenceFishDemand"),
					this.size,
					(ObservationContext)context));
		

		/*
		 * initialize all states
		 */
		for (int i = 0; i < this.size; i++) {
			
			if (!Double.isNaN(need[i])) {
				// initialize unmet need with actual need
				this.unsatisfiedState.setValue(i, new Double(need[i]));
			}

			if (!Double.isNaN(fish[i])) {
				// initialize unmet need with actual need
				this.unusedState.setValue(i, new Double(fish[i]));
			}
		}
		
		return ret;
	}

}
