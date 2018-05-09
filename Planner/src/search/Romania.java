
package search;

import java.util.*;

public class Romania
{
    private static final Object[][] CITIES =
	{
	 {"Oradea", 20, 4},
	 {"Zerind", 15, 14},
	 {"Arad", 10, 25},
	 {"Timisoara", 11, 47},
	 {"Sibiu", 42, 35},
	 {"Fagaras", 67, 36},
	 {"Rimnicu Vilcea", 48, 47},
	 {"Lugoj", 30, 54},
	 {"Mehadia", 32, 65},
	 {"Dobreta", 30, 77},
	 {"Pitesti", 70, 58},
	 {"Craiova", 53, 80},
	 {"Bucharest", 93, 68},
	 {"Giugiu", 85, 84},
	 {"Urziceni", 107, 63},
	 {"Neamt", 94, 13},
	 {"Iasi", 112, 21},
	 {"Vaslui", 120, 38},
	 {"Hirsova", 127, 63},
	 {"Eforie", 134, 77}};

    private static final Object[][] CONNECTIONS =
	{
	 {"Oradea", "Zerind", 71.0},
	 {"Oradea", "Sibiu", 151.0},
	 {"Zerind", "Arad", 75.0},
	 {"Arad", "Sibiu", 140.0},
	 {"Arad", "Timisoara", 118.0},
	 {"Timisoara", "Lugoj", 111.0},
	 {"Sibiu", "Fagaras", 99.0},
	 {"Sibiu", "Rimnicu Vilcea", 80.0},
	 {"Fagaras", "Bucharest", 211.0},
	 {"Rimnicu Vilcea", "Pitesti", 97.0},
	 {"Rimnicu Vilcea", "Craiova", 146.0},
	 {"Lugoj", "Mehadia", 70.0},
	 {"Mehadia", "Dobreta", 75.0},
	 {"Dobreta", "Craiova", 120.0},
	 {"Pitesti", "Craiova", 138.0},
	 {"Pitesti", "Bucharest", 101.0},
	 // {"Craiova"},
	 {"Bucharest", "Giugiu", 90.0},
	 {"Bucharest", "Urziceni", 85.0},
	 // {"Giugiu"},
	 {"Urziceni", "Vaslui", 142.0},
	 {"Urziceni", "Hirsova", 98.0},
	 {"Neamt", "Iasi", 87.0},
	 {"Iasi", "Vaslui", 92.0},
	 // {"Vaslui"},
	 {"Hirsova", "Eforie", 86.0}
			// {"Eforie"}
	};

    public class City implements ProblemState
    {
	private final String name;
	private final int x;
	private final int y;
	private final Map<City, Double> connections = new HashMap<City, Double> ();

	private SearchState searchState;

	public City (final String name, final int x, final int y)
	{
	    this.name = name;
	    this.x = x;
	    this.y = y;
	}

	public String getName ()
	{
	    return name;
	}

	public int getX ()
	{
	    return x;
	}

	public int getY ()
	{
	    return y;
	}

	public Map<City, Double> getConnections ()
	{
	    return connections;
	}

	@Override
	public boolean solved ()
	{
	    return destination == this;
	}

	@Override
	public Map<ProblemState, Double> expand ()
	{
	    final Map<ProblemState, Double> result = new HashMap<ProblemState, Double> (connections);
	    return result;
	}

	@Override
	public double estimateRemainingCost ()
	{
	    final int dx = x - destination.x;
	    final int dy = y - destination.y;
	    return Math.sqrt (dx * dx + dy * dy);
	}

	public void setSearchState (final SearchState searchState)
	{
	    if (this.searchState != null)
	    {
		throw new IllegalStateException ("Double visit");
	    }
	    this.searchState = searchState;
	}

	public SearchState getSearchState ()
	{
	    return searchState;
	}

	@Override
	public String toString ()
	{
	    final StringBuilder buffer = new StringBuilder ();
	    buffer.append ("#<");
	    buffer.append (getClass ().getSimpleName ());
	    buffer.append (" ");
	    buffer.append (name);
	    buffer.append (">");
	    return buffer.toString ();
	}
    }

    private final Map<String, City> cities = new HashMap<String, City> ();

    private City destination = null;

    public Romania ()
    {
	for (final Object[] slot : CITIES)
	{
	    final String name = (String)slot[0];
	    final int x = (int)slot[1];
	    final int y = (int)slot[2];
	    final City city = new City (name, x, y);
	    cities.put (name, city);
	}

	for (final Object[] slot : CONNECTIONS)
	{
	    final String fromCity = (String)slot[0];
	    final String toCity = (String)slot[1];
	    final double distance = (double)slot[2];
	    final City c1 = cities.get (fromCity);
	    final City c2 = cities.get (toCity);
	    c1.getConnections ().put (c2, distance);
	    c2.getConnections ().put (c1, distance);
	}
    }

    public Map<String, City> getCities ()
    {
	return cities;
    }

    private void setDestination (final String name)
    {
	destination = cities.get (name);
    }

    public static void main (final String[] args)
    {
	final Romania romania = new Romania ();
	// for (final Entry<String, City> entry : romania.getCities ().entrySet ())
	// {
	// final City city = entry.getValue ();
	// System.out.printf ("City %s at %d %d %n", city.getName (), city.getX (), city.getY ());
	// }
	romania.setDestination ("Bucharest");
	final BestFirstSearch solver = new BestFirstSearch ();
	solver.setSearchLimit (25);
	solver.add (romania.cities.get ("Oradea"));
	final SearchState solution = solver.solve ();
	System.out.printf ("Best Solution %s %n", solution);
	printSolutionPath (solution);
    }

    private static void printSolutionPath (final SearchState solution)
    {
	if (solution != null)
	{
	    final SearchState parent = solution.getParentState ();
	    printSolutionPath (parent);
	    System.out.printf ("City %s %s %n", solution.getProblemState (), solution.getCost ());
	}
    }

    // 151 + 99 + 211 = 461
    // 151 + 80 + 97 + 101 = 429 Oradea => Sibiu => Rimnicu Vilcea => Pitesti => Bucharest

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
