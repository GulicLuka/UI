import java.io.FileNotFoundException;
import java.util.*;

class Cell {
    public int i;
    public int j;
    public Cell parent;
    public int heuristicCost;
    public int finalCost;
    public boolean solution;

    public Cell(int i, int j) {
        this.i = i;
        this.j = j;
    }

    public String toString() {
        return "(" + this.i + "," + this.j + ")";
    }
}

class Coordinates{
    public int x;
    public int y;

    public Coordinates(int x, int y) {
        this.x = x;
        this.y = y;
    }
}

public class AStar {

    private static int stZaklad;

    public static final int DIAGONAL_COST = 14;
    public static final int V_H_COST = 10;

    private static Cell[][] grid = new Cell[0][];

    private static PriorityQueue<Cell> openCells;

    private static boolean[][] closedCells;

    private int startI;
    private int startJ;

    private int endI;
    private int endJ;

    private static final StringBuilder sb = new StringBuilder();
    private static boolean konec = false;

    private static boolean[][] obiskan;
    private static boolean[][] vis;


    private static int cena = 0;
    private static int stPremikov = 0;

    private static int[][] grid1;


    public AStar(int width, int height, int si, int sj, int ei, int ej, int[][] blocks) {
        grid = new Cell[width][height];
        closedCells = new boolean[width][height];
        openCells = new PriorityQueue<>(Comparator.comparingInt((Cell c) -> c.finalCost));

        startCell(si, sj);
        endCell(ei, ej);

        for (int i = 0; i < grid.length; i++) {
            for (int j = 0; j < grid[i].length; j++) {
                grid[i][j] = new Cell(i, j);
                grid[i][j].heuristicCost = Math.abs(i - endI) + Math.abs(j - endJ);
                grid[i][j].solution = false;
            }
        }

        grid[startI][startJ].finalCost = 0;

        for (int i = 0; i < blocks.length; i++) {
            for (int j = 0; j < blocks[i].length; j++) {
                if(blocks[i][j] == -1)
                    grid[i][j] = null;
            }
        }
    }

    public void startCell(int i, int j) {
        startI = i;
        startJ = j;
    }

    public void endCell(int i, int j) {
        endI = i;
        endJ = j;
    }

    public void updateCostIfNeeded(Cell current, Cell t, int cost) {
        if ((t == null) || closedCells[t.i][t.j])
            return;

        int tFinalCost = t.heuristicCost + cost;
        boolean isOpen = openCells.contains(t);

        if (!isOpen || (tFinalCost < t.finalCost)) {
            t.finalCost = tFinalCost;
            t.parent = current;

            if (!isOpen)
                openCells.add(t);
        }
        //vis[current.i][current.j] = true;
        vis[t.i][t.j] = true;
    }


    public void process(boolean ManhattenEuclidean) {
        openCells.add(grid[startI][startJ]);
        Cell current;

        while (true) {
            current = openCells.poll();

            if (current == null)
                break;

            closedCells[current.i][current.j] = true;

            if (current.equals(grid[endI][endJ]))
                return;

            Cell t;

            if (current.i - 1 >= 0) {
                t = grid[current.i - 1][current.j];
                updateCostIfNeeded(current, t, current.finalCost + V_H_COST);

                if(ManhattenEuclidean){
                    if (current.j - 1 >= 0) {
                    t = grid[current.i - 1][current.j - 1];
                    updateCostIfNeeded(current, t, current.finalCost + DIAGONAL_COST);
                    }

                    if (current.j + 1 < grid[0].length) {
                        t = grid[current.i - 1][current.j + 1];
                        updateCostIfNeeded(current, t, current.finalCost + DIAGONAL_COST);
                    }
                }
            }
            if (current.j - 1 >= 0) {
                t = grid[current.i][current.j - 1];
                updateCostIfNeeded(current, t, current.finalCost + V_H_COST);
            }
            if (current.j + 1 < grid[0].length) {
                t = grid[current.i][current.j + 1];
                updateCostIfNeeded(current, t, current.finalCost + V_H_COST);
            }
            if (current.i + 1 < grid.length) {
                t = grid[current.i + 1][current.j];
                updateCostIfNeeded(current, t, current.finalCost + V_H_COST);

                if (ManhattenEuclidean) {
                    if (current.j - 1 >= 0) {
                        t = grid[current.i + 1][current.j - 1];
                        updateCostIfNeeded(current, t, current.finalCost + DIAGONAL_COST);
                    }
                    if (current.j + 1 < grid[0].length) {
                        t = grid[current.i + 1][current.j + 1];
                        updateCostIfNeeded(current, t, current.finalCost + DIAGONAL_COST);
                    }
                }
            }
        }
    }

    public void displayPath() {
        if (closedCells[endI][endJ]) {
            Stack<Cell> stack = new Stack<>();

            Cell current = grid[endI][endJ];
            stack.add(current);
            grid[current.i][current.j].solution = true;

            while (current.parent != null) {
                stack.add(current.parent);
                grid[current.parent.i][current.parent.j].solution = true;
                current = current.parent;
            }

            boolean pr = false;
            while (!stack.isEmpty()) {
                Cell c = stack.pop();
                if (stack.size() > 0) {
                    sb.append(c).append(" -> ");
                    obiskan[c.i][c.j] = true;
                    if(pr) stPremikov++;
                    if(grid1[c.i][c.j] >= 0) cena += grid1[c.i][c.j];
                } else {
                    if (konec) {
                        sb.append(c);
                        obiskan[c.i][c.j] = true;
                        if(pr) stPremikov++;
                        if(grid1[c.i][c.j] >= 0) cena += grid1[c.i][c.j];
                    }
                }
                pr = true;
            }
        }
    }

    public static void main(String[] args) throws FileNotFoundException {
        grid1 = DrawLabyrinth.readFile("D:\\FRI_Ljubljana\\GitHub\\UmetnaInteligenca\\Seminarska2\\src\\labyrinths\\labyrinth_8.txt");

        boolean ManhattanEuclidean = false;

        int stVozlisc = 0;
        boolean najden = false;
        int[] start= new int[2];
        int[] end = new int[2];
        HashMap<Integer, Coordinates> treasure = new HashMap<>();
        for(int i = 0; i< grid1.length; i++){
            for(int j = 0; j<grid1[0].length; j++){
                if(grid1[i][j] == -2 && !najden){
                    start[0] = i;
                    start[1] = j;
                    stVozlisc++;
                    najden = true;
                }else if(grid1[i][j] == -3){
                    Coordinates c = new Coordinates(i,j);
                    stZaklad++;
                    stVozlisc++;
                    treasure.put(stZaklad, c);
                }else if(grid1[i][j] == -4){
                    end[0] = i;
                    end[1] = j;
                    stVozlisc++;
                }else if(grid1[i][j] >= 0){
                    stVozlisc++;
                }
            }
        }
        AStar aStar;
        int stObiskanih = 0;
        int stVseh = 0;
        obiskan = new boolean[grid1.length][grid1[0].length];
        vis = new boolean[grid1.length][grid1[0].length];
        HashMap<Integer, Coordinates> treasureEnd = new HashMap<>();

        //Random treasures
        Random random = new Random();
        ArrayList<Integer> arr = new ArrayList<>();
        for(int i = 1; i <= treasure.size(); i++) {
            int value;
            do {
                value = random.nextInt(treasure.size());
                value += 1;
            }while(arr.contains(value));
            arr.add(value);
            treasureEnd.put(i, treasure.get(value));
        }

        for (Map.Entry<Integer, Coordinates> entry : treasureEnd.entrySet()) {
            aStar = new AStar(grid1.length, grid1[0].length, start[0], start[1], entry.getValue().x, entry.getValue().y, grid1);
            start[0] =  entry.getValue().x;
            start[1] = entry.getValue().y;
            aStar.process(ManhattanEuclidean);
            aStar.displayPath();
        }
        System.out.println("A star");
        System.out.println("(Row,Col) -> (Row,Col) -> ...");
        System.out.println();
        konec = true;
        aStar = new AStar(grid1.length, grid1[0].length, start[0], start[1], end[0],end[1], grid1);
        aStar.process(ManhattanEuclidean);
        aStar.displayPath();
        System.out.print("path: ");
        System.out.println(sb);
        System.out.println("Cena poti : " + cena);
        System.out.println("Stevilo vozlisc: " + stVozlisc);
        System.out.println("Stevilo premikov : " + stPremikov);
        for(int i= 0; i< obiskan.length; i++){
            for(int j= 0; j< obiskan[i].length; j++) {
                if(obiskan[i][j])
                    stObiskanih++;
                if(vis[i][j])
                    stVseh++;
            }
        }
        System.out.println("stevilo obiskanih vozlisc poti: " + stObiskanih);
        System.out.println("Stevilo vseh pregledanih vozlisc je: " + stVseh);
        //DrawLabyrinth.drawOutPath(grid1, vis, null);
        //DrawLabyrinth.drawOutPath(grid1, obiskan,null);
        DrawLabyrinth.drawOutPathBacktrack(grid1,vis,obiskan);
    }
}