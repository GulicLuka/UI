import java.io.FileNotFoundException;
import java.util.*;

public class DFS{
    private static int ROW;
    private static int COL;
    private static int stZaklad;
    private static boolean zaklad;
    private static final String[] listSmeriNeba = {"ENWS", "NEWS", "WENS", "EWNS", "NWES", "WNES", "SNEW", "NSEW", "ESNW", "SENW", "NESW", "ENSW", "EWSN", "WESN", "SEWN", "ESWN", "WSEN", "SWEN", "SWNE", "WSNE", "NSWE", "SNWE", "WNSE", "NWSE"};
    private static final HashMap<String, Integer> PathValue = new HashMap<>();
    private static final HashMap<String, String> PathSmer = new HashMap<>();
    private static final HashMap<String, Integer> PathMoves = new HashMap<>();

    private static boolean[][] visited;



    private static ArrayList<Obiskan> pot;


    private static class Pair {
        public int first;
        public int second;

        public Pair(int first, int second) {
            this.first = first;
            this.second = second;
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

    private static class Obiskan {
        private final int x;
        private final int y;
        private boolean pregledan;
        private final ArrayList<Pair> arr;

        public Obiskan(int x, int y, boolean pregledan, ArrayList<Pair> arr ) {
            this.x = x;
            this.y = y;
            this.pregledan = pregledan;
            this.arr = arr;
        }
        public int getX() {
            return x;
        }

        public int getY() {
            return y;
        }

        public boolean isPregledan() {
            return pregledan;
        }

        public void setPregledan(boolean pregledan) {
            this.pregledan = pregledan;
        }

        public ArrayList<Pair> getArr() {
            return arr;
        }

    }

    private static Boolean isValid(int[][] grid, boolean[][] vis, int row, int col) {
        if (row < 0 || col < 0 || row >= ROW || col >= COL)
            return false;

        if(grid[row][col] == -1)
            return false;

        return !vis[row][col];
    }

    private static void DFSsearch(int row, int col, int[][] grid, boolean[][] vis, String smer) {
        int stZakladov = 0;
        int cena = 0;
        int premik = 0;
        pot = new ArrayList<>();
        StringBuilder sb = new StringBuilder();

        Stack<Pair> st = new Stack<>();
        st.push(new Pair(row, col));

        while (!st.empty()) {
            Pair curr = st.pop();

            row = curr.first;
            col = curr.second;

            if (!isValid(grid,vis, row, col))
                continue;

            if(grid[row][col] == -3){
                stZakladov++;
            }

            if(stZakladov == stZaklad)
                zaklad = true;

            if(grid[row][col] != -2) premik++;


            if(grid[row][col] == -4) {
                sb.append("(").append(row).append(",").append(col).append(")");
                if(zaklad) {
                    PathValue.put(String.valueOf(sb), cena);
                    PathSmer.put(String.valueOf(sb), smer);
                    PathMoves.put(String.valueOf(sb), premik);

                    int a = row;
                    int b = col;
                    boolean viden;
                    for (int i= 0; i< pot.size(); i++){
                        viden = false;
                        for (Obiskan obiskan : pot) {
                            for (int k = 0; k < 4; k++) {
                                if (obiskan.getArr().get(k).first == a && obiskan.getArr().get(k).second == b && !obiskan.isPregledan()) {
                                    visited[a][b] = true;
                                    obiskan.setPregledan(true);
                                    a = obiskan.getX();
                                    b = obiskan.getY();
                                    viden = true;
                                }
                            }
                            if (viden)
                                break;
                        }
                    }
                }
                st.clear();
                break;
            }

            if(grid[row][col] >= 0){
                cena += grid[row][col];
            }

            vis[row][col] = true;


            sb.append("(").append(row).append(",").append(col).append(")").append(" -> ");

            ArrayList<Pair> arr = new ArrayList<>();
            char[] chars = smer.toCharArray();
            int adjx = 0, adjy = 0;
            for (char ch: chars) {
                if(ch == 'E'){
                    adjx = row;
                    adjy = col + 1;
                }else if(ch == 'W'){
                    adjx = row;
                    adjy = col - 1;
                }else if(ch == 'S'){
                    adjx = row+1;
                    adjy = col;
                }else if(ch == 'N'){
                    adjx = row - 1;
                    adjy = col;
                }
                arr.add(new Pair(adjx, adjy));
                st.push(new Pair(adjx, adjy));
            }
            pot.add(new Obiskan(row,col,false, arr));
        }
    }

    private static String getString(HashMap<String, String> pathSmer1, HashMap<String, Integer> pathMoves1, Map.Entry<String, Integer> entry) {
        String bestSmer;
        System.out.println("MIN: " +entry.getKey());
        bestSmer = pathSmer1.get(entry.getKey());
        StringBuilder smerObr =new StringBuilder(pathSmer1.get(entry.getKey()));
        smerObr.reverse();
        System.out.println("Najboljsa smer: " + smerObr);
        System.out.println("Cena poti: " + entry.getValue());
        System.out.println("Stevilo premikov: " + pathMoves1.get(entry.getKey()));
        return bestSmer;
    }

    public static void main(String[] args) throws FileNotFoundException {
        int[][] grid = DrawLabyrinth.readFile("D:\\FRI_Ljubljana\\GitHub\\UmetnaInteligenca\\Seminarska2\\src\\labyrinths\\labyrinth_8.txt");

        int stVozlisc = 0;
        int stObiskanih = 0;
        boolean najden = false;
        int[] start= new int[2];
        for(int i = 0; i< grid.length; i++){
            for(int j = 0; j<grid[0].length; j++){
                if(grid[i][j] == -2 && !najden){
                    start[0] = i;
                    start[1] = j;
                    najden = true;
                }else if(grid[i][j] == -3){
                    stZaklad++;
                }
                if(grid[i][j] != -1)
                    stVozlisc++;
            }
        }
        ROW = grid.length;
        COL = grid[0].length;
        boolean[][] vis;
        String bestSmer = "";
        for(String smerNeba: listSmeriNeba){
            zaklad = false;
            vis = new boolean[ROW][COL];
            visited = new boolean[ROW][COL];
            DFSsearch(start[0], start[1], grid, vis,smerNeba);
        }
        HashMap<String, Integer> PathValue1 = new HashMap<>();
        HashMap<String, String> PathSmer1 = new HashMap<>();
        HashMap<String, Integer> PathMoves1 = new HashMap<>();

        System.out.println("DFS");
        System.out.println("(Row,Col) -> (Row,Col) -> ...");
        System.out.println();

        int min = Collections.min(PathValue.values());
        for (Map.Entry<String, Integer> entry : PathValue.entrySet()) {
            if(entry.getValue() == min) {
                PathSmer1.put(entry.getKey(), PathSmer.get(entry.getKey()));
                PathValue1.put(entry.getKey(), entry.getValue());
                PathMoves1.put(entry.getKey(), PathMoves.get(entry.getKey()));
            }
            System.out.println(entry.getKey() + ", Path cost = " + entry.getValue() + ", Moves = " + PathMoves.get(entry.getKey()));
        }
        System.out.println();

        int minPremik1 = Collections.min(PathMoves1.values());
        for (Map.Entry<String, Integer> entry : PathValue1.entrySet()) {
            if(PathMoves1.get(entry.getKey()) == minPremik1)
                bestSmer = getString(PathSmer1, PathMoves1, entry);
        }
        System.out.println("Stevilo vseh vozlisc v labirintu: " + stVozlisc);
        System.out.println("Stevilo obiskanih = stevilo premikov");
        vis = new boolean[ROW][COL];
        visited = new boolean[ROW][COL];
        zaklad = false;
        DFSsearch(start[0], start[1], grid, vis,bestSmer);
        //DrawLabyrinth.drawOutPath(grid, visited, null);
        for (boolean[] vi : vis) {
            for (boolean b : vi) {
                if (b)
                    stObiskanih++;
            }
        }
        System.out.println("stevilo obiskanih vozlisc: " + (stObiskanih + 1));
        DrawLabyrinth.drawOutPathBacktrack(grid, vis,visited);

        System.out.println();
    }
}