import java.util.*;
 
class BFS{
     
    private static int ROW;
    private static int COL;
    private static int stZaklad;
    private static boolean najden_zaklad = false;
    private static boolean najden_cilj = false;
    private static final String[] listSmeriNeba = {"ENWS", "NEWS", "WENS", "EWNS", "NWES", "WNES", "SNEW", "NSEW", "ESNW", "SENW", "NESW", "ENSW", "EWSN", "WESN", "SEWN", "ESWN", "WSEN", "SWEN", "SWNE", "WSNE", "NSWE", "SNWE", "WNSE", "NWSE"};
    private static HashMap<String, Integer> PathValue = new HashMap<>();
    private static HashMap<String, String> PathSmer = new HashMap<>();
    private static HashMap<String, Integer> PathMoves = new HashMap<>();
    private static HashMap<String, boolean[][]> PathVisited = new HashMap<>();
    private static ArrayList<int[]> obiskani_zakladi = new ArrayList<>();

    private static class pair {
        public int first;
        public int second;

        public pair(int first, int second) {
            this.first = first;
            this.second = second;
        }
    }

    private static Boolean isValid(int[][] grid, boolean[][] vis, int row, int col) {
        if (row < 0 || col < 0 || row >= ROW || col >= COL)
            return false;

        if(grid[row][col] == -1)
            return false;

        return !vis[row][col];
    }


    private static void BFSsearch(int row, int col, int[][] grid, boolean[][] vis, String smer) {
        System.out.println(smer);
        int stZakladov = 0;
        int cena = 0;
        int premik = 0;
        StringBuilder sb = new StringBuilder();

        Queue<pair> queue = new LinkedList<>();
        queue.add(new pair(row, col));

        while (!queue.isEmpty()) {

            pair curr = queue.remove();
            row = curr.first;
            col = curr.second;

            int[] a = {row, col};
            if(grid[row][col] == -3 && !obiskani_zakladi.contains(a)){
                obiskani_zakladi.add(a);
            }

            if(obiskani_zakladi.size() == stZaklad)
                najden_zaklad = true;

            premik++;

            if(najden_cilj && najden_zaklad){
                PathValue.put(String.valueOf(sb), cena);
                PathSmer.put(String.valueOf(sb), smer);
                PathMoves.put(String.valueOf(sb), premik);
                PathVisited.put(String.valueOf(sb), vis);
                queue.clear();
                return;
            }

            if(grid[row][col] == -4){
                sb.append(row).append(":").append(col);
                najden_cilj = true;
                if(najden_zaklad) {
                    PathValue.put(String.valueOf(sb), cena);
                    PathSmer.put(String.valueOf(sb), smer);
                    PathMoves.put(String.valueOf(sb), premik);
                    PathVisited.put(String.valueOf(sb), vis);
                }
                queue.clear();
                break;
            }

            if(grid[row][col] >= 0 || grid[row][col] == -2){
                cena += grid[row][col];
                vis[row][col] = true;
            }

            sb.append(row).append(":").append(col).append(" -> ");

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

                if (isValid(grid, vis, adjx, adjy))
                {
                    queue.add(new pair(adjx, adjy));
                    vis[adjx][adjy] = true;
                }
            }
        }
        System.out.println(sb.toString());
    }

    private static String getString(HashMap<String, String> pathSmer1, HashMap<String, Integer> pathMoves1, int minPremik, Map.Entry<String, Integer> entry) {
        String bestSmer;
        System.out.println("MIN: " +entry.getKey());
        bestSmer = pathSmer1.get(entry.getKey());
        System.out.println("Najboljsa smer: " + pathSmer1.get(entry.getKey()));
        System.out.println("Cena poti: " + entry.getValue());
        System.out.println("Stevilo premikov: " + pathMoves1.get(entry.getKey()));
        System.out.println("Minimalno stevilo premikov: " + minPremik);
        return bestSmer;
    }

    public static void setup(int[][] grid) {
        PathValue = new HashMap<>();
        PathSmer = new HashMap<>();
        PathMoves = new HashMap<>();
        obiskani_zakladi = new ArrayList<>();
        najden_zaklad = false;
        stZaklad = 0;
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
            }
        }
        ROW = grid.length;
        COL = grid[0].length;
        boolean[][] vis;
        String bestSmer = "";
        for(String smerNeba: listSmeriNeba){
            obiskani_zakladi = new ArrayList<>();
            najden_zaklad = false;
            najden_cilj = false;
            vis = new boolean[ROW][COL];
            BFSsearch(start[0], start[1], grid, vis, smerNeba);
        }
        HashMap<String, Integer> PathValue1 = new HashMap<>();
        HashMap<String, String> PathSmer1 = new HashMap<>();
        HashMap<String, Integer> PathMoves1 = new HashMap<>();

        System.out.println("BFS");
        System.out.println("Row:Col -> Row:Col -> ...");
        System.out.println();

        if(PathValue.size() < 1){
            System.out.println("Ni poti");
            return;
        }
        int min = Collections.min(PathValue.values());
        int minPremik = Collections.min(PathMoves.values());
        for (Map.Entry<String, Integer> entry : PathValue.entrySet()) {
            if(entry.getValue() == min) {
                PathSmer1.put(entry.getKey(), PathSmer.get(entry.getKey()));
                PathValue1.put(entry.getKey(), entry.getValue());
                PathMoves1.put(entry.getKey(), PathMoves.get(entry.getKey()));
            }
            System.out.println();
            System.out.println(entry.getKey() + ", Path cost = " + entry.getValue() + ", Moves = " + PathMoves.get(entry.getKey()));
        }
        System.out.println();

        int minPremik1 = Collections.min(PathMoves1.values());
        for (Map.Entry<String, Integer> entry : PathMoves1.entrySet()) {
            if(entry.getValue() == minPremik1)
                bestSmer = getString(PathSmer1, PathMoves1, minPremik, entry);
        }
        vis = new boolean[ROW][COL];
        obiskani_zakladi = new ArrayList<>();
        najden_zaklad = false;
        najden_cilj = false;
        BFSsearch(start[0], start[1], grid, vis,bestSmer);
        Naloga.drawOutPath(grid, vis);
    }

}