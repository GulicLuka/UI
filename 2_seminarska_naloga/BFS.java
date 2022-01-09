import java.util.*;
 
class BFS{
     
    private static int ROW;
    private static int COL;
    private static int cenaAllChecked = 0;
    private static int premikiAllChecked = 0;
    private static int cenaOptimalChecked = 0;
    private static int premikiOptimalChecked = 0;
    private static int stZakladov = 0;
    private static boolean najden_zaklad = false;
    private static boolean[][] allVisited; 
    private static ArrayList<int[]> obiskani_zakladi = new ArrayList<>();
    private static ArrayList<pair> backtracking = new ArrayList<>();
    static int dRow[] = { -1, 0, 1, 0 };
    static int dCol[] = { 0, 1, 0, -1 };


    private static class pair {
        public int first;
        public int second;
        public int[] parent;
        public boolean end;

        public pair(int first, int second, int[] parent, boolean end) {
            this.first = first;
            this.second = second;
            this.parent = parent;
            this.end = end;
        }

        public boolean getEnd(){
            return this.end;
        }

        public int getFirst(){
            return this.first;
        }

        public int getSecond(){
            return this.second;
        }

        public int[] getParent(){
            return this.parent;
        }
    }

    private static Boolean isValid(int[][] grid, boolean[][] vis, int row, int col) {
        if (row < 0 || col < 0 || row >= ROW || col >= COL)
            return false;

        if(grid[row][col] == -1)
            return false;

        return !vis[row][col];
    }


    private static void BFSsearch(int row, int col, int[][] grid, boolean[][] vis) {
        int id = 1;
        najden_zaklad = false;

        Queue<pair> queue = new LinkedList<>();
        int[] start = {row, col};
        queue.add(new pair(row, col, start, false));
        backtracking.add(new pair(row, col, start, false));
        vis[row][col] = true;

        while (!queue.isEmpty()) {

            pair curr = queue.remove();;

            row = curr.first;
            col = curr.second;
            
            if(grid[row][col] >= 0 || grid[row][col] == -2){
                if(grid[row][col] >= 0){
                    cenaAllChecked += grid[row][col];
                }
                vis[row][col] = true;
                allVisited[row][col] = true;
            }

            int[] a = {row,col};
            if(grid[row][col] == -3 && !najden_zaklad){
                boolean notIn = true;
                for(int[] arr : obiskani_zakladi){
                    if(arr[0] == a[0] && arr[1] == a[1]){
                        notIn = false;
                        break;
                    }
                }
                if(notIn){
                    System.out.println("No. " + id + ": Position: [" + row + ", " + col + "]");
                    obiskani_zakladi.add(a);
                    vis = new boolean[grid.length][grid[0].length];
                    queue.clear();
                    id++;
                    
                    if(obiskani_zakladi.size() == stZakladov){
                        System.out.println("Najdeni so vsi zakladi");
                        najden_zaklad = true;
                    }
                }
            }

            premikiAllChecked++;

            if(najden_zaklad && grid[row][col] == -4){
                System.out.println("No. " + id + ": Position: [" + row + ", " + col + "] CILJ");
                queue.clear();
                return;
            }

            for(int i = 0; i < 4; i++)
            {
                int adjx = row + dRow[i];
                int adjy = col + dCol[i];
     
                if (isValid(grid, vis, adjx, adjy)) {
                    int[] parent = {row, col};
                    if(najden_zaklad && grid[adjx][adjy] == -4){
                        queue.add(new pair(adjx, adjy, parent, true));
                        backtracking.add(new pair(adjx, adjy, parent, true));
                    } else{
                        queue.add(new pair(adjx, adjy, parent, false));
                        backtracking.add(new pair(adjx, adjy, parent, false));
                    }
                    vis[adjx][adjy] = true;
                    allVisited[adjx][adjy] = true;
                }
            }

            if(queue.isEmpty()){
                System.out.println("Prazn QUEUE");
            }
        }
    }

    public static boolean[][] findOptimalPath(int[][] grid){
        //find end
        int indexOfFinish = backtracking.size()-1;
        for(int i=backtracking.size()-1; i >= 0; i--){
            pair curr = backtracking.get(i);
            if(curr.getEnd()){
                indexOfFinish = i;
                break;
            }
        }

        //find optimal path
        boolean[][] optimalPath = new boolean[ROW][COL];
        for(int i=indexOfFinish; i >= 0; i--){
            pair current = backtracking.get(i);
            int currX = current.getFirst();
            int currY = current.getSecond();
            int[] parentOfCurrent = current.getParent();
            if(parentOfCurrent[0] == currX && parentOfCurrent[1] == currY){
                optimalPath[currX][currY] = true;
                return optimalPath;
            }
            for(int j=i-1; j >= 0; j--){
                pair next = backtracking.get(j);
                if(parentOfCurrent[0] == next.getFirst() && parentOfCurrent[1] == next.getSecond()){
                    optimalPath[next.getFirst()][next.getSecond()] = true;
                    premikiOptimalChecked++;
                    if(grid[next.getFirst()][next.getSecond()] >= 0)
                        cenaOptimalChecked += grid[next.getFirst()][next.getSecond()];
                    i = j+1;
                    break;
                }
            }
        }
        return optimalPath;
    }




    public static void setup(int[][] grid) {
        boolean najden = false;
        int[] start= new int[2];
        for(int i = 0; i< grid.length; i++){
            for(int j = 0; j<grid[0].length; j++){
                if(grid[i][j] == -2 && !najden){
                    start[0] = i;
                    start[1] = j;
                    najden = true;
                }else if(grid[i][j] == -3){
                    stZakladov++;
                }
            }
        }
        ROW = grid.length;
        COL = grid[0].length;
        boolean[][] vis;
        vis = new boolean[ROW][COL];
        allVisited = new boolean[ROW][COL];
        BFSsearch(start[0], start[1], grid, vis);
        boolean[][] optimal = findOptimalPath(grid);
        System.out.println("Cena vseh obiskanih vozlisc: " + cenaAllChecked);
        System.out.println("Stevilo vseh premikov: " + premikiAllChecked);
        System.out.println("Cena optimalne poti: " + cenaOptimalChecked);
        System.out.println("Stevilo premikov za optimalo pot: " + premikiOptimalChecked);
        Naloga.drawOutPath(grid, allVisited, optimal);
    }
}
