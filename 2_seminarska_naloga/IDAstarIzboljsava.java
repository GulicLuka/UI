import java.util.ArrayList;
import java.util.HashMap;

import javax.management.relation.Role;

class IDAstarIzboljsava {
    private static int ROW;
    private static int COL;
    private static int D;
    private static int[] CILJ = new int[2];
    private static int cenaAllChecked = 0;
    private static int premikiAllChecked = 0;
    private static int cenaOptimalChecked = 0;
    private static int premikiOptimalChecked = 0;
    private static int stZakladov = 0;
    private static boolean najden_zaklad = false;
    //private static boolean[][] allVisited; 
    private static boolean[][] isVisited;
    private static ArrayList<int[]> obiskani_zakladi = new ArrayList<>();
    private static ArrayList<pair> backtracking = new ArrayList<>();
    private static ArrayList<boolean[][]> listPoti = new ArrayList<>();
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

    private static Boolean isValid(int[][] grid, int row, int col, int[] parent) {
        if (row < 0 || col < 0 || row >= ROW || col >= COL)
            return false;

        if(grid[row][col] == -1)
            return false;

        if(row == parent[0] && col == parent[1])
            return false;
        else
            return true;
        //return !allVisited[row][col];
    }

    private static void IDAstar(int row, int col, int[][] grid) {
        int meja = minHevristika(row, col);
        int init_meja = meja;
        int stObiskanih = 0;
        while(true){
            //allVisited[row][col] = true;
            isVisited[row][col] = true;
            int[] parent = {row, col};
            int[] temp = search(row, col, parent, 0, meja, grid);
            if(obiskani_zakladi.size() > 0){
                for(int[] currZaklad : obiskani_zakladi){
                    if(temp[0] == currZaklad[0] && temp[1] == currZaklad[1]){
                        System.out.println("Najden zaklad:" + currZaklad[0] + ":" + currZaklad[1]);
                        listPoti.add(findOptimalPath(grid));
                        backtracking = new ArrayList<>();
                        //allVisited = new boolean[ROW][COL];
                        meja = init_meja;
                        stObiskanih++;

                        if(stObiskanih == stZakladov){
                            najden_zaklad = true;
                        }
                    }
                }

                int getRemoveindeks = -1;
                boolean removed = false;
                for(int i=0; i < obiskani_zakladi.size(); i++){
                    if(temp[0] == obiskani_zakladi.get(i)[0] && temp[1] == obiskani_zakladi.get(i)[1]){
                        getRemoveindeks = i;
                        removed = true;
                        break;
                    }
                }

                if(removed){
                    obiskani_zakladi.remove(getRemoveindeks);
                    row = temp[0];
                    col = temp[1];
                    meja = minHevristika(row, col);
                    init_meja = meja;
                    removed = false;
                    continue;
                }
                    

            } else if(temp[0] == CILJ[0] && temp[1] == CILJ[1]){
                System.out.println("Najden cilj: " + CILJ[0] + ":" + CILJ[1]);
                listPoti.add(findOptimalPath(grid));
                backtracking = new ArrayList<>();
                return;
            }

            if(temp[2] >= Integer.MAX_VALUE){
                return;
            }
            backtracking = new ArrayList<>();
            //allVisited = new boolean[ROW][COL];
            meja = temp[2];
        }
    }

    private static int[] search(int row, int col, int[] parent, int g, int meja, int[][] grid){
        int f = g + minHevristika(row, col); // skupna = costDoN + hevristika
        premikiAllChecked++;
        if(grid[row][col] > 0)
            cenaAllChecked += grid[row][col];
        if(f > meja){
            int[] niNajdbe = {row, col, f};
            return niNajdbe;
        }
        if(row == CILJ[0] && col == CILJ[1] && najden_zaklad){
            backtracking.add(new pair(row, col, parent, true));
            int[] najden = {row, col, f};
            return najden;
        } 
        
        for(int[] z : obiskani_zakladi){
            if(row == z[0] && col == z[1]){
                backtracking.add(new pair(row, col, parent, true));
                int[] najdenZaklad = {row, col, f};
                return najdenZaklad;
            }
        }
        
        backtracking.add(new pair(row, col, parent, false));
        
        int[] currParent = {row, col};
        int[] najmanj = {row, col, Integer.MAX_VALUE};
        for(int[] next : nextNodes(row, col, grid, parent)){
            int[] temp = search(next[0], next[1],currParent , g + grid[next[0]][next[1]], meja, grid);

            for(int[] z : obiskani_zakladi){
                if(temp[0] == z[0] && temp[1] == z[1]){
                    int[] najdenZaklad = {temp[0], temp[1], f};
                    return najdenZaklad;
                }
            }

            if(temp[0] == CILJ[0] && temp[1] == CILJ[1] && najden_zaklad){
                int[] najden = {temp[0], temp[1], f};
                return najden;
            }
            if(temp[2] < najmanj[2]){
                najmanj[0] = temp[0];
                najmanj[1] = temp[1];
                najmanj[2] = temp[2];
            }
        }
        return najmanj;
    }

    private static ArrayList<int[]> nextNodes(int row, int col, int[][] grid, int[] parent){
        ArrayList<int[]> nextNodesList = new ArrayList<>();
        for(int i = 0; i < 4; i++) {
            int adjx = row + dRow[i];
            int adjy = col + dCol[i];
 
            if (isValid(grid, adjx, adjy, parent)) {
                int[] next = {adjx,adjy};
                nextNodesList.add(next);
                isVisited[adjx][adjy] = true;
                //allVisited[adjx][adjy] = true;
            }
        }
        return nextNodesList;
    }

    private static int minHevristika(int currX, int currY){
        if(obiskani_zakladi.size() > 0){
            int minHev = Integer.MAX_VALUE;   
            for(int[] zaklad : obiskani_zakladi){
                int newHev = hevristika(currX, currY, zaklad[0], zaklad[1]);
                if(newHev < minHev){
                    minHev = newHev;
                }
            }
            return minHev;
        } else {
            return hevristika(currX, currY, CILJ[0], CILJ[1]);
        }
    }
    
    private static int hevristika(int currX, int currY, int goalX, int goalY){
        int distX = Math.abs(currX - goalX);
        int distY = Math.abs(currY - goalY);
        return D * (distX + distY);
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

    private static boolean[][] getPath(){
        boolean[][] finalPath = new boolean[ROW][COL];
        for(int i=0; i < ROW; i++){
            for(int j=0; j < COL; j++){
                for(boolean[][] unfinished : listPoti){
                    if(unfinished[i][j]){
                        finalPath[i][j] = true;
                        break;
                    }
                }
            }
        }
        return finalPath;
    }
    
    public static void setup(int[][] grid) {
        stZakladov = 0;
        obiskani_zakladi = new ArrayList<>();
        backtracking = new ArrayList<>();
        listPoti = new ArrayList<>();
        najden_zaklad = false;
        CILJ = new int[2];
        boolean najden = false;
        int[] start= new int[2];
        D = Integer.MAX_VALUE;
        for(int i = 0; i< grid.length; i++){
            for(int j = 0; j<grid[0].length; j++){
                //get D - min cost v labirintu
                if(grid[i][j] > 0 && D > grid[i][j]){
                    D = grid[i][j];
                }
                //najdi start
                if(grid[i][j] == -2 && !najden){
                    start[0] = i;
                    start[1] = j;
                    najden = true;
                } else if(grid[i][j] == -3){ //iskanje zakladov in shranjevanje v arraylist (belezenje ze obiskanih)
                    stZakladov++;
                    int[] zaklad = {i,j};
                    obiskani_zakladi.add(zaklad);
                } else if(grid[i][j] == -4){ //zapomni si cilj.
                    CILJ[0] = i;
                    CILJ[1] = j;
                }
            }
        }
        ROW = grid.length;
        COL = grid[0].length;
        isVisited = new boolean[ROW][COL];
        //allVisited = new boolean[ROW][COL];
        IDAstar(start[0], start[1], grid);
        boolean[][] optimal = getPath();
        System.out.println("Cena vseh obiskanih vozlisc: " + cenaAllChecked);
        System.out.println("Stevilo vseh premikov: " + premikiAllChecked);
        System.out.println("Cena optimalne poti: " + cenaOptimalChecked);
        System.out.println("Stevilo premikov za optimalo pot: " + premikiOptimalChecked);
        Naloga.drawOutPath(grid, isVisited, optimal);
    }
}
