package org.samples.java;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: pgarg
 * Date: 2/28/12
 * Time: 1:41 PM
 * To change this template use File | Settings | File Templates.
 */
public class Fridge extends Thread {

    public ArrayList<String> getFridge() {
        return fridge;
    }

    private ArrayList<String> fridge = new ArrayList<String>();
    String itemToStore = new String();
    String itemToRemove;

    public void run(){
        try{
            while(true){
                putFoodInFridge();
            }
        }  catch(InterruptedException ie){
            System.out.println("Stuck in here");
        }
    }

    private synchronized void putFoodInFridge() throws InterruptedException{

        while (itemToStore.isEmpty()){
            wait();
        }

        System.out.println("Got message to store food item: ");
        fridge.add(itemToStore);
        System.out.println("Item added to Fridge: " + itemToStore);

        itemToStore = new String();
        notify();

    }

    public void setItemToStore(String itemToStore){
        this.itemToStore = itemToStore;
    }

    public void setItemToRemove(String itemToRemove){
        this.itemToRemove = itemToRemove;
    }

    public synchronized void removeItemsFromFrige() throws InterruptedException{

        notify();
        while(fridge.isEmpty() && itemToRemove.isEmpty()){
            wait();
        }

        System.out.println("Got message to remove food item ");
        fridge.remove(itemToRemove);
        System.out.println("This is what the fridge looks like now: " + fridge);

        itemToRemove = new String();

    }
}
