package org.samples.java;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: pgarg
 * Date: 2/28/12
 * Time: 1:41 PM
 * To change this template use File | Settings | File Templates.
 */
public class StoreFood extends Thread {

    private ArrayList<String> fridge = new ArrayList<String>();
    String itemInQueue;

    public void run(){
        try{
            while(true){
                putFoodInFridge();
                sleep(1000);
            }
        }  catch(InterruptedException ie){
            //do nothing
        }
    }

    private synchronized void putFoodInFridge() throws InterruptedException{
        fridge.add(itemInQueue);

    }

    public void itemToStore(String item){
        itemInQueue = item;
    }

    public synchronized ArrayList<String> getItemsInFridge() throws InterruptedException{

    }
}
