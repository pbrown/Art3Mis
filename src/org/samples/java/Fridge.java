package org.samples.java;


import java.lang.management.ManagementFactory;
import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: pgarg
 * Date: 2/27/12
 * Time: 9:44 AM
 * To change this template use File | Settings | File Templates.
 */
public class Fridge implements Runnable {

    Thread myThread;
    ArrayList<String> itemsInFridge = new ArrayList<String>();

    public void start(){
        if(myThread == null){
            myThread = new Thread(this);
            myThread.start();
        }
    }

    public synchronized void store(String item){
        itemsInFridge.add(item);
    }

    public void printContentsOfFridge(){
           System.out.println("My fridge looks like " + itemsInFridge.toString());
    }
    public void run(){

        while(true){
             doWork();
        }

    }

    private void doWork(){
        System.out.println("I am doing working on ssomething");
    }

    public void stop(){
        if(myThread!=null){
            myThread.stop();
            myThread = null;
        }
    }

private static String getProcessId(final String fallback){
    final String jvmName = ManagementFactory.getRuntimeMXBean().getName();
    final int index =  jvmName.indexOf('@');

    if (index < 1){
        return fallback;
    }

    try{

        return Long.toString(Long.parseLong(jvmName.substring(0, index)));

    } catch(NumberFormatException ne){
        //ignore
    }

    return fallback;

}
public static void main(String[] args){
        Fridge fridge = new Fridge();
        fridge.start();
        fridge.store("apples");
        System.out.println("Thread PID: "+ Fridge.getProcessId("PID"));
        fridge.stop();
        fridge.printContentsOfFridge();
        System.out.println("Thread PID: "+ Fridge.getProcessId("PID"));

    }

}