package Restaurant_S;

import java.awt.EventQueue;

import javax.swing.JFrame;

import java.awt.FlowLayout;
import java.awt.Color;

import javax.swing.JPanel;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.JLabel;

import java.awt.Font;

import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.JTabbedPane;
import javax.swing.UIManager;
import javax.swing.JScrollBar;
import javax.swing.JList;
import javax.swing.JSpinner;
import javax.swing.JComboBox;
import javax.swing.JCheckBox;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JSeparator;
import javax.swing.JButton;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class Restaurants {

	private JFrame frame;
	private JTextField txtChickenBurger;
	private JTextField txtChickenBurgerMeal;
	private JTextField txtCheaseBurger;
	private JTextField textField_3;
	private JTextField txtR;
	private JTextField textDrinkQty;
	private JTextField txtConverter;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					Restaurants window = new Restaurants();
					window.frame.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the application.
	 */
	public Restaurants() {
		initialize();
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		frame = new JFrame();
		frame.setForeground(Color.PINK);
		frame.setBounds(0, 10, 950, 620);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().setLayout(null);
		
		JPanel panel = new JPanel();
		panel.setBorder(new LineBorder(Color.BLUE, 2));
		panel.setBounds(30, 96, 347, 200);
		frame.getContentPane().add(panel);
		panel.setLayout(null);
		
		JLabel lblNewLabel_1 = new JLabel("Chicken Burger");
		lblNewLabel_1.setBounds(10, 11, 138, 38);
		lblNewLabel_1.setFont(new Font("Tahoma", Font.BOLD, 14));
		panel.add(lblNewLabel_1);
		
		JLabel lblChickenBurgerMeal = new JLabel("Chicken Burger Meal");
		lblChickenBurgerMeal.setBounds(10, 39, 167, 38);
		lblChickenBurgerMeal.setFont(new Font("Tahoma", Font.BOLD, 14));
		panel.add(lblChickenBurgerMeal);
		
		JLabel lblCheaseBurger = new JLabel("Chease Burger");
		lblCheaseBurger.setBounds(10, 71, 138, 38);
		lblCheaseBurger.setFont(new Font("Tahoma", Font.BOLD, 14));
		panel.add(lblCheaseBurger);
		
		txtChickenBurger = new JTextField();
		txtChickenBurger.setHorizontalAlignment(SwingConstants.RIGHT);
		txtChickenBurger.setText("0");
		txtChickenBurger.setBounds(187, 22, 138, 20);
		panel.add(txtChickenBurger);
		txtChickenBurger.setColumns(10);
		
		txtChickenBurgerMeal = new JTextField();
		txtChickenBurgerMeal.setHorizontalAlignment(SwingConstants.RIGHT);
		txtChickenBurgerMeal.setText("0");
		txtChickenBurgerMeal.setBounds(187, 50, 138, 20);
		txtChickenBurgerMeal.setColumns(10);
		panel.add(txtChickenBurgerMeal);
		
		txtCheaseBurger = new JTextField();
		txtCheaseBurger.setHorizontalAlignment(SwingConstants.RIGHT);
		txtCheaseBurger.setText("0");
		txtCheaseBurger.setBounds(187, 82, 138, 20);
		txtCheaseBurger.setColumns(10);
		panel.add(txtCheaseBurger);
		
		JComboBox comboBoxDrink = new JComboBox();
		comboBoxDrink.setModel(new DefaultComboBoxModel(new String[] {"Select a Drink", "Apple Juice", "Coca Cola", "Milk Shake", "Coffe", "Tea", "Capachino", "Ice Coffe", "Orange Juice"}));
		comboBoxDrink.setBounds(10, 140, 167, 25);
		panel.add(comboBoxDrink);
		
		JLabel lblDrink = new JLabel("Drink");
		lblDrink.setFont(new Font("Tahoma", Font.BOLD, 14));
		lblDrink.setBounds(10, 101, 138, 38);
		panel.add(lblDrink);
		
		JLabel lblQty = new JLabel("Qty");
		lblQty.setFont(new Font("Tahoma", Font.BOLD, 14));
		lblQty.setBounds(187, 101, 138, 38);
		panel.add(lblQty);
		
		JCheckBox chckbxHomeDelivery = new JCheckBox("Home Delivery");
		chckbxHomeDelivery.setBounds(6, 170, 157, 23);
		panel.add(chckbxHomeDelivery);
		
		textDrinkQty = new JTextField();
		textDrinkQty.setColumns(10);
		textDrinkQty.setBounds(187, 142, 138, 20);
		panel.add(textDrinkQty);
		
		JCheckBox chckbxTax = new JCheckBox("Tax");
		chckbxTax.setBounds(187, 170, 97, 23);
		panel.add(chckbxTax);
		
		JSeparator separator = new JSeparator();
		separator.setBounds(150, 127, 1, 2);
		panel.add(separator);
		
		JSeparator separator_1 = new JSeparator();
		separator_1.setBounds(10, 127, 300, 6);
		panel.add(separator_1);
		
		JPanel panel_1 = new JPanel();
		panel_1.setBorder(new LineBorder(Color.BLUE, 2));
		panel_1.setBounds(387, 96, 195, 200);
		frame.getContentPane().add(panel_1);
		panel_1.setLayout(null);
		
		JComboBox comboBoxCountry = new JComboBox();
		comboBoxCountry.setBounds(10, 48, 167, 25);
		comboBoxCountry.setModel(new DefaultComboBoxModel(new String[] {"Choose one Country","India ", "America", "Indonesia", "Japan", "China"}));
		panel_1.add(comboBoxCountry);
		
		txtConverter = new JTextField();
		txtConverter.setHorizontalAlignment(SwingConstants.CENTER);
		txtConverter.setBounds(10, 84, 167, 30);
		txtConverter.setColumns(10);
		panel_1.add(txtConverter);
		
		JLabel lblConverter = new JLabel("");
		lblConverter.setBounds(10, 125, 167, 30);
		lblConverter.setHorizontalAlignment(SwingConstants.CENTER);
		lblConverter.setBorder(new LineBorder(Color.BLUE, 2));
		panel_1.add(lblConverter);
		
		JButton btnClose = new JButton("Close");
		btnClose.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				txtConverter.setText(null);
				lblConverter.setText(null);
			}
		});
		btnClose.setBounds(110, 166, 75, 23);
		panel_1.add(btnClose);
		
		JLabel lblNewLabel_4 = new JLabel("Currency Converter");
		lblNewLabel_4.setBounds(10, 11, 175, 23);
		lblNewLabel_4.setFont(new Font("Tahoma", Font.BOLD, 16));
		panel_1.add(lblNewLabel_4);
		
		JButton btnConverter = new JButton("Conveter");
		btnConverter.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				txtConverter.setText("Sorry! Under Progress");
				lblConverter.setText("Under Construction");
			}
		});
		btnConverter.setBounds(20, 166, 89, 23);
		panel_1.add(btnConverter);
		
		JPanel panel_2 = new JPanel();
		panel_2.setBorder(new LineBorder(Color.BLUE, 2));
		panel_2.setBounds(30, 307, 347, 143);
		frame.getContentPane().add(panel_2);
		panel_2.setLayout(null);
		
		JLabel lblCostOfDrink = new JLabel("Cost of Drink");
		lblCostOfDrink.setBounds(10, 11, 138, 38);
		lblCostOfDrink.setFont(new Font("Tahoma", Font.BOLD, 14));
		panel_2.add(lblCostOfDrink);
		
		JLabel lblCostOfMeal = new JLabel("Cost of Meal");
		lblCostOfMeal.setBounds(10, 39, 167, 38);
		lblCostOfMeal.setFont(new Font("Tahoma", Font.BOLD, 14));
		panel_2.add(lblCostOfMeal);
		
		JLabel lblCostOfDelivery = new JLabel("Cost of Delivery");
		lblCostOfDelivery.setBounds(10, 69, 138, 38);
		lblCostOfDelivery.setFont(new Font("Tahoma", Font.BOLD, 14));
		panel_2.add(lblCostOfDelivery);
		
		JLabel lblCost1 = new JLabel("");
		lblCost1.setHorizontalAlignment(SwingConstants.RIGHT);
		lblCost1.setBorder(new LineBorder(Color.BLUE, 2));
		lblCost1.setBounds(174, 25, 149, 30);
		panel_2.add(lblCost1);
		
		JLabel lblCost2 = new JLabel("");
		lblCost2.setHorizontalAlignment(SwingConstants.RIGHT);
		lblCost2.setBorder(new LineBorder(Color.BLUE, 2));
		lblCost2.setBounds(174, 53, 149, 26);
		panel_2.add(lblCost2);
		
		JLabel lblCost3 = new JLabel("");
		lblCost3.setHorizontalAlignment(SwingConstants.RIGHT);
		lblCost3.setBorder(new LineBorder(Color.BLUE, 2));
		lblCost3.setBounds(174, 81, 149, 26);
		panel_2.add(lblCost3);
		
		JPanel panel_3 = new JPanel();
		panel_3.setBorder(new LineBorder(Color.BLUE, 2));
		panel_3.setBounds(387, 307, 195, 143);
		frame.getContentPane().add(panel_3);
		panel_3.setLayout(null);
		
		JLabel lblTax = new JLabel("Tax");
		lblTax.setFont(new Font("Tahoma", Font.BOLD, 14));
		lblTax.setBounds(10, 11, 127, 38);
		panel_3.add(lblTax);
		
		JLabel lblSubTotal = new JLabel("Sub Total");
		lblSubTotal.setFont(new Font("Tahoma", Font.BOLD, 14));
		lblSubTotal.setBounds(10, 39, 127, 38);
		panel_3.add(lblSubTotal);
		
		JLabel lblSubTotal_1 = new JLabel("Total");
		lblSubTotal_1.setFont(new Font("Tahoma", Font.BOLD, 14));
		lblSubTotal_1.setBounds(10, 60, 127, 38);
		panel_3.add(lblSubTotal_1);
		
		JLabel labelTax = new JLabel("");
		labelTax.setHorizontalAlignment(SwingConstants.RIGHT);
		labelTax.setBorder(new LineBorder(Color.BLUE, 2));
		labelTax.setBounds(128, 16, 57, 30);
		panel_3.add(labelTax);
		
		JLabel lblSubT = new JLabel("");
		lblSubT.setHorizontalAlignment(SwingConstants.RIGHT);
		lblSubT.setBorder(new LineBorder(Color.BLUE, 2));
		lblSubT.setBounds(128, 44, 57, 26);
		panel_3.add(lblSubT);
		
		JLabel lblT = new JLabel("");
		lblT.setHorizontalAlignment(SwingConstants.RIGHT);
		lblT.setBorder(new LineBorder(Color.BLUE, 2));
		lblT.setBounds(128, 72, 57, 26);
		panel_3.add(lblT);
		
		JPanel panel_4 = new JPanel();
		panel_4.setBorder(new LineBorder(Color.BLUE, 2));
		panel_4.setBounds(30, 473, 552, 65);
		frame.getContentPane().add(panel_4);
		panel_4.setLayout(null);
		
		JButton btnTotal = new JButton("Total");
		btnTotal.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e){
				//1
				double ChicBurger = Double.parseDouble(txtChickenBurger.getText());
				double iChicBurger = 30.0;
				double Burger;
				Burger = (ChicBurger * iChicBurger);
				String pMeal = String.format("%.2f", Burger);
				//lblCost2.setText(pMeal);
				//2
				double ChicBurgerMeal = Double.parseDouble(txtChickenBurgerMeal.getText());
				double iChicBurgerMeal = 50.0;
				double BurgerMeal;
				BurgerMeal = (ChicBurgerMeal * iChicBurgerMeal);
				String cMeal = String.format("%.2f", BurgerMeal);
				//lblCost2.setText(cMeal);
				//3
				double CheaseBurger = Double.parseDouble(txtCheaseBurger.getText());
				double iCheaseBurger = 45.0;
				double CheaseBurgerMeal;
				CheaseBurgerMeal = (CheaseBurger * iCheaseBurger);
				String cheese = String.format("%.2f", CheaseBurgerMeal);
				//lblCost2.setText(cheese);
				/*System.out.println("=====================================================");
				System.out.println("1   "+ChicBurger+"2   "+ChicBurgerMeal+"3   "+CheaseBurger);
				System.out.println("1   "+Burger+"2   "+BurgerMeal+"3   "+CheaseBurgerMeal);
				System.out.println("=====================================================");*/
				//============================================================================
				if(ChicBurger!=0 || ChicBurgerMeal!=0 || CheaseBurger!= 0)
				{
					double d = (Burger+BurgerMeal+CheaseBurgerMeal);
					String x = String.format("%.2f", d);
					lblCost2.setText(x);
				}
				//System.out.println(lblCost2.setText(x));
				//else if()
				// home delivery
				double iDelivery = 3.0;
				if(chckbxHomeDelivery.isSelected())
				{
					String pDelivery = String.format("%.2f", iDelivery);
					lblCost3.setText(pDelivery);
				}
				else
				{
					lblCost3.setText("0");
				}
				
				//System.out.println();
				//------------Drink-------------------------
				double Drinks = Double.parseDouble(textDrinkQty.getText());
				double Tea = 5.0 * Drinks;
				double IceCoffe = 20.0* Drinks;
				double AppleJuice = 30.0* Drinks;
				double CocaCola = 25.0* Drinks;
				double MilkShake = 50.0* Drinks;
				double Coffe = 15.0* Drinks;
				double Capachino = 80.0* Drinks;
				double OrangeJuice = 35.0* Drinks;
				if(comboBoxDrink.getSelectedItem().equals("Tea"))
				{
					String x = String.format("%.2f", Tea);
					lblCost1.setText(x);
				}
				if(comboBoxDrink.getSelectedItem().equals("Ice Coffe"))
				{
					String x = String.format("%.2f", IceCoffe);
					lblCost1.setText(x);
				}
				if(comboBoxDrink.getSelectedItem().equals("Apple Juice"))
				{
					String x = String.format("%.2f", AppleJuice);
					lblCost1.setText(x);
				}
				if(comboBoxDrink.getSelectedItem().equals("Coca Cola"))
				{
					String x = String.format("%.2f", CocaCola);
					lblCost1.setText(x);
				}
				if(comboBoxDrink.getSelectedItem().equals("Capachino"))
				{
					String x = String.format("%.2f", Capachino);
					lblCost1.setText(x);
				}
				if(comboBoxDrink.getSelectedItem().equals("Orange Juice"))
				{
					String x = String.format("%.2f", OrangeJuice);
					lblCost1.setText(x);
				}
				if(comboBoxDrink.getSelectedItem().equals("Milk Shake"))
				{
					String x = String.format("%.2f", MilkShake);
					lblCost1.setText(x);
				}
				if(comboBoxDrink.getSelectedItem().equals("Coffe"))
				{
					String x = String.format("%.2f", Coffe);
					lblCost1.setText(x);
				}
				
				/*double cb   = Double.parseDouble(txtChickenBurger.getText());				
			    double ccb  = Double.parseDouble(txtChickenBurgerMeal.getText());			 
				double chb  = Double.parseDouble(txtCheaseBurger.getText());
				double allT = ((cb+ccb+chb)*8)/100;*/
				//System.out.println("===================================");
				double allT = 15.00/100.00;				
				//System.out.println(allT);
				//System.out.println("===================================");
				if(chckbxTax.isSelected())
				{
					String x = String.format("%.2f", allT);
					labelTax.setText(x);
				}
				//===================================================
				//Home Delivery
				double tax = Double.parseDouble(labelTax.getText());
				double cost1 = Double.parseDouble(lblCost1.getText());
				double cost2 = Double.parseDouble(lblCost2.getText());
				double cost3 = Double.parseDouble(lblCost3.getText());
				double sub = cost1+cost2+cost3;
				String subT = String.format("%.2f", sub);
				lblSubT.setText(subT);
				double total = sub+(sub*tax);
				String tot = String.format("%.2f", total);
				lblT.setText(tot);
 				//===================================================
				
			}
		});
		
		btnTotal.setFont(new Font("Tahoma", Font.PLAIN, 25));
		btnTotal.setBounds(26, 11, 118, 43);
		panel_4.add(btnTotal);
		
		JButton btnReset = new JButton("Reset");
		btnReset.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				txtChickenBurger.setText("0");
				txtChickenBurgerMeal.setText("0");
				txtCheaseBurger.setText("0");
				txtConverter.setText(null);
				textDrinkQty.setText(null);
				lblCost1.setText(null);
				lblCost2.setText(null);
				lblCost3.setText(null);
				labelTax.setText(null);
				lblSubT.setText(null);
				lblT.setText(null);
				lblConverter.setText(null);
				comboBoxCountry.setSelectedItem("Choose one Country");
				comboBoxDrink.setSelectedItem("Select a Drink");
			}
		});
		btnReset.setFont(new Font("Tahoma", Font.PLAIN, 25));
		btnReset.setBounds(410, 11, 118, 43);
		panel_4.add(btnReset);
		
		JButton btnExit = new JButton("Exit");
		btnExit.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				System.exit(0);
			}
		});
		btnExit.setFont(new Font("Tahoma", Font.PLAIN, 25));
		btnExit.setBounds(282, 11, 118, 43);
		panel_4.add(btnExit);
		
		JButton btnReceipt = new JButton("Receipt");
		btnReceipt.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				double tax11 = Double.parseDouble(labelTax.getText());
				double cost11 = Double.parseDouble(lblCost1.getText());
				double cost22 = Double.parseDouble(lblCost2.getText());
				double cost32 = Double.parseDouble(lblCost3.getText());
				//textR.set
				txtR.setText("Resturant Management System"
						+ "Chicken Burger: "+cost11
						+"Bacon and cheese burger:\t"+cost22
						+"Cost of drink "+cost32
						+"\nThankyou ");
				
				
			}
		});
		btnReceipt.setFont(new Font("Tahoma", Font.PLAIN, 25));
		btnReceipt.setBounds(154, 11, 118, 43);
		panel_4.add(btnReceipt);
		
		JPanel panel_5 = new JPanel();
		panel_5.setBorder(new LineBorder(Color.BLUE, 2));
		panel_5.setBounds(605, 81, 299, 457);
		frame.getContentPane().add(panel_5);
		panel_5.setLayout(null);
		
		JTabbedPane tabbedPane = new JTabbedPane(JTabbedPane.TOP);
		tabbedPane.setBounds(10, 26, 279, 376);
		panel_5.add(tabbedPane);
		
		JPanel panel_7 = new JPanel();
		tabbedPane.addTab("Calculator", null, panel_7, null);
		panel_7.setLayout(null);
		
		textField_3 = new JTextField();
		textField_3.setBounds(10, 11, 252, 36);
		panel_7.add(textField_3);
		textField_3.setColumns(10);
		
		JLabel lblNewLabel_3 = new JLabel("C");
		lblNewLabel_3.setBounds(20, 58, 53, 44);
		lblNewLabel_3.setBackground(UIManager.getColor("TextPane.selectionBackground"));
		lblNewLabel_3.setForeground(UIManager.getColor("TextPane.inactiveForeground"));
		lblNewLabel_3.setFont(new Font("Tahoma", Font.BOLD, 50));
		panel_7.add(lblNewLabel_3);
		
		JPanel panel_8 = new JPanel();
		tabbedPane.addTab("Reciept", null, panel_8, null);
		panel_8.setLayout(null);
		
		txtR = new JTextField();
		txtR.setBounds(10, 11, 254, 375);
		panel_8.add(txtR);
		txtR.setColumns(10);
		
		JPanel panel_6 = new JPanel();
		panel_6.setBounds(10, 26, 54, 26);
		panel_5.add(panel_6);
		
		JLabel lblNewLabel = new JLabel("Restaurent Management System");
		lblNewLabel.setFont(new Font("Tahoma", Font.BOLD, 45));
		lblNewLabel.setBounds(30, 11, 787, 44);
		frame.getContentPane().add(lblNewLabel);
		
	}
}
