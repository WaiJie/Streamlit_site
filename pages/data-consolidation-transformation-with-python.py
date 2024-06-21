import streamlit as st
st.set_page_config(layout="wide",initial_sidebar_state="collapsed")


python_code = """# Import required modules
import pandas as pd
import os 
import pymysql
from sqlalchemy import create_engine
from datetime import datetime

# Parameters for the sqlalchemy engine
# Using f-strings to input the username, password and database parameters in db_uri
mysqlid = 'root'
mysqlpw = 'xxxxxxxx'
mysqldb = 'xxxxxxxx'
db_uri = f"mysql+pymysql://{mysqlid}:{mysqlpw}@localhost:3306/{mysqldb}"

# initialise the sqlalchemy engine
engine = create_engine(db_uri)

# Get the filepath of current working directory
path = os.getcwd() 

# Assuming the raw folder is in the current working directory as well
# Define the filepath of the 'raw' folder
data_path = os.path.join(path, 'raw')

# Get a list of files in the 'raw' folder
file_list = os.listdir(data_path)

# list containing all possible sheetnames of interest
# If the sheetnames changes, adding to this list will include them in the data extraction
target_sheetnames = ['inpatient', 'warded', 'ip', 'in','inp' ]

# User written function to figure out the name of the sheet to extract from
# Check if the standardised sheet names are present in the list of sheet names of interest
# Then return the original sheet name given in the excel worksheet
def get_sheetname(excel_sheetnames,target_sheetnames):
    for sheet_name in excel_sheetnames:
        # Standardise sheet names to lower case to match our target sheet names
        standardised_sheet_name = sheet_name.lower()
        # Remove remove leading/trailing blanks
        standardised_sheet_name = standardised_sheet_name.strip()

        # If sheet name in excel matches the list of sheet names of interest
        # Then return that original sheet name 
        if standardised_sheet_name in target_sheetnames:
            return sheet_name

# User written function to convert the abbreviated month name to month number
# E.g. Jan will be 01 , Feb will be 02
def month_to_number(excel_name):
    # Split the excel name string by the '.' delimiter
    # String will be split by the '.' delimiter to output a list
    # The first part is the abbreviated month which is indexed by [0]. E.g. jan
    # The second part is 'xlsx' and is not required. 
    month = excel_name.split('.')[0]

    # Convert month to datetime object using datetime.strptime
    # The %b format represents abbreviated month names such as Jan, Feb
    month= datetime.strptime(month, "%b")
    
    # Convert datetime object to month number with string datatype
    # The %m format represents month as a zero padded number, such as 01,02
    month_num = datetime.strftime(month, "%m")
    
    return month_num

# Main loop to extract data from excel files and load into MySQL database.
for file in file_list:
    # String manipulation to create filepath of the excel files
    # Excel file name is concatenated with the folder directory 
    excel_path = data_path + '\\' + file
    
    # Replace all backslashes with forward slashes as backslash is an escape character in Windows
    # Only 1 forward slash is needed and it is more readable
    excel_path = excel_path.replace('\\','/')

    # Read excel file metadata such as sheet names 
    excel_file = pd.ExcelFile(excel_path)
    
    # Extract names of all sheets in the current excel file
    # Save the sheet names from the excel file in a list
    sheet_names_excel = excel_file.sheet_names

    # Each excel file in this loop has a different sheet to extract data from
    # Use user written function to figure out the name of the sheet to extract from
    # by checking if the standardised sheet names are present in the list of sheet names of interest
    # output in sheetname variable will only be one sheet which matches the target sheetnames 
    sheetname = get_sheetname(sheet_names_excel,target_sheetnames)
    
    # Print out the name of the sheet to be extracted
    print(sheetname)

    # Use pandas to read the excel file with sheetname into a pandas dataframe
    df = pd.read_excel(excel_path, sheet_name = sheetname) 
    
    # Use user written function to convert the abbreviated month name to month number
    month_number = month_to_number(file)

    # Load dataframe into MySQL database using .to_sql and specify con = engine
    # The month number is used as the table name to indicate which month data belongs to
    # This is done by using f-strings to insert the month number behind the word "in"
    df.to_sql(name=f"in{month_number}",con=engine,if_exists='replace')
    
    # Print out confirmation that tables have been created
    print(f"Table in{month_number} created") 
    
    """


st.subheader("Python Code for Data Consolidation & Transformation with Python")
st.code(python_code,"python")