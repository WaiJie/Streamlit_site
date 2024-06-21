import streamlit as st
st.set_page_config(layout="wide",initial_sidebar_state="collapsed")

st.header("Code repository")
st.write("This site showcases some of the code from the assignments/Projects highlighted in my portfolio website.")
st.write("These are mainly part of the assignments done for the Master of Analytics and Visualisation (MAVI) Programme")

st.page_link("pages/Forecasting-Temperatures-In-Singapore.py",label="R code for Forecasting temperatures in Singapore",icon = "🌡️")
st.page_link("pages/data-consolidation-transformation-with-python.py",label="Python Code for Data Consolidation & Transformation with Python",icon = "📄")
st.page_link("pages/Visualising-Singaporean-Attitudes-towards-the-US-and-China.py",label="R Code for Visualising Singaporean Attitudes towards the US and China",icon = "📊")



  