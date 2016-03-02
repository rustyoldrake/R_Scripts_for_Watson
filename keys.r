# keys.R - EXAMPLE - you need to get your own from IBM BLUEMIX. SIgn up. Start service, Manage credentials. 
# used with the WINE Demo code (STT NLC TTS demo.R)
# YOUR PRIVATE FILE DO NOT SHARE (like I did here :)
# http://blog.revolutionanalytics.com/2015/11/how-to-store-and-use-authentication-details-with-r.html

# STT - Speech-To-Text credentials on Bluemix
username_stt <-"a530b62e-aaaa-406c-9cc3-XXXXXXXXX"
password_stt <- "XXXXXXXXXXX"  
username_password_stt = paste(username_stt,":",password_stt,sep="")

# NLC CREDENTIALS - DEMO - "NLC-COFFEE" Service on BLuemix
username_nlc = "6c45fa90-aaaa-4d2c-afb2-XXXXXXXXXX"
password_nlc = "XXXXXXXXXXXX"
username_password_nlc = paste(username_nlc,":",password_nlc)

# TEXT TO SPEECH - AUTHENTICATION AND CREDENTIALS 
username_TTS <-"54816193-aaaa-486e-98de-XXXXXXXXXX" 
password_TTS <- "XXXXXXXXXX"  
username_password_TTS = paste(username_TTS,":",password_TTS,sep="")

