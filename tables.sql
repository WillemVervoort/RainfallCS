USE testwillem

CREATE TABLE main_table (
station_ID VARCHAR(20) PRIMARY KEY,
Lat FLOAT(20,10),
Lon FLOAT(20,10),
data_type VARCHAR(10),
timestamp DATETIME,
start_date DATE,
end_date DATE,
comment VARCHAR(100)
);

CREATE TABLE regr_stats (
station_ID VARCHAR(20) PRIMARY KEY,
rmse FLOAT(20,10),
r_sq FLOAT(10,9),
p_value FLOAT(10,9),
bias FLOAT(10,9),
data_type VARCHAR(10),
comment VARCHAR(100),
FOREIGN KEY (station_ID) REFERENCES main_table(station_ID)
);

CREATE TABLE regr_results (
station_ID VARCHAR(20) PRIMARY KEY,
intercept FLOAT,
se_int  FLOAT,
p_value_int FLOAT,
slope FLOAT,
se_slope FLOAT,
p_value_slope FLOAT,
data_type VARCHAR(10),
comment VARCHAR(100),
FOREIGN KEY (station_ID) REFERENCES main_table(station_ID)
); 
