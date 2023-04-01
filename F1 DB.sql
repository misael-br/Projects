# Creating Tracks table
CREATE TABLE TRACKS (
TrackID INT NOT NULL,
TrackName VARCHAR(40),
TrackCountry VARCHAR(15),
TrackLength DECIMAL(4,3),
Altitude smallint,
TrackLoc VARCHAR(22));

# Creating Races Table
CREATE TABLE RACES (
RaceID INT NOT NULL,
RaceName VARCHAR(30),
TrackID INT NOT NULL,
Season SMALLINT,
PRIMARY KEY (RaceID),
    FOREIGN KEY (TrackID) REFERENCES TRACKS(TrackID));
    
#Creating Constructors Table
CREATE TABLE CONSTRUCTORS (
ConstructorID INT NOT NULL,
ConstructorName VARCHAR(40),
ConstructorNationality VARCHAR(25),
PRIMARY KEY (ConstructorID));

#Creating Drivers Table
CREATE TABLE DRIVERS (
DriverID INT NOT NULL,
DriverNum SMALLINT,
FName VARCHAR(80),
LName VARCHAR(80),
Champion SMALLINT,
DriverNationality VARCHAR(50),
DriverCode VARCHAR(4),
PRIMARY KEY (DriverID));

#Creating Results Table
CREATE TABLE RESULTS (
ResultID INT NOT NULL,
DriverID INT NOT NULL,
RaceID INT NOT NULL,
EndPosition SMALLINT,
PointsScored SMALLINT,
PRIMARY KEY (ResultID),
FOREIGN KEY (DriverID) REFERENCES DRIVERS(DriverID),
FOREIGN KEY (RaceID) REFERENCES RACES(RaceID));

#Creating Qualy Table
CREATE TABLE QUALIFYING (
QualifyID INT NOT NULL,
RaceID INT NOT NULL,
DriverID INT NOT NULL,
QPosition SMALLINT,
Q1 TIME,
Q2 TIME,
Q3 TIME,
PRIMARY KEY (QualifyID),
FOREIGN KEY (DriverID) REFERENCES DRIVERS(DriverID),
FOREIGN KEY (RaceID) REFERENCES RACES(RaceID));

# Creating LAPTIMES Table
CREATE TABLE LAPTIMES (
LapID INT NOT NULL,
RaceID INT,
DriverID INT,
LapNum SMALLINT,
LapTime TIME,
LapPosition SMALLINT,
PRIMARY KEY (LapID),
FOREIGN KEY (DriverID) REFERENCES DRIVERS(DriverID),
FOREIGN KEY (RaceID) REFERENCES RACES(RaceID));

#Creating CONTRACTS Table
CREATE TABLE CONTRACTS (
ContractID INT auto_increment NOT NULL,
ConstructorID INT,
DriverID INT,
Season SMALLINT,
PRIMARY KEY (ContractID),
FOREIGN KEY (ConstructorID) REFERENCES CONSTRUCTORS(ConstructorID),
FOREIGN KEY (DriverID) REFERENCES DRIVERS(DriverID));

#############  Queries  ###############
# 1 - Which Driver won the 2021 season?
SELECT Concat(FNAME, LNAME) AS FullName, sum(PointsScored) AS TotalPoints
FROM RESULTS AS R
JOIN DRIVERS AS D
ON R.DriverID = D.DriverID
JOIN RACES as RA
ON R.RaceID = RA.RaceID
WHERE RA.Season = 2021
GROUP BY FullName ORDER BY TotalPoints DESC LIMIT 1;
 

# 2 - What did the driver standings for the 2021 season look like after the last race?
SELECT Concat(FNAME, LNAME) AS FullName, sum(PointsScored) AS TotalPoints
FROM RESULTS AS R
JOIN DRIVERS AS D
ON R.DriverID = D.DriverID
JOIN RACES as RA
ON R.RaceID = RA.RaceID
WHERE RA.Season = 2021
GROUP BY FullName ORDER BY TotalPoints DESC;

# What about the 2022 season?
SELECT Concat(FNAME, LNAME) AS FullName, sum(PointsScored) AS TotalPoints
FROM RESULTS AS R
JOIN DRIVERS AS D
ON R.DriverID = D.DriverID
JOIN RACES as RA
ON R.RaceID = RA.RaceID
WHERE RA.Season = 2022
GROUP BY FullName ORDER BY TotalPoints DESC;

# 4 - What was the average finishing position of each driver in the 2021 season?
SELECT concat(FName, LName) AS FullName, avg(EndPosition) as AVG_Finish
FROM RESULTS AS R
JOIN DRIVERS AS D
ON R.DriverID = D.DriverID
JOIN RACES AS RA
ON R.RaceID = RA.RaceID
WHERE RA.Season = 2021
GROUP BY FullName ORDER BY AVG_Finish ASC;

#  5 - Lets pretend we are in the middle of the 2022 season. Let's create a view to keep track of
#		driver standings for that season. (Did not have full access to 2023 data or else this would have 
#		made more sense
CREATE VIEW 2022_STANDINGS AS SELECT Concat(FNAME, LNAME) AS FullName, sum(PointsScored) AS TotalPoints
FROM RESULTS AS R
JOIN DRIVERS AS D
ON R.DriverID = D.DriverID
JOIN RACES as RA
ON R.RaceID = RA.RaceID
WHERE RA.Season = 2022
GROUP BY FullName ORDER BY TotalPoints DESC;


# 6 - What did the constructors standings look like at the end of the 2021 season?
# The math wasn't checking out for this one so I created a view to join the query with, this worked.
CREATE VIEW 2021_STANDINGS AS SELECT Concat(FNAME, LNAME) AS FullName, sum(PointsScored) AS TotalPoints, D.DriverID
FROM RESULTS AS R
JOIN DRIVERS AS D
ON R.DriverID = D.DriverID
JOIN RACES as RA
ON R.RaceID = RA.RaceID
WHERE RA.Season = 2021
GROUP BY FullName ORDER BY TotalPoints DESC;

SELECT ConstructorName, sum(TotalPoints) AS TotalPoints
FROM CONSTRUCTORS AS C
JOIN CONTRACTS AS CC
ON C.ConstructorID = CC.ConstructorID
JOIN DRIVERS AS D
ON CC.DriverID = D.DriverID
JOIN 2021_STANDINGS AS S
ON D.DriverID = S.DriverID
WHERE CC.Season = 2021
GROUP BY ConstructorName ORDER BY TotalPoints DESC;

# 7 - Making a view out of the 2021 constructors standings
2021_STANDINGSCREATE VIEW 2021_CONSTRUCTORS AS SELECT ConstructorName, sum(TotalPoints) AS TotalPoints
FROM CONSTRUCTORS AS C
JOIN CONTRACTS AS CC
ON C.ConstructorID = CC.ConstructorID
JOIN DRIVERS AS D
ON CC.DriverID = D.DriverID
JOIN 2021_STANDINGS AS S
ON D.DriverID = S.DriverID
WHERE CC.Season = 2021
GROUP BY ConstructorName ORDER BY TotalPoints DESC;
