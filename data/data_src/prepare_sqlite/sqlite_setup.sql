DROP TABLE IF EXISTS RegularSeasonDetailedResults;
CREATE TABLE RegularSeasonDetailedResults AS
SELECT
  CAST(Season AS INTEGER) AS Season,
  CAST(Daynum AS INTEGER) AS Daynum,
  CAST(Wteam AS INTEGER) AS Wteam,
  CAST(Wscore AS INTEGER) AS Wscore,
  CAST(Lteam AS INTEGER) AS Lteam,
  CAST(Lscore AS INTEGER) AS Lscore,
  Wloc,
  CAST(Numot AS INTEGER) AS Numot,
  CAST(Wfgm AS INTEGER) AS Wfgm,
  CAST(Wfga AS INTEGER) AS Wfga,
  CAST(Wfgm3 AS INTEGER) AS Wfgm3,
  CAST(Wfga3 AS INTEGER) AS Wfga3,
  CAST(Wftm AS INTEGER) AS Wftm,
  CAST(Wfta AS INTEGER) AS Wfta,
  CAST(Wor AS INTEGER) AS Wor,
  CAST(Wdr AS INTEGER) AS Wdr,
  CAST(Wast AS INTEGER) AS Wast,
  CAST(Wto AS INTEGER) AS Wto,
  CAST(Wstl AS INTEGER) AS Wstl,
  CAST(Wblk AS INTEGER) AS Wblk,
  CAST(Wpf AS INTEGER) AS Wpf,
  CAST(Lfgm AS INTEGER) AS Lfgm,
  CAST(Lfga AS INTEGER) AS Lfga,
  CAST(Lfgm3 AS INTEGER) AS Lfgm3,
  CAST(Lfga3 AS INTEGER) AS Lfga3,
  CAST(Lftm AS INTEGER) AS Lftm,
  CAST(Lfta AS INTEGER) AS Lfta,
  CAST(Lor AS INTEGER) AS Lor,
  CAST(Ldr AS INTEGER) AS Ldr,
  CAST(Last AS INTEGER) AS Last,
  CAST(Lto AS INTEGER) AS Lto,
  CAST(Lstl AS INTEGER) AS Lstl,
  CAST(Lblk AS INTEGER) AS Lblk,
  CAST(Lpf AS INTEGER) AS Lpf
FROM
  RegularSeasonDetailedResultsFixedBetterRaw
;

DROP TABLE IF EXISTS TourneyDetailedResults;
CREATE TABLE TourneyDetailedResults AS
SELECT
  CAST(season AS INTEGER) AS Season,
  CAST(daynum AS INTEGER) AS Daynum,
  CAST(wteam AS INTEGER) AS Wteam,
  CAST(wscore AS INTEGER) AS Wscore,
  CAST(lteam AS INTEGER) AS Lteam,
  CAST(lscore AS INTEGER) AS Lscore,
  wloc AS Wloc,
  CAST(numot AS INTEGER) AS Numot,
  CAST(wfgm AS INTEGER) AS Wfgm,
  CAST(wfga AS INTEGER) AS Wfga,
  CAST(wfgm3 AS INTEGER) AS Wfgm3,
  CAST(wfga3 AS INTEGER) AS Wfga3,
  CAST(wftm AS INTEGER) AS Wftm,
  CAST(wfta AS INTEGER) AS Wfta,
  CAST(wor AS INTEGER) AS Wor,
  CAST(wdr AS INTEGER) AS Wdr,
  CAST(wast AS INTEGER) AS Wast,
  CAST(wto AS INTEGER) AS Wto,
  CAST(wstl AS INTEGER) AS Wstl,
  CAST(wblk AS INTEGER) AS Wblk,
  CAST(wpf AS INTEGER) AS Wpf,
  CAST(lfgm AS INTEGER) AS Lfgm,
  CAST(lfga AS INTEGER) AS Lfga,
  CAST(lfgm3 AS INTEGER) AS Lfgm3,
  CAST(lfga3 AS INTEGER) AS Lfga3,
  CAST(lftm AS INTEGER) AS Lftm,
  CAST(lfta AS INTEGER) AS Lfta,
  CAST(lor AS INTEGER) AS Lor,
  CAST(ldr AS INTEGER) AS Ldr,
  CAST(last AS INTEGER) AS Last,
  CAST(lto AS INTEGER) AS Lto,
  CAST(lstl AS INTEGER) AS Lstl,
  CAST(lblk AS INTEGER) AS Lblk,
  CAST(lpf AS INTEGER) AS Lpf
FROM
  TourneyDetailedResultsFixedBetterRaw
;

CREATE TABLE Revenue AS
SELECT
  COALESCE(s.Team_Name, r.name) AS Team_Name,
  Conference,
  CAST(Year AS int) AS year,
  CAST(Ticket_Sales AS int) AS ticket_sales,
  CAST(Contributions AS int) AS contributions,
  CAST(Rights AS int) AS rights,
  CAST(Student_Fees AS int) AS student_fees,
  CAST(School_Funds AS int) AS school_funds,
  CAST(Other AS int) AS other,
  CAST(Total_Revenue AS int) AS total_revenue
FROM
  RevenueRaw r
LEFT OUTER JOIN
  SchoolNameConv s
ON
  r.name = s.name_in_budget
;

CREATE TABLE Expense AS
SELECT
  COALESCE(s.Team_Name, e.name) AS Team_Name,
  Conference,
  CAST(Year AS int) AS year,
  CAST(Coaching AS int) AS coaching,
  CAST(Scholarships AS int) AS scholarships,
  CAST(Building AS int) AS building,
  CAST(Other AS int) AS others,
  CAST(Total_Expense AS int) AS total_expense
FROM
  ExpenseRaw e
LEFT OUTER JOIN
  SchoolNameConv s
ON
  e.name = s.name_in_budget
;

INSERT INTO MasseyOrdinalsRaw
SELECT
  *
FROM
  MasseyOrdinals2016Thru94Raw
;

CREATE TABLE TeamConferences AS
SELECT
  CAST(season AS int) AS season,
  CAST(team_id AS int) AS team_id,
  conference
FROM
  TeamConferencesRaw
;

CREATE TABLE TeamCoaches AS
SELECT
  CAST(season AS INTEGER) AS season,
  CAST(team_id AS INTEGER) AS team_id,
  CAST(first_day AS INTEGER) AS first_day,
  CAST(last_day AS INTEGER) AS last_day,
  coach_name
FROM
  TeamCoachesRaw
;

CREATE TABLE MasseyOrdinals AS
SELECT
  CAST(season AS INTEGER) AS season,
  CAST(rating_day_num AS INTEGER) AS rating_day_num,
  sys_name,
  CAST(team AS INTEGER) AS team,
  CAST(orank AS INTEGER) AS orank
FROM
  MasseyOrdinalsRaw
;

