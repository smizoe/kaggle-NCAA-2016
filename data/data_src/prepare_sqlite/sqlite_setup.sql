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

