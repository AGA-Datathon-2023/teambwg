library(echarts4r)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(shiny)
library(argonDash)
library(argonR)

states <- c(
  "ALABAMA",
  "ALASKA",
  "AMERICAN SAMOA",
  "ARIZONA",
  "ARKANSAS",
  "CALIFORNIA",
  "COLORADO",
  "CONNECTICUT",
  "DELAWARE",
  "DISTRICT OF COLUMBIA",
  "FLORIDA",
  "GEORGIA",
  "GUAM",
  "HAWAII",
  "IDAHO",
  "ILLINOIS",
  "INDIANA",
  "IOWA",
  "KANSAS",
  "KENTUCKY",
  "LOUISIANA",
  "MAINE",
  "MARYLAND",
  "MASSACHUSETTS",
  "MICHIGAN",
  "MINNESOTA",
  "MISSISSIPPI",
  "MISSOURI",
  "MONTANA",
  "NEBRASKA",
  "NEVADA",
  "NEW HAMPSHIRE",
  "NEW JERSEY",
  "NEW MEXICO",
  "NEW YORK",
  "NORTH CAROLINA",
  "NORTH DAKOTA",
  "NORTHERN MARIANA ISLANDS",
  "OHIO",
  "OKLAHOMA",
  "OREGON",
  "PENNSYLVANIA",
  "PUERTO RICO",
  "RHODE ISLAND",
  "SOUTH CAROLINA",
  "SOUTH DAKOTA",
  "TENNESSEE",
  "TEXAS",
  "UTAH",
  "VERMONT",
  "VIRGIN ISLANDS OF THE U.S.",
  "VIRGINIA",
  "WASHINGTON",
  "WEST VIRGINIA",
  "WISCONSIN",
  "WYOMING"
)

agencies <- c(
  "Agency for International Development",
  "Committee for Purchase from People Who Are Blind or Severely Disabled",
  "Commodity Futures Trading Commission",
  "Consumer Financial Protection Bureau",
  "Consumer Product Safety Commission",
  "Corporation for National and Community Service",
  "Council of the Inspectors General on Integrity and Efficiency",
  "Court Services and Offender Supervision Agency",
  "Defense Nuclear Facilities Safety Board",
  "Department of Agriculture",
  "Department of Commerce",
  "Department of Defense",
  "Department of Education",
  "Department of Energy",
  "Department of Health and Human Services",
  "Department of Homeland Security",
  "Department of Housing and Urban Development",
  "Department of Justice",
  "Department of Labor",
  "Department of State",
  "Department of the Interior",
  "Department of the Treasury",
  "Department of Transportation",
  "Department of Veterans Affairs",
  "District of Columbia Courts",
  "Environmental Protection Agency",
  "Equal Employment Opportunity Commission",
  "Executive Office of the President",
  "Export-Import Bank of the United States",
  "Federal Communications Commission",
  "Federal Maritime Commission",
  "Federal Trade Commission",
  "General Services Administration",
  "Government Accountability Office",
  "International Trade Commission",
  "Merit Systems Protection Board",
  "Millennium Challenge Corporation",
  "Morris K. Udall and Stewart L. Udall Foundation",
  "National Aeronautics and Space Administration",
  "National Archives and Records Administration",
  "National Gallery of Art",
  "National Labor Relations Board",
  "National Science Foundation",
  "National Transportation Safety Board",
  "Nuclear Regulatory Commission",
  "Office of Personnel Management",
  "Peace Corps",
  "Pension Benefit Guaranty Corporation",
  "Railroad Retirement Board",
  "Securities and Exchange Commission",
  "Small Business Administration",
  "Smithsonian Institution",
  "Social Security Administration",
  "U.S. Agency for Global Media",
  "U.S. International Development Finance Corporation",
  "United States Chemical Safety Board",
  "United States Trade and Development Agency"
)



check_cols <- c(
  "agency_short",
  "Asian Pacific American",
  "Black American",
  "Hispanic American",
  "Native American",
  "Subcontinent Asian Asian Indian American",
  "Veteran",
  "Woman",
  "No Designation"
)

