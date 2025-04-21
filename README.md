# 📊 Statistical Data Analysis Application

This is a web-based **Statistical Data Analysis Application** developed using **R Shiny**. It is designed to provide users with an intuitive interface for importing, visualizing, and analyzing data—both qualitative and quantitative—through descriptive statistics and normality tests.

## 🚀 Features

- 📂 **Data Import**: Upload data from `.csv` or `.xlsx` files, or input data manually.
- 📊 **Descriptive Statistics**: Automatically calculate min, max, quartiles, mean, median, standard deviation, and variance.
- 🧪 **Normality Tests**: Includes:
  - Shapiro-Wilk Test
  - Anderson-Darling Test
  - Kolmogorov-Smirnov Test
- 📈 **Data Visualization**: Integrated plots using `ggplot2`.
- 📋 **Interactive Tables**: View your data with the `DT` package.
- 🧠 **User-Friendly Interface**: Built with `shinydashboard` and enhanced with `shinyjs`.

## 🛠️ Technologies Used

- `shiny`
- `shinydashboard`
- `readxl`, `readr`
- `ggplot2`
- `dplyr`
- `DT`
- `nortest`
- `shinyjs`

## 🧱 Application Architecture

This app follows the **Model-View-Controller (MVC)** design pattern:

- **Model**: Handles data loading and statistical calculations.
- **View**: Built with `shinydashboard` for a clean, navigable UI.
- **Controller**: The server logic responds to user inputs and triggers analysis.

## 📷 Screenshots

![distribution graphic](https://github.com/robert1357/normalidad/blob/main/1.png?raw=true)
![distribution graphic](https://github.com/robert1357/normalidad/blob/main/2.png?raw=true)
![distribution graphic](https://github.com/robert1357/normalidad/blob/main/3.png?raw=true)
![distribution graphic](https://github.com/robert1357/normalidad/blob/main/image.png?raw=true)
## 📦 Installation

To run the app locally, follow these steps:

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/statistical-analysis-app.git
   cd statistical-analysis-app
