type MealType = 
    | Breakfast
    | Lunch
    | Dinner
    | Snack

type MealItem = {
    Name: string
    Calories: int
}

type Meal = {
    Date: System.DateTime
    MealType: MealType
    Items: MealItem list
    TotalCalories: int
}

type User = {
    Name: string
    Meals: Meal list
}

let parseMealItem (line: string) =
    let parts = line.Split(':')
    let name = parts.[0].Trim()
    let calories = parts.[1].Replace("kcal", "").Trim() |> int
    { Name = name; Calories = calories }

let parseMeal (dateStr: string) (mealType: MealType) (mealLines: string list) =
    let date = System.DateTime.Parse(dateStr)
    let mealItems = mealLines |> List.map parseMealItem
    let totalCalories = mealItems |> List.sumBy (fun item -> item.Calories)
    { Date = date; MealType = mealType; Items = mealItems; TotalCalories = totalCalories }

let parseUser (userName: string) (mealsData: (string * MealType * string list) list) =
    let meals = 
        mealsData 
        |> List.map (fun (dateStr, mealType, mealLines) -> parseMeal dateStr mealType mealLines)
    { Name = userName; Meals = meals }


let displayUserMeals (user: User) =
    printfn "User: %s" user.Name
    user.Meals
    |> List.iter (fun meal -> 
        printfn "Date: %O | Meal: %A | Total Calories: %d" meal.Date meal.MealType meal.TotalCalories
        meal.Items |> List.iter (fun item -> printfn "  - %s: %d kcal" item.Name item.Calories)
    )


