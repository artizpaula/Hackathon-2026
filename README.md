# FlyMood

## Inspiration
Our goal was to make travel planning more personal and intuitive by automatically recommending destinations based on a user’s mood and available time. We wanted to explore how AI and real-world data (like calendars) can be combined to create context-aware recommendations.

## What it does
FlyMood suggests travel destinations based on how you feel in the moment. Users complete a short emotional quiz and connect their calendar. The app then uses AI to recommend places that match both their mood and available time.

## How we built it
FlyMood is built as a Shiny web application. We integrated multiple APIs to:
- Access and analyze calendar availability  
- Retrieve travel and destination information  
- Generate AI-based recommendations  

The system combines emotional input with real scheduling data to produce personalized travel suggestions.

## Challenges we ran into
API integration was one of the most challenging parts of the project. Responses were sometimes inconsistent, which required extensive debugging, testing, and iterative improvements to ensure reliability.

## Accomplishments that we're proud of
- Integrated a personal calendar system  
- Automatically detected available time slots for travel  
- Built a functional end-to-end prototype within a hackathon timeframe  

## What we learned
- How APIs behave in real-world conditions  
- The importance of simplicity in Shiny applications for stability and usability  
- How to iterate quickly under time constraints while keeping a working product  

## What's next for FlyMood
We plan to introduce a travel history feature, allowing users to:
- Save past recommendations  
- Review previous trips  
- Improve and expand their preferences over time  

## Built With
- API.ai  
- CSS  
- JavaScript  
- RStudio  
- Shiny  
