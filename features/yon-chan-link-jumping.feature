Feature: Jumping using quote buttons

  Scenario: Attempting to jump within the same thread
    When I render "small-thread.json" as "/g/"
    When I go to beginning of buffer
    When I go to the front of the word ">>33503825"
    Then the cursor should be on line "24"
    And I press "<return>"
    Then the cursor should be on line "19"
