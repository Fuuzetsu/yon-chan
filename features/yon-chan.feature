Feature: Greentext
  
  Scenario: Highlighting greentext single-line
    When I insert:
    """
    <span class="quote">>what the fuck? 4 hours to charge?</span>
    """
    And I press "C-c C-r gr"
    Then I should see:
    """
    >what the fuck? 4 hours to charge?
    """
