Feature: Highlighting
  
  Scenario: Highlighting greentext single-line
    When I insert:
    """
    <span class="quote">>Single line greentext</span>
    """
    When I go to beginning of buffer
    And I press "C-c C-r gl"
    Then I should see exactly:
    """
    >Single line greentext
    """

  Scenario: Highlighting greentext multi-line
    When I insert:
    """
    <span class="quote">>Multi line 
    green
    text</span>
    """
    When I go to beginning of buffer
    And I press "C-c C-r gl"
    Then I should see exactly:
    """
    >Multi line greentext
    """

  Scenario: Highlighting empty greentext
    When I insert:
    """
    <span class="quote">></span>
    """
    When I go to beginning of buffer
    And I press "C-c C-r gl"
    Then I should see exactly:
    """
    >
    """

  Scenario: Try highlighting non-eligible text
    When I insert:
    """
    Hello world! Nothing to see here.
    """
    When I go to beginning of buffer
    And I press "C-c C-r gl"
    Then I should see exactly:
    """
    Hello world! Nothing to see here.
    """

  Scenario: Highlight starting line with trailing
    When I insert:
    """
    <span class="quote">>please go OP</span>
    Long lines are long.
    """
    When I go to beginning of buffer
    And I press "C-c C-r gl"
    Then I should see exactly:
    """
    >please go OP
    Long lines are long.
    """

  Scenario: Highlight non-starting line
    When I insert:
    """
    Hello world!
    <span class="quote">>please go OP</span>
    Hi!
    """
    When I go to beginning of buffer
    And I press "C-c C-r gl"
    Then I should see exactly:
    """
    Hello world!
    >please go OP
    Hi!
    """

  Scenario: Multi-quote
    When I insert:
    """
    top
    <span class="quote">>please go OP</span>
    <span class="quote">>staying</span>
    Long lines are long.
    <span class="quote">>test test test</span>
    bottom
    """
    When I go to beginning of buffer
    And I press "C-c C-r gl"
    Then I should see exactly:
    """
    top
    >please go OP
    >staying
    Long lines are long.
    >test test test
    bottom
    """
