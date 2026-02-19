<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# ADR-005: Enterprise Integration Patterns for Audit System

## Status
Accepted

## Context
The copybook-rs Enterprise Audit System must seamlessly integrate with diverse enterprise infrastructure including SIEM systems, monitoring platforms, log aggregation services, and custom enterprise applications. Each integration target has different protocols, data formats, authentication mechanisms, and performance characteristics that must be handled efficiently while maintaining audit system performance.

### Enterprise Integration Requirements

#### SIEM Integration
- **Splunk**: HTTP Event Collector (HEC) with JSON payload
- **IBM QRadar**: REST API with custom event format
- **Microsoft Sentinel**: Log Analytics HTTP Data Collector API
- **Elastic Security**: Elasticsearch index API with ECS format
- **Generic SIEM**: Common Event Format (CEF) over Syslog

#### Monitoring Platforms
- **Prometheus**: Pull-based metrics with HTTP endpoint
- **Datadog**: Push-based metrics with API
- **New Relic**: Custom events and metrics API
- **Grafana**: Dashboard integration via Prometheus/Elasticsearch
- **Custom monitoring**: Webhook and REST API integration

#### Log Aggregation
- **Elasticsearch**: Direct indexing with bulk API
- **Fluentd**: Log forwarding with structured data
- **Logstash**: JSON log processing pipeline
- **AWS CloudWatch**: Log streams with structured events
- **Azure Monitor**: Log Analytics workspace integration

#### Enterprise Systems
- **Active Directory**: User authentication and authorization
- **ServiceNow**: Incident creation and workflow integration
- **Slack/Teams**: Real-time alerting and notifications
- **PagerDuty**: Incident response and escalation
- **Custom APIs**: REST, GraphQL, and message queue integration

### Technical Constraints
- Must support multiple protocols simultaneously (HTTP, TCP, UDP, WebSocket)
- Authentication must integrate with enterprise identity systems
- Performance impact <3% for integration operations
- Support for batching and real-time streaming
- Circuit breaker and retry logic for reliability
- Configurable rate limiting and backpressure handling

### Security Requirements
- TLS encryption for all network communications
- Support for client certificates and mutual TLS
- API key rotation and secure credential management
- Audit trail for all integration activities
- Data classification aware routing and filtering

## Decision
We will implement a universal integration framework using protocol adapters, pluggable transporters, and enterprise-grade reliability patterns with comprehensive configuration management.

### 1. Universal Integration Architecture
```rust
pub struct IntegrationManager {
    adapters: HashMap<String, Box<dyn IntegrationAdapter>>,
    transporters: HashMap<TransportType, Box<dyn Transporter>>,
    formatters: HashMap<EventFormat, Box<dyn EventFormatter>>,
    router: EventRouter,
    reliability_manager: ReliabilityManager,
}
```

### 2. Protocol Adapter Pattern
Each integration type implements a common adapter interface:
```rust
#[async_trait]
pub trait IntegrationAdapter {
    async fn send_event(&self, event: &FormattedEvent) -> Result<DeliveryResult, IntegrationError>;
    async fn send_batch(&self, events: Vec<&FormattedEvent>) -> Result<BatchDeliveryResult, IntegrationError>;
    async fn test_connection(&self) -> Result<bool, IntegrationError>;
    fn get_adapter_info(&self) -> AdapterInfo;
    async fn get_metrics(&self) -> AdapterMetrics;
}
```

### 3. Pluggable Transport System
Transport mechanisms separated from business logic:
- HTTP/HTTPS transporter with connection pooling
- TCP/UDP transporter with socket management
- WebSocket transporter with reconnection logic
- Message queue transporter (AMQP, Kafka)
- File system transporter for local integration

### 4. Event Formatting Pipeline
Configurable event transformation for different target systems:
- JSON formatter with customizable schema
- CEF (Common Event Format) for SIEM integration
- ECS (Elastic Common Schema) for Elasticsearch
- Prometheus metrics format for monitoring
- Custom formatters for specific enterprise requirements

### 5. Enterprise Reliability Patterns
Production-grade reliability with enterprise SLA support:
- Circuit breaker pattern for fault tolerance
- Exponential backoff retry logic
- Dead letter queue for failed events
- Health checks and monitoring integration
- Performance metrics and alerting

## Implementation Architecture

### Core Integration Manager
```rust
#[derive(Debug)]
pub struct IntegrationManager {
    adapters: HashMap<String, Box<dyn IntegrationAdapter>>,
    transporters: HashMap<TransportType, Box<dyn Transporter>>,
    formatters: HashMap<EventFormat, Box<dyn EventFormatter>>,
    router: EventRouter,
    reliability_manager: ReliabilityManager,
    metrics: Arc<Mutex<IntegrationMetrics>>,
    config: IntegrationConfig,
}

impl IntegrationManager {
    pub async fn new(config: IntegrationConfig) -> Result<Self, IntegrationError> {
        let mut adapters: HashMap<String, Box<dyn IntegrationAdapter>> = HashMap::new();
        let mut transporters: HashMap<TransportType, Box<dyn Transporter>> = HashMap::new();
        let mut formatters: HashMap<EventFormat, Box<dyn EventFormatter>> = HashMap::new();

        // Initialize transport layer
        if config.http_enabled {
            transporters.insert(
                TransportType::Http,
                Box::new(HttpTransporter::new(&config.http_config)?)
            );
        }

        if config.tcp_enabled {
            transporters.insert(
                TransportType::Tcp,
                Box::new(TcpTransporter::new(&config.tcp_config)?)
            );
        }

        // Initialize formatters
        formatters.insert(EventFormat::Json, Box::new(JsonFormatter::new()));
        formatters.insert(EventFormat::Cef, Box::new(CefFormatter::new()));
        formatters.insert(EventFormat::Ecs, Box::new(EcsFormatter::new()));

        // Initialize adapters based on configuration
        if config.splunk_enabled {
            adapters.insert(
                "splunk".to_string(),
                Box::new(SplunkAdapter::new(
                    &config.splunk_config,
                    transporters.get(&TransportType::Http).unwrap()
                )?)
            );
        }

        // Initialize routing and reliability
        let router = EventRouter::new(&config.routing_config)?;
        let reliability_manager = ReliabilityManager::new(&config.reliability_config)?;

        Ok(Self {
            adapters,
            transporters,
            formatters,
            router,
            reliability_manager,
            metrics: Arc::new(Mutex::new(IntegrationMetrics::default())),
            config,
        })
    }

    pub async fn send_audit_event(&self, event: &AuditEvent) -> Result<IntegrationResult, IntegrationError> {
        let mut delivery_results = Vec::new();

        // Determine target integrations based on routing rules
        let targets = self.router.route_event(event);

        // Process each target integration
        for target in targets {
            let delivery_result = self.send_to_target(event, &target).await;
            delivery_results.push(delivery_result);
        }

        // Aggregate results
        let successful_deliveries = delivery_results.iter()
            .filter(|r| r.is_ok())
            .count();

        let failed_deliveries = delivery_results.len() - successful_deliveries;

        Ok(IntegrationResult {
            total_targets: delivery_results.len(),
            successful_deliveries,
            failed_deliveries,
            delivery_results,
            overall_success: failed_deliveries == 0,
            processing_timestamp: SystemTime::now(),
        })
    }

    async fn send_to_target(
        &self,
        event: &AuditEvent,
        target: &IntegrationTarget,
    ) -> Result<TargetDeliveryResult, IntegrationError> {
        let start_time = Instant::now();

        // Apply event filtering
        if !self.should_send_to_target(event, target) {
            return Ok(TargetDeliveryResult {
                target_name: target.name.clone(),
                success: true,
                delivery_time_ms: 0,
                filtered: true,
                error: None,
            });
        }

        // Get appropriate formatter
        let formatter = self.formatters.get(&target.format)
            .ok_or_else(|| IntegrationError::FormatterNotFound {
                format: target.format.clone(),
            })?;

        // Format event for target
        let formatted_event = formatter.format_event(event)?;

        // Get adapter and send
        let adapter = self.adapters.get(&target.adapter_name)
            .ok_or_else(|| IntegrationError::AdapterNotFound {
                adapter: target.adapter_name.clone(),
            })?;

        // Apply reliability patterns
        let delivery_result = self.reliability_manager
            .execute_with_reliability(|| {
                adapter.send_event(&formatted_event)
            })
            .await?;

        let delivery_time = start_time.elapsed();

        // Update metrics
        self.update_delivery_metrics(&target.name, delivery_result.success, delivery_time).await;

        Ok(TargetDeliveryResult {
            target_name: target.name.clone(),
            success: delivery_result.success,
            delivery_time_ms: delivery_time.as_millis() as u64,
            filtered: false,
            error: delivery_result.error,
        })
    }
}
```

### Splunk Integration Adapter
```rust
#[derive(Debug)]
pub struct SplunkAdapter {
    hec_client: SplunkHecClient,
    config: SplunkConfig,
    transporter: Arc<dyn Transporter>,
    metrics: SplunkMetrics,
}

impl SplunkAdapter {
    pub fn new(
        config: &SplunkConfig,
        transporter: &Arc<dyn Transporter>,
    ) -> Result<Self, IntegrationError> {
        let hec_client = SplunkHecClient::new(
            &config.hec_endpoint,
            &config.hec_token,
        )?;

        Ok(Self {
            hec_client,
            config: config.clone(),
            transporter: transporter.clone(),
            metrics: SplunkMetrics::default(),
        })
    }
}

#[async_trait]
impl IntegrationAdapter for SplunkAdapter {
    async fn send_event(&self, event: &FormattedEvent) -> Result<DeliveryResult, IntegrationError> {
        let splunk_event = SplunkHecEvent {
            time: event.timestamp.timestamp(),
            source: "copybook-rs".to_string(),
            sourcetype: "copybook:audit".to_string(),
            index: self.config.index.clone(),
            event: event.data.clone(),
        };

        let payload = serde_json::to_vec(&splunk_event)
            .map_err(|e| IntegrationError::SerializationError { error: e.to_string() })?;

        let transport_request = TransportRequest {
            method: HttpMethod::Post,
            url: format!("{}/services/collector", self.config.hec_endpoint),
            headers: vec![
                ("Authorization".to_string(), format!("Splunk {}", self.config.hec_token)),
                ("Content-Type".to_string(), "application/json".to_string()),
            ],
            body: payload,
            timeout: Duration::from_secs(30),
        };

        let response = self.transporter.send_request(transport_request).await?;

        let success = response.status_code >= 200 && response.status_code < 300;

        Ok(DeliveryResult {
            success,
            status_code: Some(response.status_code),
            response_time_ms: response.response_time_ms,
            bytes_sent: response.bytes_sent,
            error: if success { None } else { Some(response.error_message) },
        })
    }

    async fn send_batch(&self, events: Vec<&FormattedEvent>) -> Result<BatchDeliveryResult, IntegrationError> {
        let mut batch_payload = Vec::new();

        for event in events {
            let splunk_event = SplunkHecEvent {
                time: event.timestamp.timestamp(),
                source: "copybook-rs".to_string(),
                sourcetype: "copybook:audit".to_string(),
                index: self.config.index.clone(),
                event: event.data.clone(),
            };

            let event_json = serde_json::to_string(&splunk_event)
                .map_err(|e| IntegrationError::SerializationError { error: e.to_string() })?;

            batch_payload.push(event_json);
        }

        let batch_body = batch_payload.join("\n");

        let transport_request = TransportRequest {
            method: HttpMethod::Post,
            url: format!("{}/services/collector", self.config.hec_endpoint),
            headers: vec![
                ("Authorization".to_string(), format!("Splunk {}", self.config.hec_token)),
                ("Content-Type".to_string(), "application/json".to_string()),
            ],
            body: batch_body.into_bytes(),
            timeout: Duration::from_secs(60),
        };

        let response = self.transporter.send_request(transport_request).await?;

        let success = response.status_code >= 200 && response.status_code < 300;

        Ok(BatchDeliveryResult {
            total_events: events.len(),
            successful_events: if success { events.len() } else { 0 },
            failed_events: if success { 0 } else { events.len() },
            success,
            response_time_ms: response.response_time_ms,
            error: if success { None } else { Some(response.error_message) },
        })
    }

    async fn test_connection(&self) -> Result<bool, IntegrationError> {
        let test_event = SplunkHecEvent {
            time: SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs() as i64,
            source: "copybook-rs-test".to_string(),
            sourcetype: "test".to_string(),
            index: self.config.index.clone(),
            event: serde_json::json!({"message": "Connection test", "test": true}),
        };

        let formatted_event = FormattedEvent {
            format: EventFormat::Json,
            data: serde_json::to_value(&test_event).unwrap(),
            timestamp: chrono::Utc::now().into(),
            size_bytes: 0,
        };

        match self.send_event(&formatted_event).await {
            Ok(result) => Ok(result.success),
            Err(_) => Ok(false),
        }
    }

    fn get_adapter_info(&self) -> AdapterInfo {
        AdapterInfo {
            name: "Splunk HEC".to_string(),
            version: "1.0".to_string(),
            transport_type: TransportType::Http,
            supported_formats: vec![EventFormat::Json],
            batch_support: true,
            real_time_support: true,
        }
    }

    async fn get_metrics(&self) -> AdapterMetrics {
        AdapterMetrics {
            total_events_sent: self.metrics.total_events_sent,
            successful_deliveries: self.metrics.successful_deliveries,
            failed_deliveries: self.metrics.failed_deliveries,
            average_response_time_ms: self.metrics.average_response_time_ms,
            bytes_sent: self.metrics.total_bytes_sent,
            last_successful_delivery: self.metrics.last_successful_delivery,
            connection_status: self.test_connection().await.unwrap_or(false),
        }
    }
}
```

### HTTP Transport Layer
```rust
#[derive(Debug)]
pub struct HttpTransporter {
    client: reqwest::Client,
    connection_pool: ConnectionPool,
    config: HttpTransportConfig,
    metrics: Arc<Mutex<HttpTransportMetrics>>,
}

impl HttpTransporter {
    pub fn new(config: &HttpTransportConfig) -> Result<Self, IntegrationError> {
        let client = reqwest::Client::builder()
            .timeout(Duration::from_secs(config.timeout_seconds))
            .pool_max_idle_per_host(config.max_idle_connections)
            .pool_idle_timeout(Duration::from_secs(config.idle_timeout_seconds))
            .tcp_keepalive(Duration::from_secs(config.keepalive_seconds))
            .build()
            .map_err(|e| IntegrationError::TransportError { error: e.to_string() })?;

        Ok(Self {
            client,
            connection_pool: ConnectionPool::new(config.connection_pool_config.clone()),
            config: config.clone(),
            metrics: Arc::new(Mutex::new(HttpTransportMetrics::default())),
        })
    }
}

#[async_trait]
impl Transporter for HttpTransporter {
    async fn send_request(&self, request: TransportRequest) -> Result<TransportResponse, IntegrationError> {
        let start_time = Instant::now();

        let mut req_builder = match request.method {
            HttpMethod::Get => self.client.get(&request.url),
            HttpMethod::Post => self.client.post(&request.url),
            HttpMethod::Put => self.client.put(&request.url),
            HttpMethod::Delete => self.client.delete(&request.url),
        };

        // Add headers
        for (key, value) in request.headers {
            req_builder = req_builder.header(&key, &value);
        }

        // Add body if present
        if !request.body.is_empty() {
            req_builder = req_builder.body(request.body.clone());
        }

        // Set timeout
        req_builder = req_builder.timeout(request.timeout);

        // Execute request
        let response = req_builder.send().await
            .map_err(|e| IntegrationError::NetworkError { error: e.to_string() })?;

        let status_code = response.status().as_u16();
        let response_time = start_time.elapsed();

        // Read response body
        let response_body = response.text().await
            .unwrap_or_else(|_| "Failed to read response body".to_string());

        // Update metrics
        {
            let mut metrics = self.metrics.lock().await;
            metrics.total_requests += 1;
            metrics.total_response_time += response_time;
            metrics.bytes_sent += request.body.len() as u64;

            if status_code >= 200 && status_code < 300 {
                metrics.successful_requests += 1;
            } else {
                metrics.failed_requests += 1;
            }
        }

        Ok(TransportResponse {
            status_code,
            response_time_ms: response_time.as_millis() as u64,
            bytes_sent: request.body.len(),
            response_body,
            error_message: if status_code >= 400 {
                Some(format!("HTTP {} error", status_code))
            } else {
                None
            },
        })
    }

    async fn test_connectivity(&self, endpoint: &str) -> Result<bool, IntegrationError> {
        let test_request = TransportRequest {
            method: HttpMethod::Get,
            url: endpoint.to_string(),
            headers: vec![],
            body: vec![],
            timeout: Duration::from_secs(10),
        };

        match self.send_request(test_request).await {
            Ok(response) => Ok(response.status_code < 500),
            Err(_) => Ok(false),
        }
    }

    fn get_transport_info(&self) -> TransportInfo {
        TransportInfo {
            transport_type: TransportType::Http,
            supports_batching: true,
            supports_streaming: false,
            max_payload_size: self.config.max_payload_size,
            connection_pool_size: self.config.max_idle_connections,
        }
    }
}
```

### Event Routing System
```rust
#[derive(Debug)]
pub struct EventRouter {
    routing_rules: Vec<RoutingRule>,
    default_targets: Vec<IntegrationTarget>,
    rule_cache: LruCache<String, Vec<IntegrationTarget>>,
}

impl EventRouter {
    pub fn route_event(&self, event: &AuditEvent) -> Vec<IntegrationTarget> {
        let cache_key = self.generate_cache_key(event);

        if let Some(cached_targets) = self.rule_cache.get(&cache_key) {
            return cached_targets.clone();
        }

        let mut targets = Vec::new();

        // Apply routing rules in order
        for rule in &self.routing_rules {
            if rule.matches(event) {
                targets.extend(rule.targets.clone());

                // If rule is exclusive, stop processing
                if rule.exclusive {
                    break;
                }
            }
        }

        // Use default targets if no rules matched
        if targets.is_empty() {
            targets = self.default_targets.clone();
        }

        // Cache result
        self.rule_cache.put(cache_key, targets.clone());

        targets
    }

    fn generate_cache_key(&self, event: &AuditEvent) -> String {
        format!(
            "{}:{}:{}:{}",
            event.event_type,
            event.severity,
            event.context.security.classification,
            event.context.compliance_profiles.len()
        )
    }
}

#[derive(Debug, Clone)]
pub struct RoutingRule {
    pub rule_id: String,
    pub name: String,
    pub condition: RoutingCondition,
    pub targets: Vec<IntegrationTarget>,
    pub exclusive: bool,
    pub priority: u32,
}

#[derive(Debug, Clone)]
pub enum RoutingCondition {
    And(Vec<RoutingCondition>),
    Or(Vec<RoutingCondition>),
    Not(Box<RoutingCondition>),
    EventType(AuditEventType),
    Severity(EventSeverity),
    SecurityClassification(SecurityClassification),
    ComplianceProfile(ComplianceProfile),
    UserRole(String),
    MetadataContains { key: String, value: String },
    Always,
}

impl RoutingRule {
    pub fn matches(&self, event: &AuditEvent) -> bool {
        self.condition.evaluate(event)
    }
}

impl RoutingCondition {
    pub fn evaluate(&self, event: &AuditEvent) -> bool {
        match self {
            RoutingCondition::And(conditions) => {
                conditions.iter().all(|c| c.evaluate(event))
            }
            RoutingCondition::Or(conditions) => {
                conditions.iter().any(|c| c.evaluate(event))
            }
            RoutingCondition::Not(condition) => {
                !condition.evaluate(event)
            }
            RoutingCondition::EventType(event_type) => {
                event.event_type == *event_type
            }
            RoutingCondition::Severity(severity) => {
                event.severity == *severity
            }
            RoutingCondition::SecurityClassification(classification) => {
                event.context.security.classification == *classification
            }
            RoutingCondition::ComplianceProfile(profile) => {
                event.context.compliance_profiles.contains(profile)
            }
            RoutingCondition::UserRole(role) => {
                // Implementation would check user roles from context
                event.context.metadata.get("user_role")
                    .map_or(false, |user_role| user_role == role)
            }
            RoutingCondition::MetadataContains { key, value } => {
                event.context.metadata.get(key)
                    .map_or(false, |metadata_value| metadata_value.contains(value))
            }
            RoutingCondition::Always => true,
        }
    }
}
```

### Reliability Management
```rust
#[derive(Debug)]
pub struct ReliabilityManager {
    circuit_breaker: CircuitBreakerManager,
    retry_policy: RetryPolicy,
    dead_letter_queue: DeadLetterQueue,
    health_checker: HealthChecker,
}

impl ReliabilityManager {
    pub async fn execute_with_reliability<F, Fut, T>(
        &self,
        operation: F,
    ) -> Result<T, IntegrationError>
    where
        F: Fn() -> Fut,
        Fut: Future<Output = Result<T, IntegrationError>>,
    {
        let operation_id = generate_operation_id();

        // Check circuit breaker state
        if !self.circuit_breaker.is_call_allowed(&operation_id) {
            return Err(IntegrationError::CircuitBreakerOpen {
                service: operation_id.clone(),
            });
        }

        // Execute with retry policy
        let mut last_error = None;

        for attempt in 1..=self.retry_policy.max_attempts {
            match operation().await {
                Ok(result) => {
                    // Record success in circuit breaker
                    self.circuit_breaker.record_success(&operation_id);
                    return Ok(result);
                }
                Err(e) => {
                    last_error = Some(e);

                    // Record failure in circuit breaker
                    self.circuit_breaker.record_failure(&operation_id);

                    // Check if we should retry
                    if attempt < self.retry_policy.max_attempts {
                        let delay = self.calculate_retry_delay(attempt);
                        tokio::time::sleep(delay).await;
                        continue;
                    }
                }
            }
        }

        // All retries exhausted, send to dead letter queue if configured
        if let Some(ref dlq) = self.dead_letter_queue {
            if let Some(error) = &last_error {
                dlq.send_to_dead_letter_queue(&operation_id, error).await;
            }
        }

        Err(last_error.unwrap())
    }

    fn calculate_retry_delay(&self, attempt: u32) -> Duration {
        let base_delay = self.retry_policy.initial_delay_ms;
        let exponential_delay = base_delay * (2_u64.pow(attempt - 1));
        let capped_delay = exponential_delay.min(self.retry_policy.max_delay_ms);

        // Add jitter to prevent thundering herd
        let jitter = rand::random::<f64>() * 0.1; // 10% jitter
        let final_delay = (capped_delay as f64 * (1.0 + jitter)) as u64;

        Duration::from_millis(final_delay)
    }
}

#[derive(Debug)]
pub struct CircuitBreakerManager {
    breakers: HashMap<String, CircuitBreaker>,
    config: CircuitBreakerConfig,
}

#[derive(Debug)]
pub struct CircuitBreaker {
    state: CircuitBreakerState,
    failure_count: u32,
    last_failure_time: Option<Instant>,
    success_count: u32,
    config: CircuitBreakerConfig,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CircuitBreakerState {
    Closed,
    Open,
    HalfOpen,
}

impl CircuitBreakerManager {
    pub fn is_call_allowed(&self, service: &str) -> bool {
        if let Some(breaker) = self.breakers.get(service) {
            breaker.is_call_allowed()
        } else {
            true // Allow calls if no breaker exists
        }
    }

    pub fn record_success(&mut self, service: &str) {
        self.breakers
            .entry(service.to_string())
            .or_insert_with(|| CircuitBreaker::new(self.config.clone()))
            .record_success();
    }

    pub fn record_failure(&mut self, service: &str) {
        self.breakers
            .entry(service.to_string())
            .or_insert_with(|| CircuitBreaker::new(self.config.clone()))
            .record_failure();
    }
}

impl CircuitBreaker {
    pub fn is_call_allowed(&self) -> bool {
        match self.state {
            CircuitBreakerState::Closed => true,
            CircuitBreakerState::Open => {
                // Check if timeout period has elapsed
                if let Some(last_failure) = self.last_failure_time {
                    last_failure.elapsed() > self.config.timeout_duration
                } else {
                    false
                }
            }
            CircuitBreakerState::HalfOpen => {
                // Allow limited calls to test if service has recovered
                self.success_count < self.config.half_open_max_calls
            }
        }
    }

    pub fn record_success(&mut self) {
        match self.state {
            CircuitBreakerState::Closed => {
                // Reset failure count on success
                self.failure_count = 0;
            }
            CircuitBreakerState::HalfOpen => {
                self.success_count += 1;
                if self.success_count >= self.config.half_open_max_calls {
                    // Enough successes, close the circuit
                    self.state = CircuitBreakerState::Closed;
                    self.failure_count = 0;
                    self.success_count = 0;
                }
            }
            CircuitBreakerState::Open => {
                // Transition to half-open on first success after timeout
                self.state = CircuitBreakerState::HalfOpen;
                self.success_count = 1;
                self.failure_count = 0;
            }
        }
    }

    pub fn record_failure(&mut self) {
        self.failure_count += 1;
        self.last_failure_time = Some(Instant::now());

        match self.state {
            CircuitBreakerState::Closed => {
                if self.failure_count >= self.config.failure_threshold {
                    self.state = CircuitBreakerState::Open;
                }
            }
            CircuitBreakerState::HalfOpen => {
                // Any failure in half-open state trips the breaker
                self.state = CircuitBreakerState::Open;
                self.success_count = 0;
            }
            CircuitBreakerState::Open => {
                // Already open, just update failure count
            }
        }
    }
}
```

## Configuration Management

### Comprehensive Integration Configuration
```yaml
# Enterprise integration configuration
enterprise_integration:
  # Global settings
  enabled: true
  max_concurrent_integrations: 50
  default_timeout_seconds: 30
  health_check_interval_seconds: 300

  # Transport layer configuration
  transports:
    http:
      enabled: true
      max_connections_per_host: 10
      timeout_seconds: 30
      keepalive_seconds: 300
      idle_timeout_seconds: 60
      max_payload_size_mb: 100

    tcp:
      enabled: true
      connection_timeout_seconds: 10
      read_timeout_seconds: 30
      write_timeout_seconds: 30
      buffer_size_kb: 64

    websocket:
      enabled: false
      reconnect_interval_seconds: 30
      max_reconnect_attempts: 5

  # Event formatting
  formatters:
    json:
      enabled: true
      pretty_print: false
      include_metadata: true

    cef:
      enabled: true
      version: 0
      device_vendor: "Copybook-rs"
      device_product: "Enterprise Audit System"

    ecs:
      enabled: true
      version: "8.0"
      include_host_metadata: true

  # Integration adapters
  adapters:
    splunk:
      enabled: true
      adapter_type: "http"
      endpoint: "${SPLUNK_HEC_ENDPOINT}"
      token: "${SPLUNK_HEC_TOKEN}"
      index: "copybook_audit"
      source: "copybook-rs"
      sourcetype: "copybook:audit"
      batch_size: 100
      batch_timeout_seconds: 30
      ssl_verify: true

    elastic:
      enabled: true
      adapter_type: "http"
      endpoint: "${ELASTICSEARCH_ENDPOINT}"
      api_key: "${ELASTICSEARCH_API_KEY}"
      index_pattern: "copybook-audit-%Y.%m.%d"
      document_type: "_doc"
      bulk_size: 500
      bulk_timeout_seconds: 30

    prometheus:
      enabled: true
      adapter_type: "http"
      metrics_path: "/metrics"
      port: 8080
      push_gateway: "${PROMETHEUS_PUSHGATEWAY}"
      job_name: "copybook-audit"

    datadog:
      enabled: false
      adapter_type: "http"
      api_key: "${DATADOG_API_KEY}"
      site: "datadoghq.com"
      service_name: "copybook-audit"

    webhook:
      enabled: true
      adapter_type: "http"
      endpoints:
        - name: "security_webhook"
          url: "${SECURITY_WEBHOOK_URL}"
          method: "POST"
          headers:
            Authorization: "Bearer ${SECURITY_WEBHOOK_TOKEN}"
          event_filters:
            - event_type: "SecurityEvent"
            - severity: ["High", "Critical"]

  # Event routing rules
  routing:
    rules:
      - rule_id: "security_events"
        name: "Route Security Events"
        condition:
          event_type: "SecurityEvent"
        targets:
          - adapter: "splunk"
            format: "cef"
          - adapter: "webhook"
            format: "json"
        exclusive: false
        priority: 10

      - rule_id: "compliance_events"
        name: "Route Compliance Events"
        condition:
          or:
            - event_type: "ComplianceCheck"
            - metadata_contains:
                key: "compliance_framework"
                value: "SOX"
        targets:
          - adapter: "elastic"
            format: "ecs"
          - adapter: "splunk"
            format: "json"
        exclusive: false
        priority: 20

      - rule_id: "high_priority_events"
        name: "Route High Priority Events"
        condition:
          and:
            - severity: "Critical"
            - security_classification: "MaterialTransaction"
        targets:
          - adapter: "splunk"
            format: "cef"
          - adapter: "webhook"
            format: "json"
          - adapter: "datadog"
            format: "json"
        exclusive: true
        priority: 1

    default_targets:
      - adapter: "elastic"
        format: "ecs"
      - adapter: "prometheus"
        format: "metrics"

  # Reliability and fault tolerance
  reliability:
    circuit_breaker:
      enabled: true
      failure_threshold: 5
      timeout_seconds: 60
      half_open_max_calls: 3

    retry_policy:
      enabled: true
      max_attempts: 3
      initial_delay_ms: 100
      max_delay_ms: 10000
      exponential_backoff: true
      jitter_enabled: true

    dead_letter_queue:
      enabled: true
      max_queue_size: 10000
      retention_hours: 24
      retry_interval_seconds: 300

  # Security configuration
  security:
    tls:
      min_version: "1.2"
      verify_certificates: true
      client_cert_path: "${CLIENT_CERT_PATH}"
      client_key_path: "${CLIENT_KEY_PATH}"

    authentication:
      api_keys:
        rotation_interval_days: 90
        header_name: "X-API-Key"

      oauth2:
        enabled: false
        client_id: "${OAUTH2_CLIENT_ID}"
        client_secret: "${OAUTH2_CLIENT_SECRET}"
        token_endpoint: "${OAUTH2_TOKEN_ENDPOINT}"

  # Performance monitoring
  monitoring:
    enabled: true
    metrics_collection_interval_seconds: 60
    performance_alerts:
      response_time_threshold_ms: 5000
      error_rate_threshold_percent: 5.0
      queue_depth_threshold: 1000

  # Event filtering
  filters:
    global:
      - type: "severity_threshold"
        minimum_severity: "Low"

      - type: "rate_limiting"
        max_events_per_second: 1000
        burst_capacity: 5000

    adapter_specific:
      splunk:
        - type: "event_type_filter"
          allowed_types: ["SecurityEvent", "ComplianceCheck", "ErrorEvent"]

      elastic:
        - type: "data_classification_filter"
          allowed_classifications: ["Internal", "Confidential", "MaterialTransaction"]
```

## Performance Optimization

### Batch Processing and Connection Pooling
```rust
impl IntegrationManager {
    pub async fn send_event_batch(&self, events: Vec<&AuditEvent>) -> Result<BatchIntegrationResult, IntegrationError> {
        let mut target_batches: HashMap<String, Vec<&AuditEvent>> = HashMap::new();

        // Group events by target for efficient batching
        for event in events {
            let targets = self.router.route_event(event);
            for target in targets {
                target_batches
                    .entry(target.name.clone())
                    .or_insert_with(Vec::new)
                    .push(event);
            }
        }

        // Process batches in parallel
        let batch_futures: Vec<_> = target_batches
            .into_iter()
            .map(|(target_name, target_events)| {
                let adapter = self.adapters.get(&target_name).unwrap();
                async move {
                    let formatted_events: Vec<FormattedEvent> = target_events
                        .into_iter()
                        .map(|event| self.format_event_for_target(event, &target_name))
                        .collect::<Result<Vec<_>, _>>()?;

                    adapter.send_batch(formatted_events.iter().collect()).await
                }
            })
            .collect();

        let batch_results = futures::future::join_all(batch_futures).await;

        // Aggregate batch results
        let mut total_events = 0;
        let mut successful_events = 0;
        let mut failed_events = 0;

        for result in batch_results {
            match result {
                Ok(batch_result) => {
                    total_events += batch_result.total_events;
                    successful_events += batch_result.successful_events;
                    failed_events += batch_result.failed_events;
                }
                Err(_) => {
                    // Handle batch processing errors
                    failed_events += 1;
                }
            }
        }

        Ok(BatchIntegrationResult {
            total_events,
            successful_events,
            failed_events,
            processing_time_ms: 0, // Would be calculated
            batch_success_rate: successful_events as f64 / total_events as f64,
        })
    }
}
```

### Performance Monitoring
```rust
#[derive(Debug, Clone)]
pub struct IntegrationPerformanceMetrics {
    // Throughput metrics
    pub events_per_second: f64,
    pub bytes_per_second: f64,
    pub batch_size_avg: f64,

    // Latency metrics
    pub avg_response_time_ms: f64,
    pub p95_response_time_ms: f64,
    pub p99_response_time_ms: f64,

    // Reliability metrics
    pub success_rate: f64,
    pub error_rate: f64,
    pub circuit_breaker_trips: u64,

    // Resource utilization
    pub connection_pool_utilization: f64,
    pub memory_usage_mb: f64,
    pub cpu_usage_percent: f64,
}

impl IntegrationManager {
    pub async fn collect_performance_metrics(&self) -> IntegrationPerformanceMetrics {
        let metrics = self.metrics.lock().await;

        IntegrationPerformanceMetrics {
            events_per_second: metrics.calculate_events_per_second(),
            bytes_per_second: metrics.calculate_bytes_per_second(),
            batch_size_avg: metrics.calculate_avg_batch_size(),
            avg_response_time_ms: metrics.calculate_avg_response_time(),
            p95_response_time_ms: metrics.calculate_percentile_response_time(0.95),
            p99_response_time_ms: metrics.calculate_percentile_response_time(0.99),
            success_rate: metrics.calculate_success_rate(),
            error_rate: metrics.calculate_error_rate(),
            circuit_breaker_trips: metrics.circuit_breaker_trips,
            connection_pool_utilization: self.get_connection_pool_utilization().await,
            memory_usage_mb: self.get_memory_usage().await,
            cpu_usage_percent: self.get_cpu_usage().await,
        }
    }
}
```

## Testing Framework

### Integration Testing
```rust
#[cfg(test)]
mod integration_tests {
    use super::*;

    #[tokio::test]
    async fn test_splunk_integration() {
        let config = IntegrationConfig::test_config();
        let manager = IntegrationManager::new(config).await.unwrap();

        let test_event = create_test_audit_event();
        let result = manager.send_audit_event(&test_event).await.unwrap();

        assert!(result.overall_success);
        assert_eq!(result.successful_deliveries, 1);
        assert_eq!(result.failed_deliveries, 0);
    }

    #[tokio::test]
    async fn test_batch_processing_performance() {
        let config = IntegrationConfig::test_config();
        let manager = IntegrationManager::new(config).await.unwrap();

        let test_events = create_test_event_batch(1000);
        let start_time = Instant::now();

        let result = manager.send_event_batch(test_events.iter().collect()).await.unwrap();
        let processing_time = start_time.elapsed();

        assert!(result.batch_success_rate > 0.95);
        assert!(processing_time.as_millis() < 5000); // Should complete within 5 seconds
    }

    #[tokio::test]
    async fn test_circuit_breaker_functionality() {
        let config = IntegrationConfig::test_config();
        let manager = IntegrationManager::new(config).await.unwrap();

        // Simulate multiple failures to trip circuit breaker
        for _ in 0..10 {
            let _ = manager.send_to_failing_service().await;
        }

        // Circuit breaker should now be open
        assert!(manager.reliability_manager.circuit_breaker.is_open("failing_service"));

        // Subsequent calls should fail fast
        let start_time = Instant::now();
        let result = manager.send_to_failing_service().await;
        let response_time = start_time.elapsed();

        assert!(result.is_err());
        assert!(response_time.as_millis() < 100); // Should fail fast
    }
}
```

## Alternatives Considered

### Alternative 1: Monolithic Integration Service
**Approach**: Single service handling all integrations internally
**Rejected Because**:
- Poor separation of concerns
- Difficult to scale individual integrations
- Complex testing and maintenance
- Single point of failure

### Alternative 2: Message Queue Based Architecture
**Approach**: Use message queues (RabbitMQ, Kafka) for all integrations
**Rejected Because**:
- Additional infrastructure complexity
- Higher latency for real-time integrations
- Message ordering and delivery guarantees complexity
- Over-engineering for the use case

### Alternative 3: Function-as-a-Service (FaaS) Integration
**Approach**: Deploy each integration as a separate cloud function
**Rejected Because**:
- Cold start latency issues
- Vendor lock-in concerns
- Complex deployment and configuration management
- Cost implications for high-volume processing

## Related Decisions
- ADR-001: Audit Event Schema Design
- ADR-002: Cryptographic Integrity Implementation
- ADR-003: Performance Optimization Strategy
- ADR-004: Multi-Framework Compliance Engine

## References
- [Enterprise Integration Patterns](https://www.enterpriseintegrationpatterns.com/)
- [Circuit Breaker Pattern](https://martinfowler.com/bliki/CircuitBreaker.html)
- [Common Event Format (CEF) Guide](https://www.microfocus.com/documentation/arcsight/arcsight-smartconnectors-8.3/cef-implementation-standard/)
- [Elastic Common Schema (ECS)](https://www.elastic.co/guide/en/ecs/current/index.html)
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
