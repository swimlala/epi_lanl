CDF   s   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   l   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      history       X2007-03-09T20:46:02Z creation; 2015-10-19T17:41:42Z last update (coriolis COFC software)      F   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    =|   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    =�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    =�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    =�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    =�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    =�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    =�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  =�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  >T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  �  >�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        ?T   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    ?\   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    ?`   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  ?d   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    ?�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    ?�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  ?�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  ?�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  @0   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    @p   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T      
resolution        ?q   comment_on_resolution         �JULD resolution is 6 minutes, except when JULD = JULD_LOCATION or when JULD = JULD_FIRST_MESSAGE (TRAJ file variable); in that case, JULD resolution is 1 second        @x   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    @�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >��	4E�        @�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           @�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           @�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    @�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    @�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    @�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        B�   PROFILE_CNDC_QC                	long_name         #Global quality flag of CNDC profile    conventions       Argo reference table 2a    
_FillValue                    B�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    B�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    B�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    B�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        8ѷ     `  B�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  FH   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        8ѷ     `  G    CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  J�   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        8ѷ     `  KX   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        `  N�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  R   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        `  R�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  VP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  W(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  Z�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  ]�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ^�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  b    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  b�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  fX   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  i�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  j�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  m�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  n�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  r(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    r�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    z�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  p  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �  �8   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                     ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                     �Argo profile    3.1 1.2 19500101000000  20070309204602  20151019174142  1900421 1900421 GOODHOPE                                                        GOODHOPE                                                        Sabrina SPEICH et Michel ARHAN                                  Sabrina SPEICH et Michel ARHAN                                  PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               T   TAA  IFIF4120273                         4120273                         2C  2C  DD  PROVOR                          PROVOR                          OIN-03-F3-007                   OIN-03-F3-007                   n/a                             n/a                             842 842 @�e�UUUU@�e�UUUU11  @�e�UUUU@�e�UUUU�DY�^5?}�DY�^5?}@D�p��
>@D�p��
>11  ARGOS   ARGOS   Primary sampling: averaged [10 sec sampling, 25 dbar average from 2000 dbar to 500 dbar; 10 sec sampling, 10 dbar average from 500 dbar to 10.0 dbar]                                                                                                           Near-surface sampling: averaged, unpumped [10 sec sampling, 10 dbar average from 10.0 dbar to surface]                                                                                                                                                                FF  AA  FF  AA  @��W@���@��r@���@�@O@�@O@�Mj@�ƨ@��@���@�
�@���@���@�v�@�0U@��~@�@���@��@�8�@�C�@���@�˒@��@�8�@�~�@�ی@�m�@�?}@��D@���@�	@���@��2@�o @�!-@���@�Y@�V@��`@���@��+@���@���@��s@��b@���@�Y@��.@�@}�@{�@{&@y��@w��@v^5@t<�@r3�@o��@m|@k�@h��@e�9@c(@b1�@a�@^��@]-w@[�@Y@W��@WH�@V6�@U#�@Tb@SY@R��@Q�@Qu�@Q#�@P�/@Q�@P��@PbN@P�@O�a@O�@N�@Nff@N($@Nff@N�@N0U@M��@M��@M�h@M�@Mk�@M�@L��@L�@L��@L�@L�)@L�@L�@L�9@L��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                           @��W@���@��r@���@�@O@�@O@�Mj@�ƨ@��@���@�
�@���@���@�v�@�0U@��~@�@���@��@�8�@�C�@���@�˒@��@�8�@�~�@�ی@�m�@�?}@��D@���@�	@���@��2@�o @�!-@���@�Y@�V@��`@���@��+@���@���@��s@��b@���@�Y@��.@�@}�@{�@{&@y��@w��@v^5@t<�@r3�@o��@m|@k�@h��@e�9@c(@b1�@a�@^��@]-w@[�@Y@W��@WH�@V6�@U#�@Tb@SY@R��@Q�@Qu�@Q#�@P�/@Q�@P��@PbN@P�@O�a@O�@N�@Nff@N($@Nff@N�@N0U@M��@M��@M�h@M�@Mk�@M�@L��@L�@L��@L�@L�)@L�@L�@L�9@L��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                           :�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�o:�oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A`  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CM  CW  Ca  Ck  Cu  C  C�� C�� C�� C�� C�  C�� C�  C�� C�� C�� C�� C�� C�� Cŀ Cʀ Cπ CԀ Cـ Cހ C� C� C� C� C�� D @ D@ D� D  D@ D� D%� D+� D2  D8@ D>� DE  DK  DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�` D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�` Dɀ D�� D�� D�  D�  D�  D�@ D߀ D� D� D�� D�  D�  D�  D�  @�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                           A`  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CM  CW  Ca  Ck  Cu  C  C�� C�� C�� C�� C�  C�� C�  C�� C�� C�� C�� C�� C�� Cŀ Cʀ Cπ CԀ Cـ Cހ C� C� C� C� C�� D @ D@ D� D  D@ D� D%� D+� D2  D8@ D>� DE  DK  DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�` D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�` Dɀ D�� D�� D�  D�  D�  D�@ D߀ D� D� D�� D�  D�  D�  D�  @�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                           @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  @�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B�B�TB�B�yB�B�B�B��B��B��B�BBB��B��B��B�B�/B�'B�3B��B��B��B�%Bv�Bp�B5?B�B>wB �BbB�B��B��B��B�qBŢBǮBÖB�dB��B�+BcTBVB �B	7B��BɺB��B�bB�7BaHBC�B'�B
��B
�B
��B
� B
^5B
5?B
B	�NB	�TB	�B	�^B	��B	�PB	t�B	k�B	n�B	k�B	bNB	\)B	_;B	p�B	�B	�PB	��B	��B	�3B	�XB	��B	��B	�)B	�TB	�B	�B	��B
DB
�B
�B
�B
)�B
=qB
E�B
P�B
S�B
YB
aHB
iyB
m�B
~�B
�B
�B
�VB
��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                           B��B��B�B�TB�B�yB�B�B�B��B��B��B�BBB��B��B��B�B�/B�'B�3B��B��B��B�%Bv�Bp�B5?B�B>wB �BbB�B��B��B��B�qBŢBǮBÖB�dB��B�+BcTBVB �B	7B��BɺB��B�bB�7BaHBC�B'�B
��B
�B
��B
� B
^5B
5?B
B	�NB	�TB	�B	�^B	��B	�PB	t�B	k�B	n�B	k�B	bNB	\)B	_;B	p�B	�B	�PB	��B	��B	�3B	�XB	��B	��B	�)B	�TB	�B	�B	��B
DB
�B
�B
�B
)�B
=qB
E�B
P�B
S�B
YB
aHB
iyB
m�B
~�B
�B
�B
�VB
��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                           <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�&�A�1'A��HA��A�VA�1'A�r�A���A�dZA�  A���A���A��/A�+A���A�\)A�=qA���A��A��A~Q�A}%AyVAu��ArM�AoG�Am&�Ah �Ae%Ae%Ab�A`A�A]�AV�jAUG�ATM�ATVAS�AS%ARn�AQ33ANĜALĜAIƨAH��AD=qAB  A?x�A:��A5�;A3�A0��A-+A)K�A%p�A I�AG�A�A�FA
ĜAn�@�Ĝ@�^5@�/@ް!@�V@�5?@���@��@���@��-@�dZ@�G�@�@�V@�%@�(�@�G�@���@���@��@{t�@w�P@r~�@mO�@d�/@`r�@Z-@VE�@W;d@XQ�@R^5@N�y@I�#@G
=@EV@B�!@>ff@:=q@6�R@4��@3��@4j@3��@0��@0A�@1�7A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                           A��A�&�A�1'A��HA��A�VA�1'A�r�A���A�dZA�  A���A���A��/A�+A���A�\)A�=qA���A��A��A~Q�A}%AyVAu��ArM�AoG�Am&�Ah �Ae%Ae%Ab�A`A�A]�AV�jAUG�ATM�ATVAS�AS%ARn�AQ33ANĜALĜAIƨAH��AD=qAB  A?x�A:��A5�;A3�A0��A-+A)K�A%p�A I�AG�A�A�FA
ĜAn�@�Ĝ@�^5@�/@ް!@�V@�5?@���@��@���@��-@�dZ@�G�@�@�V@�%@�(�@�G�@���@���@��@{t�@w�P@r~�@mO�@d�/@`r�@Z-@VE�@W;d@XQ�@R^5@N�y@I�#@G
=@EV@B�!@>ff@:=q@6�R@4��@3��@4j@3��@0��@0A�@1�7A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                           <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            CNDC_ADJUSTED = CNDC                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            CNDC_ADJUSTED = CNDC                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant pressure drift detected - Decision of the PIs - Calibration error is manufacturer specified accuracy                                                                                                                                             No significant temperature drift detected - Decision of the PIs - Calibration error is manufacturer specified accuracy                                                                                                                                          No correction - Decision of the PIs ; CNDC, CNDC_ADJUSTED and CNDC_ADJUSTED_ERROR values divided by 10                                                                                                                                                          No correction - Decision of the PIs -  Calibration error is manufacturer specified accuracy                                                                                                                                                                     No significant pressure drift detected - Decision of the PIs - Calibration error is manufacturer specified accuracy                                                                                                                                             No significant temperature drift detected - Decision of the PIs - Calibration error is manufacturer specified accuracy                                                                                                                                          No correction - Decision of the PIs ; CNDC, CNDC_ADJUSTED and CNDC_ADJUSTED_ERROR values divided by 10                                                                                                                                                          No correction - Decision of the PIs -  Calibration error is manufacturer specified accuracy                                                                                                                                                                     2011020314552820110203145528201102031455282011020314552720110203145528201102031455282011020314552820110203145527TC  TC          OA  OA  3.023.02                                                                                                                                2007041800000020070418000000QC  QC  TEMP            TEMP            G�O�G�O�G�O�G�O�G�O�G�O�2007-03-28      2007-03-28      TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383720070717143837CF  CF  PSAL            PSAL            @�  @�  G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383720070717143837CF  CF  PSAL            PSAL            A`  A`  G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            A�  A�  G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            B  B  G�O�G�O�B�B�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            B4  B4  G�O�G�O�B�TB�T                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            B\  B\  G�O�G�O�B�B�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            B�  B�  G�O�G�O�B�yB�y                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            B�  B�  G�O�G�O�B�B�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            B�  B�  G�O�G�O�B�B�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            B�  B�  G�O�G�O�B�B�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            B�  B�  G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            B�  B�  G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            B�  B�  G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C  C  G�O�G�O�B�B�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C  C  G�O�G�O�BB                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C  C  G�O�G�O�BB                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C%  C%  G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C/  C/  G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C9  C9  G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            CC  CC  G�O�G�O�B�B�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            CM  CM  G�O�G�O�B�/B�/                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            CW  CW  G�O�G�O�B�'B�'                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            Ca  Ca  G�O�G�O�B�3B�3                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            Ck  Ck  G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            Cu  Cu  G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C  C  G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C�� C�� G�O�G�O�B�%B�%                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C�� C�� G�O�G�O�Bv�Bv�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C�� C�� G�O�G�O�Bp�Bp�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C�� C�� G�O�G�O�B5?B5?                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C�  C�  G�O�G�O�B�B�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C�� C�� G�O�G�O�B>wB>w                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C�  C�  G�O�G�O�B �B �                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C�� C�� G�O�G�O�BbBb                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C�� C�� G�O�G�O�B�B�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C�� C�� G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C�� C�� G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C�� C�� G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C�� C�� G�O�G�O�B�qB�q                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            Cŀ Cŀ G�O�G�O�BŢBŢ                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            Cʀ Cʀ G�O�G�O�BǮBǮ                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            Cπ Cπ G�O�G�O�BÖBÖ                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            CԀ CԀ G�O�G�O�B�dB�d                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            Cـ Cـ G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            Cހ Cހ G�O�G�O�B�+B�+                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C� C� G�O�G�O�BcTBcT                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C� C� G�O�G�O�BVBV                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C� C� G�O�G�O�B �B �                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C� C� G�O�G�O�B	7B	7                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            C�� C�� G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D @ D @ G�O�G�O�BɺBɺ                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D@ D@ G�O�G�O�B��B��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D� D� G�O�G�O�B�bB�b                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D  D  G�O�G�O�B�7B�7                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D@ D@ G�O�G�O�BaHBaH                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D� D� G�O�G�O�BC�BC�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D%� D%� G�O�G�O�B'�B'�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D+� D+� G�O�G�O�B
��B
��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D2  D2  G�O�G�O�B
�B
�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D8@ D8@ G�O�G�O�B
��B
��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D>� D>� G�O�G�O�B
� B
�                                 TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            DE  DE  G�O�G�O�B
^5B
^5                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            DK  DK  G�O�G�O�B
5?B
5?                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            DQ� DQ� G�O�G�O�B
B
                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            DW� DW� G�O�G�O�B	�NB	�N                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D^  D^  G�O�G�O�B	�TB	�T                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            Dd@ Dd@ G�O�G�O�B	�B	�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            Dj� Dj� G�O�G�O�B	�^B	�^                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            Dp� Dp� G�O�G�O�B	��B	��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            Dw  Dw  G�O�G�O�B	�PB	�P                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D}@ D}@ G�O�G�O�B	t�B	t�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�� D�� G�O�G�O�B	k�B	k�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�� D�� G�O�G�O�B	n�B	n�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�  D�  G�O�G�O�B	k�B	k�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�  D�  G�O�G�O�B	bNB	bN                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�@ D�@ G�O�G�O�B	\)B	\)                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�` D�` G�O�G�O�B	_;B	_;                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�` D�` G�O�G�O�B	p�B	p�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�� D�� G�O�G�O�B	�B	�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�� D�� G�O�G�O�B	�PB	�P                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�� D�� G�O�G�O�B	��B	��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�  D�  G�O�G�O�B	��B	��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�  D�  G�O�G�O�B	�3B	�3                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�@ D�@ G�O�G�O�B	�XB	�X                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�` D�` G�O�G�O�B	��B	��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�� D�� G�O�G�O�B	��B	��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�� D�� G�O�G�O�B	�)B	�)                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383820070717143838CF  CF  PSAL            PSAL            D�� D�� G�O�G�O�B	�TB	�T                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�� D�� G�O�G�O�B	�B	�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�  D�  G�O�G�O�B	�B	�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�  D�  G�O�G�O�B	��B	��                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�@ D�@ G�O�G�O�B
DB
D                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�` D�` G�O�G�O�B
�B
�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�` D�` G�O�G�O�B
�B
�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            Dɀ Dɀ G�O�G�O�B
�B
�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�� D�� G�O�G�O�B
)�B
)�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�� D�� G�O�G�O�B
=qB
=q                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�  D�  G�O�G�O�B
E�B
E�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�  D�  G�O�G�O�B
P�B
P�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�  D�  G�O�G�O�B
S�B
S�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�@ D�@ G�O�G�O�B
YB
Y                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D߀ D߀ G�O�G�O�B
aHB
aH                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D� D� G�O�G�O�B
iyB
iy                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D� D� G�O�G�O�B
m�B
m�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�� D�� G�O�G�O�B
~�B
~�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�  D�  G�O�G�O�B
�B
�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�  D�  G�O�G�O�B
�B
�                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�  D�  G�O�G�O�B
�VB
�V                                TC  TC          CVQCCVQC1.7.1.7.                                                                                                                                2007071714383920070717143839CF  CF  PSAL            PSAL            D�  D�  G�O�G�O�B
��B
��                                IF  IF  CODMCODMCOOACOOA1   1   DMQCGL01                                                        DMQCGL01                                                        2008041021141220080410211412QCP$QCP$PSAL            PSAL            G�O�G�O�G�O�G�O�G�O�G�O�                                IF  IF  ARGQARGQSCOOSCOO1.3 1.3                                                                                                                                 2010121010524720101210105247CF  CF  PSAL            PSAL            @�  @�  D�  D�  ?�  ?�                                  IF  IF  ARSQARSQOW  OW  1.0 1.0 COR2010V1                                                       COR2010V1                                                       2011020314552920110203145529IP  IP  PSAL            PSAL            @�  @�  D�  D�  G�O�G�O�                                IF  IF                                                                                                                                                          2012110612582120121106125821CV  CV  CNDC            CNDC            @�  @�  D�  D�  G�O�G�O�                                IF  IF          COFCCOFC2.7 2.7                                                                                                                                 2015101917414220151019174142                                        G�O�G�O�G�O�G�O�G�O�G�O�                                