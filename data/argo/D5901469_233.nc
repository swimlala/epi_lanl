CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-20T21:22:09Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        F   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    =    FORMAT_VERSION                 	long_name         File format version    
_FillValue                    =0   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    =4   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    =8   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    =H   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    =X   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    =h   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  =�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  >@   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  �  ?    CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        ?�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    ?�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    ?�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  `  ?�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    @8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    @D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  `  @H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  `  @�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  `  A   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    Ah   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           At   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    A�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            A�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           A�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           A�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    A�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    A�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    A�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        D�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    E    PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_CNDC_QC                	long_name         #Global quality flag of CNDC profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    E   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        R�  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     R�  �`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     R� �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     R� fT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     R� ͤ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     R� 4�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     R� ��   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �<   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     R� ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � A�   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     R� V8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     R� ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     R� ,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � b�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     R� w|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �x   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 �Argo profile    3.1 1.2 19500101000000  20181120212209  20200901154648  5901469 5901469 5901469 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  2688                            2688                            2688                            2C  2B  2C  DAD APEX                            APEX                            APEX                            2730                            2730                            2730                            112607                          112607                          112607                          846 846 846 @�Y@��2@�Y@��2@�Y@��2111 @�Y@}'Ұ@�Y@}'Ұ@�Y@}'Ұ@6�-V@6�-V@6�-V�dU&�x���dU&�x���dU&�x��111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          BCA BCA  CA BCA     @9��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B ffB  B  B  B   B(  B0  B8  BA33BH  BO��BXffB`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=���    =���    =���        =���        =���=���        >���        =���                    >L��        >L��=���    >L��            =���        >L��>���        >L��        =���                =���        =���=���    >L��    =���    =���                =���                        =���            =���    =���                =���                    =���    =���>L��                        =���        =���        =���        >L��=���    =���>L��    =���>L��>L��    =���>���>���    =���                =���>���>���    =���                                        =���        =���            >���>���        >���>���>���=���        =���=���        >L��>���>���        =���>���>���        >L��>L��        =���=���>L��>L��=���            =���>L��>���>���>L��            >L��>L��>���>L��=���    >L��    >L��=���    =���>L��=���>L��>L��=���=���>L��    >L��    >L��>L��>L��=���>L��>L��>L��>���>���?   >L��>���>���>���>L��>���>���>���>���>L��>���>���>���>���>���>���>���>���>L��>L��>L��>���>���>���>���>L��>L��>���>���>���>L��>L��>���>L��>���>���>���>���>���>���>���>���>���>���>L��>L��>L��>L��>���>���>���>���>���>L��>���>���>L��>���>���>���=���>L��>���>���>���>���>L��=���>���>���>���>���>���>���>L��>���>���>L��>L��>���>L��>���>���>���>���>���>L��>���>L��>���>���>���>L��>���>L��>L��>���>���>���>���>L��>���>���>L��>L��>���>���>���>���>���>���>L��>���>���>���>���>���>L��>���>L��>���>���>���>���>L��=���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>L��>���=���>���>���>���>���>���>���>���>���>L��>���>���>L��>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>L��=���>���>L��>���>L��>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>L��>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>L��=���>���>L��>���>���>���>���>���>���>���>���>���>���=���>L��>���=���>L��>L��>���>���>���>���?   ?��?   ?��?333?L��?L��?333?L��?L��?fff?fff?fff?�  ?���?���?���?���?���?�ff?�ff?�33?�33?�33?�  ?�  ?�  ?ٙ�?ٙ�?ٙ�?�ff?�ff?�ff?�ff?�33?�33@   @   @   @ff@ff@��@��@33@33@33@��@��@   @   @   @&ff@&ff@&ff@,��@333@333@333@333@9��@9��@@  @Fff@Fff@L��@L��@S33@Y��@Y��@`  @`  @fff@l��@l��@y��@y��@y��@�33@�33@�ff@�ff@���@�  @�  @�33@�ff@���@���@�  @�33@�ff@�ff@���@���@�  @�33@�ff@���@���@�  @�33@�ff@ə�@���@�  @�33@�ff@ٙ�@���@�33@�ff@陚@���@�  @�33@�ff@���A   A��A33A��AffA  A	��A��AffA  A��A33AffA  A��A33A��A   A!��A#33A$��A&ffA)��A+33A,��A0  A1��A333A4��A8  A9��A;33A>ffA@  AC33AD��AFffAH  AK33AL��AP  AQ��AS33AVffAX  AY��A\��A^ffAa��Ac33AfffAh  Ak33Al��AnffAq��At��AvffAx  A{33A|��A~ffA���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�33A�  A���A�ffA�33A���A�ffA�33A���Ař�A�33A�  Aə�A�ffA�  A���A�ffA�33A���Aљ�A�33A���Aՙ�A�33A�  A���A�ffA�33A���Aݙ�A�33A�  AᙚA�ffA�33A�  A噚A�ffA�  A���A陚A�ffA�  A���A홚A�ffA�  A���A�A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33B   B ffB ��B��B  BffB33B��B  BffB��B��B  BffB��B33B  BffB��B	33B
  B
ffB
��B33B  BffB��B33B��B  B��B33B��B  BffB33B��B  BffB33B��B  BffB33B��B  BffB��B��B  BffB��B33B��BffB��B33B��B  B��B33B��B  BffB��B33B��B ffB ��B!33B!��B"  B"ffB"��B#33B#��B$  B$ffB$ffB$��B%33B%��B&  B&ffB&��B'33B'33B'��B(  B(ffB(��B)33B)33B)��B*  B*ffB*��B*��B+33B+��B,  B,ffB,ffB,��B-33B-��B.  B.ffB.ffB.��B/33B/��B0  B0  B0ffB0��B133B133B1��B2  B2ffB2��B333B333B3��B4  B4  B4ffB4��B533B533B5��B6  B6ffB6��B6��B733B7��B8  B8  B8ffB8��B933B9��B9��B:  B:ffB:ffB:��B;33B;��B;��B<  B<  B<ffB<��B=33B=33B=��B=��B>  B>ffB>ffB>��B>��B?33B?��B?��B@  B@  B@ffB@ffB@��B@��BA33BA33BA��BA��BA��BB  BB  BB  BB  BBffBBffBBffBBffBB��BB��BB��BB��BB��BC33BC33BC33BC33BC33BC��BC��BC��BC��BC��BD  BD  BD  BD  BDffBDffBDffBDffBDffBD��BD��BD��BD��BE33BE33BE33BE33BE��BE��BE��BE��BF  BF  BF  BF  BFffBFffBFffBFffBF��BF��BF��BF��BF��BG33BG33BG33BG��BG��BG��BG��BG��BH  BH  BH  BH  BHffBHffBHffBHffBHffBH��BH��BH��BI33BI33BI33BI33BI��BI��BI��BI��BI��BJ  BJ  BJ  BJ  BJffBJffBJffBJffBJ��BJ��BJ��BJ��BK33BK33BK33BK33BK33BK��BK��BK��BK��BL  BL  BL  BL  BL  BLffBLffBLffBLffBLffBL��BL��BL��BL��BM33BM33BM33BM33BM33BM33BM��BM��BM��BM��BM��BN  BN  BN  BN  BN  BNffBNffBNffBNffBNffBNffBN��BN��BN��BN��BN��BO33BO33BO33BO33BO33BO33BO��BO��BO��BO��BO��BO��BP  BP  BP  BP  BPffBPffBPffBPffBPffBPffBP��BP��BP��BP��BQ33BQ33BQ33BQ33BQ��BQ��BQ��BQ��BR  BR  BR  BR  BRffBRffBRffBR��BR��BR��BS33BS33BS��BS��BT  BT  BTffBTffBT��BT��BU33BU33BU��BU��BV  BV  BVffBV��BV��BW33BW33BW��BW��BX  BXffBXffBX��BX��BY33BY33BY��BY��BY��BZ  BZ  BZffBZffBZffBZ��BZ��BZ��B[33B[33B[33B[��B[��B[��B\  B\  B\ffB\ffB\ffB\��B\��B\��B]33B]33B]��B]��B]��B^  B^  B^  B^ffB^ffB^��B^��B^��B_33B_33B_��B_��B_��B`  B`  B`ffB`ffB`ffB`��B`��B`��Ba33Ba33Ba��Ba��Bb  Bb  Bb  BbffBbffBb��Bb��Bb��Bc33Bc33Bc��Bc��Bd  Bd  Bd  BdffBdffBd��Bd��Be33Be33Be��Be��Bf  Bf  BfffBfffBfffBf��Bg33Bg33Bg��Bg��Bg��Bh  BhffBhffBh��Bh��Bi33Bi33Bi��Bi��Bi��Bj  Bj  BjffBjffBj��Bj��Bk33Bk33Bk��Bl  Bl  BlffBlffBl��Bl��Bm33Bm��Bm��Bn  BnffBnffBn��Bo33Bo33Bo��Bp  BpffBpffBp��Bq33Bq��Br  Br  BrffBr��Bs33Bs��Bt  BtffBtffBt��Bu33Bu��Bv  BvffBv��Bw33Bw��Bx  BxffBx��By33By��Bz  BzffBz��B{33B{��B|  B|ffB|��B}33B}��B~  B~ffB~��B33B��B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�33B�ffB���B���B�33B�ffB���B���B�  B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B���B���B�33B�ffB���B�  B�33B�ffB���B�  B�33B���B���B�33B�ffB���B�  B�33B�ffB���B�  B�ffB���B���B�33B�ffB���B�  B�33B���B���B�33B�ffB���B�  B�33B�ffB���B�  B�33B���B���B�  B�ffB���B���B�33B�ffB���B�  B�33B�ffB�B�  B�33B�ffBÙ�B�  B�33B�ffBę�B���B�33B�ffBř�B���B�33B�ffBƙ�B���B�33B�ffBǙ�B���B�33B�ffBș�B���B�33B�ffBə�B�  B�33B�ffBʙ�B�  B�33B�ffB˙�B�  B�33B�ffB̙�B�  B�33B�ffCJ�CJ  CI�fCI�fCI��CI�3CI��CI� CIffCIL�CI33CI�CI  CI  CH�fCH��CH�3CH��CH� CHffCHL�CH33CH�CH  CG�fCG��CG�3CG��CG� CGffCGL�CG33CG�CG  CF�fCF�3CF��CF� CFffCFL�CF33CF�CF  CE�fCE�3CE��CE� CEffCEL�CE�CE  CD�fCD��CD��CD� CDffCDL�CD�CD  CC�fCC��CC��CC� CCffCCL�CC�CC  CB�fCB��CB��CB� CBffCBL�CB�CB  CA�fCA�3CA��CA� CAL�CA33CA�C@�fC@��C@�3C@� C@ffC@33C@�C?�fC?��C?�3C?� C?ffC?33C?�C>�fC>��C>��C>� C>L�C>33C>  C=�fC=�3C=� C=ffC=33C=�C<�fC<��C<��C<� C<L�C<�C<  C;��C;��C;� C;L�@9��@9��@@  @Fff@Fff@L��@L��@S33@Y��@Y��@`  @`  @fff@l��@l��@y��@y��@y��@�33@�33@�ff@�ff@���@�  @�  @�33@�ff@���@���@�  @�33@�ff@�ff@���@���@�  @�33@�ff@���@���@�  @�33@�ff@ə�@���@�  @�33@�ff@ٙ�@���@�33@�ff@陚@���@�  @�33@�ff@���A   A��A33A��AffA  A	��A��AffA  A��A33AffA  A��A33A��A   A!��A#33A$��A&ffA)��A+33A,��A0  A1��A333A4��A8  A9��A;33A>ffA@  AC33AD��AFffAH  AK33AL��AP  AQ��AS33AVffAX  AY��A\��A^ffAa��Ac33AfffAh  Ak33Al��AnffAq��At��AvffAx  A{33A|��A~ffA���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�33A�  A���A�ffA�33A���A�ffA�33A���Ař�A�33A�  Aə�A�ffA�  A���A�ffA�33A���Aљ�A�33A���Aՙ�A�33A�  A���A�ffA�33A���Aݙ�A�33A�  AᙚA�ffA�33A�  A噚A�ffA�  A���A陚A�ffA�  A���A홚A�ffA�  A���A�A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33B   B ffB ��B��B  BffB33B��B  BffB��B��B  BffB��B33B  BffB��B	33B
  B
ffB
��B33B  BffB��B33B��B  B��B33B��B  BffB33B��B  BffB33B��B  BffB33B��B  BffB��B��B  BffB��B33B��BffB��B33B��B  B��B33B��B  BffB��B33B��B ffB ��B!33B!��B"  B"ffB"��B#33B#��B$  B$ffB$ffB$��B%33B%��B&  B&ffB&��B'33B'33B'��B(  B(ffB(��B)33B)33B)��B*  B*ffB*��B*��B+33B+��B,  B,ffB,ffB,��B-33B-��B.  B.ffB.ffB.��B/33B/��B0  B0  B0ffB0��B133B133B1��B2  B2ffB2��B333B333B3��B4  B4  B4ffB4��B533B533B5��B6  B6ffB6��B6��B733B7��B8  B8  B8ffB8��B933B9��B9��B:  B:ffB:ffB:��B;33B;��B;��B<  B<  B<ffB<��B=33B=33B=��B=��B>  B>ffB>ffB>��B>��B?33B?��B?��B@  B@  B@ffB@ffB@��B@��BA33BA33BA��BA��BA��BB  BB  BB  BB  BBffBBffBBffBBffBB��BB��BB��BB��BB��BC33BC33BC33BC33BC33BC��BC��BC��BC��BC��BD  BD  BD  BD  BDffBDffBDffBDffBDffBD��BD��BD��BD��BE33BE33BE33BE33BE��BE��BE��BE��BF  BF  BF  BF  BFffBFffBFffBFffBF��BF��BF��BF��BF��BG33BG33BG33BG��BG��BG��BG��BG��BH  BH  BH  BH  BHffBHffBHffBHffBHffBH��BH��BH��BI33BI33BI33BI33BI��BI��BI��BI��BI��BJ  BJ  BJ  BJ  BJffBJffBJffBJffBJ��BJ��BJ��BJ��BK33BK33BK33BK33BK33BK��BK��BK��BK��BL  BL  BL  BL  BL  BLffBLffBLffBLffBLffBL��BL��BL��BL��BM33BM33BM33BM33BM33BM33BM��BM��BM��BM��BM��BN  BN  BN  BN  BN  BNffBNffBNffBNffBNffBNffBN��BN��BN��BN��BN��BO33BO33BO33BO33BO33BO33BO��BO��BO��BO��BO��BO��BP  BP  BP  BP  BPffBPffBPffBPffBPffBPffBP��BP��BP��BP��BQ33BQ33BQ33BQ33BQ��BQ��BQ��BQ��BR  BR  BR  BR  BRffBRffBRffBR��BR��BR��BS33BS33BS��BS��BT  BT  BTffBTffBT��BT��BU33BU33BU��BU��BV  BV  BVffBV��BV��BW33BW33BW��BW��BX  BXffBXffBX��BX��BY33BY33BY��BY��BY��BZ  BZ  BZffBZffBZffBZ��BZ��BZ��B[33B[33B[33B[��B[��B[��B\  B\  B\ffB\ffB\ffB\��B\��B\��B]33B]33B]��B]��B]��B^  B^  B^  B^ffB^ffB^��B^��B^��B_33B_33B_��B_��B_��B`  B`  B`ffB`ffB`ffB`��B`��B`��Ba33Ba33Ba��Ba��Bb  Bb  Bb  BbffBbffBb��Bb��Bb��Bc33Bc33Bc��Bc��Bd  Bd  Bd  BdffBdffBd��Bd��Be33Be33Be��Be��Bf  Bf  BfffBfffBfffBf��Bg33Bg33Bg��Bg��Bg��Bh  BhffBhffBh��Bh��Bi33Bi33Bi��Bi��Bi��Bj  Bj  BjffBjffBj��Bj��Bk33Bk33Bk��Bl  Bl  BlffBlffBl��Bl��Bm33Bm��Bm��Bn  BnffBnffBn��Bo33Bo33Bo��Bp  BpffBpffBp��Bq33Bq��Br  Br  BrffBr��Bs33Bs��Bt  BtffBtffBt��Bu33Bu��Bv  BvffBv��Bw33Bw��Bx  BxffBx��By33By��Bz  BzffBz��B{33B{��B|  B|ffB|��B}33B}��B~  B~ffB~��B33B��B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�33B�ffB���B���B�33B�ffB���B���B�  B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B���B���B�33B�ffB���B�  B�33B�ffB���B�  B�33B���B���B�33B�ffB���B�  B�33B�ffB���B�  B�ffB���B���B�33B�ffB���B�  B�33B���B���B�33B�ffB���B�  B�33B�ffB���B�  B�33B���B���B�  B�ffB���B���B�33B�ffB���B�  B�33B�ffB�B�  B�33B�ffBÙ�B�  B�33B�ffBę�B���B�33B�ffBř�B���B�33B�ffBƙ�B���B�33B�ffBǙ�B���B�33B�ffBș�B���B�33B�ffBə�B�  B�33B�ffBʙ�B�  B�33B�ffB˙�B�  B�33B�ffB̙�B�  B�33B�ffCJ�CJ  CI�fCI�fCI��CI�3CI��CI� CIffCIL�CI33CI�CI  CI  CH�fCH��CH�3CH��CH� CHffCHL�CH33CH�CH  CG�fCG��CG�3CG��CG� CGffCGL�CG33CG�CG  CF�fCF�3CF��CF� CFffCFL�CF33CF�CF  CE�fCE�3CE��CE� CEffCEL�CE�CE  CD�fCD��CD��CD� CDffCDL�CD�CD  CC�fCC��CC��CC� CCffCCL�CC�CC  CB�fCB��CB��CB� CBffCBL�CB�CB  CA�fCA�3CA��CA� CAL�CA33CA�C@�fC@��C@�3C@� C@ffC@33C@�C?�fC?��C?�3C?� C?ffC?33C?�C>�fC>��C>��C>� C>L�C>33C>  C=�fC=�3C=� C=ffC=33C=�C<�fC<��C<��C<� C<L�C<�C<  C;��C;��C;� C;L�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�@1�@q�@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A��
A�
=A��	B�B�B�B�B'�B/�B7�B@�RBG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/��C1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����	�����	�����	�������	�������	���	����>8Q�������	����������=������=�����	��=�����������	����=��>8Q����=���������	�����������	�������	���	��=�������	�����	�����������	���������������	���������	�����	�����������	�������������	�����	=�����������������	�������	�������	����=�����	�����	=�������	=��=�������	>�\)>�\)�����	�����������	>�\)>8Q�����	�����������������������	�������	������>8Q�>8Q����>8Q�>8Q�>8Q켣�	�������	���	����=��>�\)>�\)�������	>8Q�>8Q����=��=���������	���	=��=�����	���������	=��>8Q�>�\)=��������=��=��>8Q�=�����	��=����=�����	�����	=�����	=��=�����	���	=����=����=��=��=�����	=��=��=��>8Q�>�\)>\=��>�\)>8Q�>8Q�=��>�\)>8Q�>�\)>�\)=��>8Q�>8Q�>�\)>�\)>8Q�>8Q�>�\)>8Q�=��=��=��>8Q�>8Q�>8Q�>8Q�=��=��>8Q�>8Q�>8Q�=��=��>8Q�=��>8Q�>8Q�>8Q�>8Q�>�\)>�\)>�\)>�\)>8Q�>8Q�=��=��=��=��>8Q�>8Q�>8Q�>8Q�>8Q�=��>8Q�>�\)=��>8Q�>8Q�>8Q켣�	=��>8Q�>8Q�>8Q�>8Q�=�����	>8Q�>8Q�>�\)>8Q�>8Q�>8Q�=��>�\)>8Q�=��=��>8Q�=��>�\)>�\)>8Q�>8Q�>8Q�=��>8Q�=��>8Q�>8Q�>8Q�=��>8Q�=��=��>8Q�>�\)>�\)>8Q�=��>8Q�>8Q�=��=��>8Q�>8Q�>8Q�>8Q�>�\)>8Q�=��>8Q�>8Q�>8Q�>8Q�>8Q�=��>8Q�=��>8Q�>8Q�>8Q�>8Q�=�����	>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>�\)>�\)>8Q�=��>8Q�>8Q�>8Q�>�\)>�\)>�\)>�\)>8Q�=��>8Q켣�	>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�=��>8Q�>8Q�=��=��>8Q�>8Q�>8Q�>8Q�>�\)>�\)>8Q�>8Q�>�\)>8Q�>8Q�>8Q�>8Q�=�����	>8Q�=��>8Q�=��>8Q�>8Q�>8Q�>8Q�>8Q�>�\)=��>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�=��=��>8Q�>8Q�>8Q�>8Q�>�\)>8Q�>8Q�>�\)>�\)>�\)>�\)>�\)>8Q�=�����	>8Q�=��>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>�\)>�\)>8Q켣�	=��>8Q켣�	=��=��>�\)>8Q�>�\)>�\)>\>�>\>�?z�?.{?.{?z�?.{?.{?G�?G�?G�?aG�?z�H?z�H?z�H?z�H?�=q?�
=?�
=?��
?��
?��
?���?���?���?�=q?�=q?�=q?�
=?�
=?�
=?�
=?��
?��
?��?��?��?�p�?�p�@�@�@�@�@�@�@�@Q�@Q�@Q�@�R@�R@�R@%�@+�@+�@+�@+�@1�@1�@8Q�@>�R@>�R@E�@E�@K�@Q�@Q�@XQ�@XQ�@^�R@e�@e�@q�@q�@q�@~�R@~�R@��\@��\@���@�(�@�(�@�\)@��\@�@���@�(�@�\)@��\@��\@�@���@�(�@�\)@��\@�@���@�(�@�\)@\@�@���@�(�@�\)@ҏ\@�@���@�\)@�\@�@���@�(�@�\)@�\@���@�(�@�\*AG�A�HAz�A{A�A
�HAz�A{A�AG�Az�A{A�AG�A�HA{A�A!G�A"�HA$z�A'�A)G�A*�HA.{A/�A1G�A2�HA6{A7�A9G�A<z�A>{AAG�AB�HADz�AF{AIG�AJ�HAN{AO�AQG�ATz�AV{AW�AZ�HA\z�A_�AaG�Adz�Af{AiG�Aj�HAlz�Ao�Ar�HAtz�Av{AyG�Az�HA|z�A�A�p�A�=pA��
A���A�p�A�
=A��
A�p�A�=pA��
A���A�=pA�
=A���A�p�A�
=A��
A�p�A�=pA��
A���A�=pA�
=A��
A�p�A�=pA��
A�p�A�=pA�
=A���A�p�A�
=A��
A�p�A�=pA�
=A���A�p�A�
=A��
A�p�A�=pA��
A���A�=pA�
=A���A�=pA�
=A���A�p�A�=pA��
A�p�A�=pA��
Aģ�A�=pA�
=Aȣ�A�p�A�
=A��
A�p�A�=pA��
AУ�A�=pA��
Aԣ�A�=pA�
=A��
A�p�A�=pA��
Aܣ�A�=pA�
=A��A�p�A�=pA�
=A��A�p�A�
=A��
A��A�p�A�
=A��
A��A�p�A�
=A��
A��A�p�A�
=A��
A���A�=pA�
=A��
A�p�A�=pA�
=A��
A�p�A�=pA�
=A��	B Q�B�B�B�B�RB�B�B�BQ�B�B�B�BQ�B�RB�B�BQ�B�RB	�B	�B
Q�B
�RB�B�BQ�B�RB�B�BQ�B�RB�B�B�B�RB�B�B�B�RB�B�B�B�RB�B�B�BQ�B�B�B�BQ�B�RB�B�BQ�B�RB�B�BQ�B�RB�B�B�BQ�B�RB�B�B Q�B �RB!�B!�B!�B"Q�B"�RB#�B#�B#�B#�B$Q�B$�RB%�B%�B%�B&Q�B&�RB&�RB'�B'�B'�B(Q�B(�RB(�RB)�B)�B)�B*Q�B*Q�B*�RB+�B+�B+�B+�B,Q�B,�RB-�B-�B-�B-�B.Q�B.�RB/�B/�B/�B/�B0Q�B0�RB0�RB1�B1�B1�B2Q�B2�RB2�RB3�B3�B3�B3�B4Q�B4�RB4�RB5�B5�B5�B6Q�B6Q�B6�RB7�B7�B7�B7�B8Q�B8�RB9�B9�B9�B9�B9�B:Q�B:�RB;�B;�B;�B;�B;�B<Q�B<�RB<�RB=�B=�B=�B=�B=�B>Q�B>Q�B>�RB?�B?�B?�B?�B?�B?�B@Q�B@Q�B@�RB@�RBA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BBQ�BBQ�BBQ�BBQ�BBQ�BB�RBB�RBB�RBB�RBB�RBC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BDQ�BDQ�BDQ�BDQ�BD�RBD�RBD�RBD�RBE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BFQ�BFQ�BFQ�BFQ�BFQ�BF�RBF�RBF�RBG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BHQ�BHQ�BHQ�BH�RBH�RBH�RBH�RBI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BJQ�BJQ�BJQ�BJQ�BJ�RBJ�RBJ�RBJ�RBJ�RBK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BLQ�BLQ�BLQ�BLQ�BL�RBL�RBL�RBL�RBL�RBL�RBM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BNQ�BNQ�BNQ�BNQ�BNQ�BN�RBN�RBN�RBN�RBN�RBN�RBO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BPQ�BPQ�BPQ�BPQ�BP�RBP�RBP�RBP�RBQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BRQ�BRQ�BRQ�BR�RBR�RBS�BS�BS�BS�BS�BS�BTQ�BTQ�BT�RBT�RBU�BU�BU�BU�BU�BVQ�BVQ�BV�RBV�RBW�BW�BW�BW�BW�BXQ�BXQ�BX�RBX�RBY�BY�BY�BY�BY�BY�BY�BY�BZQ�BZQ�BZQ�BZ�RBZ�RBZ�RB[�B[�B[�B[�B[�B[�B[�B[�B\Q�B\Q�B\Q�B\�RB\�RB]�B]�B]�B]�B]�B]�B]�B]�B^Q�B^Q�B^Q�B^�RB^�RB_�B_�B_�B_�B_�B_�B_�B_�B`Q�B`Q�B`Q�B`�RB`�RBa�Ba�Ba�Ba�Ba�Ba�Ba�BbQ�BbQ�BbQ�Bb�RBb�RBc�Bc�Bc�Bc�Bc�Bc�Bc�BdQ�BdQ�Bd�RBd�RBe�Be�Be�Be�Be�Be�Be�BfQ�Bf�RBf�RBg�Bg�Bg�Bg�Bg�Bg�BhQ�BhQ�Bh�RBh�RBi�Bi�Bi�Bi�Bi�Bi�Bi�BjQ�BjQ�Bj�RBj�RBk�Bk�Bk�Bk�Bk�BlQ�BlQ�Bl�RBm�Bm�Bm�Bm�Bm�BnQ�Bn�RBn�RBo�Bo�Bo�Bo�BpQ�Bp�RBq�Bq�Bq�Bq�BrQ�Br�RBs�Bs�Bs�Bs�BtQ�Bt�RBu�Bu�Bu�BvQ�Bv�RBw�Bw�Bw�BxQ�Bx�RBy�By�By�BzQ�Bz�RB{�B{�B{�B|Q�B|�RB}�B}�B}�B~Q�B~�RB�B�B�B�(�B�\)B��\B�B�(�B�\)B��\B�B���B�(�B�\)B��\B�B���B�\)B��\B�B���B�(�B�\)B��\B�B�(�B�\)B��\B�B���B�(�B�\)B��\B���B�(�B�\)B��\B�B���B�\)B��\B�B���B�(�B�\)B��\B�B���B�\)B��\B�B���B�(�B�\)B��\B�B���B�(�B�\)B��\B�B���B�(�B�\)B��\B�B���B�(�B�\)B��\B�B���B�(�B�\)B��\B�B���B�(�B�\)B��\B�B���B�(�B��\B�B���B�(�B�\)B��\B�B���B�(�B�\)B��\B�B���B�(�B��\B�B���B�(�B�\)B��\B�B���B�(�B�\)B�B���B�(�B�\)B��\B�B���B�(�B�\)B�B���B�(�B�\)B��\B�B���B�\)B�\)B�B���B�(�B�\)B��\B�B���B�(�B��\B�B���B�(�B�\)B��\B�B���B�\)B��\B�B���B�(�B�\)B��\B�B�(�B�\)B��\B�B���B�(�B��\B�B���B�(�B�\)B��\B�B�(�B�\)B��\B�B���B�(�B�\)B�B���B�(�B�\)B��\B�B���B�\)B��\B�B���B�(�B��\B�B���B�(�B�\)B��\B���B�(�B�\)B��\B���B�(�B�\)B��\B�B�(�B�\)B��\B�B�(�B�\)B��\B�B���B�(�B��\B�B���B�(�B��\B�B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�B��\B�B���B�(�B��\B�B���B�\)B��\B���B�(�B�\)B�B���B�(�B��\B�B���B�\)B��\B���B�(�B�\)B�B���B�(�B��\B�B�(�B�\)B��\B���B�(�B��\B�B���B�\)B��\B���B�(�B�\)B�B���B�(�B��\B�B���B�\)B��\B�B�(�B�\)B��\B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�B�\)Bď\B���B�(�B�\)Bŏ\B���B�(�B�\)BƏ\B���B�(�B�\)BǏ\B���B�(�B�\)Bȏ\B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�CI��CI�HCIǮCIǮCI�CI�{CIz�CIaHCIG�CI.CI{CH��CH�HCH�HCHǮCH�CH�{CHz�CHaHCHG�CH.CH{CG��CG�HCGǮCG�CG�{CGz�CGaHCGG�CG.CG{CF��CF�HCFǮCF�{CFz�CFaHCFG�CF.CF{CE��CE�HCEǮCE�{CEz�CEaHCEG�CE.CD��CD�HCDǮCD�CDz�CDaHCDG�CD.CC��CC�HCCǮCC�CCz�CCaHCCG�CC.CB��CB�HCBǮCB�CBz�CBaHCBG�CB.CA��CA�HCAǮCA�{CAz�CAaHCA.CA{C@��C@ǮC@�C@�{C@aHC@G�C@{C?��C?ǮC?�C?�{C?aHC?G�C?{C>��C>ǮC>�C>z�C>aHC>.C>{C=�HC=ǮC=�{C=aHC=G�C={C<��C<ǮC<�C<z�C<aHC<.C;��C;�HC;�C;z�C;aHC;.@1�@1�@8Q�@>�R@>�R@E�@E�@K�@Q�@Q�@XQ�@XQ�@^�R@e�@e�@q�@q�@q�@~�R@~�R@��\@��\@���@�(�@�(�@�\)@��\@�@���@�(�@�\)@��\@��\@�@���@�(�@�\)@��\@�@���@�(�@�\)@\@�@���@�(�@�\)@ҏ\@�@���@�\)@�\@�@���@�(�@�\)@�\@���@�(�@�\*AG�A�HAz�A{A�A
�HAz�A{A�AG�Az�A{A�AG�A�HA{A�A!G�A"�HA$z�A'�A)G�A*�HA.{A/�A1G�A2�HA6{A7�A9G�A<z�A>{AAG�AB�HADz�AF{AIG�AJ�HAN{AO�AQG�ATz�AV{AW�AZ�HA\z�A_�AaG�Adz�Af{AiG�Aj�HAlz�Ao�Ar�HAtz�Av{AyG�Az�HA|z�A�A�p�A�=pA��
A���A�p�A�
=A��
A�p�A�=pA��
A���A�=pA�
=A���A�p�A�
=A��
A�p�A�=pA��
A���A�=pA�
=A��
A�p�A�=pA��
A�p�A�=pA�
=A���A�p�A�
=A��
A�p�A�=pA�
=A���A�p�A�
=A��
A�p�A�=pA��
A���A�=pA�
=A���A�=pA�
=A���A�p�A�=pA��
A�p�A�=pA��
Aģ�A�=pA�
=Aȣ�A�p�A�
=A��
A�p�A�=pA��
AУ�A�=pA��
Aԣ�A�=pA�
=A��
A�p�A�=pA��
Aܣ�A�=pA�
=A��A�p�A�=pA�
=A��A�p�A�
=A��
A��A�p�A�
=A��
A��A�p�A�
=A��
A��A�p�A�
=A��
A���A�=pA�
=A��
A�p�A�=pA�
=A��
A�p�A�=pA�
=A��	B Q�B�B�B�B�RB�B�B�BQ�B�B�B�BQ�B�RB�B�BQ�B�RB	�B	�B
Q�B
�RB�B�BQ�B�RB�B�BQ�B�RB�B�B�B�RB�B�B�B�RB�B�B�B�RB�B�B�BQ�B�B�B�BQ�B�RB�B�BQ�B�RB�B�BQ�B�RB�B�B�BQ�B�RB�B�B Q�B �RB!�B!�B!�B"Q�B"�RB#�B#�B#�B#�B$Q�B$�RB%�B%�B%�B&Q�B&�RB&�RB'�B'�B'�B(Q�B(�RB(�RB)�B)�B)�B*Q�B*Q�B*�RB+�B+�B+�B+�B,Q�B,�RB-�B-�B-�B-�B.Q�B.�RB/�B/�B/�B/�B0Q�B0�RB0�RB1�B1�B1�B2Q�B2�RB2�RB3�B3�B3�B3�B4Q�B4�RB4�RB5�B5�B5�B6Q�B6Q�B6�RB7�B7�B7�B7�B8Q�B8�RB9�B9�B9�B9�B9�B:Q�B:�RB;�B;�B;�B;�B;�B<Q�B<�RB<�RB=�B=�B=�B=�B=�B>Q�B>Q�B>�RB?�B?�B?�B?�B?�B?�B@Q�B@Q�B@�RB@�RBA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BBQ�BBQ�BBQ�BBQ�BBQ�BB�RBB�RBB�RBB�RBB�RBC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BDQ�BDQ�BDQ�BDQ�BD�RBD�RBD�RBD�RBE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BFQ�BFQ�BFQ�BFQ�BFQ�BF�RBF�RBF�RBG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BHQ�BHQ�BHQ�BH�RBH�RBH�RBH�RBI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BJQ�BJQ�BJQ�BJQ�BJ�RBJ�RBJ�RBJ�RBJ�RBK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BLQ�BLQ�BLQ�BLQ�BL�RBL�RBL�RBL�RBL�RBL�RBM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BNQ�BNQ�BNQ�BNQ�BNQ�BN�RBN�RBN�RBN�RBN�RBN�RBO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BPQ�BPQ�BPQ�BPQ�BP�RBP�RBP�RBP�RBQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BRQ�BRQ�BRQ�BR�RBR�RBS�BS�BS�BS�BS�BS�BTQ�BTQ�BT�RBT�RBU�BU�BU�BU�BU�BVQ�BVQ�BV�RBV�RBW�BW�BW�BW�BW�BXQ�BXQ�BX�RBX�RBY�BY�BY�BY�BY�BY�BY�BY�BZQ�BZQ�BZQ�BZ�RBZ�RBZ�RB[�B[�B[�B[�B[�B[�B[�B[�B\Q�B\Q�B\Q�B\�RB\�RB]�B]�B]�B]�B]�B]�B]�B]�B^Q�B^Q�B^Q�B^�RB^�RB_�B_�B_�B_�B_�B_�B_�B_�B`Q�B`Q�B`Q�B`�RB`�RBa�Ba�Ba�Ba�Ba�Ba�Ba�BbQ�BbQ�BbQ�Bb�RBb�RBc�Bc�Bc�Bc�Bc�Bc�Bc�BdQ�BdQ�Bd�RBd�RBe�Be�Be�Be�Be�Be�Be�BfQ�Bf�RBf�RBg�Bg�Bg�Bg�Bg�Bg�BhQ�BhQ�Bh�RBh�RBi�Bi�Bi�Bi�Bi�Bi�Bi�BjQ�BjQ�Bj�RBj�RBk�Bk�Bk�Bk�Bk�BlQ�BlQ�Bl�RBm�Bm�Bm�Bm�Bm�BnQ�Bn�RBn�RBo�Bo�Bo�Bo�BpQ�Bp�RBq�Bq�Bq�Bq�BrQ�Br�RBs�Bs�Bs�Bs�BtQ�Bt�RBu�Bu�Bu�BvQ�Bv�RBw�Bw�Bw�BxQ�Bx�RBy�By�By�BzQ�Bz�RB{�B{�B{�B|Q�B|�RB}�B}�B}�B~Q�B~�RB�B�B�B�(�B�\)B��\B�B�(�B�\)B��\B�B���B�(�B�\)B��\B�B���B�\)B��\B�B���B�(�B�\)B��\B�B�(�B�\)B��\B�B���B�(�B�\)B��\B���B�(�B�\)B��\B�B���B�\)B��\B�B���B�(�B�\)B��\B�B���B�\)B��\B�B���B�(�B�\)B��\B�B���B�(�B�\)B��\B�B���B�(�B�\)B��\B�B���B�(�B�\)B��\B�B���B�(�B�\)B��\B�B���B�(�B�\)B��\B�B���B�(�B��\B�B���B�(�B�\)B��\B�B���B�(�B�\)B��\B�B���B�(�B��\B�B���B�(�B�\)B��\B�B���B�(�B�\)B�B���B�(�B�\)B��\B�B���B�(�B�\)B�B���B�(�B�\)B��\B�B���B�\)B�\)B�B���B�(�B�\)B��\B�B���B�(�B��\B�B���B�(�B�\)B��\B�B���B�\)B��\B�B���B�(�B�\)B��\B�B�(�B�\)B��\B�B���B�(�B��\B�B���B�(�B�\)B��\B�B�(�B�\)B��\B�B���B�(�B�\)B�B���B�(�B�\)B��\B�B���B�\)B��\B�B���B�(�B��\B�B���B�(�B�\)B��\B���B�(�B�\)B��\B���B�(�B�\)B��\B�B�(�B�\)B��\B�B�(�B�\)B��\B�B���B�(�B��\B�B���B�(�B��\B�B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�B��\B�B���B�(�B��\B�B���B�\)B��\B���B�(�B�\)B�B���B�(�B��\B�B���B�\)B��\B���B�(�B�\)B�B���B�(�B��\B�B�(�B�\)B��\B���B�(�B��\B�B���B�\)B��\B���B�(�B�\)B�B���B�(�B��\B�B���B�\)B��\B�B�(�B�\)B��\B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�B�\)Bď\B���B�(�B�\)Bŏ\B���B�(�B�\)BƏ\B���B�(�B�\)BǏ\B���B�(�B�\)Bȏ\B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�CI��CI�HCIǮCIǮCI�CI�{CIz�CIaHCIG�CI.CI{CH��CH�HCH�HCHǮCH�CH�{CHz�CHaHCHG�CH.CH{CG��CG�HCGǮCG�CG�{CGz�CGaHCGG�CG.CG{CF��CF�HCFǮCF�{CFz�CFaHCFG�CF.CF{CE��CE�HCEǮCE�{CEz�CEaHCEG�CE.CD��CD�HCDǮCD�CDz�CDaHCDG�CD.CC��CC�HCCǮCC�CCz�CCaHCCG�CC.CB��CB�HCBǮCB�CBz�CBaHCBG�CB.CA��CA�HCAǮCA�{CAz�CAaHCA.CA{C@��C@ǮC@�C@�{C@aHC@G�C@{C?��C?ǮC?�C?�{C?aHC?G�C?{C>��C>ǮC>�C>z�C>aHC>.C>{C=�HC=ǮC=�{C=aHC=G�C={C<��C<ǮC<�C<z�C<aHC<.C;��C;�HC;�C;z�C;aHC;.G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A��HA��HA��HA��TA���A�A֩�A֩�A֛�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֓uA֑hA֏\A֋DA։7AօAփAԬA�dZA�^5AӁA�33A�
=A�z�A�ZA���A¥�A��jA�1'A�%A���A�A�A��A��hA�+A��/A��wA��A��A�jA�hsA��
A��A�-A�ĜA�|�A���A��A�1'A��\A��;A���A��/A��A��wA��A� �A�|�A�"�A��hA��mA��mA��A�JA��A��A�ffA�7LA�/A�dZA��;A��A���A��A��A�^5A�7LA��uA�ZA�ƨA�t�A�"�A�I�A��HA��A�O�A�oA�`BA�p�A��#A���A�ZA��A��+A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AʸRAЇ+A�t�A�$�A֡�A�9XA���AыDA�ĜA���A���A͙�A�\)AվwAՏ\Aȟ�A�;dA�
=AƃAƥ�A��A�x�AҋDA�I�A�+A���AָRA���A�1'A�oA��A�A�A�&�A�ƨA�XA�S�AָRA��A���A��A�ƨA˃Aѩ�A֬A���A�=qA���A�&�A���A���A��mA�x�A�VA���A��A��A��mA��A�A�1'A�VA��A�^5A�33A�
=AǼjAƺ^A�Aˣ�A�\)A�+A�A��TA�;dA�/A�ffA�$�A�M�A�A�A̅A���A�jA�+A�/A���A�z�A�dZA� �A��AָRA�n�A��/A�bA�{A���A��A�oA��mA�33A��A˾wA�M�A�`BAɓuA�p�A�ƨAְ!A�A�A�G�A־wA�1A�C�A�t�A���A��A�oA�A�A�p�Aʲ-A�9XA��mA�dZA�ĜAҝ�A�ƨA�A���Aǰ!A��A�r�Aɺ^A���A��#A��A���A��TAƺ^A��Aӥ�A���A�VAѸRA�z�Aȝ�A�?}A���A���A�(�AȶFA�C�A���A���A֍PA��A�"�Aћ�A��A�Q�A̼jA�A�A���A���A֬Aɡ�A��A�ȴA���A�Q�A�K�A�x�A�ȴA�r�A�ZA�|�A�O�AָRA���AָRA�n�Aș�A�E�A���A���A�ȴA���A�A���A��A��#A�A���A�ƨA�ȴA֓uA�A�bA�XAՙ�A֩�A�O�A���A�$�A��#A�dZA�ƨAּjA�G�Aֲ-A҃A�t�A�r�A�9XA�ƨA���A�ĜA�ƨA�A���A�ȴA�ĜA�ƨA�ĜA־wA���Aֺ^A�A�ƨA���A���A���A���A���A���A�ƨA���A���A���A���A���A�ȴA��A�A���A���A�ȴA���A�ĜA־wA���A���A�ƨA���AּjA���Aְ!A���Aֺ^A���A���A���A�ȴA���A�ȴA���A���A�ƨA�dZA�Aֲ-A���A���A�ȴA�ƨA�A���A���A�ȴA���A�A���A�ƨA�ȴA�ȴA���A���A���A���A���A���A�ƨA���A���A���A���A���A���A���A���A���A�ƨA���A���A���A���A���A�ĜA���A�ȴA���A���A�ƨA�ȴA���A���A���A���A���A���A���A��
A���A�ƨA���A���A�ȴA���A���A���A�ȴA�ĜA���A���A���A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A��
A���A���A���A��A��
A���A���A��
A��
A���A���A���A��#A��A��
A��
A��
A��
A��A���A���A���A��
A���A���A���A���A���A���A���A���A��
A��
A��
A���A��
A��
A��A���A��
A��
A��
A���A���Aֲ-A���A���A���A��
A��A���A���A��
A���A��A��A���A���A��
A��A���A���A���A��
A��
A���A��#A��
A��#A��
A��#A��A��/A��#A��#A��
A���A��
A��A���A�?}A��
A��#A��;A��/A��A��/A��#A��
A��#A��A��A��A�&�A��
A���A��A���A���A��A��
A��
A��
A��A��
A���A��
A��A��A��A��#A��
A��
A��
A��
A��A��A��
A��;A��#A��/A��A��/A��;A��/A��/A��/A��A��A��A��/A��A��A��#A��;A��/A��#A��/A��#A��/A��/A��A��/A��#A��#A��/A��#A��#A��/A��#A��;A��#A��A��A��A��#A���A��#A��#A��#A��#A��A��#A��A��#A��
A��A��A��
A��/A��A��#A��A��A��#A��
A��A��/A��A��#A��#A��#A��A��#A��A��/A��A��
A��
A��A���A���A��#A��#A��#A��#A��A��#A��A��/A��A��#A��/A��A��
A��A��#A��A��A��A��
A��#A��#A��#A��
A��#A��
A��
A��#A��#A��/A��A��/A��/A��/A��#A���A���A���A���A���A���A���A���A���A�ƨA�ƨA���A���A���A���A��
A��
A��#A���A�ȴA�ƨA�ȴA���A�ĜA�ĜA�ƨA�ȴA���A�A�ƨA�A֥�A֣�A֥�A֡�A֡�A֣�A֣�A֡�A֣�A֡�A֧�A֩�A֧�A֥�A֡�A֣�A֣�A֡�A֣�A֧�A֩�A֬A֧�A֩�A֩�A֥�A֝�A֣�A֣�A֡�A֛�A֙�A֙�A֙�A֗�A֗�A֗�A֗�A֕�A֗�A֕�A֕�A֑hA֑hA֑hA֓uA֓uA֓uA֑hA֑hA֑hA֑hA֑hA֑hA֓uA֑hA֏\A֑hA֑hA֏\A֑hA֏\A֏\A֑hA֏\A֑hA֑hA֏\A֏\A֑hA֑hA֓uA֏\A֏\A֑hA֑hA֑hA֏\A֑hA֏\A֍PA֑hA֑hA֑hA֑hA֑hA֏\A֏\A֑hA֏\A֑hA֑hA֑hA֑hA֑hA֑hA֓uA֑hA֓uA֓uA֏\A֏\A֑hA֑hA֓uA֑hA֏\A֑hA֏\A֏\A֏\A֑hA֏\A֓uA֑hA֏\A֏\A֑hA֑hA֏\A֑hA֑hA֑hA֑hA֑hA֓uA֓uA֓uA֑hA֑hA֑hA֓uA֑hA֑hA֑hA֑hA֑hA֑hA֑hA֑hA֑hA֓uA֑hA֑hA֑hA֑hA֑hA֑hA֑hA֑hA֏\A֑hA֏\A֏\A֋DA֍PA֍PA։7A֋DAև+A֋DA֍PA֋DA։7A֍PA֑hA֑hA֏\A֑hA֏\A֏\A֏\A֍PA֏\A֏\A֏\A֍PA֏\A֍PA֏\A֍PA֋DA֍PA֍PA֍PA֋DA֍PA։7A֋DA֋DA֋DA֋DA֋DA։7A֋DA։7A։7A։7A։7A։7A։7A։7Aև+Aև+Aև+Aև+Aև+Aև+Aև+Aև+Aև+Aև+Aև+Aև+Aև+Aև+AօAև+Aև+A։7Aև+Aև+A։7Aև+AօAցAփAփAփAփAփAփAցAփAցAցAցAցAցAցAցAցAցAցA�~�A�~�A�~�A�~�A�~�A�~�AցAցAցAցAցAցAփAցAցAցAցAցAփAցAցA�~�AփA�~�AցAցAցAցAցAցAցAցAցAցAցA�~�A�|�A�|�A�z�A�z�A�|�A�|�A�z�A�~�A�|�A�|�A�z�A�|�A�z�A�z�A�z�A�z�A�z�A�x�A�|�A�z�A�|�A�z�A�|�A�x�A�z�A�z�A�z�A�z�A�z�A�x�A�x�A�x�A�x�A�x�A�v�A�t�A�v�A�v�A�t�A�t�A�t�A�t�A�r�A�p�A�p�A�n�A�p�A�n�A�p�A�n�A�p�A�n�A�n�A�l�A�n�A�p�A�l�A�l�A�n�A�n�A�l�A�l�A�p�A�l�A�n�A�l�A�r�A�n�A�jA�hsA�l�A�jA�hsA�ffA�ffA�dZA�ffA�bNA�`BA�bNA�^5A�bNA�bNA�`BA�bNA�`BA�`BA�`BA�bNA�^5A�`BA�^5A�^5A�^5A�bNA�`BA�^5A�^5A�`BA�`BA�`BA�`BA�`BA�`BA�\)A�\)A�\)A�^5A�^5A�\)A�^5A�\)A�\)A�\)A�^5A�\)A�`BA�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�`BA�`BA�`BA�bNA�`BA�`BA�bNA�`BA�bNA�^5A�^5A�`BA�`BA�bNA�^5A�^5A�^5A�^5A�^5A�^5A�^5A�\)A�^5A�^5A�\)A�^5A�^5A�`BA�\)A�^5A�^5A�^5A�^5A�^5A�\)A�^5A�\)A�^5A�\)A�^5A�^5A�^5A�^5A�^5A�\)A�\)A�^5A�`BA�`BA�`BA�`BA�`BA�`BA�^5A�bNA�`BA�bNA�`BA�`BA�`BA�bNA�bNA�`BA�bNA�bNA�bNA�bNA�bNA�`BA�bNA�bNA�`BA�bNA�`BA�dZA�bNA�bNA�bNA�bNA�dZA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�XA�O�A�\)A�C�A�?}A�1'A�1'A�1'A�5?A�=qA�7LA�/A�(�A�-A� �A��A� �A�"�A��A�-A�$�A��A�(�A��A��A��A�oA���A��yAծA�dZAԝ�A�A�AӸRA��/A�K�A�%A���A�hsA�I�A�{A��A��;A��#A���AиRAЮAЕ�A�x�A�n�A�jA�bNA�VA�E�A�33A� �A�bA�A�  A���A���A���A��A϶FAϕ�A�v�A�n�A�jA�`BA�/A�&�A�$�A��A��A��A��A�oA�oA�bA�bA�
=A�
=A�JA�
=A�
=A�JA�JA�JA�JA�oA�VA�VA�oA�bA�bA���AάAΥ�AΣ�AΣ�AΡ�AΡ�AΛ�AΛ�AΛ�AΕ�AΏ\A΋DA·+A΁A�|�A�v�A�r�A�r�A�t�A�r�A�l�A�`BA�S�A�9XA�A͸RA͗�A�t�A�E�A�oA��A̬Ȁ\A�|�A�t�A�K�A�$�A�
=A��A��;A˙�A�ȴA˺^Aˣ�A˕�AˋDAˁA�p�A�p�A�n�A�p�A�r�A�r�A�r�A�ffA�dZA�\)A�XA�S�A�?}A�9XA�7LA�5?A�1'A�(�A� �A��A�bA���A��A�ƨA�t�A��`A�9XAȶFA�{A���AǕ�AǓuAǇ+A�bNA�K�A�JAƇ+A�hsA���A��AōPA�`BA�bA��#Aİ!AāA�C�A�1'A�+A�+A�+A�$�A�$�A� �A��A��A��A��A�oA�
=A�%A�A�  A���A��mAô9AÙ�A�r�A�`BA�^5A�XA�S�A�O�A�M�A�E�A�&�A�VA�  A��A���A�ƨA´9AA�ZA�/A��A�A���A���A��A��yA��mA��;A��/A��#A��A���A���A���A�A��9A���A��7A��A�z�A�t�A�r�A�n�A�l�A�bNA�`BA�^5A�XA�K�A�?}A�9XA�$�A��A�%A��A��^A���A���A�t�A�jA�n�A�^5A�-A��A��A�ƨA�ȴA��A��RA��
A��FA��!A�hsA���A���A�~�A�|�A��A�p�A�n�A�K�A�+A�  A��yA��#A��jA��!A���A��PA��A�z�A�x�A�v�A�v�A�t�A�jA�hsA�hsA�ffA�bNA�ZA�XA�S�A�O�A�K�A�K�A�G�A�C�A�A�A�=qA�;dA�9XA�5?A�1'A�&�A��A��A�1A�  A�  A���A���A���A��A��A��A��A��`A��`A��`A��`A��HA��;A��/A��/A��#A��
A���A�ƨA���A���A��hA��DA��A�z�A�x�A�v�A�r�A�r�A�t�A�r�A�p�A�l�A�hsA�dZA�ffA�`BA�XA�G�A�7LA� �A�VA�%A�VA�{A�oA�JA���A���A���A�  A�
=A��A��TA��#A��/A��/A��#A���A�ȴA�ĜA�ƨA���A���A���A���A���A���A���A��jA���A��wA��wA��jA��wA��!A��9A��-A��9A��FA��^A��^A��^A��^A��RA��FA��FA���A��uA��\A��7A��+A��A�|�A�hsA�`BA�\)A�ZA�O�A�;dA�/A�$�A�"�A�$�A�&�A� �A���A�ƨA�x�A�z�A���A�x�A�jA�S�A�1'A��A��`A��/A���A��RA���A���A�|�A�7LA�"�A�JA���A��;A�ȴA��^A��9A���A���A��PA�~�A�p�A�hsA�XA�K�A�E�A�?}A�7LA�&�A�bA�%A�A�A���A���A���A��HA���A�A�A��wA��RA��A���A���A��DA�~�A�t�A�jA�XA�?}A��A��A���A�r�A�S�A�S�A�VA�XA�XA�\)A�\)A�ZA�XA�VA�O�A�E�A�=qA�+A�+A��A�{A�bA�JA���A���A��A��;A��;A��/A��
A��A���A�ĜA���A��wA��wA��RA��9A��A��A��A�jA�\)A�K�A��A�hsA�ffA�t�A���A���A���A��-A��FA���A�A��A�%A��A��A��`A��A��A�$�A�A�A�9XA�S�A�\)A�XA�~�A�x�A�t�A�r�A�|�A�z�A�z�A��uA��^A��A���A���A��#A���A��A��`A���A�VA�bA�bA�1A�oA�
=A�oA��A�=qA�5?A�/A�I�A�n�A�Q�A�S�A�`BA��\A�ffA�`BA�S�A�ZA��A���A���A��A��;A���A�oA��A�A�jA�\)A�|�A�v�A��A�I�A���A���A��A��A���A�7LA��A�Q�A��hA�dZA�=qA���A��wA��#A��A�;dA�E�A�hsA���A�v�A�hsA���A��FA�  A�7LA�7LA�^5A�G�A�K�A�hsA���A��hA�ĜA��#A��A�oA��A�7LA�-A�?}A��HA��TA��HA��;A��TA��TA��HA��HA��HA��HA��;A��HA��;A��;A��HA��;A��TA��HA��`A��HA��TA��TA��TA��TA��HA��HA��HA��/A��;A��;A��;A��;A��;A��;A��HA��;A��TA��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��TA��;A��mA��TA��TA��HA��;A��HA��TA��TA��mA��TA��`A��`A��`A��`A��#A��;A��#A��A��;A��
A���A���A���A���A���A���A���A��#A��A��#A��/A��/A���A���A���A�ȴA�ĜA�ȴA�ƨA�ĜA���A���A���A���Aְ!A֬A֩�A֬A֩�A֧�A֥�A֩�A֧�A֥�A֬Aְ!A֮A֮A֧�A֩�A֧�A֧�A֩�A֩�Aְ!Aְ!Aְ!Aְ!A֮A֬A֟�A֧�A֮A֣�A֣�A֡�A֟�A֟�A֝�A֙�A֙�A֙�A֙�A֝�A֗�A֗�A֙�A֗�A֗�A֗�A֕�A֗�A֕�A֕�A֕�A֕�A֕�A֗�A֗�A֕�A֕�A֕�A֓uA֕�A֓uA֓uA֕�A֕�A֓uA֓uA֕�A֕�A֓uA֗�A֗�A֓uA֓uA֕�A֕�A֓uA֕�A֕�A֕�A֕�A֓uA֕�A֕�A֕�A֕�A֗�A֕�A֕�A֕�A֕�A֕�A֕�A֗�A֕�A֕�A֕�A֙�A֗�A֕�A֗�A֗�A֗�A֕�A֕�A֗�A֕�A֓uA֕�A֕�A֓uA֓uA֓uA֕�A֕�A֗�A֕�A֕�A֓uA֕�A֕�A֕�A֗�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֗�A֗�A֕�A֕�A֕�A֓uA֓uA֕�A֕�A֕�A֗�A֕�A֓uA֕�A֕�A֕�A֕�A֕�A֕�A֕�A֓uA֓uA֓uA֕�A֕�A֓uA֑hA֓uA֋DA֍PA֋DA֏\A֏\A֍PA֋DA֍PA֑hA֑hA֓uA֑hA֑hA֕�A֓uA֓uA֓uA֑hA֑hA֓uA֓uA֏\A֑hA֑hA֑hA֑hA֏\A֏\A֍PA֑hA֑hA֍PA֏\A֏\A֏\A֏\A֍PA֍PA֍PA֍PA֍PA֍PA֍PA֍PA֋DA֋DA։7A֋DA֋DA֋DA֍PA֋DA֍PA֋DA֍PA֍PA֋DA։7A֋DA֋DA։7A։7A։7A֋DA֋DA֋DA֋DA֍PA֋DAև+AօAև+AօAօAօAօAօAօAօAօAօAօAփAփAփAփAօAփAօAօAցAփAփAօAօAփAօAօAօAօAօAօAօAօAօAօAօAօAփAօAօAօAօAփAփAօAօAօAօAփAօAօAփA�~�A�|�A�~�A�~�A�~�AցA�|�A�~�A�~�AփA�~�A�~�A�|�A�|�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�~�AցA�~�A�~�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�z�A�|�A�|�A�z�A�x�A�x�A�z�A�z�A�z�A�z�A�z�A�t�A�r�A�t�A�r�A�r�A�r�A�r�A�p�A�p�A�r�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�r�A�p�A�p�A�p�A�p�A�p�A�p�A�n�A�p�A�p�A�p�A�p�A�n�A�p�A�l�A�jA�ffA�`BA�bNA�`BA�`BA�bNA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�^5A�`BA�`BA�bNA�bNA�bNA�bNA�dZA�bNA�`BA�`BA�`BA�^5A�^5A�^5A�bNA�bNA�bNA�dZA�`BA�`BA�^5A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�bNA�`BA�dZA�dZA�bNA�bNA�bNA�`BA�bNA�`BA�bNA�`BA�`BA�`BA�^5A�`BA�`BA�`BA�^5A�`BA�`BA�`BA�^5A�^5A�^5A�`BA�^5A�`BA�`BA�`BA�^5A�`BA�`BA�`BA�^5A�^5A�`BA�`BA�`BA�^5A�`BA�bNA�bNA�bNA�bNA�bNA�bNA�^5A�bNA�`BA�bNA�bNA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�dZA�dZA�dZA�dZA�dZA�ffA�bNA�ffA�ffA�ffA�dZA�dZA�dZA�bNA�dZA�ffA�dZA�ffA�ffA�dZA�dZA�dZA�bNA�^5A�VA�O�A�E�A�E�A�C�A�A�A�?}A�?}A�C�A�C�A�E�A�E�A�A�A�=qA�=qA�7LA�5?A�5?A�5?A�5?A�33A�1'A�/A�+A�oA���A��A՟�A�(�A԰!A�bNAӴ9A��A�XA��A���AуA�dZA�?}A�
=A��`A��;A��A�ƨAиRAЬAБhA�x�A�p�A�jA�`BA�S�A�A�A�/A��A�bA�%A�A�  A���A��A�Aϝ�A�~�A�r�A�l�A�^5A�=qA�/A�$�A�"�A� �A��A��A��A�oA�oA�bA�VA�JA�VA�VA�JA�JA�VA�VA�bA�oA��A�oA�oA�{A�bA��TAΰ!AΧ�AΧ�AΥ�AΥ�AΡ�AΡ�AΡ�AΝ�AΗ�AΕ�AΑhA΋DA΅A�~�A�x�A�v�A�v�A�v�A�t�A�l�A�^5A�O�A�1'A��AͮA͇+A�ffA�-A��A̺^A̟�A̋DA�v�A�XA�1'A�oA�A��yA��;A���A���A˴9A˙�AˍPA�x�A�~�A�v�A�v�A�x�A�x�A�v�A�x�A�r�A�hsA�ffA�\)A�XA�A�A�;dA�;dA�9XA�33A�/A�(�A��A�bA�  A��Aʺ^A�XA���A��AȁA���A���AǓuAǑhA�~�A�`BA�?}A���A�|�A�K�A��A�ĜAōPA�M�A�
=A���Aģ�A�ffA�=qA�1'A�/A�-A�-A�(�A�$�A�"�A��A��A��A��A�bA�
=A�1A�A�  A���A���AìAÇ+A�jA�`BA�^5A�XA�S�A�Q�A�M�A�9XA��A�bA���A��HA���A¾wA¡�A�t�A�K�A�&�A�oA�A�  A���A��A��A��mA��TA��;A��/A��A��
A���A���A�A��!A���A��+A��A�z�A�x�A�t�A�p�A�jA�hsA�bNA�^5A�XA�K�A�C�A�7LA�1'A�+A�&�A��A�oA�A���A��A��A��mA��/A���A��FA���A��A�l�A�C�A�&�A�bA�  A��`A���A�ƨA��9A���A��7A��+A�x�A�l�A�M�A�33A�1A��A���A��^A��A���A��DA��A�~�A�z�A�z�A�x�A�t�A�l�A�jA�jA�ffA�dZA�\)A�XA�VA�S�A�Q�A�K�A�G�A�C�A�C�A�?}A�=qA�;dA�5?A�+A��A��A��A�1A�A�A�A���A���A���A���A��A��yA��A��mA��mA��mA��`A��HA��/A��/A��#A���A���A���A���A���A��uA��7A��A�|�A�z�A�v�A�t�A�v�A�t�A�t�A�r�A�l�A�hsA�hsA�ffA�VA�S�A�?}A�-A�$�A�oA�bA�{A��A�bA�JA���A�1A�
=A�bA���A��TA��;A��;A��;A��HA��TA���A���A���A���A���A��
A��
A��
A���A�ĜA�A�A���A���A���A��wA��9A��-A��9A��FA��^A���A��jA��wA���A�ƨA�ƨA��wA��jA��A��uA���A��hA��\A��A�z�A�hsA�bNA�ffA�XA�E�A�=qA�5?A�/A�(�A�/A�1'A�$�A�"�A� �A��A�oA���A�ƨA��uA�G�A�%A��mA��TA���A���A��!A���A���A�hsA�/A�&�A�JA���A��;A���A��wA��^A���A���A��PA��A�p�A�bNA�Q�A�K�A�C�A�;dA�1'A��A�VA�
=A�%A�A���A���A��A��HA���A�ĜA�ĜA�A��FA���A���A���A��DA��A�x�A�l�A�XA�;dA�bA���A���A�hsA�XA�XA�ZA�XA�\)A�^5A�`BA�^5A�ZA�XA�O�A�E�A�5?A�/A�+A��A��A�oA�JA���A��A��A��TA��HA��;A��#A���A�ȴA�ƨA���A��wA��jA��FA��9A��9A��-A�`BA�^5A�t�A�|�A��A��A��hA���A��-A���A��A���A���A���A�A���A�%A��A�&�A��A�$�A�33A�33A�5?A�K�A�jA�bNA�v�A�z�A��A��DA��hA���A��hA��hA���A��A��-A���A�ĜA���A���A��yA��A���A��A���A���A�A��A�oA��A��A�(�A�/A�;dA�I�A�I�A�M�A�n�A�p�A�ffA�l�A��A��A��uA���A���A�ƨA�A��A�E�A�p�A�t�A�x�A���A��A�C�A��7A���A��^A��mA��A��;A���A�JA��A�33A�$�A�G�A�l�A���A�ȴA�A�A�A�`BA�r�A���A�ȴA���A�
=A�bA��A�+A�M�A�bNA�t�A��A���A���A��A��FA��wA���A��/A��mA��A�VA�hsA��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�A��HA��HA��HA��TA���A�A֩�A֩�A֛�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֓uA֑hA֏\A֋DA։7AօAփAԬA�dZA�^5AӁA�33A�
=A�z�A�ZA���A¥�A��jA�1'A�%A���A�A�A��A��hA�+A��/A��wA��A��A�jA�hsA��
A��A�-A�ĜA�|�A���A��A�1'A��\A��;A���A��/A��A��wA��A� �A�|�A�"�A��hA��mA��mA��A�JA��A��A�ffA�7LA�/A�dZA��;A��A���A��A��A�^5A�7LA��uA�ZA�ƨA�t�A�"�A�I�A��HA��A�O�A�oA�`BA�p�A��#A���A�ZA��A��+A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AʸRAЇ+A�t�A�$�A֡�A�9XA���AыDA�ĜA���A���A͙�A�\)AվwAՏ\Aȟ�A�;dA�
=AƃAƥ�A��A�x�AҋDA�I�A�+A���AָRA���A�1'A�oA��A�A�A�&�A�ƨA�XA�S�AָRA��A���A��A�ƨA˃Aѩ�A֬A���A�=qA���A�&�A���A���A��mA�x�A�VA���A��A��A��mA��A�A�1'A�VA��A�^5A�33A�
=AǼjAƺ^A�Aˣ�A�\)A�+A�A��TA�;dA�/A�ffA�$�A�M�A�A�A̅A���A�jA�+A�/A���A�z�A�dZA� �A��AָRA�n�A��/A�bA�{A���A��A�oA��mA�33A��A˾wA�M�A�`BAɓuA�p�A�ƨAְ!A�A�A�G�A־wA�1A�C�A�t�A���A��A�oA�A�A�p�Aʲ-A�9XA��mA�dZA�ĜAҝ�A�ƨA�A���Aǰ!A��A�r�Aɺ^A���A��#A��A���A��TAƺ^A��Aӥ�A���A�VAѸRA�z�Aȝ�A�?}A���A���A�(�AȶFA�C�A���A���A֍PA��A�"�Aћ�A��A�Q�A̼jA�A�A���A���A֬Aɡ�A��A�ȴA���A�Q�A�K�A�x�A�ȴA�r�A�ZA�|�A�O�AָRA���AָRA�n�Aș�A�E�A���A���A�ȴA���A�A���A��A��#A�A���A�ƨA�ȴA֓uA�A�bA�XAՙ�A֩�A�O�A���A�$�A��#A�dZA�ƨAּjA�G�Aֲ-A҃A�t�A�r�A�9XA�ƨA���A�ĜA�ƨA�A���A�ȴA�ĜA�ƨA�ĜA־wA���Aֺ^A�A�ƨA���A���A���A���A���A���A�ƨA���A���A���A���A���A�ȴA��A�A���A���A�ȴA���A�ĜA־wA���A���A�ƨA���AּjA���Aְ!A���Aֺ^A���A���A���A�ȴA���A�ȴA���A���A�ƨA�dZA�Aֲ-A���A���A�ȴA�ƨA�A���A���A�ȴA���A�A���A�ƨA�ȴA�ȴA���A���A���A���A���A���A�ƨA���A���A���A���A���A���A���A���A���A�ƨA���A���A���A���A���A�ĜA���A�ȴA���A���A�ƨA�ȴA���A���A���A���A���A���A���A��
A���A�ƨA���A���A�ȴA���A���A���A�ȴA�ĜA���A���A���A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A��
A���A���A���A��A��
A���A���A��
A��
A���A���A���A��#A��A��
A��
A��
A��
A��A���A���A���A��
A���A���A���A���A���A���A���A���A��
A��
A��
A���A��
A��
A��A���A��
A��
A��
A���A���Aֲ-A���A���A���A��
A��A���A���A��
A���A��A��A���A���A��
A��A���A���A���A��
A��
A���A��#A��
A��#A��
A��#A��A��/A��#A��#A��
A���A��
A��A���A�?}A��
A��#A��;A��/A��A��/A��#A��
A��#A��A��A��A�&�A��
A���A��A���A���A��A��
A��
A��
A��A��
A���A��
A��A��A��A��#A��
A��
A��
A��
A��A��A��
A��;A��#A��/A��A��/A��;A��/A��/A��/A��A��A��A��/A��A��A��#A��;A��/A��#A��/A��#A��/A��/A��A��/A��#A��#A��/A��#A��#A��/A��#A��;A��#A��A��A��A��#A���A��#A��#A��#A��#A��A��HA��TA��HA��;A��TA��TA��HA��HA��HA��HA��;A��HA��;A��;A��HA��;A��TA��HA��`A��HA��TA��TA��TA��TA��HA��HA��HA��/A��;A��;A��;A��;A��;A��;A��HA��;A��TA��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��TA��;A��mA��TA��TA��HA��;A��HA��TA��TA��mA��TA��`A��`A��`A��`A��#A��;A��#A��A��;A��
A���A���A���A���A���A���A���A��#A��A��#A��/A��/A���A���A���A�ȴA�ĜA�ȴA�ƨA�ĜA���A���A���A���Aְ!A֬A֩�A֬A֩�A֧�A֥�A֩�A֧�A֥�A֬Aְ!A֮A֮A֧�A֩�A֧�A֧�A֩�A֩�Aְ!Aְ!Aְ!Aְ!A֮A֬A֟�A֧�A֮A֣�A֣�A֡�A֟�A֟�A֝�A֙�A֙�A֙�A֙�A֝�A֗�A֗�A֙�A֗�A֗�A֗�A֕�A֗�A֕�A֕�A֕�A֕�A֕�A֗�A֗�A֕�A֕�A֕�A֓uA֕�A֓uA֓uA֕�A֕�A֓uA֓uA֕�A֕�A֓uA֗�A֗�A֓uA֓uA֕�A֕�A֓uA֕�A֕�A֕�A֕�A֓uA֕�A֕�A֕�A֕�A֗�A֕�A֕�A֕�A֕�A֕�A֕�A֗�A֕�A֕�A֕�A֙�A֗�A֕�A֗�A֗�A֗�A֕�A֕�A֗�A֕�A֓uA֕�A֕�A֓uA֓uA֓uA֕�A֕�A֗�A֕�A֕�A֓uA֕�A֕�A֕�A֗�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֗�A֗�A֕�A֕�A֕�A֓uA֓uA֕�A֕�A֕�A֗�A֕�A֓uA֕�A֕�A֕�A֕�A֕�A֕�A֕�A֓uA֓uA֓uA֕�A֕�A֓uA֑hA֓uA֋DA֍PA֋DA֏\A֏\A֍PA֋DA֍PA֑hA֑hA֓uA֑hA֑hA֕�A֓uA֓uA֓uA֑hA֑hA֓uA֓uA֏\A֑hA֑hA֑hA֑hA֏\A֏\A֍PA֑hA֑hA֍PA֏\A֏\A֏\A֏\A֍PA֍PA֍PA֍PA֍PA֍PA֍PA֍PA֋DA֋DA։7A֋DA֋DA֋DA֍PA֋DA֍PA֋DA֍PA֍PA֋DA։7A֋DA֋DA։7A։7A։7A֋DA֋DA֋DA֋DA֍PA֋DAև+AօAև+AօAօAօAօAօAօAօAօAօAօAփAփAփAփAօAփAօAօAցAփAփAօAօAփAօAօAօAօAօAօAօAօAօAօAօAօAփAօAօAօAօAփAփAօAօAօAօAփAօAօAփA�~�A�|�A�~�A�~�A�~�AցA�|�A�~�A�~�AփA�~�A�~�A�|�A�|�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�~�AցA�~�A�~�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�z�A�|�A�|�A�z�A�x�A�x�A�z�A�z�A�z�A�z�A�z�A�t�A�r�A�t�A�r�A�r�A�r�A�r�A�p�A�p�A�r�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�r�A�p�A�p�A�p�A�p�A�p�A�p�A�n�A�p�A�p�A�p�A�p�A�n�A�p�A�l�A�jA�ffA�`BA�bNA�`BA�`BA�bNA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�^5A�`BA�`BA�bNA�bNA�bNA�bNA�dZA�bNA�`BA�`BA�`BA�^5A�^5A�^5A�bNA�bNA�bNA�dZA�`BA�`BA�^5A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�bNA�`BA�dZA�dZA�bNA�bNA�bNA�`BA�bNA�`BA�bNA�`BA�`BA�`BA�^5A�`BA�`BA�`BA�^5A�`BA�`BA�`BA�^5A�^5A�^5A�`BA�^5A�`BA�`BA�`BA�^5A�`BA�`BA�`BA�^5A�^5A�`BA�`BA�`BA�^5A�`BA�bNA�bNA�bNA�bNA�bNA�bNA�^5A�bNA�`BA�bNA�bNA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�dZA�dZA�dZA�dZA�dZA�ffA�bNA�ffA�ffA�ffA�dZA�dZA�dZA�bNA�dZA�ffA�dZA�ffA�ffA�dZA�dZA�dZA�bNA�^5A�VA�O�A�E�A�E�A�C�A�A�A�?}A�?}A�C�A�C�A�E�A�E�A�A�A�=qA�=qA�7LA�5?A�5?A�5?A�5?A�33A�1'A�/A�+A�oA���A��A՟�A�(�A԰!A�bNAӴ9A��A�XA��A���AуA�dZA�?}A�
=A��`A��;A��A�ƨAиRAЬAБhA�x�A�p�A�jA�`BA�S�A�A�A�/A��A�bA�%A�A�  A���A��A�Aϝ�A�~�A�r�A�l�A�^5A�=qA�/A�$�A�"�A� �A��A��A��A�oA�oA�bA�VA�JA�VA�VA�JA�JA�VA�VA�bA�oA��A�oA�oA�{A�bA��TAΰ!AΧ�AΧ�AΥ�AΥ�AΡ�AΡ�AΡ�AΝ�AΗ�AΕ�AΑhA΋DA΅A�~�A�x�A�v�A�v�A�v�A�t�A�l�A�^5A�O�A�1'A��AͮA͇+A�ffA�-A��A̺^A̟�A̋DA�v�A�XA�1'A�oA�A��yA��;A���A���A˴9A˙�AˍPA�x�A�~�A�v�A�v�A�x�A�x�A�v�A�x�A�r�A�hsA�ffA�\)A�XA�A�A�;dA�;dA�9XA�33A�/A�(�A��A�bA�  A��Aʺ^A�XA���A��AȁA���A���AǓuAǑhA�~�A�`BA�?}A���A�|�A�K�A��A�ĜAōPA�M�A�
=A���Aģ�A�ffA�=qA�1'A�/A�-A�-A�(�A�$�A�"�A��A��A��A��A�bA�
=A�1A�A�  A���A���AìAÇ+A�jA�`BA�^5A�XA�S�A�Q�A�M�A�9XA��A�bA���A��HA���A¾wA¡�A�t�A�K�A�&�A�oA�A�  A���A��A��A��mA��TA��;A��/A��A��
A���A���A�A��!A���A��+A��A�z�A�x�A�t�A�p�A�jA�hsA�bNA�^5A�XA�K�A�C�A�7LA�1'A�+A�&�A��A�oA�A���A��A��A��mA��/A���A��FA���A��A�l�A�C�A�&�A�bA�  A��`A���A�ƨA��9A���A��7A��+A�x�A�l�A�M�A�33A�1A��A���A��^A��A���A��DA��A�~�A�z�A�z�A�x�A�t�A�l�A�jA�jA�ffA�dZA�\)A�XA�VA�S�A�Q�A�K�A�G�A�C�A�C�A�?}A�=qA�;dA�5?A�+A��A��A��A�1A�A�A�A���A���A���A���A��A��yA��A��mA��mA��mA��`A��HA��/A��/A��#A���A���A���A���A���A��uA��7A��A�|�A�z�A�v�A�t�A�v�A�t�A�t�A�r�A�l�A�hsA�hsA�ffA�VA�S�A�?}A�-A�$�A�oA�bA�{A��A�bA�JA���A�1A�
=A�bA���A��TA��;A��;A��;A��HA��TA���A���A���A���A���A��
A��
A��
A���A�ĜA�A�A���A���A���A��wA��9A��-A��9A��FA��^A���A��jA��wA���A�ƨA�ƨA��wA��jA��A��uA���A��hA��\A��A�z�A�hsA�bNA�ffA�XA�E�A�=qA�5?A�/A�(�A�/A�1'A�$�A�"�A� �A��A�oA���A�ƨA��uA�G�A�%A��mA��TA���A���A��!A���A���A�hsA�/A�&�A�JA���A��;A���A��wA��^A���A���A��PA��A�p�A�bNA�Q�A�K�A�C�A�;dA�1'A��A�VA�
=A�%A�A���A���A��A��HA���A�ĜA�ĜA�A��FA���A���A���A��DA��A�x�A�l�A�XA�;dA�bA���A���A�hsA�XA�XA�ZA�XA�\)A�^5A�`BA�^5A�ZA�XA�O�A�E�A�5?A�/A�+A��A��A�oA�JA���A��A��A��TA��HA��;A��#A���A�ȴA�ƨA���A��wA��jA��FA��9A��9A��-A�`BA�^5A�t�A�|�A��A��A��hA���A��-A���A��A���A���A���A�A���A�%A��A�&�A��A�$�A�33A�33A�5?A�K�A�jA�bNA�v�A�z�A��A��DA��hA���A��hA��hA���A��A��-A���A�ĜA���A���A��yA��A���A��A���A���A�A��A�oA��A��A�(�A�/A�;dA�I�A�I�A�M�A�n�A�p�A�ffA�l�A��A��A��uA���A���A�ƨA�A��A�E�A�p�A�t�A�x�A���A��A�C�A��7A���A��^A��mA��A��;A���A�JA��A�33A�$�A�G�A�l�A���A�ȴA�A�A�A�`BA�r�A���A�ȴA���A�
=A�bA��A�+A�M�A�bNA�t�A��A���A���A��A��FA��wA���A��/A��mA��A�VA�hsA��A��HA��TA��HA��;A��TA��TA��HA��HA��HA��HA��;A��HA��;A��;A��HA��;A��TA��HA��`A��HA��TA��TA��TA��TA��HA��HA��HA��/A��;A��;A��;A��;A��;A��;A��HA��;A��TA��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��TA��;A��mA��TA��TA��HA��;A��HA��TA��TA��mA��TA��`A��`A��`A��`A��#A��;A��#A��A��;A��
A���A���A���A���A���A���A���A��#A��A��#A��/A��/A���A���A���A�ȴA�ĜA�ȴA�ƨA�ĜA���A���A���A���Aְ!A֬A֩�A֬A֩�A֧�A֥�A֩�A֧�A֥�A֬Aְ!A֮A֮A֧�A֩�A֧�A֧�A֩�A֩�Aְ!Aְ!Aְ!Aְ!A֮A֬A֟�A֧�A֮A֣�A֣�A֡�A֟�A֟�A֝�A֙�A֙�A֙�A֙�A֝�A֗�A֗�A֙�A֗�A֗�A֗�A֕�A֗�A֕�A֕�A֕�A֕�A֕�A֗�A֗�A֕�A֕�A֕�A֓uA֕�A֓uA֓uA֕�A֕�A֓uA֓uA֕�A֕�A֓uA֗�A֗�A֓uA֓uA֕�A֕�A֓uA֕�A֕�A֕�A֕�A֓uA֕�A֕�A֕�A֕�A֗�A֕�A֕�A֕�A֕�A֕�A֕�A֗�A֕�A֕�A֕�A֙�A֗�A֕�A֗�A֗�A֗�A֕�A֕�A֗�A֕�A֓uA֕�A֕�A֓uA֓uA֓uA֕�A֕�A֗�A֕�A֕�A֓uA֕�A֕�A֕�A֗�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֗�A֗�A֕�A֕�A֕�A֓uA֓uA֕�A֕�A֕�A֗�A֕�A֓uA֕�A֕�A֕�A֕�A֕�A֕�A֕�A֓uA֓uA֓uA֕�A֕�A֓uA֑hA֓uA֋DA֍PA֋DA֏\A֏\A֍PA֋DA֍PA֑hA֑hA֓uA֑hA֑hA֕�A֓uA֓uA֓uA֑hA֑hA֓uA֓uA֏\A֑hA֑hA֑hA֑hA֏\A֏\A֍PA֑hA֑hA֍PA֏\A֏\A֏\A֏\A֍PA֍PA֍PA֍PA֍PA֍PA֍PA֍PA֋DA֋DA։7A֋DA֋DA֋DA֍PA֋DA֍PA֋DA֍PA֍PA֋DA։7A֋DA֋DA։7A։7A։7A֋DA֋DA֋DA֋DA֍PA֋DAև+AօAև+AօAօAօAօAօAօAօAօAօAօAփAփAփAփAօAփAօAօAցAփAփAօAօAփAօAօAօAօAօAօAօAօAօAօAօAօAփAօAօAօAօAփAփAօAօAօAօAփAօAօAփA�~�A�|�A�~�A�~�A�~�AցA�|�A�~�A�~�AփA�~�A�~�A�|�A�|�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�~�AցA�~�A�~�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�z�A�|�A�|�A�z�A�x�A�x�A�z�A�z�A�z�A�z�A�z�A�t�A�r�A�t�A�r�A�r�A�r�A�r�A�p�A�p�A�r�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�r�A�p�A�p�A�p�A�p�A�p�A�p�A�n�A�p�A�p�A�p�A�p�A�n�A�p�A�l�A�jA�ffA�`BA�bNA�`BA�`BA�bNA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�^5A�`BA�`BA�bNA�bNA�bNA�bNA�dZA�bNA�`BA�`BA�`BA�^5A�^5A�^5A�bNA�bNA�bNA�dZA�`BA�`BA�^5A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�bNA�`BA�dZA�dZA�bNA�bNA�bNA�`BA�bNA�`BA�bNA�`BA�`BA�`BA�^5A�`BA�`BA�`BA�^5A�`BA�`BA�`BA�^5A�^5A�^5A�`BA�^5A�`BA�`BA�`BA�^5A�`BA�`BA�`BA�^5A�^5A�`BA�`BA�`BA�^5A�`BA�bNA�bNA�bNA�bNA�bNA�bNA�^5A�bNA�`BA�bNA�bNA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�dZA�dZA�dZA�dZA�dZA�ffA�bNA�ffA�ffA�ffA�dZA�dZA�dZA�bNA�dZA�ffA�dZA�ffA�ffA�dZA�dZA�dZA�bNA�^5A�VA�O�A�E�A�E�A�C�A�A�A�?}A�?}A�C�A�C�A�E�A�E�A�A�A�=qA�=qA�7LA�5?A�5?A�5?A�5?A�33A�1'A�/A�+A�oA���A��A՟�A�(�A԰!A�bNAӴ9A��A�XA��A���AуA�dZA�?}A�
=A��`A��;A��A�ƨAиRAЬAБhA�x�A�p�A�jA�`BA�S�A�A�A�/A��A�bA�%A�A�  A���A��A�Aϝ�A�~�A�r�A�l�A�^5A�=qA�/A�$�A�"�A� �A��A��A��A�oA�oA�bA�VA�JA�VA�VA�JA�JA�VA�VA�bA�oA��A�oA�oA�{A�bA��TAΰ!AΧ�AΧ�AΥ�AΥ�AΡ�AΡ�AΡ�AΝ�AΗ�AΕ�AΑhA΋DA΅A�~�A�x�A�v�A�v�A�v�A�t�A�l�A�^5A�O�A�1'A��AͮA͇+A�ffA�-A��A̺^A̟�A̋DA�v�A�XA�1'A�oA�A��yA��;A���A���A˴9A˙�AˍPA�x�A�~�A�v�A�v�A�x�A�x�A�v�A�x�A�r�A�hsA�ffA�\)A�XA�A�A�;dA�;dA�9XA�33A�/A�(�A��A�bA�  A��Aʺ^A�XA���A��AȁA���A���AǓuAǑhA�~�A�`BA�?}A���A�|�A�K�A��A�ĜAōPA�M�A�
=A���Aģ�A�ffA�=qA�1'A�/A�-A�-A�(�A�$�A�"�A��A��A��A��A�bA�
=A�1A�A�  A���A���AìAÇ+A�jA�`BA�^5A�XA�S�A�Q�A�M�A�9XA��A�bA���A��HA���A¾wA¡�A�t�A�K�A�&�A�oA�A�  A���A��A��A��mA��TA��;A��/A��A��
A���A���A�A��!A���A��+A��A�z�A�x�A�t�A�p�A�jA�hsA�bNA�^5A�XA�K�A�C�A�7LA�1'A�+A�&�A��A�oA�A���A��A��A��mA��/A���A��FA���A��A�l�A�C�A�&�A�bA�  A��`A���A�ƨA��9A���A��7A��+A�x�A�l�A�M�A�33A�1A��A���A��^A��A���A��DA��A�~�A�z�A�z�A�x�A�t�A�l�A�jA�jA�ffA�dZA�\)A�XA�VA�S�A�Q�A�K�A�G�A�C�A�C�A�?}A�=qA�;dA�5?A�+A��A��A��A�1A�A�A�A���A���A���A���A��A��yA��A��mA��mA��mA��`A��HA��/A��/A��#A���A���A���A���A���A��uA��7A��A�|�A�z�A�v�A�t�A�v�A�t�A�t�A�r�A�l�A�hsA�hsA�ffA�VA�S�A�?}A�-A�$�A�oA�bA�{A��A�bA�JA���A�1A�
=A�bA���A��TA��;A��;A��;A��HA��TA���A���A���A���A���A��
A��
A��
A���A�ĜA�A�A���A���A���A��wA��9A��-A��9A��FA��^A���A��jA��wA���A�ƨA�ƨA��wA��jA��A��uA���A��hA��\A��A�z�A�hsA�bNA�ffA�XA�E�A�=qA�5?A�/A�(�A�/A�1'A�$�A�"�A� �A��A�oA���A�ƨA��uA�G�A�%A��mA��TA���A���A��!A���A���A�hsA�/A�&�A�JA���A��;A���A��wA��^A���A���A��PA��A�p�A�bNA�Q�A�K�A�C�A�;dA�1'A��A�VA�
=A�%A�A���A���A��A��HA���A�ĜA�ĜA�A��FA���A���A���A��DA��A�x�A�l�A�XA�;dA�bA���A���A�hsA�XA�XA�ZA�XA�\)A�^5A�`BA�^5A�ZA�XA�O�A�E�A�5?A�/A�+A��A��A�oA�JA���A��A��A��TA��HA��;A��#A���A�ȴA�ƨA���A��wA��jA��FA��9A��9A��-A�`BA�^5A�t�A�|�A��A��A��hA���A��-A���A��A���A���A���A�A���A�%A��A�&�A��A�$�A�33A�33A�5?A�K�A�jA�bNA�v�A�z�A��A��DA��hA���A��hA��hA���A��A��-A���A�ĜA���A���A��yA��A���A��A���A���A�A��A�oA��A��A�(�A�/A�;dA�I�A�I�A�M�A�n�A�p�A�ffA�l�A��A��A��uA���A���A�ƨA�A��A�E�A�p�A�t�A�x�A���A��A�C�A��7A���A��^A��mA��A��;A���A�JA��A�33A�$�A�G�A�l�A���A�ȴA�A�A�A�`BA�r�A���A�ȴA���A�
=A�bA��A�+A�M�A�bNA�t�A��A���A���A��A��FA��wA���A��/A��mA��A�VA�hsA��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=�s>��>џ�>d�f@�nD=��1?��>�P�@%��>0-�@�o@Y��>��@.��@�s�=��>[�@1=q/=��b=�ڥ=�n�>�$t@���=��?�_�@�i�>U�?�)t?Ӷ�=���=�+,>���@�dE=���>�9�@�in?9��>4D?�2�@�M=٤>�ջ@�jj=�IR>Y3]=��> >@��=�O>o
�@�d�?���?��@�K�>U�=`�l=�*o>��=���=��>.�?AJ@h�=�x-=���>��=�K=���@ �O=��=�w�=ɐm>!��@�dE=�MU=��=���=�-=�/�>m��?��>4�@y�=���=��>D�@�k�>��@�jj@�jj=xG=��=��N?�'R=�7a>4��?]>-=�Yu>)�e@"�7>$-�@,e�=���=�<!@P�@�in>6�v>5'�@�gw>��Z> 4n?�	�@�r�>X7�>@
|@�m�@�j�@�r=��>�?�q�>(�k@[��>;�`@�qa@�o~?���>I��=�y=��F=�r�><`=�O?�f{?��=���=��2=�NQ>�b@�q=���>�0�@�l>/<�>1��?���@�w2@�u�>��>���@�qa@�sX@�vu=�,�=��\?� i@�q>vy=͔>h�M@�sm@�q�@�s�=��Q?�d�@�r@�r@�r�>'l�>;d@�sX@�w�=�0>�F?�fQ@�k'@�l�@�g�@s��=���=��?��@�v�@�u:@�v@�zN?z�=��>x�?]��@�x�@�y)@�y�@�v�=��x@~g@�x?��@�y)>�},?&o�?�]�?ye�?��@�t�@�t@�p;@�p�@N�@t�f@�w�@h1{@�y�@�zN@�t�@�y)@�x�@�x�@�y}@�xW@�xW@�w�@�xW@�x�@�xW@�y}@�x�@�x�@�y}@�z�@�zN@�y}@�w2@�xW@�{t@�|1@�|1@�{@�{@�{@�uO@�{@�z�@�zN@�zN@�y}@�xW@�{t@�|�@�}V@�y}@�x�@�w�@�w�@�v@�u:@�v�@�|1@�{t@�y)@�y}@�y}@�z�@�w�@�u:@�u�@BK4@�x�@�xl@�xW@�y�@�x�@�xW@�vu@�v@�y}@�y)@�w�@�y)@�y}@�y)@�zN@�y}@�y�@�y�@�x�@�y)@�z�@�zN@�w�@�{@�{@�{�@�{�@�|1@�{@�z�@�{t@�y�@�zN@�zN@�y}@�{�@�}@�zN@�zN@�zN@�y�@�{@�z�@�x�@�x�@�z�@�y}@�w�@�{t@�z�@�y�@�{@�|�@�|�@�x�@�z�@�zN@�y)@�zN@�y}@�y�@�y�@�w�@�|1@�{�@�{@�z�@�{@�y�@�zN@�|�@�{�@�|�@�{�@�}@�}V@�|�@�{t@�z�@�{@��@�{�@�{t@�{�@�{�@�{�@�{�@�|1@�{�@�~|@�~(@�}V@�}V@�}V@�}V@�}�@�}V@�~(@�~�@�~�@�}�@�~�@�}V@�N@�N@��@�N@�N@��@��@�~�@�~�@�~�@�~(@�}V@�}@�~�@��@�~�@�N@�N@�~|@�~�@�~�@�~(@�N@�~�@�~�@�~(@�~(@�}@�}�@�~|@�~(@�~�@��@�~�@�~�@�~�@�}V@�}�@�~�@�}�@�}�@�}�@�N@�~�@�~�@�~|@�~�@�}V@�~�@��@�N@�~(@�}�@�~�@�~�@�~�@��@�N@�~|@�N@��@�~�@�~(?'˒@�~�@�~�@�~�@�~�@�~(@��@�~�@�~�@�~(@�~�@��@�~�@�5~@�~�@�~(@�~�@�~�@�}V@�~�@��@�~�@���@���@�N@�N@�~�@�c@�~�@�~�@��@�N@�c@��E@���@���@���@��@�}�@���@�}�@��@���@��E@��E@���@���@���@��E@���@���@���@��@���@���@���@��@���@��Z@���@���@��@���@���@���@���@���@���@���@���@��Z@��Z@��Z@��@���@���@���@��Z@���@��@��@��@���@��Z@��@��@��,@���@��@���@���@��E@���@���@���@��Z@���@��Q@���@���@��Q@���@���@���@���@���@���@���@��,@��Z@��,@��,@��,@���@���@���@��Q@���@��Q@���@���@��,@��Q@���@���@���@��Q@��<@���@��Q@���@��w@���@��Q@���@���@���@��@��Q@��Q@��@��@��#@���@���@��#@��o@��,@���@��@�x@��@�}�@�},@�~R@�}�@�},@�},@�}�@��@��@���@��@��o@�|�@�{5@�{5@�z@�y�@�z@�y�@�y�@�yS@�x�@�x�@�u@�o?@�n�@�n�@�n�@�mH@�m�@�nD@�m�@�m�@�m�@�n�@�p@�p@�o�@�n�@�m�@�n@�n�@�n�@�n�@�pe@�q�@�q�@�pe@�p�@�o @�m�@�nD@�oi@�m]@�l�@�k@�k{@�k@�k@�i�@�i�@�i�@�i�@�i�@�i�@�i�@�h�@�h^@�h�@�h�@�h�@�h�@�i/@�i/@�i/@�i/@�h�@�h�@�i/@�i/@�i�@�i�@�h�@�h�@�h^@�h�@�h�@�h�@�i/@�i@�in@�i�@�i�@�i/@�i/@�i/@�h^@�i�@�i�@�j@�j@�j@�j@�j@�jU@�jU@�j@�j�@�jj@�j@�j�@�jj@�jj@�k'@�k�@�k�@�k'@�k�@�k�@�k�@�k�@�k�@�k�@�k�@�k�@�k'@�k�@�l"@�k�@�la@�k�@�k�@�k�@�la@�la@�la@�la@�l�@�m@�m@�la@�l�@�m3@�l�@�m3@�m3@�m�@�nn@�m�@�m�@�m3@�nY@�m�@�m�@�m�@�m�@�m�@�m�@�m�@�m�@�nY@�m�@�n�@�nY@�nY@�nY@�n�@�n@�m�@�n�@�n�@�nY@�n�@�nY@�n@�n@�nY@�nY@�m�@�m3@�mH@�l�@�lv@�l�@�l�@�m�@�m�@�m�@�n@�n�@�o�@�o�@�o*@�o*@�n�@�n�@�nn@�n�@�o*@�o*@�o�@�n�@�l�@�n�@�nn@�n�@�n�@�nn@�nn@�m�@�o @�n�@�n@�nn@�n�@�n�@�n�@�nn@�nn@�n@�nn@�n�@�nn@�nn@�m�@�nn@�n@�n@�nD@�nn@�n�@�nD@�nn@�n�@�n�@�n�@�o?@�n�@�n�@�n@�n�@�n�@�n@�n�@�n�@�n@�n�@�n�@�m�@�nD@�m�@�l�@�m�@�m�@�m]@�m]@�m]@�l�@�m]@�m]@�l�@�l�@�l�@�l�@�l�@�l�@�l�@�m�@�n@�m]@�m�@�m�@�m�@�m�@�m�@�m�@�m]@�m�@�n/@�m�@�m]@�m�@�n/@�m�@�n/@�n/@�n/@�n�@�n/@�n/@�n�@�nY@�n�@�n�@�n�@�n�@�n�@�n�@�n�@�n�@�n/@�n�@�n/@�n/@�m�@�m�@�mr@�mr@�m�@�m�@�m�@�m�@�m�@�m�@�mr@�m�@�mr@�n/@�mr@�n/@�m�@�n/@�mr@�mr@�n/@�m�@�n/@�n/@�m�@�m�@�mr@�m�@�m�@�m�@�m�@�n/@�mr@�m�@�mr@�m	@�mr@�mr@�mr@�l�@�l�@�lv@�k�@�k{@�k@�k@�k{@�kQ@�kQ@�k@�j�@�j�@�jU@�j�@�j�@�k@�j�@�jU@�i�@�i�@�jU@�i�@�i�@�i@�i�@�i�@�i@�i@�h^@�i@�h^@�h^@�h
@�h
@�h
@�g�@�g8@�f{@�e�@�eV@�eV@�eV@�eV@�eV@�e,@�e,@�e,@�d�@�eV@�f@�f@�e�@�f@�f@�f@�eV@�eV@�eV@�d�@�eV@�e,@�e�@�e,@�e,@�eV@�eV@�eV@�e�@�eV@�e�@�e�@�d�@�eV@�eV@�eV@�eV@�e�@�e�@�e�@�f@�e�@�f@�e�@�eV@�e�@�f@�f@�f@�f@�f@�f@�f{@�f@�f�@�f@�f{@�f{@�g#@�f@�e�@�e�@�f@�f@�e�@�f@�e�@�eV@�d�@�eV@�e�@�eV@�e�@�e�@�e�@�e�@�eA@�d�@�d@�d0@�d�@�d�@�d0@�d�@�d�@�c�@�c�@�c�@�c@�c@�c@�c@�b�@�bx@�b�@�bx@�b9@�a�@�a�@�a|@�b9@�a@�_�@�_�@�_@�_@�]�@�]:@�\@�[B@�Z�@�[B@�[B@�[�@�Z�@�Z�@�YK@�X�@�X�@�Wi@�W @�U�@�TL@�R@�N{@�K4@�If@�J8@�J8@�I@�G@�D�@�Dg@�Dg@�D�@�C�@�?�@�77@�0U@�,g@�(9@�&B@�&B@�&B@�!�@��@��@��@��@��@�
@�@��r@��M@���@��|@��:@���@��a@�ۡ@��@�׈@��E@�҉@�ɰ@��@���@��r@���@�l�@�<`@��@��x@��D@���@�pe@�T�@�:�@�,�@�$5@�K@��@��@�@�9@�q@���@���@��,@��@���@��@���@���@���@��k@���@�ͳ@�Ɇ@��3@��H@���@��@��C@���@��+@���@���@���@��@���@���@���@���@���@��s@���@���@��k@��[@��5@���@��@��S@���@��@��K@��%@�� @���@��~@���@��s@�x-@�n�@�i�@�gM@�d�@�c5@�a�@�_�@�]�@�[�@�XO@�T"@�P]@�L�@�H�@�CB@�>W@�9@�5�@�2�@�.4@�)_@�%�@�y@��@�
�@��/@���@�Ց@��u@���@��k@��n@�la@�^�@�V@�M+@�@�@�33@�+�@�#�@�~@��@��@��@��P@��@��l@��.@���@��U@�۶@�ڥ@��o@���@��@�ϖ@��G@�õ@��H@���@��@��7@���@��@��(@��B@���@��^@�{�@�qv@�eA@�V�@�9C@�5@��@���@�\�@�4�@�!B@��@�@���@��2@��9@��e@��{@�n�@�If@�0U@��@���@��@��@���@��A@�� @���@�� @��@��H@��v@��"@��U@��4@��#@���@��4@��g@���@��F@��h@��@���@��{@�w�@�p�@�k<@�h�@�f�@�do@�b�@�_�@�[�@�S&@�IR@�B�@�:@�0@�&@��@��@�6@��Z@��`@��e@��v@��e@�ݘ@�ڥ@�؄@��g@��[@��c@��)@��[@��K@��m@�Ɠ@���@���@���@��@��~@���@���@���@���@��g@���@���@��}@���@���@��@�tT@�lv@�Xy@�GZ@�4�@�)_@�"�@��@�	@�5@�)@��	@���@���@���@��@��@��@��5@��@���@��@�զ@�Ҟ@��l@��T@�Ë@��8@��W@���@��{@���@��@���@��#@�~|@�zc@�v�@�r@�p;@�n�@�m3@�l�@�l@�jj@�h�@�g�@�f�@�f'@�d@�b9@�a(@�_�@�^�@�^@�\�@�[-@�ZG@�X�@�W�@�W*@�U�@�R�@�P@�K�@�G�@�F_@�A�@�@�@�?�@�>B@�=\@�;�@�;%@�9�@�9@�6@�4�@�4n@�3�@�3�@�1{@�0�@�/@�.�@�,�@�)�@�&@�!�@�P@�<@�@��@�Z@��@�
�@�
�@�	�@�	�@�p@�_@��@��@��@� �@���@��@@��@���@��@��@��r@��.@��@��*@���@�ݘ@��<@���@��#@��@��R@���@��1@��`@��u@�̸@��d@��C@���@�Ǐ@���@���@���@��G@���@���@��a@���@���@���@��U@���@���@��#@���@���@���@��@��Q@��@��E@���@��x@���@���@���@��X@���@���@��@��@���@��@���@���@���@��7@��o@�|p@�w�@�s.@�oT@�o�@�l�@�h�@�a�@�Q�@�@%@�D�@�@�@�/�@�(x@��@��4@��@��U@�ӄ@���@��@���@���@���@��/@��@�y@�p�@�hs@�d@�_1@�Y6@�Se@�N�@�I@�C�@�?h@�:?@�5@�1�@�.@�*@�%�@��@�@�j@��@�E@�=@�5@�@��@���@���@��U@��8@��@��O@��@���@���@�Ҟ@��u@��&@���@��@���@��&@�|1@�o�@�n/@�o�@�p�@�rG@�t~@�x@�y}@�y)@�x�@�w@�tT@�q�@�la@�h�@�dE@�a�@�`-@�]y@�Z�@�W @�T@�P�@�O"@�M�@�Ln@�JM@�H�@�D(@�B@�@�@�?�@�>�@�=\@�;�@�:�@�9C@�7�@���@���@��u@��@��h@��@��u@��A@���@���@���@��K@���@��@���@��Y@�õ@���@��m@���@��K@�׈@�ީ@��K@��:@���@��@���@��@�
�@��@�'@��@��@�$�@�.�@�6@�;%@�@�@�D|@�B�@�@�@�A�@�CW@�C�@�E�@�F5@�G�@�H�@�L�@�Oa@�N�@�R@�[�@�a@�i�@�o�@�qv@�r�@�t @�v@�{@��
@���@���@��@���@��K@��0@��T@���@��L@�ȟ@��F@�Ԁ@���@���@��R@�5i@�e�@�m@�s�@�t?@�v6@��@���@��@���@��A@��@��@���@�R@�b@�@�@�EN@�P]@�Z@�S;@�bc@�k{@�u�@��N@���@���@��F@��@���@��0@�ܱ@�ۡ@�� @��@�W@�e@�
@��@�K@��@�@��@�%@��@�@�@�@�:@�@��@�O@�O@�y@�O@��@��@��@�J@�@�t@�@��@�@��@��@��@��@��@��@�@�@�@�O@��@��@��@��@��@��@��@�y@�y@�O@�:@�O@��@��@�@�5@�@��@�!@�O@�:@��@�@��@��@�@��@�J@��@��@��@��@�\@�@�@��@�:@�G@� �@��@� �@���@� @���@���@��@�@�}@�!@�:@� �@� *@� �@��@���@���@���@���@�;@�K@��@� �@��j@���@��=@��(@���@��@��A@��@��@��@��(@���@���@���@��@��@��@���@��R@��|@���@���@��@��@��@��@��@���@��s@��A@���@��@�� @��J@��9@��)@���@���@��g@���@��@��@��>@��B@��@��l@��W@��l@��l@��@���@���@��@���@���@���@���@���@���@��@��@��@���@���@���@��@��)@��>@��@��@��@���@���@���@��g@��g@���@���@���@���@���@��@��$@��c@��c@��@��N@��c@��@��@���@���@�� @���@��@��J@��@���@���@��@���@���@��J@��@��E@���@��J@��t@��@��J@��t@��@���@��@��@��o@��@��0@��o@��0@��Z@���@���@���@��,@��@��V@��@��@��@���@���@��@��@��@��@���@��=@���@���@���@��R@��R@��@��=@��R@��R@��@��g@��g@���@��(@��R@��@��@��,@��@��@��Z@���@���@��@��@��@��@��k@���@��#@���@��@��#@���@��@���@���@���@���@���@��|@��g@���@��R@��=@��g@��|@��=@��@���@��@��g@���@��|@��@��@���@��@���@���@���@���@��@���@��@��@��@��@��@���@��@���@���@���@��(@��@��@��@��@��@��@��@���@��(@���@���@��=@���@��@���@���@���@���@���@���@��@��@��Z@��g@��Z@��E@��@��E@��Z@��Z@���@���@��Z@���@��E@��@��@��@��@��@���@���@���@���@��@��@��@��@��,@��@��@��,@��k@��A@��V@��k@��V@��@��k@��@��@��@��@��@��@��@���@��A@��@��E@��@��@���@���@��Z@���@��@��@��E@���@��Z@��Z@��Z@��Z@��Z@���@��@��@��@��A@��@��o@��E@��@���@��@���@���@��E@��@��@���@���@��_@�� @��_@��J@�� @��@���@��@���@���@��@���@���@���@���@��@��B@��B@��@��@��-@��@��B@��-@��B@��B@��B@��-@��@��@��@��-@��B@��@��-@��B@��@��-@���@���@��[@��@��!@��@��@��G@���@��\@���@���@���@���@��@���@���@��@��q@��G@��@��@��\@��@��@��@��@���@��.@��q@��G@���@���@���@���@��@��@���@���@��@��2@���@���@���@��@���@��@��\@��@���@��@��@��@��@��\@��G@��G@��q@��@��@��@��@��q@��@��@��@��@���@��X@��X@��@��@���@��q@��@��@��@��@��@��\@��2@��@��G@��2@��\@��@���@���@��@��G@��2@��\@��@��@��\@��\@��\@��\@��2@���@��G@��q@��@��@��\@��@���@���@��@��@���@��@���@���@���@��@���@��@���@��)@���@���@���@���@���@���@��@��X@��@��m@��m@��m@��@��@��@��m@��C@���@���@��@��@��@��@��G@���@���@��@���@��@���@�ڐ@��Q@��+@��E@��s@��
@��@�ײ@��0@��@�ֶ@��@��(@�Ѣ@��$@��S@��@��@���@�̣@���@���@���@��5@��i@���@�rq@�<�@�!�@��@���@�^�@�G0@�-M@��@��r@��R@��]@��u@��\@�Ɠ@��D@���@���@��"@��^@���@���@��[@���@��e@��^@�~R@�x�@�t?@�r\@�q�@�pe@�j@�Z�@�K4@�>�@�7v@�5T@�1�@�$@�!�@�%@�+,@�*�@�+k@�,=@�,g@�+�@�+�@�+V@�+,@�,(@�,�@�,�@�,�@�,g@�-#@�,�@�/�@�0�@�4�@�1@�0@�0+@�1Q@�$�@��@��@� �@���@��@���@��X@���@���@��@���@���@���@��@��N@��t@��d@��`@��@��%@��@��X@���@�ϖ@��_@���@��
@�v�@�]:@�D�@�$5@�#@��@� ?@���@��C@�ѷ@��C@��@���@��2@���@���@���@��@�}�@��b@�|�@�}�@�}�@�}�@�|�@�|1@�z@�t*@�s.@�l�@�j�@�^�@�[W@�[@�Y�@�W~@�UG@�R�@�LD@�D�@�=G@�5~@�!B@���@���@�h
@�(�@���@���@�ł@���@��@���@��L@�~�@�H�@�<�@��@��]@��+@��4@��N@��n@�w2@�_�@�K�@�Ft@�D�@�D(@�D=@�BF@�CW@�C@�?S@�?>@�>B@�<`@�:�@�;@�;%@�9�@�6�@�5�@�,�@�3@��@��@�	�@�	�@��@�J@�:@��@���@��g@��@��;@���@��!@��u@���@��@��R@��H@��{@�}�@�|�@�|�@�x�@�v�@�tT@�r\@�p�@�o�@�n/@�m�@�la@�h�@�do@�^_@�Sz@�LD@�I�@�F�@�D=@�CB@�A�@�>�@�<�@�;:@�9.@�6�@�0�@�.^@�)�@�%1@�"�@� �@��@�P@��@�@�	�@��@��@��@���@��s@��O@��z@��=@���@��|@���@���@��k@��@���@��M@�{@�q"@�pP@�i�@�f'@�Z@�O7@�=�@�5�@�(�@�!@��@��@��@��@�
�@�	�@�	l@�	@��@�)@�.@�@�\@��@��.@��]@��a@���@��@���@��3@���@���@��@���@���@��@��@��@��2@��u@���@��@��r@���@��#@��'@�ؙ@��@��b@�҉@���@��5@��t@���@���@�ρ@��@��@��K@�ʂ@���@�Ȋ@��@��$@��W@���@��.@���@���@��*@��m@���@��m@���@��H@���@��@���@��^@���@���@���@��&@��@���@�}�@�@��@�~�@�~g@�v�@�{J@�y�@�~�@�t�@�l�@�jj@�j@@�i�@�k<@�lL@�gM@�c�@�b�@�c�@�e@�g#@�f�@�f'@�e�@�_�@�^_@�]�@�]�@�]�@�]�@�\�@�X�@�Xd@�X�@�Y6@�ZG@�]O@�[l@�\h@�\}@�`k@�_[@�]O@�\@�T�@�L0@�L�@�J�@�I�@�E�@�C@�;�@�8�@�8G@�4�@�,�@�'R@�"�@� \@��@�K@� �@�m@��@��@��@��@�@��@��/@���@���@��h@��@��<@���@�z:@�p�@�k�@�Z2@�?S@�;%@�0j@�'=@��@��@�0@�@�W@���@��f@��@��p@��@��@��8@���@�ԕ@���@�Ǐ@���@���@��{@��j@��c@���@���@��:@��@��b@��f@���@��@���@��y@�� @��H@���@��@�|�@�t*@�j@�Y�@�AJ@�(N@��@�@��@�b@��@��@�/@��@��@��@�&@�@��@�]@�I@��@�	B@��@�W@�G@��@��@���@��8@��@��k@��@���@��@��@��&@��*@���@��3@��"@���@��@�R�@�Tv@�]�@�_�@�eA@�c�@�j�@�q�@�y�@�|�@��@��@���@���@���@��@���@��D@��@��?@��G@��S@���@���@��E@��z@�¤@��>@��N@�Ҟ@�֌@���@��Z@��H@���@���@��@���@���@��@���@��@��X@��@�\@��@�)@�:@�Z@�+@�{@��@��@�O@�O@�#�@�*�@�.s@�/�@�:�@�;:@�82@�;%@�C�@�I�@�M�@�O�@�S&@�c�@�d@�m	@���@��@��)@��E@���@��@�	�@�-@�6e@�@�@�T�@�V�@�Uq@�X�@�c�@�m	@�yh@�vu@�~�@���@��v@���@��^@��y@���@�
=@��@�,|@�D(@�N{@�U�@�N�@�\>@�l�@�u@�|�@��n@��K@��@���@��A@���@��h@���@���@���@��t@��@�KG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   4444344444334434444444434434444443443444444344443443443444444443444444444434444444444443433444444444444444344344434433344443433444444444444434434443344333444344433344333443344433334443333444433334434344444333343333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�nBG�O�G�O�G�O�G�O�G�O�@�o@Y��G�O�G�O�@�s�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�@�i�G�O�G�O�G�O�G�O�G�O�G�O�@�dEG�O�G�O�@�inG�O�G�O�G�O�G�O�G�O�G�O�@�jmG�O�G�O�G�O�G�O�@��G�O�G�O�@�d�G�O�G�O�@�K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@h�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�dJG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�k�G�O�@�ji@�jkG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�ipG�O�G�O�@�gxG�O�G�O�G�O�@�r�G�O�G�O�@�m�@�j�@�r G�O�G�O�G�O�G�O�@[��G�O�@�qd@�o�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�qG�O�G�O�@�lG�O�G�O�G�O�@�w3@�u�G�O�G�O�@�qb@�sZ@�vyG�O�G�O�G�O�@�qG�O�G�O�G�O�@�sn@�q�@�s�G�O�G�O�@�r@�r@�r�G�O�G�O�@�sW@�w�G�O�G�O�G�O�@�k)@�l�@�g�@s��G�O�G�O�G�O�@�v�@�u:@�v@�zRG�O�G�O�G�O�G�O�@�x�@�y*@�y�@�v�G�O�G�O�@�xG�O�@�y*G�O�G�O�G�O�G�O�G�O�@�t�@�t@�p8@�p�G�O�@t�e@�w�@h1~@�y�@�zO@�t�@�y*@�x�@�x�@�y}@�xU@�xU@�w�@�xZ@�x�@�xZ@�y�@�x�@�x�@�y~@�z�@�zL@�y�@�w/@�xX@�{u@�|1@�|-@�{@�{@�{@�uN@�{@�z�@�zR@�zM@�y�@�xU@�{v@�|�@�}V@�y�@�x�@�w�@�w�@�v@�u=@�v�@�|4@�{v@�y-@�y�@�y�@�z�@�w�@�u;@�u�G�O�@�x�@�xq@�xV@�y�@�x�@�xX@�vt@�v@�y�@�y*@�w�@�y(@�y~@�y*@�zM@�y|@�y�@�y�@�x�@�y-@�z�@�zU@�w�@�{@�{@�{�@�{�@�|4@�{@�z�@�{u@�y�@�zR@�zK@�y~@�{�@�}@�zR@�zM@�zR@�y�@�{
@�z�@�x�@�x�@�z�@�y�@�w�@�{w@�z�@�y�@�{
@�|�@�|�@�x�@�z�@�zR@�y*@�zO@�y~@�y�@�y�@�w�@�|4@�{�@�{@�z�@�{@�y�@�zK@�|�@�{�@�|�@�{�@�}@�}X@�|�@�{v@�z�@�{@��@�{�@�{w@�{�@�{�@�{�@�{�@�|3@�{�@�~z@�~*@�}V@�}U@�}Z@�}Z@�}�@�}V@�~,@�~�@�~�@�}�@�~�@�}Z@�R@�O@��@�M@�M@��@��"@� @�~�@�~�@�~)@�}X@�}@�~�@��@�~�@�M@�M@�~z@�~�@�~�@�~)@�M@�~�@�~�@�~)@�~)@�}@�}�@�~|@�~&@�~�@��@� @� @�~�@�}X@�}�@�~�@�}�@�}�@�}�@�O@�~�@�~�@�~|@�~�@�}Y@�~�@��"@�M@�~'@�}�@�~�@�~�@�~�@�� @�N@�~z@�L@��@�~�@�~*G�O�@�~�@�~�@�~�@�~�@�~,@��@�~�@�~�@�~'@�~�@��@�~�@�5~@�~�@�~)@�~�@�~�@�}U@�~�@��@�~�@���@���@�P@�K@�~�@�b@�~�@�~�@��@�P@�e@��F@���@���@���@��"@�}�@���@�}�@�� @���@��G@��F@���@���@���@��G@���@���@���@��@���@���@���@��@���@��Z@���@���@��@���@���@���@���@���@���@���@���@��Z@��^@��^@��@���@���@���@��Z@���@��@��@��@�@��@�&@��@�@�@�@�7@�@��@�S@�P@�z@�P@��@��@��@�P@�@�v@�@��@�@��@��@��@��@��@��@�@�@�@�P@��@��@��@��@��@��@��@�z@�z@�N@�;@�N@��@��@�@�7@�@��@�&@�R@�:@��@�@��@��@�@��@�J@��@��@��@��@�[@�@�@��@�>@�F@� �@��@� �@���@� @���@���@��@�@�|@�"@�:@� �@� ,@� �@��@���@���@���@���@�;@�K@��@�@��k@���@��?@��%@���@��@��D@��@��~@��@��(@���@���@���@��@��@��~@���@��Q@��{@���@���@��@��@��"@��@��@���@��s@��?@���@��@��@��M@��:@��&@���@���@��i@���@��@��@��@@��>@��@��i@��Z@��m@��m@��@���@���@��@���@���@���@��@��@���@��@��@��@��@��@���@��@��-@��@@��@��@��@���@���@���@��k@��i@���@���@���@���@���@��@��&@��e@��e@��@��R@��e@��@��@���@���@��$@���@��@��L@��@���@���@��@���@���@��O@��@��F@���@��M@��v@��@��H@��t@��@���@��	@��@��n@��@��0@��o@��3@��[@���@���@���@��,@��@��X@��@��@��@���@���@��@��@���@��@���@��=@���@���@���@��T@��S@��@��:@��S@��T@��@��f@��h@���@��/@��S@��@��@��,@��@��@��a@���@���@��@��@��@��@��e@���@��"@���@��@��"@���@��@���@���@���@���@���@��z@��k@���@��R@��>@��j@��~@��?@��@���@��@��j@���@��}@��@��@���@���@���@���@���@���@��@���@��@��@��@��@��@���@���@���@��@���@��(@��@��@���@��@��@��@��@���@��-@���@���@��>@���@��@���@���@���@���@���@���@��@��@��Z@��l@��Z@��F@��@��B@��^@��\@���@���@��_@���@��H@��@��@��@��@��@���@���@���@���@��@��@��@��@��.@��@��@��.@��m@��A@��[@��m@��X@��@��r@��@��@��@��@��@��@��@���@��?@��@��H@��!@��@���@���@��\@���@��@��@��H@���@��Y@��\@��[@��^@��\@���@��@��@��@��E@��@��r@��F@��	@���@��@���@���@��K@��@��	@���@���@��b@��!@��`@��G@�� @��@���@��@���@���@��@���@���@�� @��@��@��C@��C@��@��@��.@��@��>@��.@��>@��>@��F@��+@��@��@��@��.@��B@��@��.@��C@��@��.@���@���@��\@��@��@��@��@��K@���@��^@���@���@���@���@��"@���@���@��"@��p@��H@��@��@��^@��@��@��@��@���@��*@��r@��I@���@���@���@���@��@��@���@���@��@��-@���@���@���@��@���@��"@��W@��@���@��@��@��@��@��a@��J@��J@��s@��@��@��@��@��r@��@��@��@��@���@��Z@��Z@��@��@���@��u@��@��@��@��@�� @��a@��-@��@��K@��2@��^@��@���@���@��#@��D@��/@��^@��@��@��^@��^@��^@��^@��2@���@��J@��s@��@��@��a@��@���@���@��@��@���@��@���@���@���@��@���@��@���@��'@���@���@���@���@���@���@��@��Z@��@��o@��m@��l@��@��@��@��q@��B@���@���@��@��@��@��@��H@���@���@��@���@��@���@�ڌ@��S@��.@��J@��v@��@��@�ײ@��3@��@�ֹ@��@��.@�ѡ@��&@��T@��@��@���@�̪@���@���@���@��;@��h@���@�rs@�<�@�!�@��@���@�^�@�G,@�-N@��@��u@��T@��\@��w@��_@�Ɠ@��F@���@���@��%@��\@���@���@��[@���@��j@��b@�~O@�x�@�t>@�ra@�q�@�pb@�j�@�Z�@�K7@�>�@�7w@�5W@�1�@�$@�!�@�%@�+.@�*�@�+j@�,:@�,f@�+�@�+�@�+Z@�+)@�,'@�,�@�,�@�,�@�,j@�- @�,�@�/�@�0�@�4�@�1@�0@�0-@�1S@�$�@��@��@� �@���@��@���@��V@���@���@���@���@���@���@��@��O@��v@��e@��^@��@��$@��~@��[@���@�ϗ@��b@���@��@�v�@�]<@�D�@�$4@�"@��@� A@���@��B@�ѹ@��B@��@���@��2@���@���@���@��@�}�@��b@�|�@�}�@�}�@�}�@�|�@�|2@�z@�t*@�s+@�l�@�j�@�^�@�[V@�[@�Y�@�Wy@�UH@�R�@�LC@�D�@�=J@�5�@�!B@���@���@�h@�(�@���@���@�ł@���@��@���@��O@�~�@�H�@�<�@��@��_@��.@��6@��J@��j@�w2@�_�@�K�@�Fr@�D�@�D'@�D>@�BH@�CX@�C@�?Q@�??@�>B@�<`@�:�@�;@�;&@�9�@�6�@�5�@�,�@�6@��@��@�	�@�	�@��@�M@�:@��@���@��i@��@��>@���@�� @��s@���@��@��Q@��J@��z@�}�@�|�@�|�@�x�@�v�@�tV@�r\@�p�@�o�@�n.@�m�@�l]@�h�@�ds@�^^@�Sz@�LE@�I�@�F�@�D>@�CA@�A�@�>�@�<�@�;:@�9.@�6�@�0�@�.\@�)�@�%4@�"�@� �@��@�Q@��@�@�	�@��@��@��@���@��o@��Q@��y@��?@���@��z@���@���@��j@��@���@��M@�{@�q$@�pR@�i�@�f)@�Z@�O8@�=�@�5�@�(�@�#@��@��@��@��@�
�@�	�@�	j@�	@��@�)@�.@�@�b@��@��0@��]@��b@���@��~@���@��4@���@���@���@���@���@��@��@��@��2@��x@���@��@��q@���@��"@��%@�ؙ@��@��e@�Ҍ@���@��8@��w@���@���@�σ@�� @��@��R@�ʇ@���@�ȍ@��@��$@��W@���@��*@���@���@��(@��n@���@��p@���@��J@���@��	@���@��^@���@���@���@��&@��@���@�}�@�@�� @�~�@�~e@�v�@�{L@�y�@�~�@�t�@�l�@�jj@�jD@�i�@�k>@�lL@�gN@�c�@�b�@�c�@�e@�g"@�f�@�f'@�e�@�_�@�^\@�]�@�]�@�]�@�]�@�\�@�X�@�Xi@�X�@�Y5@�ZE@�]O@�[n@�\f@�\}@�`m@�_]@�]R@�\@�T�@�L2@�L�@�J�@�I�@�E�@�C@�;�@�8�@�8H@�4�@�,�@�'S@�"�@� b@��@�J@� �@�n@��@��@��@��@�@��@��.@���@���@��h@��	@��=@���@�z:@�p�@�k�@�Z5@�?V@�;&@�0m@�'@@��@��@�3@�@�W@���@��g@��@��u@��@��@��6@���@�ԕ@���@�ǒ@���@���@��z@��m@��f@���@���@��:@��@��e@��f@���@��@���@��z@��@��J@���@��@�|�@�t*@�j@�Y�@�AN@�(R@��@�@��@�e@��@��@�.@��@��@��@�&@�@��@�_@�K@��@�	E@��@�X@�J@��@��#@���@��7@��@��j@��@���@��@��@��$@��*@���@��6@��!@���@���@�R�@�Ts@�]�@�_�@�e>@�c�@�j�@�q�@�y�@�|�@��@��@���@���@���@��@���@��@@��@��?@��F@��V@���@���@��E@��~@�¦@��>@��P@�ҝ@�֍@���@��X@��F@���@���@��@���@���@��@���@��@��U@��@�^@��@�+@�;@�Y@�,@�|@��@��@�O@�R@�#�@�*�@�.t@�/�@�:�@�;:@�85@�;'@�C�@�I�@�M�@�O�@�S'@�c�@�d@�m@���@��@��(@��E@���@��@�	�@�-@�6f@�@�@�T�@�V�@�Uq@�X�@�c�@�m
@�yg@�vr@�~�@���@��r@���@��_@��{@���@�
?@��@�,|@�D*@�N}@�U�@�N�@�\A@�l�@�u@�|�@��q@��K@��
@���@��A@���@��j@���@���@���@��t@��@�N@�@��@�&@��@�@�@�@�7@�@��@�S@�P@�z@�P@��@��@��@�P@�@�v@�@��@�@��@��@��@��@��@��@�@�@�@�P@��@��@��@��@��@��@��@�z@�z@�N@�;@�N@��@��@�@�7@�@��@�&@�R@�:@��@�@��@��@�@��@�J@��@��@��@��@�[@�@�@��@�>@�F@� �@��@� �@���@� @���@���@��@�@�|@�"@�:@� �@� ,@� �@��@���@���@���@���@�;@�K@��@�@��k@���@��?@��%@���@��@��D@��@��~@��@��(@���@���@���@��@��@��~@���@��Q@��{@���@���@��@��@��"@��@��@���@��s@��?@���@��@��@��M@��:@��&@���@���@��i@���@��@��@��@@��>@��@��i@��Z@��m@��m@��@���@���@��@���@���@���@��@��@���@��@��@��@��@��@���@��@��-@��@@��@��@��@���@���@���@��k@��i@���@���@���@���@���@��@��&@��e@��e@��@��R@��e@��@��@���@���@��$@���@��@��L@��@���@���@��@���@���@��O@��@��F@���@��M@��v@��@��H@��t@��@���@��	@��@��n@��@��0@��o@��3@��[@���@���@���@��,@��@��X@��@��@��@���@���@��@��@���@��@���@��=@���@���@���@��T@��S@��@��:@��S@��T@��@��f@��h@���@��/@��S@��@��@��,@��@��@��a@���@���@��@��@��@��@��e@���@��"@���@��@��"@���@��@���@���@���@���@���@��z@��k@���@��R@��>@��j@��~@��?@��@���@��@��j@���@��}@��@��@���@���@���@���@���@���@��@���@��@��@��@��@��@���@���@���@��@���@��(@��@��@���@��@��@��@��@���@��-@���@���@��>@���@��@���@���@���@���@���@���@��@��@��Z@��l@��Z@��F@��@��B@��^@��\@���@���@��_@���@��H@��@��@��@��@��@���@���@���@���@��@��@��@��@��.@��@��@��.@��m@��A@��[@��m@��X@��@��r@��@��@��@��@��@��@��@���@��?@��@��H@��!@��@���@���@��\@���@��@��@��H@���@��Y@��\@��[@��^@��\@���@��@��@��@��E@��@��r@��F@��	@���@��@���@���@��K@��@��	@���@���@��b@��!@��`@��G@�� @��@���@��@���@���@��@���@���@�� @��@��@��C@��C@��@��@��.@��@��>@��.@��>@��>@��F@��+@��@��@��@��.@��B@��@��.@��C@��@��.@���@���@��\@��@��@��@��@��K@���@��^@���@���@���@���@��"@���@���@��"@��p@��H@��@��@��^@��@��@��@��@���@��*@��r@��I@���@���@���@���@��@��@���@���@��@��-@���@���@���@��@���@��"@��W@��@���@��@��@��@��@��a@��J@��J@��s@��@��@��@��@��r@��@��@��@��@���@��Z@��Z@��@��@���@��u@��@��@��@��@�� @��a@��-@��@��K@��2@��^@��@���@���@��#@��D@��/@��^@��@��@��^@��^@��^@��^@��2@���@��J@��s@��@��@��a@��@���@���@��@��@���@��@���@���@���@��@���@��@���@��'@���@���@���@���@���@���@��@��Z@��@��o@��m@��l@��@��@��@��q@��B@���@���@��@��@��@��@��H@���@���@��@���@��@���@�ڌ@��S@��.@��J@��v@��@��@�ײ@��3@��@�ֹ@��@��.@�ѡ@��&@��T@��@��@���@�̪@���@���@���@��;@��h@���@�rs@�<�@�!�@��@���@�^�@�G,@�-N@��@��u@��T@��\@��w@��_@�Ɠ@��F@���@���@��%@��\@���@���@��[@���@��j@��b@�~O@�x�@�t>@�ra@�q�@�pb@�j�@�Z�@�K7@�>�@�7w@�5W@�1�@�$@�!�@�%@�+.@�*�@�+j@�,:@�,f@�+�@�+�@�+Z@�+)@�,'@�,�@�,�@�,�@�,j@�- @�,�@�/�@�0�@�4�@�1@�0@�0-@�1S@�$�@��@��@� �@���@��@���@��V@���@���@���@���@���@���@��@��O@��v@��e@��^@��@��$@��~@��[@���@�ϗ@��b@���@��@�v�@�]<@�D�@�$4@�"@��@� A@���@��B@�ѹ@��B@��@���@��2@���@���@���@��@�}�@��b@�|�@�}�@�}�@�}�@�|�@�|2@�z@�t*@�s+@�l�@�j�@�^�@�[V@�[@�Y�@�Wy@�UH@�R�@�LC@�D�@�=J@�5�@�!B@���@���@�h@�(�@���@���@�ł@���@��@���@��O@�~�@�H�@�<�@��@��_@��.@��6@��J@��j@�w2@�_�@�K�@�Fr@�D�@�D'@�D>@�BH@�CX@�C@�?Q@�??@�>B@�<`@�:�@�;@�;&@�9�@�6�@�5�@�,�@�6@��@��@�	�@�	�@��@�M@�:@��@���@��i@��@��>@���@�� @��s@���@��@��Q@��J@��z@�}�@�|�@�|�@�x�@�v�@�tV@�r\@�p�@�o�@�n.@�m�@�l]@�h�@�ds@�^^@�Sz@�LE@�I�@�F�@�D>@�CA@�A�@�>�@�<�@�;:@�9.@�6�@�0�@�.\@�)�@�%4@�"�@� �@��@�Q@��@�@�	�@��@��@��@���@��o@��Q@��y@��?@���@��z@���@���@��j@��@���@��M@�{@�q$@�pR@�i�@�f)@�Z@�O8@�=�@�5�@�(�@�#@��@��@��@��@�
�@�	�@�	j@�	@��@�)@�.@�@�b@��@��0@��]@��b@���@��~@���@��4@���@���@���@���@���@��@��@��@��2@��x@���@��@��q@���@��"@��%@�ؙ@��@��e@�Ҍ@���@��8@��w@���@���@�σ@�� @��@��R@�ʇ@���@�ȍ@��@��$@��W@���@��*@���@���@��(@��n@���@��p@���@��J@���@��	@���@��^@���@���@���@��&@��@���@�}�@�@�� @�~�@�~e@�v�@�{L@�y�@�~�@�t�@�l�@�jj@�jD@�i�@�k>@�lL@�gN@�c�@�b�@�c�@�e@�g"@�f�@�f'@�e�@�_�@�^\@�]�@�]�@�]�@�]�@�\�@�X�@�Xi@�X�@�Y5@�ZE@�]O@�[n@�\f@�\}@�`m@�_]@�]R@�\@�T�@�L2@�L�@�J�@�I�@�E�@�C@�;�@�8�@�8H@�4�@�,�@�'S@�"�@� b@��@�J@� �@�n@��@��@��@��@�@��@��.@���@���@��h@��	@��=@���@�z:@�p�@�k�@�Z5@�?V@�;&@�0m@�'@@��@��@�3@�@�W@���@��g@��@��u@��@��@��6@���@�ԕ@���@�ǒ@���@���@��z@��m@��f@���@���@��:@��@��e@��f@���@��@���@��z@��@��J@���@��@�|�@�t*@�j@�Y�@�AN@�(R@��@�@��@�e@��@��@�.@��@��@��@�&@�@��@�_@�K@��@�	E@��@�X@�J@��@��#@���@��7@��@��j@��@���@��@��@��$@��*@���@��6@��!@���@���@�R�@�Ts@�]�@�_�@�e>@�c�@�j�@�q�@�y�@�|�@��@��@���@���@���@��@���@��@@��@��?@��F@��V@���@���@��E@��~@�¦@��>@��P@�ҝ@�֍@���@��X@��F@���@���@��@���@���@��@���@��@��U@��@�^@��@�+@�;@�Y@�,@�|@��@��@�O@�R@�#�@�*�@�.t@�/�@�:�@�;:@�85@�;'@�C�@�I�@�M�@�O�@�S'@�c�@�d@�m@���@��@��(@��E@���@��@�	�@�-@�6f@�@�@�T�@�V�@�Uq@�X�@�c�@�m
@�yg@�vr@�~�@���@��r@���@��_@��{@���@�
?@��@�,|@�D*@�N}@�U�@�N�@�\A@�l�@�u@�|�@��q@��K@��
@���@��A@���@��j@���@���@���@��t@��@�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   4444344444334434444444434434444443443444444344443443443444444443444444444434444444444443433444444444444444344344434433344443433444444444444434434443344333444344433344333443344433334443333444433334434344444333343333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�:��p:��T:���:��U:��s:��s:��s:���:��s:���:���:���:���:���:��:��:��:��b:��0:��~:��0:���:��0:�� :��:�� :��:��d:��b:��v:��s:��p:���:��:���:�� :���:���:���:���:���:���:���:���:���:���:��:��0:��O:��3:��:��B:���:���:��:��p:�� :�� :���:���:��]:���:���:���:���:��k:��p:���:��^:���:��[:��V:���:���:���:���:���:���:���:���:��:��?:���:��v:��:��u:��*:��R:���:��:��a:���:���:��:���:���:��Z:���:���:��f:���:���:��9:��	:���:���:���:���:���:��*:��y:��	:���:���:���:��+:���:���:��P:��:���:���:��i:��@:���:���:���:��A:��d:���:���:��{:���:���:��*:��l:��N:���:��:��K:��::��/:��=:��=:��L:��}:��}:��l:��{:���:���:��z:���:���:��N:��N:��l:��z:���:���:���:���:���:��:��:��:��':��(:��+:���:���:��(:��+:��U:��]:��E:��x:���:���:���:���:���:���:���:���:��#:��$:��F:��&:��6:��d:���:���:���:���:���:���:��f:���:��:���:��d:���:���:��a:���:���:���:���:��\:��=:���:��:��>:��:��/:���:��}:���:���:���:���:��:��:��-:��K:��I:��*:��6:��<:��-:��K:���:��Y:��]:��i:���:���:��x:���:���:���:���:���:���:��i:���:���:���:���:���:��z:���:��3:���:���:���:���:���:��`:���:��':��C:��':���:��C:��(:��6:��:��):��%:��(:��%:���:���:��K:���:���:���:���:���:���:��
:��:���:��i:���:���:��v:��K:��::��N:��J:��K:��i:��{:��I:��:��:��:��*:��+:��N:��<:��Y:��l:��i:���:��:��:��::��*:��:��
:��-:��K:���:��]:��g:���:��g:���:��}:��}:��P:��}:���:���:��\:��`:��.:���:��.:��:���:��:��1:��/:��N:��O:��2:��L:�� :��:���:��a:��o:��m:���:��{:���:���:���:���:���:���:���:���:���:���:���:���:���:���:���:��:�� :��
:��:��9:��9:��-:��9:��9:��K:���:���:�� :��:��m:��L:��L:��/:��O:���:��_:�� :��M:��-:��/:��/:��1:��/:��P:��^:��^:��m:���:���:��@:��:���:���:��n:���:���:��#:�� :���:���:���:��t:��C:��s:��`:��C:���:��:��6:��:��:��6:��W:���:���:��z:��l:��:��:���:���:��:���:��:��:��:��:�� :��:���:���:���:��:��:�� :��:��:���:��:���:���:��q:���:���:��#:���:���:��:���:��c:��e:��U:��D:���:��R:��a:���:���:���:���:���:���:��2:���:���:���:��:��L:���:���:��S:��D:��b:��S:���:���:��:��:���:���:��c:��e:��E:���:��e:���:���:���:��:���:���:���:���:���:���:���:���:���:���:���:���:���:��A:��>:���:���:���:��p:��p:���:���:�� :���:���:���:���:���:���:���:���:���:���:���:���:���:��c:��c:���:���:���:���:���:���:���:���:���:���:���:��e:���:���:���:���:���:���:�� :��:���:���:���:���:��:��:��:��0:�� :���:���:��	:���:���:���:���:���:���:���:��p:���:���:��~:��}:���:���:���:���:��^:�� :��:��/:��2:��>:���:���:��U:��U:��x:���:��g:��?:�~�:�~�:�}�:�}':�|�:�|8:�}:�|�:�}:�}:�{�:�z�:�z�:�x*:�w�:�w0:�v�:�v6:�ug:�tq:�s:�p�:�j�:�b�:�Z:�J�:�0�:�x:��I:��y:��d:�bt:�P�:�=D:�$I:�+:�:� �:��v:��e:��M:��:��y:��F:���:��+:��j:�� :��l:��_:��z:��v:��):���:���:��9:���:���:��R:���:���:��[:��:��~:���:�v�:�t�:�wC:�{�:�{�:�|:�|�:�|�:�|P:�| :�|:�{�:�|�:�}):�}:�}9:�|�:�}W:�},:�B:��*:���:��N:��:��:��|:�w):�_�:�\�:�\:�[):�\�:�[<:�[	:�[i:�[+:�X-:�V�:�UG:�T~:�O�:�NG:�Ke:�J�:�KS:�K:�Jh:�I,:�ES:�@x:�7D:�#�:�:��l:���:��:��:���:���:��?:���:���:��v:�y:�s{:�h�:�ce:�\�:�X:�TO:�I�:�D�:�9�:�?!:�9�:�:I:�:
:�:
:�9�:�8�:�7d:�2�:�2<:�-f:�,:�"�:� a:� ":�:�|:��:��:�:��:�	�:�	:���:��":���:�j:�:�:�f:�):�:�;:��:�:��:va:&::~´:~��:~~�:~W�:~#V:~�:}��:}��:}��:}��:}��:}�G:}�i:}�y:}�:}��:}�:}��:}�r:}��:}�:}��:}��:}��:}��:}�j:}|i:}a�:}W�:}K�:}G�:}G�:}D�:}D:}B}:}@H:}7�:}$�:}:}�:|��:|��:|��:|ױ:|��:|�M:|��:|~�:|u�:|t�:|t�:|n#:|k�:|g�:|d�:|b�:|a:|^�:|^:|[�:|V=:|P:|F�:|6�:|+�:|'�:|#@:|�:|Y:|j:|�:|�:|R:|A:|R:|�:{�:{��:{�S:{��:{�:{�:{߃:{�x:{�):{�l:{�:{�0:{�:{��:{�@:{��:{��:{y�:{b�:{Ni:{>:{3�:{!^:{j:{
:{ <:z�a:z�:z�K:z�v:z�:z��:z��:z��:z�C:zw^:zh�:z`�:zN�:zQ�:zNF:zJT:zH�:zH:zG�:zE�:zA�:z@C:z@ :z?:z<�:z8�:z7�:z6:z4�:z3>:z2D:z/�:z-�:z,�:z+:z)t:z'�:z&:z�:z�:zU:z?:z	�:z:z�:z7:z�:zG:y��:y�:y��:y��:y��:y�g:y��:y�:y��:y�X:y�:y��:y�:y��:y��:y��:yҢ:y�U:yţ:y��:y��:y�U:y��:y��:y��:y�(:y��:y��:y�:y�,:y��:y�V:y��:y�	:y��:y�(:y��:y��:y{�:yv�:yx�:y��:yxu:yw�:yl+:ys):yp�:yx�:yiU:y]�:yY�:yY�:yX�:y[:y\�:yU4:yP:yN:yO�:yQ�:yT�:yTW:ySz:yR�:yJ(:yG�:yG4:yF�:yF�:yF�:yE�:y?�:y>�:y?X:y@:yA�:yF::yCi:yD�:yE :yJ�:yIN:yF?:yDe:y9�:y,�:y-�:y*<:y(�:y#:y�:y.:yZ:y�:y	]:x��:x�W:x�:x��:x�:x�M:x�:x�:x�T:x�}:xٻ:xӴ:xÃ:x�y:x�>:xU�:x0:x�:x(:x�:x�:w��:w�:w�^:w� :w��:w�v:w�e:wu�:wf):wV�:wN�:wKk:wAW:w3:w-�:w&�:w:w:w	|:w�:v��:v��:v��:v�I:v�2:v�C:v�):vՖ:v�:v�n:v��:v��:v��:v�:v��:v�?:v�:v�X:v��:v��:v�v:v�x:v|�:vv5:viP:vZ:vA�:v:u��:u�x:u��:u�:u��:u�-:u��:u��:u�!:u�:u�.:u�n:u��:u�M:u��:u�+:u�K:u�%:u�q:u��:u�/:u��:u��:u�7:u�:u�b:u�g:u��:u�k:u�9:u�W:u��:u�:u��:u�:u��:u��:u�N:X?�:XB-:XP:XR�:X[V:XY&:Xc�:Xn:Xy�:X~t:X��:X��:X�=:X��:X��:X�t:X�B:X�?:X�:X�;:X��:X��:X��:X�;:X�=:X�:X�K:X�+:X�D:X�7:Y:Y.:Y�:Y�:Y8:Y�:Y�:Y	:Y&b:Y*�:Y1:Y0�:YB8:YF(:YHD:YH�:YJ�:YL�:YW8:Y_�:Ya�:Yh�:Yl�:Yp�:Yp�:Yx�:Y��:Y��:Y�:Y��:Y��:Y�p:Y��:Y��:Y��:Y�w:Y��:Y��:Y��:Y�8:Y�:Z)w:ZFq:ZO�:Z[�:Z|Y:Z��:Z�C:[N:[O:[#�:[A�:[D�:[B�:[G�:[X�:[f.:[x�:[tF:[�s:[��:[�2:[��:\�:\ �:\=�:\Q�:\a�:\�	:\��:\��:\�):\�8:\̜:\�A:\��:\�s:]Q:]:]&1:](�:]1 :]6H:]O5:]O�:]QE:]r0:]��:]�s:]�jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�#B�#B�#B�#B�#B�)B�)B�/B
�{B�5B�5B��B)�B#�B  B�B�B  BBB  BB1B	7BJBVBPBPBPB	7BB��B��B��BJBVB
=BBBB  BB�B.B,B�BhBB��B�B�B�;B��B�jB��B�\B|�Bs�Bn�BbNBT�BA�B49B(�B	7B��B�BBPB�sB�
B��B��B�}B�jB�!B��B~�BQ�BO�B6FB�B	7BB
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?֑u@
5?���Bi�?�?@�r�?�qvAsb[?Y-VB*PA�j?:K�A�7�B��?_�?���Ao�>�Ϸ>��>>�u:?{@�mA�e�?S@��nB(�?$��A2��A 
>�6z>�'k?��WB��?3Y?��
B(Y@{??U�A1�ACr�?<?�)AB-�?��?��>�c�?'A��?��?���B<i@�u2@I��Bx?���>��>���?8nn>�/�>��A?/̎@F:�A��Z>�� >� ?@BB?J�?��Ak�>��>�>>�1�?FG�BXr?�>���>�4?ė?g�?��W@��Y?!&A^�O>�?�?r�jBd�?���B)CBE�>��2>��_>��@�ɬ?�b?b��@���>�w?Q6�At�@?L��A}��>��$?IsA�XrB+~?e!?_�B$B@�|?D_[AW�B)�?�c�?m��B(GB%�BK�?
u@��@�w�?Q�A�CC?f�VB*B)�@�'�?�>�(�>��>�V�?!�?��A��@ϯ�>��M>�'?�?1ZB��?b�?��]B��?\��?]�w@�ڒB,Bj]?1�?��B&�B)\BD�>���?+V@��8Bme?�pe>�[u?�1/B..B'&B62?�2A%�B)�B&�BX/?R.?���B*�B�|>�`p?=a,@ԭsB)�B(&B&�A�ӏ?�l?*A#��B1:B,�B0�B3�@<�?�"?1	�@�BhB.?B0�B0�BB�?NrA:��B�A *B;�?�$W@[��A/:k@��A�ZB-&B0WBY�B1IA��8A��"BO�A�I�B1�B0nB-�B1B2-B2�B0{B0�B0,B0DB3aB-fB4�B2�B0�B.�B.�B.aB.�B/�B,�B04B0�B/�B.�B0FB-�B1�BnB4<B0�B0fB1/B/�B0�B6-B2}B1�B1<B2�B3�B2:B6�B/�B3�B1NB/B.�B0kB/�B1�B.<B/�B-�A���B2-B8+B-�B/?B/�B04B0B0�B/�B0(B-�B2�B.�B0�B1/B0�B.~B/?B.7B.�B/2B/�B/�B0FB0FB01B1B/�B/}B/:B0�B/?B2 B.�B.�B0BB0pB0^B2�B-AB0�B0NB/�B0�B/�B/�B/�B2:B0�B0�B/�B-"B-�B0B0�B/:B/�B0(B0nB.B.uB0�B0�B/�B01B1B/:B1�B0�B.B/QB1B0"B.�B/�B0�B0B/�B/2B.�BҡB,LB0�B/pB.�B-B-�B0�B-�B.�B/B1�B/9B.hB.hB0XB/�B3�B-�B/B.�B/�B.hB0<B/bB3�B0�B0�B0�B1�B0�B1B1B/�B/�B/�B0SB0�B/�B0,B0�B/gB/�B.�B/�B0#B/�B/�B/�B0�B<JB0hB08B0�B/�B/�B0�B0�B/�B/�B-�B.�B/�B0XB.�B/bB1B1rB1B/�B.pB1{B/VB0,B-�B.�B-�B/B-rB/MB.�B/gB0�B0�B.�B2Q@^!B/�B.CB,^B-'B.[B..B.NB/�B-�B/B/�B/A��B/�B/�B.�B0SB/9B/B0�B/�B1>B0�B0B1�B/�B/UB.�B.�B.�B0B0B1�B1sB0�B0DB0�B+sB/�B,<B/�B0]B.zB/;B.�B.�B1B0�B1#B.�B1B1qB/:B.�B/xB0�B/pB/�B/hB0]B1YB.�B/mB/B.�B/pB/B.HB/	B.3B/�B0�B1B0�B0B2IB/�B0�B0<B0OB1B/�B0fB0>B1�B1B1VB1�B.�B/�B/bB0�B/�B/B0�B/�B0GB2$B1[B1 B1KB1cB0�B0�B/ZB0�B1�B1ZB/�B2B2B/�B0HB/�B0@B1`B0�B1OB.�B0�B/fB/�B1_B2B1OB0EB0�B1\B0�B2B1%B/�B0B11B/KB0�B2 B/�B/�B/�B1.B/�B0HB/8B0XB0>B0�B0pB0�B.QB1B.B0B0FB1�B1�B.TB/aB/^B.;B/AB.tB-8B.B.�B/\B-�B0BB.�B.�B-�B,�B+rB.(B)wB%�B0B1B/�B0fB0�B0oB/�B0�B/�B1@B0BB/hB/�B/�B0JB/�B0%B0�B0kB0B0eB/�B/�B/�B-�B.B1�B0sB.�B.�B/�B0�B0BB02B/�B/�B/�B/eB0%B/�B0kB/AB0�B0�B0�B/�B0
B02B0�B0�B0�B0uB0dB0�B/�B0�B1�B0,B0B0~B0B0�B0�B0AB0�B0aB0B18B0�B/�B/�B.ZB0�B0�B0tB0kB0[B1B0KB1PB2B0#B0�B0iB/�B0�B1	B1B0�B1�B1B0�B0�B0�B0�B0�B0cB0�B0KB/�B1	B1_B1	B0�B0`B0�B11B0�B1�B1�B1�B0�B1�B0�B1jB1�B1�B1\B0�B2B1<B1B2FB1�B1^B0BB1BB0�B1�B1�B1|B0�B1lB1dB1SB1�B1CB1�B1�B1�B1yB1B1(B0�B1vB1nB1@B1�B1(B0�B1�B1B1�B1B2CB1�B1B2CB1�B3ZB2aB1�B2vB3pB2�B1�B1�B1�B1"B1�B1�B1"B2AB1�B1�B2B2B/�B2B0�B1�B2�B1�B1yB0�B2�B1B2�B2B2hB2XB2PB1�B2�B1{B2�B2�B2�B2zB1�B2jB2B2�B2�B3B3(B2�B2�B3[B2�B2�B3�B3;B2�B2wB2�B3�B2^B2�B1�B2FB2�B1�B1�B3B3�B2�B3JB3$B2�B2�B2�B3IB2�B3�B39B31B3)B3)B3!B3B3B3�B4B4(B4EB4=B4[B4-B4SB3�B3.B3�B3�B3|B3B2�B3�B3\B3�B3�B3�B3,B3�B3�B4�B2�B4�B3�B3�B3�B3�B3�B3�B3�B3YB3�B3HB3@B3�B4lB4B4�B5&B4TB4rB4�B3UB4bB3�B4�B3�B5KB4�B5CB4�B5;B5SB3�B5+B4B5#B4QB5�B4�B4iB4�B4�B4�B5mB5�B5B5]B5B5}B6�B5�B5�B5�B5�B5�B5.B5�B5�B5�B7$B65B6�B5�B6rB5�B6B6rB73B6�B5�B6�B6wB5�B6B6oB6oB4B6gB5RB5�B3NB4/B6kB6�B4�B5nB68B7B6�B7B5�B6iB6�B6B7�B6B6B6�B5�B6�B6xB6�B6�B8AB7B8AB89B89B5�B6�B7�B7)B6�B6�B7MB6�B6�B6�B8AB8AB8�B7xB7�B8�B7B89B81B81B7gB8|B6�B7�B8	B7�B8	B7�B7WB7�B8B7�B7�B7�B7�B7�B7�B7'B7�B6VB7}B7}B7KB7B6;B7�B7�B7B6�B6FB7�B7/B6�B7'B7�B7'B7�B8NB7}B7}B7�B6bB5�B5EB7#B6ZB6B6ZB6RB5�B6rB5�B5�B4�B5�B4�B4�B4sB4�B4kB4�B4�B3�B2�B3aB2YB0�B0�B0�B1VB.�B.�B-
B-B,�B-B,FB,�B,�B+�B*�B*B)�B(�B)3B'ZB%�B$�B �B�BaB�B�B�BB2B�B�B�B�BQB�B�B�B*B�B��B7B�B B OB��B�B��B�B�eB�B��B�HB�BސB��B�^B͔B̪B�[B�B��B�<B��B�QB��B�<B��B�B�hB�B�B�gBKBB�B�B!�B1�B:�B=�B<B;fB<AB8wB<�BD5BD BB BA�BB�BD�BHBLBPBQBN�BJ�BFMBD�BJYBQ�BY8BbBb�B`�Ba-Bt�Bz<B|�B�B�B�{B�:B�zB�eB��B�B�(B��BGBgB~�B|yB{�Bz�By�Bu�Bt�Bp:BhBaQBX�Bl�Bx�Bx�BxZBv�Bu�BtRBt�Bq�Bm�Bl�Bk�Bi\Bf(BdB`�B`B^�BZ�BU�BR�BN�BLHBFjBA`BB�BRBR�BPKBMBJBH�BM�BP�BO�BG�BKBSNBV_BU�BUBi�BQ�BQWBPEBQBPDBP*BR�BPBO�BM BI�BHBC�BB�B>�B=�B8�B/1B1�B/NB-ZB+�B'�B$�B[B�BVB�B�B�?B�B�B��B�>BGB!�B'B �BcBdBdB �BYB	�B�B�B�BnB}B�BBB-�B1�B2�B1�B1
B2�B2UB2;B1�B1�B/�B/xB/^B1�B2,B0CB.�B,VB)�B2�B7BA1BFBD�BE<BE"BDB@�B<,B?BB�B@%B>�BA�B<�B9(B=�BA"BG�BK(BPaBQ�BP�BRBR�BPYBQ�BP�BP2BO>BO�BM�BM�BLhBL]BJ�BNdBO9BNOBNhBM�BMBI�BK?BI^BF BD�BBVBA�B9!B9�B+�B"�B!B$�B(.B#:B.B(�B yB 9B0�BD�BM�BO�BJ0BU�BT*BF�BL#BD:B[�BE�BB�BDBB7B:�B6�B.FB4kB<2BDBGBFjBMNBNLBQ*BTNBV�BX1BW�BX!BWeBV�BX�BX�BW�BX%BW�BYCBYBYeBY�BZ�BY�BY�BZuBY�BZ�BZ�BY�BYBW�BW�BZ�BY;BZ�B\�B[�B[1B[�B[gB\<B\�BZ�BZ�B[�B[jBZ�BZ�BZBZ[BYmBYBX&BV�BW3BU�BX�B\OB\�B\B]B^�B^�B_/B_�B_�B]�B]�B\�B\WB\OB\�BZBY?BW�BX�B\�BZ�BaRBeHBbDB`dB^�B^CB`�Be�BbfBb�BWB]CB_�Bb8BaqB`�Ba.Bb_Bc*Be|Bd�BcsBa�BbBa�B_�B]BadBcBafBa�B`�B`�B^`Bb�B`'Ba�B`]B`�B]�B]B\FB[0BZUBZBVeBV/BV�BVzBS�BRlBQ�BP�BV~BY4BW�BS1BO�BSBBS�BS]BPxBO�BLJBJ�BR
BY+BgsBj�B\1BW�BVVBDB;DBFxBD�B=�B=�B;B;�B12B)�B:�B;#B:�B9�B<�BAyBB�B?=B=�BA^B?HB@	BAtB?�BAWBB�BA�B@{B?bB@�BEBF{BD�BB%BBxB?�B=0B@CB?2BB�B@�B?)B=B< B=XB9yB9(B9B8=B6BB4zB3<B1�B1vB<_BFBP�BRlBR1BR�BUBV�BX+BX�BYBX9BW�BYtBW�B[�BW:B]+B\MB[OBZ=B]@B\�B\�Ba#B_�B_HB_�B]mB\�B_B_rB_-B^0B_LB_WB`�B_ZB]�B:dBKnBSrB:BD�BFBA�B;B4�B(�BC�BD�B;dB/�B=�B3�BA%BB�BL�BG�B9B?�B;BE�B>�B;�BEABB,BL�BT�BW�B[dBd�Bh�Bc�B]�Bk�BsBiBgBp�BYLB_�BmBPWBQ�BRDBW?BTB\5B[qBV[BK�BY�BbKB`.BWBe
Be�Ba�BO�Bf�Bn�BuBwyBhAB[8Be�Bn�Bd�B��Bj�Bm�B|�BW=Ba#BWSBl�B�*B��By^Bi�Bd�B�Bc�Bp�B��B��B|�B��B�/B��B�]B��B�wB�mB�
B��B��B��B�DB��B��B��B��B�KB�[B��B��B�$B��B�[B�IB�LB��B�`B�%B��B�"B�cB�B�!B�B٠B�7B�/B��B�B��B�WB��B�B��B��BٜB�TB��B��B�B��B��B�uBذBؕB�\B�TB�'B��B�"B�-B�$B�B�UBپBضBٮB�B��BؖB؎B�sB�kB�=B�"B�-B؃BشBؾB��B��B�SB�TB�B�B�,B�\B�6BׄB؃B֏B׷B�FB�cB�\B�.B�B��BؘB֍B�B�iB�JB؜B�rB�B�xB�KB��B��B��B�vB�B�GBمB�kBخB�qBػB��B��BخB��B�5B�KB�MBڱB�%B�0B�FB��BؽB٬B؇B�B�fB��B��BؼBؙB��B�qBحB�B؊B؟B�B�[B؃B��B�YBزB��B؝B�lB�jB�B؏B��B�B��B�ZB��B�B�rB�B�xB�CB�B��B�"B��B٬B��B٧BٱB��B��BٯB��B�B��BّB��B�fB�9B��B�B�QB�zB�9B�<B�BقBڌB��B��B�wB�oB٘B�OB�B�oB�hB٘BوB�,BٕB٘B��BٹB�BَB٘BٮB٦B��B��B�:B��B��B��BءBِB�PB�nB�pBَBٯB�BٱB��B�FBٜB٧B�-B�CB�sBٯB��BٔB�-B�tBڤB�B��B��BلB�0B�NB�vB�\BڌBڴBڬB��B�B��BڗBڢB�YB�>BڔB��BڗB��BڒBۜB��BڄBڢBڭBڥB��BڠB�_B��B�B�eBڻB�TB��B��BۿBۆB��B�B�HB�BۇBۆBڬB�B�XB��B۱B�xB�
B�B��B۹B۩B��B��B��B��B�rB��BڿBۤBۧB�.B��B�B�B�lB��B�gBۗB��BۀB�]B�hB�`B�XB�uBۀB�B۴B�~BۛB۹B۱B�B۴B�
B��B�B�*B�fB�-BۄB�iB�B��B�B�nB۲B�yBۄB��B�tB�BܟB��B�_B܏BܚBܒB�ZB�RB�B�dB�B��B܈BܦBܰBܨB��B��B��B��B�=B�HB�5B��B��BܴB�B��B�B��B�B�B�B�B�%B��B�
B�B�B� B�+B�5B�B��B��B�0B�(B�XB�XB�B�HB�@B�B�B��B�,B�B݂B܍B�B�&B�DB�9B�OB��B��B��B��B��B��B��B�B�B�B�)B��B�B��B݂B�BB�B�	B�B��B�kB�=B�*B��B��B�SB�SB܌B�8B��B�VB��B��B��BܿB��B�FB�7B�oB��B��BݍB݅B�'B�'B�rB�'B�DB�2B�DB�DB�<B�)B�OB�B�B�"B�4B�B�"B��B��B�B��BݳB�BݡB��B�B�AB��B޶B�BަB��BޓB�yB��BދBޞB��B�@B�B��B�{B��B߃B�]B�pB�UB�{B�B�0B��B�kB�XB�EB�2B�cB�EB�~B�cB�XBަB�nB�-B�B�SB�-BދB޼B��B�B��B�%B�B�B޴BޙBޙB޿B��B��B��B�(B��BކB�~B��B�(B�MBޮBޮB��B��B�PBޟB��B��B��B޲B�BބB�^B�LB�0B�VB�|B�CB��B��B�B�aB�B�tBޙBޒB�3B�lB�lB�lB�B��B�QB�wBޯB�QB�\B��B�B�B��B��B��B�AB��B��B��B�B�AB��B�#B�[B�#B�SB�fB�@B�B��BݵBݏB��B��B�iB��B��B��BݿBݒB�lB��B�B�dB�+B�wB�B�zB�B�B��B۟B��BܗBݕB�]B�B�B�BܲB�B۲B�\B�ABېB۞B۱B��B�,B�hB�/B�3B�BؗBצB֏B��B׿B�]BݷB��B�B��B��B�B�B�B�B#B�B�BJB�B�BXB�B/B	B�B]B)B�B�BeB}BFB�B[BkB?B�BB�BQB&BiB�BB�B�B�B#�B*6B*�B,B.PB/@B0�B0�B1hB2B3�B3�B3hB4lB3�B3�B3�B5>B5�B6�B5�B4�B4B6�B<�B4 B3QB2�B2<B4`B3�B3�B4B5MB4B3MB2�B4;B1B1aB04B0B0�B1B0�B20B2�B2�B3;B5WB/�B0�B0B/B.�B'B#�B#�B -B �B�BHB�B,B�B�B�B�B@B�B
?B+B
{B[B
:B
:B
bB�B	MB�B�B�B�B'BVB	B�B�BuBQB�rB�B��B�DB��B��B��B�B�dB�!B�B��B��B��B��B�iB��B�B��B��B�UB��B�{B�QB��B�B��B�B��B��B�ZB�fB�!B�B�+B�B��B��B�B�iB�,B�B�7B�AB�"B�qB�B�WB�B�yB�B��B��B��B��BZB�B ,B(B �B�HB�PB �B��B �B�gB
B&B
BnBvBBB:BkB(B2B�BB�B�BCBvBKBgB�BHB�BIB�B yBzB!BB aBWB�B��B B��B EB �B *B �B �B�LB�B�+B PBB�B�B�BBB�B nB 7B�>B�IB�9B��B�fB�wB�xB��B�IB��B�2B��B��B�&B XBB��BwB�B�BB�BBwB1B
B�B�BB�B�B�ByB>B	B	B	@B�B	-B�B�B	�B
B)B
-B	qB	�B	�B�B	/B	0B	B	%B	aB
;B�B	LB	iB	�B	B	�B	B
=BUB	YB	�B�B4B�BB�BB|B�BJBjB~B�BmB�BB�B�B�BB0BHBNB�B�BqBBB�BoBBDB&B�B�BBB�B�B�B.B�B�BiB�B�B�BEB�B.B�B�B�B2B�BQBIBBLB�B�B,B�B%B�B[BrB�B�B�B#B�BgB�B�BB�B!B�B!BuBgB�B�BeBfBdBIB
EB
�B�B	�B	�BKB�BB�B�B�B	�B�BB/B}BjB �B B,B�GB��B��B�!B�B�MB��B�oB��B�sB�nB��B�NB��B��B�TB�lB�gB��B�B�PB�B�KB��B��B��B��B�B��B�'B�,B�B��B�/B�\B�VB��B��B�B��B��B�HB��B��B�~B��B��B�	B��B�HB�>B �BiB�B	lB	�B�B�BB�B�B�B4BpB�B�B�B�B�BCBjB,B�B�BB�B�BB�B�BoBFB
�sB
�B
�kB
��B
��B
��B
�LB
�uB
�iB
�2B
�
B �B
�B
�B
�B
��B
�7B
��B
��B
�zB
�FB
�bB
�	B
�vB
��B
��B
�IB
��B �B
��B
�UB
�=B
�B�B�B �B �B�B �B%B#B BgB�B�B�B�B&B7B
B�B
\B�B
B{B)B	IB�BtB\B
�B�B�BB�B�B�BMBBB	�B�B	B�B#�B<B!LB!qB)�B'oB)�B+�B,uB0TB)PB,�B/{B3'B6&B0B0`B4HB?�B?�B7�B?aBE�BA�BE�BJ�BN2BS�BXBO	BQ�BRBRB[BVhBY�BYB[qB[�Bc�B`�B]�B`�Bg�B_B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�422222222222222222222222422222222222222222222222222222222222222222222222222222222222222242222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444344444334434444444434434444443443444444344443443443444444443444444444434444444444443433444444444444444344344434433344443433444444444444434434443344333444344433344333443344433334443333444433334434344444333343333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�B�B�B�B�B�B�B�B�B�"B�B�(B�(B�"B�&B�(B�0B�.B�.B�.B�,B�3B�1B�:G�O�B�=B�?B��B*B#�B B�B�B BBB BB<B	@BUB`BZB\BYB	ABB��B��B��BTBaB
EB)B*B(B 	BB�B.!B,B�BrB-B��B��B�B�EB��B�vB��B�eB|�Bs�Bn�BbZBU	BA�B4CB(�B	@B��B�B(B\B�}B�B��G�O�B��B�sB�,B��BBQ�BO�B6RB�B	ABB
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bi�G�O�G�O�G�O�G�O�G�O�B*ZA�xG�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�e�G�O�G�O�B(�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�B(bG�O�G�O�G�O�G�O�G�O�G�O�B.G�O�G�O�G�O�G�O�A��*G�O�G�O�B<vG�O�G�O�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��lG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BX}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bd�G�O�B)MBE�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B+�G�O�G�O�B$LG�O�G�O�G�O�B)�G�O�G�O�B(QB%�BK�G�O�G�O�G�O�G�O�A�CSG�O�B*B)�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�B��G�O�G�O�G�O�B,BjfG�O�G�O�B&�B)gBD�G�O�G�O�G�O�BmpG�O�G�O�G�O�B.:B'.B6<G�O�G�O�B)�B&�BX9G�O�G�O�B+ B��G�O�G�O�G�O�B)�B(1B&�A�ӡG�O�G�O�G�O�B1EB,�B0�B3�G�O�G�O�G�O�G�O�B.HB1B0�BB�G�O�G�O�B�G�O�B<G�O�G�O�G�O�G�O�G�O�B-0B0aBY�B1TG�O�A��0BO�A�I�B1�B0yB-�B1
B28B3 B0�B1B05B0NB3kB-qB4�B2�B0�B/B.�B.nB.�B/�B,�B0?B0�B/�B.�B0QB-�B1�BnB4GB0�B0pB17B/�B1B68B2�B1�B1GB2�B3�B2FB6�B/�B3�B1XB/B.�B0xB/�B1�B.FB/�B-�G�O�B28B88B-�B/IB/�B0?B0 B0�B/�B02B-�B2�B.�B0�B17B0�B.�B/IB.AB.�B/<B/�B/�B0QB0QB0=B1B/�B/�B/EB0�B/IB2B.�B.�B0KB0{B0iB2�B-MB0�B0WB0B0�B/�B0B/�B2FB0�B0�B/�B-*B-�B0B0�B/EB/�B02B0yB.+B.B0�B0�B/�B0=B1B/EB1�B0�B.B/ZB1B0)B.�B/�B0�B0$B/�B/<B.�BҫB,VB0�B/xB.�B-B-�B0�B-�B.�B/&B1�B/BB.rB.rB0bB/�B3�B-�B/B.�B/�B.rB0JB/lB3�B1 B1 B0�B1�B0�B1B1&B/�B0B/�B0[B0�B/�B05B1 B/nB/�B.�B/�B0-B/�B/�B/�B0�B<TB0qB0AB0�B/�B/�B0�B0�B/�B0B.B.�B/�B0bB.�B/lB1B1}B1B/�B.{B1�B/`B05B-�B.�B-�B/"B-}B/XB.�B/nB0�B0�B.�B2ZG�O�B/�B.NB,iB-4B.fB.8B.XB/�B-�B/B/�B/"A��B/�B/�B.�B0[B/BB/B0�B/�B1KB0�B0 B1�B/�B/^B.�B.�B.�B0B0"B1�B1~B0�B0OB0�B+}B/�B,KB/�B0hB.�B/GB.�B.�B1B0�B1.B.�B1B1yB/EB.�B/B0�B/}B/�B/qB0eB1cB.�B/xB/$B.�B/{B/B.VB/B.=B/�B0�B1*B0�B0B2VB/�B1B0DB0XB1!B�B�,B�#B٪B�AB�8B� B�B��B�`B��B�B�B��B٥B�\B��B��B�B��B��B�BػB؟B�eB�^B�1B��B�,B�8B�.B�%B�^B��BؽBٷB�B��B؟BؕB�}B�vB�GB�/B�5B؎BػB��B��B��B�\B�`B�*B�B�3B�dB�?B׍B؋B֕B��B�LB�jB�hB�7B�'B��BءB֓B�B�sB�RBاB�}B�B؁B�VB��B�B�B�B�)B�PBُB�uBصB�|B��B�B��BظB��B�?B�VB�[BڼB�0B�;B�MB��B��BٸBؑB�B�qB�B�B��BءB��B�|BصB�"BؓBةB�)B�cB؎B�B�eBغB��BاB�wB�qB�BؗB��B�B��B�bB��B�&B�}B�BقB�NB�(B��B�+B�BٸB��BٰBٻB��B��BٸB��B�&B��BٚB��B�rB�DB��B�B�[BىB�@B�BBٌBٌBڗB��B��BځB�vB٢B�[B�B�xB�uB٠BٖB�4B٢B٢B��B��B�BٚB٢BٶBٰB��B��B�GB��B��B�
BثBٞB�YB�xB�{BٖBٻB�BٻB�
B�OB٤BٰB�6B�MB�BٻB��BٞB�6B�BڮB�B��B��BَB�8B�WBځB�eBڗBھBڲB��B�B��BڡBڨB�dB�IBڞB��BڞB��BڜBۨB��BڍBڨBڷBڰB��BڨB�iB�B�$B�oB��B�aB��B��B��BےB��B�B�UB�BےBېBڲB�B�_B��BۺBځB�B�B��B��B۴B��B��B��B�B�{B��B��BۮB۳B�8B��B�"BۈB�xB�B�sBۡB��BۈB�gB�uB�iB�aB�BیB�B��B܋BۦB��B۹B�B��B�B��B�B�3B�pB�8BێB�sB�B�B�"B�zB۽BۄBیB��B�}B�$BܪB��B�mBܘBܤBܜB�bB�]B�B�qB�
B��BܒBܬBܼBܴB�B��B��B��B�HB�SB�?B��B� BܼB�B� B�
B�B� B�B�B�B�.B�
B�B� B�B�*B�5B�?B�$B�B��B�8B�1B�bB�bB�B�QB�IB�B�$B��B�5B�B݊BܔB�B�0B�MB�?B�YB�B��B��B�B��B� B��B�B�(B�(B�2B��B݊B��B݊B�KB�(B�B�B�B�wB�FB�5B��B�B�_B�]BܖB�AB��B�aB��B�B��B��B�B�NB�AB�|B�	B��BݘBݐB�1B�1B�|B�1B�KB�=B�KB�KB�IB�1B�YB�!B�B�,B�=B�B�,B��B��B�$B��BݹB�&BݨB��B�B�KB�B��B�BޯB��BޝBނB��BޔBިB��B�IB�$B��B߆B��BߌB�bB�yB�_BބB�B�9B��B�vB�_B�OB�>B�mB�NBއB�kB�_BެB�xB�7B�B�[B�7BޖB��B��B�$B��B�1B�B�B��BަBަB��B��B��B��B�3B��BޒBއB��B�3B�UB޸B޸B��B��B�ZBުB�B��B��B޾B�BޏB�dB�UB�>B�_BއB�IB��B��B�B�hB�B�}BަBޛB�>B�vB�vB�vB�B��B�]BހB޸B�[B�hB��B�!B�B��B��B��B�JB�B��B�B�&B�KB��B�.B�bB�.B�_B�mB�MB�B��BݼBݚB��B��B�sB��B��B��B��BݜB�tB��B�B�nB�7B�B�$B܄B�'B�'B��BۨB��BܟBݜB�hB�'B�B�%BܽB�!B۽B�fB�MBۛBۧBۿB��B�7B�sB�5B�>B�BإB׳B֕B��B��B�eB��B�	B�B��B��B�B�B�B�B.B�B�BUB�B�BbB�B9BB�BeB3B�B�BoB�BRB�BiBuBMB�BB�B[B2BtB�B(B�B�B�B#�B*@B*�B,B.VB/KB1B0�B1vB2
B3�B3�B3pB4uB3�B3�B3�B5JB5�B6�B5�B4�B4B6�B<�B4B3[B2�B2DB4lB3�B3�B4B5VB4B3UB2�B4GB1'B1mB0>B0B0�B1$B0�B29B3B2�B3FB5bB/�B0�B0 B/B.�B'B#�B#�B 8B �B�BSB�B6B�B�B�B�BIB�B
KB3B
�BdB
FB
FB
jB�B	UB�B�B�B�B3B]BB�B�BBZB�{B�'B��B�NB��B��B��B�B�pB�,B�B��B�B��B��B�uB��B�B��B��B�_B��B�B�YB��B�B��B�B� B�B�aB�pB�+B�B�5B�!B�B��B�B�tB�4B�B�AB�LB�*B�{B�&B�`B�B��B�B��B��B��B��BeB�B 6B3BB�QB�XB �B��B �B�qBB1BBwBB�B�BCBtB/B<B�BB�B�BKB�BVBqB�BSB�BUB�B �B�B,BB kB_B�B��B 'B��B QB �B 5B �B �B�WB�B�6B YBB�B�B�BKB�B |B AB�FB�WB�DB��B�rB��B��B��B�SB��B�=B��B��B�/B dBB�B�B�B�B%B�B)B�B;BB�B�B)B�B�B�B�BHB	B	B	KB�B	5B�B�B
B
B7B
7B	�B	�B	�B�B	9B	9B	B	.B	jB
FB	B	VB	sB	�B	B	�B	)B
DB_B	gB	�B	B?B�BB�B�B�B�BUBsB�B�BzB�BBBB�B'B<BSBYB�B�BzBLB�ByBBKB0B�B�BBB�B�B�B:B�B�BsB�B�B�BPB�B7B�B�B�B:B�BZBRBBVB�B�B4B�B.B�BgB|B�B�B�B0B�BoB�B�BB�B+B�B.BBrB�B�BoBqBqBWB
PB
�B�B	�B	�BVB�B BB�B B
B�BB:B�BvB �B+B:B�PB��B��B�-B�B�WB��B�{B��B�~B�yB��B�YB��B��B�`B�vB�nB�B�(B�XB�B�UB��B��B��B��B�!B��B�6B�5B�!B��B�<B�fB�cB��B��B�B��B��B�RB��B��B��B��B��B�B��B�RB�GB �BuB�B	vB	�B�B�BB�B�BB?BxB�B�B�B�B�BMBuB7B�B�B'B�B�B B�B�B{BNB
��B
�B
�vB
��B
��B
��B
�XB
�~B
�tB
�:B
�B �B
��B
�B
�B
��B
�@B
� B
��B
��B
�QB
�pB
�B
��B
��B
��B
�VB
��B �B
��B
�aB
�FB
�!BB�B �B �B�B �B0B4B �BpB�B�B�B�B2BBBB	B
hB�B
B�B4B	XB�B~BhB
�B
B�B+B�B�B�BXBB'B	�B�BB�B#�BHB!WB!{B)�B'yB)�B+�B,B0_B)\B,�B/�B33B6.B0B0oB4QB@B?�B7�B?oBE�BA�BE�BKBN?BS�BXBOBQ�BRBR�B[BVtBY�BY-B[}B[�Bc�B`�B]�BaBg�B_B	��B�B�,B�#B٪B�AB�8B� B�B��B�`B��B�B�B��B٥B�\B��B��B�B��B��B�BػB؟B�eB�^B�1B��B�,B�8B�.B�%B�^B��BؽBٷB�B��B؟BؕB�}B�vB�GB�/B�5B؎BػB��B��B��B�\B�`B�*B�B�3B�dB�?B׍B؋B֕B��B�LB�jB�hB�7B�'B��BءB֓B�B�sB�RBاB�}B�B؁B�VB��B�B�B�B�)B�PBُB�uBصB�|B��B�B��BظB��B�?B�VB�[BڼB�0B�;B�MB��B��BٸBؑB�B�qB�B�B��BءB��B�|BصB�"BؓBةB�)B�cB؎B�B�eBغB��BاB�wB�qB�BؗB��B�B��B�bB��B�&B�}B�BقB�NB�(B��B�+B�BٸB��BٰBٻB��B��BٸB��B�&B��BٚB��B�rB�DB��B�B�[BىB�@B�BBٌBٌBڗB��B��BځB�vB٢B�[B�B�xB�uB٠BٖB�4B٢B٢B��B��B�BٚB٢BٶBٰB��B��B�GB��B��B�
BثBٞB�YB�xB�{BٖBٻB�BٻB�
B�OB٤BٰB�6B�MB�BٻB��BٞB�6B�BڮB�B��B��BَB�8B�WBځB�eBڗBھBڲB��B�B��BڡBڨB�dB�IBڞB��BڞB��BڜBۨB��BڍBڨBڷBڰB��BڨB�iB�B�$B�oB��B�aB��B��B��BےB��B�B�UB�BےBېBڲB�B�_B��BۺBځB�B�B��B��B۴B��B��B��B�B�{B��B��BۮB۳B�8B��B�"BۈB�xB�B�sBۡB��BۈB�gB�uB�iB�aB�BیB�B��B܋BۦB��B۹B�B��B�B��B�B�3B�pB�8BێB�sB�B�B�"B�zB۽BۄBیB��B�}B�$BܪB��B�mBܘBܤBܜB�bB�]B�B�qB�
B��BܒBܬBܼBܴB�B��B��B��B�HB�SB�?B��B� BܼB�B� B�
B�B� B�B�B�B�.B�
B�B� B�B�*B�5B�?B�$B�B��B�8B�1B�bB�bB�B�QB�IB�B�$B��B�5B�B݊BܔB�B�0B�MB�?B�YB�B��B��B�B��B� B��B�B�(B�(B�2B��B݊B��B݊B�KB�(B�B�B�B�wB�FB�5B��B�B�_B�]BܖB�AB��B�aB��B�B��B��B�B�NB�AB�|B�	B��BݘBݐB�1B�1B�|B�1B�KB�=B�KB�KB�IB�1B�YB�!B�B�,B�=B�B�,B��B��B�$B��BݹB�&BݨB��B�B�KB�B��B�BޯB��BޝBނB��BޔBިB��B�IB�$B��B߆B��BߌB�bB�yB�_BބB�B�9B��B�vB�_B�OB�>B�mB�NBއB�kB�_BެB�xB�7B�B�[B�7BޖB��B��B�$B��B�1B�B�B��BަBަB��B��B��B��B�3B��BޒBއB��B�3B�UB޸B޸B��B��B�ZBުB�B��B��B޾B�BޏB�dB�UB�>B�_BއB�IB��B��B�B�hB�B�}BަBޛB�>B�vB�vB�vB�B��B�]BހB޸B�[B�hB��B�!B�B��B��B��B�JB�B��B�B�&B�KB��B�.B�bB�.B�_B�mB�MB�B��BݼBݚB��B��B�sB��B��B��B��BݜB�tB��B�B�nB�7B�B�$B܄B�'B�'B��BۨB��BܟBݜB�hB�'B�B�%BܽB�!B۽B�fB�MBۛBۧBۿB��B�7B�sB�5B�>B�BإB׳B֕B��B��B�eB��B�	B�B��B��B�B�B�B�B.B�B�BUB�B�BbB�B9BB�BeB3B�B�BoB�BRB�BiBuBMB�BB�B[B2BtB�B(B�B�B�B#�B*@B*�B,B.VB/KB1B0�B1vB2
B3�B3�B3pB4uB3�B3�B3�B5JB5�B6�B5�B4�B4B6�B<�B4B3[B2�B2DB4lB3�B3�B4B5VB4B3UB2�B4GB1'B1mB0>B0B0�B1$B0�B29B3B2�B3FB5bB/�B0�B0 B/B.�B'B#�B#�B 8B �B�BSB�B6B�B�B�B�BIB�B
KB3B
�BdB
FB
FB
jB�B	UB�B�B�B�B3B]BB�B�BBZB�{B�'B��B�NB��B��B��B�B�pB�,B�B��B�B��B��B�uB��B�B��B��B�_B��B�B�YB��B�B��B�B� B�B�aB�pB�+B�B�5B�!B�B��B�B�tB�4B�B�AB�LB�*B�{B�&B�`B�B��B�B��B��B��B��BeB�B 6B3BB�QB�XB �B��B �B�qBB1BBwBB�B�BCBtB/B<B�BB�B�BKB�BVBqB�BSB�BUB�B �B�B,BB kB_B�B��B 'B��B QB �B 5B �B �B�WB�B�6B YBB�B�B�BKB�B |B AB�FB�WB�DB��B�rB��B��B��B�SB��B�=B��B��B�/B dBB�B�B�B�B%B�B)B�B;BB�B�B)B�B�B�B�BHB	B	B	KB�B	5B�B�B
B
B7B
7B	�B	�B	�B�B	9B	9B	B	.B	jB
FB	B	VB	sB	�B	B	�B	)B
DB_B	gB	�B	B?B�BB�B�B�B�BUBsB�B�BzB�BBBB�B'B<BSBYB�B�BzBLB�ByBBKB0B�B�BBB�B�B�B:B�B�BsB�B�B�BPB�B7B�B�B�B:B�BZBRBBVB�B�B4B�B.B�BgB|B�B�B�B0B�BoB�B�BB�B+B�B.BBrB�B�BoBqBqBWB
PB
�B�B	�B	�BVB�B BB�B B
B�BB:B�BvB �B+B:B�PB��B��B�-B�B�WB��B�{B��B�~B�yB��B�YB��B��B�`B�vB�nB�B�(B�XB�B�UB��B��B��B��B�!B��B�6B�5B�!B��B�<B�fB�cB��B��B�B��B��B�RB��B��B��B��B��B�B��B�RB�GB �BuB�B	vB	�B�B�BB�B�BB?BxB�B�B�B�B�BMBuB7B�B�B'B�B�B B�B�B{BNB
��B
�B
�vB
��B
��B
��B
�XB
�~B
�tB
�:B
�B �B
��B
�B
�B
��B
�@B
� B
��B
��B
�QB
�pB
�B
��B
��B
��B
�VB
��B �B
��B
�aB
�FB
�!BB�B �B �B�B �B0B4B �BpB�B�B�B�B2BBBB	B
hB�B
B�B4B	XB�B~BhB
�B
B�B+B�B�B�BXBB'B	�B�BB�B#�BHB!WB!{B)�B'yB)�B+�B,B0_B)\B,�B/�B33B6.B0B0oB4QB@B?�B7�B?oBE�BA�BE�BKBN?BS�BXBOBQ�BRBR�B[BVtBY�BY-B[}B[�Bc�B`�B]�BaBg�B_B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�422222222222222222222222422222222222222222222222222222222222222222222222222222222222222242222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444344444334434444444434434444443443444444344443443443444444443444444444434444444444443433444444444444444344344434433344443433444444444444434434443344333444344433344333443344433334443333444433334434344444333343333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�<#�
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
G�O�<#�
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
G�O�<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No adjustment applied; profiles too shallow to allow a reliable calibration/quality control check.                                                                                                                                                              Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202009011546492020090115464920200901154649202009011546492020090115464920200901154649202009011546492020090115464920200901154649202009011546492020090115464920200901154649AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201811202122092018112021220920181120212209    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202122092018112021220920181120212209  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202122092018112021220920181120212209  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4000            0               4000            UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202009011546492020090115464920200901154649  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                