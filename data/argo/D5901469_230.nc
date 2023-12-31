CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-20T21:22:08Z creation      
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
resolution        =���   axis      Z        Q   E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     Q   �P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �P   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     Q  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     Q  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @ ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     Q  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @ �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     Q  +   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     Q  |   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @ �   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     Q  �P   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @ 2P   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     Q  F�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     Q  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @ �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     Q  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @ M�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     Q  b   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ؄   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ؐ   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ؜   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ب   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ٔ   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ٠   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20181120212208  20200901154643  5901469 5901469 5901469 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  2688                            2688                            2688                            2C  2B  2C  DAD APEX                            APEX                            APEX                            2730                            2730                            2730                            112607                          112607                          112607                          846 846 846 @�Y'��@�Y'��@�Y'��111 @�Y(�n@�Y(�n@�Y(�n@6��\)@6��\)@6��\)�dTI�^5?�dTI�^5?�dTI�^5?111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ACA BCA  CA BCA >L��@@  @�  @�  A   A   AA��A`  A�  A�  A�  A�33A�  A�  A�  A�  B ffBffBffB  B   B(  B0  B8  B@ffBHffBP  BW��B_��BhffBp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�            =���=���        =���                =���    >L��>L��        =���    =���=���>L��    >L��>���    =���>L��>L��        >L��        =���=���        >L��                    =���                >L��=���                        =���=���        =���    =���>L��=���=���>L��=���        =���>L��        =���=���    =���    =���    =���    >L��>L��        >L��    =���>L��?   >���    =���=���                    >L��>L��        =���    =���            >L��=���    =���        =���=���                =���>L��    =���=���    =���    =���>L��    =���    >L��    >���>L��=���        =���>L��=���    =���    =���>L��        >L��>���?   >���    =���=���=���        =���>L��>���        =���    =���>���=���    =���    =���>L��>L��    =���    =���=���=���    =���=���>L��        =���=���>���>L��=���=���    >L��=���    =���>���>���=���=���=���>L��>���>L��>L��>L��=���=���=���>L��>L��>L��>���>���>L��>L��>L��>L��>���>L��>L��>���>���>L��>L��>���>���>���>���>���>���>���>���>L��=���>L��>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>L��>���>���>���=���>���>L��>���>���>L��>���>���>���>���>���>���>���>���>���>L��>L��>���>���>L��>���>���>L��>���>L��>���>L��>���>L��>���>���>���>���>���>���>���>���>L��>L��>L��>���>���>���>L��>L��>���>���>L��>���>L��>���>���>���>���>���>���>���>L��>L��>���>���>���>���>L��>���>L��=���>���>���?   >���>L��>L��>L��>L��>L��>���>L��>���>���>L��>L��>���>���>L��>���>���>���>���>���>L��=���>L��>���>L��>���>���>���>���>L��>L��    >���>���>L��>L��>���>���>���>L��>L��>���>���>���>L��>L��>���>���>���>���>���>���>���>���>L��>L��>���>���>L��>���>���>���>���>L��>���>���>���>L��>���>���>L��>���>L��>���>���>���>L��>���>���>���>L��>L��>L��>���>���>L��>���>���>���>���>L��>���>���>L��>L��>L��>���>���>���>L��?   >���>L��>���>���>���>���>L��>���>L��>L��>���>���>L��>L��>L��>���>���>���>���>L��>L��>���>���>L��>L��>���>���>���?   >���>���>���?   ?   >���>L��>���>L��>L��>���>���>���>���>���>���>���>���>���>���>���=���>L��>L��>L��>L��>���>���>���>���?   ?   ?333?333?L��?L��?fff?�  ?�  ?���?���?�ff?�33?�  ?�  ?���?ٙ�?ٙ�?�ff?�33@   @ff@ff@��@33@��@   @   @&ff@333@333@@  @@  @L��@S33@`  @fff@s33@y��@�  @�33@���@���@�33@�ff@���@�  @�ff@���@�  @�ff@���@���@�33@ə�@�  @�33@ٙ�@���@�33@�ff@���@�33@�ff@���A   A33A��AffA	��A��AffA  A33A��A  A��A��AffA   A#33A$��A&ffA)��A+33A,��A0  A1��A333A6ffA8  A9��A;33A>ffA@  AA��AD��AFffAH  AI��AK33ANffAP  AQ��AS33AT��AVffAY��A[33A\��A^ffA`  Ac33Ad��AfffAh  Ak33Al��AnffAq��As33At��Ax  Ay��A{33A|��A�  A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A�  Ař�A�ffA�  A���Aə�A�33A�  A���A͙�A�33A�  A���A�ffA�33A�  A���Aՙ�A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A噚A�ffA�33A���A陚A�33A�  A���A�ffA�33A���A�A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�ffB   B ffB33B��BffB��B33B  BffB33B��BffB��B33B  BffB	33B	��B
ffB
��B33B  BffB��B��B  B��B33B��BffB��B33B  BffB��B��B  BffB��B��B  BffB33B��B  BffB33B��B  BffB33B��B  BffB33B��B  BffB��B��B   B ffB ��B!33B!��B"  B"ffB"��B#33B#��B$  B$ffB$��B%33B&  B&ffB&��B'33B'��B(  B(ffB(��B)33B)��B*  B*ffB*��B+33B+��B,  B,ffB,��B-��B.  B.ffB.��B/33B/��B0  B0ffB0��B133B1��B2  B2ffB2��B333B3��B4  B4ffB4��B533B533B5��B6  B6ffB6��B733B7��B8  B8ffB8��B933B9��B:  B:ffB:��B:��B;33B;��B<  B<ffB<��B=33B=��B>  B>ffB>ffB>��B?33B?��B@  B@  B@ffB@��BA33BA��BA��BB  BBffBBffBB��BC33BC��BC��BD  BD  BDffBD��BD��BE33BE33BE��BE��BF  BF  BF  BFffBFffBF��BF��BF��BG33BG33BG��BG��BG��BG��BH  BH  BHffBHffBHffBH��BH��BH��BI33BI33BI33BI��BI��BI��BI��BJ  BJ  BJ  BJffBJffBJffBJ��BJ��BJ��BJ��BK33BK33BK��BK��BK��BK��BL  BL  BL  BL  BLffBLffBLffBL��BL��BL��BL��BM33BM33BM33BM��BM��BM��BM��BN  BN  BN  BN  BNffBNffBN��BN��BN��BN��BO33BO33BO33BO33BO��BO��BO��BP  BP  BP  BP  BPffBPffBPffBP��BP��BP��BQ33BQ33BQ33BQ33BQ��BQ��BQ��BR  BR  BR  BRffBRffBRffBR��BR��BR��BS33BS33BS33BS��BS��BS��BT  BT  BT  BTffBTffBTffBT��BT��BT��BU33BU33BU33BU��BU��BU��BV  BV  BV  BVffBVffBV��BV��BV��BW33BW33BW��BW��BW��BX  BX  BX  BXffBXffBX��BX��BY33BY33BY��BY��BY��BZ  BZ  BZffBZffBZ��BZ��B[33B[33B[��B[��B\  B\  B\ffB\ffB\��B]33B]33B]��B]��B^  B^ffB^ffB^��B_33B_��B_��B`  B`ffB`ffB`��Ba33Ba��Bb  Bb  BbffBb��Bc33Bc��Bd  BdffBd��Bd��Be33Be��Bf  BfffBf��Bg33Bg��Bh  BhffBhffBh��Bi33Bi��Bj  BjffBjffBj��Bk33Bk��Bk��Bl  BlffBlffBl��Bl��Bm33Bm��Bm��Bn  Bn  BnffBnffBn��Bn��Bo33Bo33Bo��Bo��Bp  Bp  BpffBpffBp��Bp��Bq33Bq33Bq��Bq��Br  Br  BrffBr��Br��Bs33Bs33Bs��Bs��Bt  Bt  BtffBt��Bt��Bu33Bu33Bu��Bu��Bv  Bv  BvffBv��Bv��Bw33Bw33Bw��Bw��Bx  BxffBxffBx��By33By33By��Bz  Bz  BzffBz��Bz��B{33B{��B{��B|  B|ffB|ffB|��B}33B}33B}��B~  B~  B~ffB~��B33B33B��B�  B�33B�ffB�ffB���B���B�  B�  B�33B�ffB���B���B�  B�33B�33B�ffB���B���B�  B�33B�ffB�ffB���B���B�  B�33B�ffB�ffB���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB�ffB���B���B�  B�  B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�  B�33B�ffB�ffB���B���B���B�  B�33B�33B�ffB�ffB���B���B���B�  B�33B�33B�ffB�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B�  B�  B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�33B�ffB�ffB���B���B���B�  B�  B�33B�ffB�ffB�ffB���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B�  B�  B�  B�33B�ffB�ffB���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B�  B�  B�33B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�ffB���B���B�33B�ffB���B�  B�33B���B���B�  B�ffB���B�  B�33B���B�  B�33B���B���B�  B�ffB���B�  B�ffB���B�  B�33B���B���B�33B�ffB���B�33B�ffB���B�  B�ffB���B�  B�ffB���B�  B�33B���B���B�33B�ffB���B�  B�ffB���B�  B�ffB���B���B�33B�ffB���B�  B�33B���B���B�33B�ffB�B�  B�33B�ffB���B�  B�33Bę�B���B�  B�ffBř�B�  B�33B�ffBƙ�B�  B�33BǙ�B���B�  B�ffBș�B���B�33B�ffB���B�  B�33Bʙ�B���B�33B�ffB˙�B�  B�33B�ffB���B�  B�33B͙�CG�CG  CG  CF��CF��CF�3CF��CF� CFffCFL�CF33CF�CF  CE�fCE��CE�3CE��CEffCEL�CEL�CE�CE  CD�fCD��CD�3CD��CDffCDL�CD33CD�CD  CC��CC�3CC� CCffCCL�CC33CC  CB�fCB��CB��CB� CBL�CB33CB�CA�fCA��CA��CA� CAffCA33CA�CA  C@��C@�3C@� C@ffC@33C@�C@  C?��C?�3C?� C?ffC?L�C?�C?  C>��C>�3C>� C>ffC>L�C>�C>  C=��C=�3C=��C=ffC=L�C=�C=  C<��C<�3C<� C<ffC<L�C<�C<  C;��C;�3C;� C;ffC;33C;�C:�fC:��C:��C:� C:L�C:�C:  C9��C9��C9� C9L�C9�C9  C8��C8��C8ffC8L�C8�C7�fC7�3C7��C7ffC733C7  C6�fC6�3@@  @@  @L��@S33@`  @fff@s33@y��@�  @�33@���@���@�33@�ff@���@�  @�ff@���@�  @�ff@���@���@�33@ə�@�  @�33@ٙ�@���@�33@�ff@���@�33@�ff@���A   A33A��AffA	��A��AffA  A33A��A  A��A��AffA   A#33A$��A&ffA)��A+33A,��A0  A1��A333A6ffA8  A9��A;33A>ffA@  AA��AD��AFffAH  AI��AK33ANffAP  AQ��AS33AT��AVffAY��A[33A\��A^ffA`  Ac33Ad��AfffAh  Ak33Al��AnffAq��As33At��Ax  Ay��A{33A|��A�  A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A�  Ař�A�ffA�  A���Aə�A�33A�  A���A͙�A�33A�  A���A�ffA�33A�  A���Aՙ�A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A噚A�ffA�33A���A陚A�33A�  A���A�ffA�33A���A�A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�ffB   B ffB33B��BffB��B33B  BffB33B��BffB��B33B  BffB	33B	��B
ffB
��B33B  BffB��B��B  B��B33B��BffB��B33B  BffB��B��B  BffB��B��B  BffB33B��B  BffB33B��B  BffB33B��B  BffB33B��B  BffB��B��B   B ffB ��B!33B!��B"  B"ffB"��B#33B#��B$  B$ffB$��B%33B&  B&ffB&��B'33B'��B(  B(ffB(��B)33B)��B*  B*ffB*��B+33B+��B,  B,ffB,��B-��B.  B.ffB.��B/33B/��B0  B0ffB0��B133B1��B2  B2ffB2��B333B3��B4  B4ffB4��B533B533B5��B6  B6ffB6��B733B7��B8  B8ffB8��B933B9��B:  B:ffB:��B:��B;33B;��B<  B<ffB<��B=33B=��B>  B>ffB>ffB>��B?33B?��B@  B@  B@ffB@��BA33BA��BA��BB  BBffBBffBB��BC33BC��BC��BD  BD  BDffBD��BD��BE33BE33BE��BE��BF  BF  BF  BFffBFffBF��BF��BF��BG33BG33BG��BG��BG��BG��BH  BH  BHffBHffBHffBH��BH��BH��BI33BI33BI33BI��BI��BI��BI��BJ  BJ  BJ  BJffBJffBJffBJ��BJ��BJ��BJ��BK33BK33BK��BK��BK��BK��BL  BL  BL  BL  BLffBLffBLffBL��BL��BL��BL��BM33BM33BM33BM��BM��BM��BM��BN  BN  BN  BN  BNffBNffBN��BN��BN��BN��BO33BO33BO33BO33BO��BO��BO��BP  BP  BP  BP  BPffBPffBPffBP��BP��BP��BQ33BQ33BQ33BQ33BQ��BQ��BQ��BR  BR  BR  BRffBRffBRffBR��BR��BR��BS33BS33BS33BS��BS��BS��BT  BT  BT  BTffBTffBTffBT��BT��BT��BU33BU33BU33BU��BU��BU��BV  BV  BV  BVffBVffBV��BV��BV��BW33BW33BW��BW��BW��BX  BX  BX  BXffBXffBX��BX��BY33BY33BY��BY��BY��BZ  BZ  BZffBZffBZ��BZ��B[33B[33B[��B[��B\  B\  B\ffB\ffB\��B]33B]33B]��B]��B^  B^ffB^ffB^��B_33B_��B_��B`  B`ffB`ffB`��Ba33Ba��Bb  Bb  BbffBb��Bc33Bc��Bd  BdffBd��Bd��Be33Be��Bf  BfffBf��Bg33Bg��Bh  BhffBhffBh��Bi33Bi��Bj  BjffBjffBj��Bk33Bk��Bk��Bl  BlffBlffBl��Bl��Bm33Bm��Bm��Bn  Bn  BnffBnffBn��Bn��Bo33Bo33Bo��Bo��Bp  Bp  BpffBpffBp��Bp��Bq33Bq33Bq��Bq��Br  Br  BrffBr��Br��Bs33Bs33Bs��Bs��Bt  Bt  BtffBt��Bt��Bu33Bu33Bu��Bu��Bv  Bv  BvffBv��Bv��Bw33Bw33Bw��Bw��Bx  BxffBxffBx��By33By33By��Bz  Bz  BzffBz��Bz��B{33B{��B{��B|  B|ffB|ffB|��B}33B}33B}��B~  B~  B~ffB~��B33B33B��B�  B�33B�ffB�ffB���B���B�  B�  B�33B�ffB���B���B�  B�33B�33B�ffB���B���B�  B�33B�ffB�ffB���B���B�  B�33B�ffB�ffB���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB�ffB���B���B�  B�  B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�  B�33B�ffB�ffB���B���B���B�  B�33B�33B�ffB�ffB���B���B���B�  B�33B�33B�ffB�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B�  B�  B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�33B�ffB�ffB���B���B���B�  B�  B�33B�ffB�ffB�ffB���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B�  B�  B�  B�33B�ffB�ffB���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B�  B�  B�33B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�ffB���B���B�33B�ffB���B�  B�33B���B���B�  B�ffB���B�  B�33B���B�  B�33B���B���B�  B�ffB���B�  B�ffB���B�  B�33B���B���B�33B�ffB���B�33B�ffB���B�  B�ffB���B�  B�ffB���B�  B�33B���B���B�33B�ffB���B�  B�ffB���B�  B�ffB���B���B�33B�ffB���B�  B�33B���B���B�33B�ffB�B�  B�33B�ffB���B�  B�33Bę�B���B�  B�ffBř�B�  B�33B�ffBƙ�B�  B�33BǙ�B���B�  B�ffBș�B���B�33B�ffB���B�  B�33Bʙ�B���B�33B�ffB˙�B�  B�33B�ffB���B�  B�33B͙�CG�CG  CG  CF��CF��CF�3CF��CF� CFffCFL�CF33CF�CF  CE�fCE��CE�3CE��CEffCEL�CEL�CE�CE  CD�fCD��CD�3CD��CDffCDL�CD33CD�CD  CC��CC�3CC� CCffCCL�CC33CC  CB�fCB��CB��CB� CBL�CB33CB�CA�fCA��CA��CA� CAffCA33CA�CA  C@��C@�3C@� C@ffC@33C@�C@  C?��C?�3C?� C?ffC?L�C?�C?  C>��C>�3C>� C>ffC>L�C>�C>  C=��C=�3C=��C=ffC=L�C=�C=  C<��C<�3C<� C<ffC<L�C<�C<  C;��C;�3C;� C;ffC;33C;�C:�fC:��C:��C:� C:L�C:�C:  C9��C9��C9� C9L�C9�C9  C8��C8��C8ffC8L�C8�C7�fC7�3C7��C7ffC733C7  C6�fC6�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999>8Q�@>�R@~�R@�\)@�\)A�AAG�A_�A�A��
A��
A�
=A��
A��
A��
A��
B Q�BQ�BQ�B�B�B'�B/�B7�B@Q�BHQ�BO�BW�B_�BhQ�Bo�Bw�B�B���B���B���B���B�(�B���B���B�B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C*{C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����
���
���
=��
=��
���
���
=��
���
���
���
���
=��
���
>8Q�>8Q켣�
���
=��
���
=��
=��
>8Q켣�
>8Q�>�\)���
=��
>8Q�>8Q켣�
���
>8Q켣�
���
=��
=��
���
���
>8Q켣�
���
���
���
���
=��
���
���
���
���
>8Q�=��
���
���
���
���
���
���
=��
=��
���
���
=��
���
=��
>8Q�=��
=��
>8Q�=��
���
���
=��
>8Q켣�
���
=��
=��
���
=��
���
=��
���
=��
���
>8Q�>8Q켣�
���
>8Q켣�
=��
>8Q�>�>�\)���
=��
=��
���
���
���
���
���
>8Q�>8Q켣�
���
=��
���
=��
���
���
���
>8Q�=��
���
=��
���
���
=��
=��
���
���
���
���
=��
>8Q켣�
=��
=��
���
=��
���
=��
>8Q켣�
=��
���
>8Q켣�
>�\)>8Q�=��
���
���
=��
>8Q�=��
���
=��
���
=��
>8Q켣�
���
>8Q�>\>�>�\)���
=��
=��
=��
���
���
=��
>8Q�>�\)���
���
=��
���
=��
>�\)=��
���
=��
���
=��
>8Q�>8Q켣�
=��
���
=��
=��
=��
���
=��
=��
>8Q켣�
���
=��
=��
>�\)>8Q�=��
=��
���
>8Q�=��
���
=��
>�\)>�\)=��
=��
=��
>8Q�>�\)>8Q�>8Q�>8Q�=��
=��
=��
>8Q�>8Q�>8Q�>�\)>�\)>8Q�>8Q�>8Q�>8Q�>\>8Q�>8Q�>\>\>8Q�>8Q�>�\)>�\)>�\)>\>�\)>�\)>\>�\)>8Q�=��
>8Q�>�\)>�\)>�\)>�\)>�\)>8Q�>�\)>\>\>�\)>\>�\)>\>�\)>8Q�>�\)>�\)>�\)=��
>�\)>8Q�>�\)>�\)>8Q�>�\)>�\)>�\)>�\)>\>�\)>\>\>�\)>8Q�>8Q�>�\)>�\)>8Q�>�\)>�\)>8Q�>�\)>8Q�>�\)>8Q�>�\)>8Q�>�\)>\>�\)>\>�\)>�\)>\>�\)>8Q�>8Q�>8Q�>�\)>�\)>�\)>8Q�>8Q�>�\)>�\)>8Q�>�\)>8Q�>\>�\)>�\)>�\)>�\)>�\)>�\)>8Q�>8Q�>�\)>�\)>�\)>�\)>8Q�>�\)>8Q�=��
>�\)>�\)>�>�\)>8Q�>8Q�>8Q�>8Q�>8Q�>�\)>8Q�>�\)>�\)>8Q�>8Q�>\>�\)>8Q�>\>�\)>�\)>�\)>\>8Q�=��
>8Q�>�\)>8Q�>�\)>�\)>�\)>\>8Q�>8Q켣�
>\>�\)>8Q�>8Q�>�\)>\>�\)>8Q�>8Q�>�\)>\>�\)>8Q�>8Q�>�\)>\>�\)>�\)>\>�\)>�\)>�\)>8Q�>8Q�>�\)>�\)>8Q�>�\)>�\)>�\)>�\)>8Q�>\>\>�\)>8Q�>�\)>�\)>8Q�>�\)>8Q�>\>�\)>�\)>8Q�>�\)>�\)>\>8Q�>8Q�>8Q�>�\)>�\)>8Q�>�\)>�\)>\>�\)>8Q�>�\)>�\)>8Q�>8Q�>8Q�>�\)>�\)>�\)>8Q�>�>�\)>8Q�>\>\>�\)>�\)>8Q�>�\)>8Q�>8Q�>�\)>�\)>8Q�>8Q�>8Q�>�\)>�\)>\>�\)>8Q�>8Q�>\>�\)>8Q�>8Q�>�\)>�\)>�\)>�>\>�\)>�\)>�>�>�\)>8Q�>�\)>8Q�>8Q�>\>\>�\)>�\)>�\)>�\)>�\)>�\)>\>\>�\)=��
>8Q�>8Q�>8Q�>8Q�>�\)>\>�\)>\>�>�?.{?.{?G�?G�?aG�?z�H?z�H?�=q?�
>?��
?���?�p�?�p�?�=q?�
>?�
>?��
?��?�p�@�@�@�@�@Q�@�R@�R@%�@1�@1�@>�R@>�R@K�@Q�@^�R@e�@q�@xQ�@~�R@��\@���@�(�@��\@�@�(�@�\)@�@���@�\)@�@���@�(�@\@���@�\)@ҏ\@���@�(�@�\@�@�(�@�\@�@�(�@�\)A�GAz�AzA	G�Az�AzA�A�GAz�A�AG�Az�AzA�A"�GA$z�A&zA)G�A*�GA,z�A/�A1G�A2�GA6zA7�A9G�A:�GA>zA?�AAG�ADz�AFzAG�AIG�AJ�GANzAO�AQG�AR�GATz�AVzAYG�AZ�GA\z�A^zA_�Ab�GAdz�AfzAg�Aj�GAlz�AnzAqG�Ar�GAtz�Aw�AyG�Az�GA|z�A�A���A�p�A�=pA��
A���A�=pA�
=A��
A���A�=pA�
=A���A�p�A�=pA�
=A���A�p�A�
=A��
A���A�=pA�
=A���A�p�A�
=A��
A���A�=pA�
=A���A�p�A�=pA��
A���A�=pA�
=A��
A�p�A�=pA��
A���A�p�A�
=A��
A���A�=pA�
=A���A�p�A�
=A��
A���A�=pA�
=A���A�p�A�
=A��
A���A�=pA�
=A��
A�p�A�=pA��
Aȣ�A�p�A�
=A��
Ạ�A�p�A�
=A��
AУ�A�=pA�
=A��
Aԣ�A�p�A�
=A��
Aأ�A�=pA�
=A��
Aܣ�A�=pA�
=A��
A��A�=pA�
=A��
A�p�A�=pA�
=A��A�p�A�
=A��
A��A�=pA�
=A��A�p�A�
=A��
A�p�A�=pA��
A���A�=pA�
=A���A�p�A�=pA��
B Q�B�B�BQ�B�RB�B�BQ�B�B�BQ�B�RB�B�BQ�B	�B	�B
Q�B
�RB�B�BQ�B�RB�B�B�RB�B�BQ�B�RB�B�BQ�B�RB�B�BQ�B�RB�B�BQ�B�B�B�BQ�B�B�B�BQ�B�B�B�BQ�B�B�B�BQ�B�RB�B�B Q�B �RB!�B!�B!�B"Q�B"�RB#�B#�B#�B$Q�B$�RB%�B%�B&Q�B&�RB'�B'�B'�B(Q�B(�RB)�B)�B)�B*Q�B*�RB+�B+�B+�B,Q�B,�RB-�B-�B.Q�B.�RB/�B/�B/�B0Q�B0�RB1�B1�B1�B2Q�B2�RB3�B3�B3�B4Q�B4�RB5�B5�B5�B5�B6Q�B6�RB7�B7�B7�B8Q�B8�RB9�B9�B9�B:Q�B:�RB:�RB;�B;�B;�B<Q�B<�RB=�B=�B=�B>Q�B>Q�B>�RB?�B?�B?�B?�B@Q�B@�RBA�BA�BA�BA�BBQ�BBQ�BB�RBC�BC�BC�BC�BC�BDQ�BD�RBD�RBE�BE�BE�BE�BE�BE�BE�BFQ�BFQ�BF�RBF�RBF�RBG�BG�BG�BG�BG�BG�BG�BG�BHQ�BHQ�BHQ�BH�RBH�RBH�RBI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BJQ�BJQ�BJQ�BJ�RBJ�RBJ�RBJ�RBK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BLQ�BLQ�BLQ�BL�RBL�RBL�RBL�RBM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BNQ�BNQ�BN�RBN�RBN�RBN�RBO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BPQ�BPQ�BPQ�BP�RBP�RBP�RBQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BRQ�BRQ�BRQ�BR�RBR�RBR�RBS�BS�BS�BS�BS�BS�BS�BS�BS�BTQ�BTQ�BTQ�BT�RBT�RBT�RBU�BU�BU�BU�BU�BU�BU�BU�BU�BVQ�BVQ�BV�RBV�RBV�RBW�BW�BW�BW�BW�BW�BW�BW�BXQ�BXQ�BX�RBX�RBY�BY�BY�BY�BY�BY�BY�BZQ�BZQ�BZ�RBZ�RB[�B[�B[�B[�B[�B[�B\Q�B\Q�B\�RB]�B]�B]�B]�B]�B^Q�B^Q�B^�RB_�B_�B_�B_�B`Q�B`Q�B`�RBa�Ba�Ba�Ba�BbQ�Bb�RBc�Bc�Bc�BdQ�Bd�RBd�RBe�Be�Be�BfQ�Bf�RBg�Bg�Bg�BhQ�BhQ�Bh�RBi�Bi�Bi�BjQ�BjQ�Bj�RBk�Bk�Bk�Bk�BlQ�BlQ�Bl�RBl�RBm�Bm�Bm�Bm�Bm�BnQ�BnQ�Bn�RBn�RBo�Bo�Bo�Bo�Bo�Bo�BpQ�BpQ�Bp�RBp�RBq�Bq�Bq�Bq�Bq�Bq�BrQ�Br�RBr�RBs�Bs�Bs�Bs�Bs�Bs�BtQ�Bt�RBt�RBu�Bu�Bu�Bu�Bu�Bu�BvQ�Bv�RBv�RBw�Bw�Bw�Bw�Bw�BxQ�BxQ�Bx�RBy�By�By�By�By�BzQ�Bz�RBz�RB{�B{�B{�B{�B|Q�B|Q�B|�RB}�B}�B}�B}�B}�B~Q�B~�RB�B�B�B�B�(�B�\)B�\)B��]B�B���B���B�(�B�\)B��]B�B���B�(�B�(�B�\)B��]B�B���B�(�B�\)B�\)B��]B�B���B�(�B�\)B�\)B��]B�B���B�(�B�\)B��]B�B�B���B�(�B�\)B��]B�B�B���B�(�B�\)B��]B�B�B���B�(�B�\)B��]B��]B�B���B�(�B�\)B�\)B��]B�B���B���B�(�B�\)B��]B��]B�B���B�(�B�(�B�\)B��]B�B�B���B�(�B�(�B�\)B��]B��]B�B���B���B�(�B�\)B�\)B��]B�B�B���B�(�B�(�B�\)B�\)B��]B�B�B���B�(�B�(�B�\)B�\)B��]B�B�B���B�(�B�(�B�\)B��]B��]B�B���B�(�B�(�B�\)B��]B��]B�B���B�(�B�(�B�\)B��]B�B���B���B�(�B�\)B��]B��]B�B���B�(�B�(�B�\)B��]B��]B�B���B�(�B�(�B�\)B�\)B��]B�B�B���B���B�(�B�\)B�\)B�\)B��]B�B�B���B���B�(�B�(�B�\)B�\)B��]B��]B�B�B���B���B�(�B�(�B�\)B�\)B��]B�B�B���B���B�(�B�(�B�\)B�\)B��]B��]B�B�B���B���B�(�B�(�B�\)B�\)B��]B��]B�B���B���B�(�B�(�B�\)B�\)B��]B��]B�B���B���B���B�(�B�\)B�\)B��]B��]B�B���B���B�(�B�(�B�\)B�\)B��]B�B���B���B�(�B�(�B�\)B��]B��]B�B���B�(�B�(�B�\)B��]B�B�B���B�(�B�\)B��]B�B�B���B�(�B�\)B��]B�B���B�(�B�\)B��]B�B���B�(�B�\)B��]B�B���B�(�B�\)B��]B�B���B�(�B�\)B��]B�B�(�B�\)B��]B�B���B�(�B�\)B��]B�B�(�B�\)B��]B�B���B�(�B�\)B��]B���B�(�B�\)B��]B�B���B�(�B��]B�B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�B�\)B�B���B�\)B��]B�B�(�B�\)B��]B���B�(�B��]B�B���B�\)B�B���B�(�B��]B���B�(�B��]B�B���B�\)B�B���B�\)B��]B���B�(�B��]B�B�(�B�\)B�B�(�B�\)B�B���B�\)B��]B���B�\)B��]B���B�(�B��]B�B�(�B�\)B�B���B�\)B��]B���B�\)B��]B�B�(�B�\)B�B���B�(�B��]B�B�(�B�\)B]B���B�(�B�\)B�B���B�(�Bď]B�B���B�\)Bŏ]B���B�(�B�\)BƏ]B���B�(�BǏ]B�B���B�\)Bȏ]B�B�(�B�\)B�B���B�(�Bʏ]B�B�(�B�\)Bˏ]B���B�(�B�\)B�B���B�(�B͏]CG{CF��CF��CFǮCFǮCF�CF�{CFz�CFaGCFG�CF.CF{CE��CE�GCEǮCE�CE�{CEaGCEG�CEG�CE{CD��CD�GCDǮCD�CD�{CDaGCDG�CD.CD{CC��CCǮCC�CCz�CCaGCCG�CC.CB��CB�GCBǮCB�{CBz�CBG�CB.CB{CA�GCAǮCA�{CAz�CAaGCA.CA{C@��C@ǮC@�C@z�C@aGC@.C@{C?��C?ǮC?�C?z�C?aGC?G�C?{C>��C>ǮC>�C>z�C>aGC>G�C>{C=��C=ǮC=�C=�{C=aGC=G�C={C<��C<ǮC<�C<z�C<aGC<G�C<{C;��C;ǮC;�C;z�C;aGC;.C;{C:�GC:ǮC:�{C:z�C:G�C:{C9��C9ǮC9�{C9z�C9G�C9{C8��C8ǮC8�{C8aGC8G�C8{C7�GC7�C7�{C7aGC7.C6��C6�GC6�@>�R@>�R@K�@Q�@^�R@e�@q�@xQ�@~�R@��\@���@�(�@��\@�@�(�@�\)@�@���@�\)@�@���@�(�@\@���@�\)@ҏ\@���@�(�@�\@�@�(�@�\@�@�(�@�\)A�GAz�AzA	G�Az�AzA�A�GAz�A�AG�Az�AzA�A"�GA$z�A&zA)G�A*�GA,z�A/�A1G�A2�GA6zA7�A9G�A:�GA>zA?�AAG�ADz�AFzAG�AIG�AJ�GANzAO�AQG�AR�GATz�AVzAYG�AZ�GA\z�A^zA_�Ab�GAdz�AfzAg�Aj�GAlz�AnzAqG�Ar�GAtz�Aw�AyG�Az�GA|z�A�A���A�p�A�=pA��
A���A�=pA�
=A��
A���A�=pA�
=A���A�p�A�=pA�
=A���A�p�A�
=A��
A���A�=pA�
=A���A�p�A�
=A��
A���A�=pA�
=A���A�p�A�=pA��
A���A�=pA�
=A��
A�p�A�=pA��
A���A�p�A�
=A��
A���A�=pA�
=A���A�p�A�
=A��
A���A�=pA�
=A���A�p�A�
=A��
A���A�=pA�
=A��
A�p�A�=pA��
Aȣ�A�p�A�
=A��
Ạ�A�p�A�
=A��
AУ�A�=pA�
=A��
Aԣ�A�p�A�
=A��
Aأ�A�=pA�
=A��
Aܣ�A�=pA�
=A��
A��A�=pA�
=A��
A�p�A�=pA�
=A��A�p�A�
=A��
A��A�=pA�
=A��A�p�A�
=A��
A�p�A�=pA��
A���A�=pA�
=A���A�p�A�=pA��
B Q�B�B�BQ�B�RB�B�BQ�B�B�BQ�B�RB�B�BQ�B	�B	�B
Q�B
�RB�B�BQ�B�RB�B�B�RB�B�BQ�B�RB�B�BQ�B�RB�B�BQ�B�RB�B�BQ�B�B�B�BQ�B�B�B�BQ�B�B�B�BQ�B�B�B�BQ�B�RB�B�B Q�B �RB!�B!�B!�B"Q�B"�RB#�B#�B#�B$Q�B$�RB%�B%�B&Q�B&�RB'�B'�B'�B(Q�B(�RB)�B)�B)�B*Q�B*�RB+�B+�B+�B,Q�B,�RB-�B-�B.Q�B.�RB/�B/�B/�B0Q�B0�RB1�B1�B1�B2Q�B2�RB3�B3�B3�B4Q�B4�RB5�B5�B5�B5�B6Q�B6�RB7�B7�B7�B8Q�B8�RB9�B9�B9�B:Q�B:�RB:�RB;�B;�B;�B<Q�B<�RB=�B=�B=�B>Q�B>Q�B>�RB?�B?�B?�B?�B@Q�B@�RBA�BA�BA�BA�BBQ�BBQ�BB�RBC�BC�BC�BC�BC�BDQ�BD�RBD�RBE�BE�BE�BE�BE�BE�BE�BFQ�BFQ�BF�RBF�RBF�RBG�BG�BG�BG�BG�BG�BG�BG�BHQ�BHQ�BHQ�BH�RBH�RBH�RBI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BJQ�BJQ�BJQ�BJ�RBJ�RBJ�RBJ�RBK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BLQ�BLQ�BLQ�BL�RBL�RBL�RBL�RBM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BNQ�BNQ�BN�RBN�RBN�RBN�RBO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BO�BPQ�BPQ�BPQ�BP�RBP�RBP�RBQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BRQ�BRQ�BRQ�BR�RBR�RBR�RBS�BS�BS�BS�BS�BS�BS�BS�BS�BTQ�BTQ�BTQ�BT�RBT�RBT�RBU�BU�BU�BU�BU�BU�BU�BU�BU�BVQ�BVQ�BV�RBV�RBV�RBW�BW�BW�BW�BW�BW�BW�BW�BXQ�BXQ�BX�RBX�RBY�BY�BY�BY�BY�BY�BY�BZQ�BZQ�BZ�RBZ�RB[�B[�B[�B[�B[�B[�B\Q�B\Q�B\�RB]�B]�B]�B]�B]�B^Q�B^Q�B^�RB_�B_�B_�B_�B`Q�B`Q�B`�RBa�Ba�Ba�Ba�BbQ�Bb�RBc�Bc�Bc�BdQ�Bd�RBd�RBe�Be�Be�BfQ�Bf�RBg�Bg�Bg�BhQ�BhQ�Bh�RBi�Bi�Bi�BjQ�BjQ�Bj�RBk�Bk�Bk�Bk�BlQ�BlQ�Bl�RBl�RBm�Bm�Bm�Bm�Bm�BnQ�BnQ�Bn�RBn�RBo�Bo�Bo�Bo�Bo�Bo�BpQ�BpQ�Bp�RBp�RBq�Bq�Bq�Bq�Bq�Bq�BrQ�Br�RBr�RBs�Bs�Bs�Bs�Bs�Bs�BtQ�Bt�RBt�RBu�Bu�Bu�Bu�Bu�Bu�BvQ�Bv�RBv�RBw�Bw�Bw�Bw�Bw�BxQ�BxQ�Bx�RBy�By�By�By�By�BzQ�Bz�RBz�RB{�B{�B{�B{�B|Q�B|Q�B|�RB}�B}�B}�B}�B}�B~Q�B~�RB�B�B�B�B�(�B�\)B�\)B��]B�B���B���B�(�B�\)B��]B�B���B�(�B�(�B�\)B��]B�B���B�(�B�\)B�\)B��]B�B���B�(�B�\)B�\)B��]B�B���B�(�B�\)B��]B�B�B���B�(�B�\)B��]B�B�B���B�(�B�\)B��]B�B�B���B�(�B�\)B��]B��]B�B���B�(�B�\)B�\)B��]B�B���B���B�(�B�\)B��]B��]B�B���B�(�B�(�B�\)B��]B�B�B���B�(�B�(�B�\)B��]B��]B�B���B���B�(�B�\)B�\)B��]B�B�B���B�(�B�(�B�\)B�\)B��]B�B�B���B�(�B�(�B�\)B�\)B��]B�B�B���B�(�B�(�B�\)B��]B��]B�B���B�(�B�(�B�\)B��]B��]B�B���B�(�B�(�B�\)B��]B�B���B���B�(�B�\)B��]B��]B�B���B�(�B�(�B�\)B��]B��]B�B���B�(�B�(�B�\)B�\)B��]B�B�B���B���B�(�B�\)B�\)B�\)B��]B�B�B���B���B�(�B�(�B�\)B�\)B��]B��]B�B�B���B���B�(�B�(�B�\)B�\)B��]B�B�B���B���B�(�B�(�B�\)B�\)B��]B��]B�B�B���B���B�(�B�(�B�\)B�\)B��]B��]B�B���B���B�(�B�(�B�\)B�\)B��]B��]B�B���B���B���B�(�B�\)B�\)B��]B��]B�B���B���B�(�B�(�B�\)B�\)B��]B�B���B���B�(�B�(�B�\)B��]B��]B�B���B�(�B�(�B�\)B��]B�B�B���B�(�B�\)B��]B�B�B���B�(�B�\)B��]B�B���B�(�B�\)B��]B�B���B�(�B�\)B��]B�B���B�(�B�\)B��]B�B���B�(�B�\)B��]B�B�(�B�\)B��]B�B���B�(�B�\)B��]B�B�(�B�\)B��]B�B���B�(�B�\)B��]B���B�(�B�\)B��]B�B���B�(�B��]B�B���B�(�B�\)B�B���B�(�B�\)B�B���B�(�B�\)B�B���B�\)B��]B�B�(�B�\)B��]B���B�(�B��]B�B���B�\)B�B���B�(�B��]B���B�(�B��]B�B���B�\)B�B���B�\)B��]B���B�(�B��]B�B�(�B�\)B�B�(�B�\)B�B���B�\)B��]B���B�\)B��]B���B�(�B��]B�B�(�B�\)B�B���B�\)B��]B���B�\)B��]B�B�(�B�\)B�B���B�(�B��]B�B�(�B�\)B]B���B�(�B�\)B�B���B�(�Bď]B�B���B�\)Bŏ]B���B�(�B�\)BƏ]B���B�(�BǏ]B�B���B�\)Bȏ]B�B�(�B�\)B�B���B�(�Bʏ]B�B�(�B�\)Bˏ]B���B�(�B�\)B�B���B�(�B͏]CG{CF��CF��CFǮCFǮCF�CF�{CFz�CFaGCFG�CF.CF{CE��CE�GCEǮCE�CE�{CEaGCEG�CEG�CE{CD��CD�GCDǮCD�CD�{CDaGCDG�CD.CD{CC��CCǮCC�CCz�CCaGCCG�CC.CB��CB�GCBǮCB�{CBz�CBG�CB.CB{CA�GCAǮCA�{CAz�CAaGCA.CA{C@��C@ǮC@�C@z�C@aGC@.C@{C?��C?ǮC?�C?z�C?aGC?G�C?{C>��C>ǮC>�C>z�C>aGC>G�C>{C=��C=ǮC=�C=�{C=aGC=G�C={C<��C<ǮC<�C<z�C<aGC<G�C<{C;��C;ǮC;�C;z�C;aGC;.C;{C:�GC:ǮC:�{C:z�C:G�C:{C9��C9ǮC9�{C9z�C9G�C9{C8��C8ǮC8�{C8aGC8G�C8{C7�GC7�C7�{C7aGC7.C6��C6�GC6�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�|�A�x�A�~�AփAփA�|�A�~�AփAև+A։7A։7A։7A։7A֏\A֑hA֗�A֗�A֗�A֗�A֕�A֕�A֕�A֕�A֗�A���A֏\A�n�A���Aә�A�jA�^5A�G�A���A͗�A���A�G�AǃAîA�?}A�G�A��A�^5A�ȴA�5?A�^5A��A�|�A�A���A���A�v�A��TA�\)A�VA���A��A�hsA�E�A�A�A�A�bNA��A��uA��A��-A��A���A�5?A��hA�7LA�S�A�~�A��A���A��FA�\)A��RA�9XA�5?A�
=A�+A�&�A��A�JA��HA�/A���A���A��DA�&�A��RA��A�7LA�v�A���A�^5A�(�A� �A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�ȴA�%A�|�AìAÅA�^5Aǥ�A�$�A��A�K�A�-A�33A��A�;dA�bAƝ�A�=qA�ƨA̬A�I�A�(�A�r�A���A�I�A�(�A�VA�?}A�9XA�I�A̬A��HA���A�%Aş�Aƕ�A�
=A�hsAѴ9A���A��`AÓuA�x�A���A�z�A�`BA�E�A�~�A�C�A���A�
=A�;dA�5?A�jAė�Aŏ\Aĩ�Aġ�A��A��mA��AʶFA���AҺ^A�Q�A��A�7LA� �AՕ�A�l�A�ƨA�K�A�I�A�bA��
A��
AՋDA�n�Aа!A̸RA��A�+AˍPAͩ�A�A�A�E�A�7LA�A��A�{A�oA�/A�^5A�O�Aϕ�A�bNAվwA���A�bA�ffA�z�AϮA�5?A�A�A�/A��mA�jA��mA�I�A��Aՙ�A�E�A�  A�M�A��AБhA�(�A�&�A��mA��HA���AǼjA�;dA�^5A���A�?}A��Aк^A�A�A�ȴA�JA��A�9XAҡ�A�(�AѸRA�bNAӬA�  AӼjA�S�A�-A˕�A���A��A�`BAҶFAŴ9AƇ+A���A��A�M�A�dZA��A�ĜA�ZA�bNA�XA��/A�"�A���A�;dAĶFA�33A�dZAӼjA�ZA��/A��TA���AǑhAϙ�A�XA�$�A�"�Aȕ�A�`BA�{A��/A�ZA�l�A��;AЏ\A���A�?}A�C�A��Aϥ�A�hsA�XA΁A�oA��A�/A���A�^5A�I�A�^5A�A���A�?}A��TAѲ-A�XA�XA�K�A�bA��;A�;dA�O�A�O�A�C�A�Q�A�K�A�z�A�G�A��TA�S�A�VA�S�A�O�A�O�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�O�A�ZA�K�A�M�A�O�A�Q�A�Q�A�O�A�\)A�Q�A�VA�Q�A�I�A�G�A�C�A�O�A�O�A�S�A�XA�S�A�O�A�S�A�O�A�O�A�ZA�S�A�VA�Q�A�XA�`BA�bNA�bNA�^5A�ZA���A�\)A�XA�ZA�\)A�\)A�ZA�VA�\)A�\)A�`BA�XA�`BA�^5A�XA�VA�^5A�\)A�ZA�XA�dZA�XA�XA�bNA�bNA�bNA�VA�bNA�`BA�dZA�r�A�ffA�jA�ZA�l�A�l�A�ffA�ffA�ffA�jA�l�A�l�A�l�A�ffA�hsA�jA�jA�hsA�^5A�C�A�n�A�l�A�l�A�n�A�n�A�jA�ffA�\)A�l�A�p�A�l�A�jA�jA�l�A�jA�ZA�\)A�\)A�n�A�jA�bNA�hsA�S�A�ffA�C�A�hsA�hsA�ffA�dZA�bNA�VA�ffA�jA�dZA�bNA�bNA�`BA�dZA�bNA�^5A��TA�\)A�`BA�bNA�`BA�`BA�`BA�bNA�bNA�\)A���A�G�A�bNA�^5A�ZA�\)A�\)A�bNA�VA�Q�A�XA�XA�\)A�XA�S�A�XA�XA�XA�VA�XA�Q�A�XA�\)A�G�A���A�VA�VA�G�A�bNA�`BA�dZA�^5A�ZA�\)A�hsA�hsA�hsA�dZA�bNA�VA�ZA�^5A�`BA�ffA�ffA�ffA�hsA�C�A�hsA�ffA�ffA�bNA�dZA�bNA�dZA�`BA�bNA�dZA�jA�ffA�dZA�dZA�`BA�`BA�\)A�`BA�`BA�dZA�^5A�l�A�hsA�^5A�jA�n�A�n�A�n�A�jA�n�A�jA�jA�hsA�jA�dZA�ffA�hsA�hsA�ffA�hsA�p�A�Q�A�p�A�r�A�p�A�l�AոRA�p�A�r�A�l�A�n�A�p�A�l�A�p�A�n�A�r�A�hsA�ffA�p�A�n�A�l�A�l�A�n�A�n�A�jA�hsA�bNA�hsA�bNA�p�A�hsA�ffA�\)A�ffA�bNA�bNA�jA�p�A�n�A�ffA�^5A�p�A�v�A�t�A�r�A�t�A�v�A�x�A�v�A�x�A�z�A�z�A�|�A�z�A�|�A�|�A�|�A�|�A�z�A�z�A�x�A�v�A�v�A�x�A�v�A�x�A�v�A�v�A�x�A�x�A�z�A�z�A�~�A�|�A�z�A�z�A�x�A�x�A�x�A�t�A�x�A�v�A�v�A�v�A�t�A�r�A�t�A�t�A�v�A�|�A�|�A�~�A�~�A�~�A�|�A�~�A�~�A�|�A�~�AցAցAցA�~�A�~�A�~�AցAցA�~�A�~�AցAցA�~�A�~�AցAցAցAփAցAցA�~�AցA�|�A�|�A�|�A�z�A�z�A�z�A�|�A�|�A�|�A�z�A�z�A�z�A�z�A�z�A�z�A�x�A�z�A�z�A�x�A�z�A�|�A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�|�A�|�A�z�A�|�A�z�A�z�A�z�A�z�A�|�A�~�A�~�A�~�A�~�A�~�AցAցAցAփA�~�A�~�A�~�A�~�A�~�AցA�~�AփAփAօAօAօAփAօAօAօAփAփAօAօAօAօAօAօAօAև+Aև+AօAև+AփAև+AօAև+Aև+AօAև+A։7Aև+A։7A֋DA։7A։7Aև+Aև+Aև+AօAև+Aև+Aև+AօAօAօAև+Aև+AօAև+Aև+Aև+AօAև+Aև+Aև+Aև+Aև+Aև+Aև+Aև+Aև+A։7A։7Aև+Aև+Aև+Aև+Aև+AօAև+Aև+A֋DA։7Aև+A։7A։7A֋DA֋DA֍PA֋DA֍PA֍PA֏\A֏\A֏\A֏\A֏\A֏\A֏\A֍PA֏\A֋DA֋DA֋DA֍PA֋DA֑hA֏\A֏\A֑hA֓uA֏\A֓uA֕�A֕�A֕�A֗�A֗�A֕�A֗�A֕�A֕�A֗�A֗�A֗�A֕�A֕�A֗�A֗�A֗�A֗�A֕�A֓uA֓uA֕�A֕�A֕�A֓uA֕�A֗�A֕�A֗�A֕�A֕�A֕�A֕�A֕�A֕�A֗�A֗�A֗�A֗�A֕�A֙�A֗�A֕�A֕�A֗�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֗�A֕�A֕�A֕�A֗�A֕�A֓uA֕�A֓uA֓uA֑hA֑hA֏\A֏\A֓uA֓uA֑hA֑hA֓uA֓uA֓uA֑hA֓uA֓uA֑hA֓uA֑hA֓uA֑hA֓uA֓uA֓uA֑hA֓uA֓uA֓uA֕�A֓uA֑hA֓uA֓uA֑hA֓uA֑hA֕�A֓uA֓uA֓uA֑hA֑hA֑hA֓uA֓uA֓uA֓uA֓uA֑hA֓uA֓uA֓uA֓uA֓uA֓uA֓uA֓uA֓uA֓uA֕�A֓uA֓uA֓uA֓uA֑hA֓uA֓uA֓uA֓uA֕�A֓uA֓uA֕�A֓uA֕�A֓uA֓uA֓uA֓uA֗�A֓uA֕�A֕�A֓uA֕�A֓uA֓uA֓uA֓uA֕�A֕�A֓uA֕�A֕�A֓uA֓uA֑hA֓uA֑hA֑hA֕�A֑hA֓uA֑hA֓uA֓uA֑hA֓uA֓uA֏\A֑hA֑hA֑hA֓uA֓uA֑hA֑hA֓uA֑hA֑hA֑hA֓uA֓uA֑hA֓uA֑hA֓uA֑hA֓uA֑hA֓uA֓uA֑hA֓uA֓uA֏\A֑hA֑hA֑hA֑hA֑hA֓uA֑hA֓uA֑hA֑hA֑hA֓uA֓uA֑hA֑hA֓uA֓uA֓uA֓uA֑hA֓uA֑hA֓uA֕�A֑hA֑hA֑hA֓uA֑hA֓uA֓uA֑hA֑hA֓uA֏\A֏\A֏\A֏\A֍PA֑hA֑hA֏\A֏\A֑hA֏\A֏\A֏\A֋DA֍PAև+Aև+A֋DA։7Aև+A։7Aև+Aև+A։7Aև+Aև+AօAօAօAփAփAօAօAցAփAցA�|�A�z�A�~�A�z�A�x�A�x�A�x�A�x�A�x�A�v�A�r�A�t�A�x�A�t�A�p�A�n�A�t�A�t�A�t�A�t�A�r�A�r�A�r�A�v�A�t�A�t�A�t�A�v�A�r�A�jA�r�A�r�A�p�A�l�A�jA�jA�l�A�l�A�p�A�n�A�hsA�r�A�t�A�n�A�jA�bNA�p�A�ffA�E�A�7LA�A�A�Q�A�M�A�K�A�A�A�=qA�;dA�A�A�?}A�9XA�33A�1A�1A�(�A�9XA�/A�(�A�$�A�
=A�JA�JA�"�A�"�A��A���A���A�Aմ9AնFAթ�Aթ�A���Aթ�AծA�ĜAծAՍPA�jA�bNA�`BA�\)A�VA�K�A�VA�1A�%A��/A�A�/Aӝ�A�C�A�%A��A���A�ƨAҺ^A҃AҴ9Aҙ�A�|�A�~�A�|�A�~�A�t�A�hsA�dZA�dZA�dZA�bNA�bNA�bNA�`BA�`BA�`BA�`BA�bNA�dZA�dZA�bNA�bNA�dZA�dZA�dZA�ffA�dZA�ffA�ffA�ffA�ffA�dZA�bNA�dZA�dZA�`BA�`BA�bNA�^5A�bNA�^5A�^5A�\)A�^5A�`BA�bNA�dZA�bNA�`BA�\)A�\)A�`BA�^5A�^5A�\)A�\)A�^5A�\)A�ZA�^5A�XA�XA�S�A�VA�XA�VA�XA�XA�VA�XA�S�A�VA�S�A�VA�VA�VA�VA�S�A�VA�S�A�Q�A�K�A�VA�Q�A�K�A�E�A�K�A�K�A�O�A�VA�O�A�K�A�C�A�A�A�?}A�1'A�+A�&�A�"�A��A�VA�AѲ-A�r�A�M�A�C�A�&�A���AЬA�S�A�^5A�%A�~�A�|�A�|�A�v�A�hsA�\)A��A��/AΧ�A�t�A�Q�A�I�A�C�A�?}A�;dA�;dA�$�A���A���A���A;wAͬA͡�A͏\ÁA�z�A�t�A�r�A�jA�ffA�bNA�`BA�S�A�C�A�?}A�(�A�{A�
=A���A��A��A��;A��A���A�ƨA���A���A��
A��A���A���A��
A���A̾wA���A̴9A̬A̰!A̧�A̩�A̧�A̡�A̡�A̓uA̙�ÃA̋DȦ+A̅A�dZA�jA�`BA�\)A�\)A�O�A�S�A�K�A�oA��A�33A�1'A�A�A�;dA�-A�-A�$�A�"�A� �A��A��A�$�A�(�A�$�A�$�A�$�A�$�A��A�VA�  A��
A�jA���A�^5A�VAȅA�A�A��A��
A�K�A�1Aư!AƁA�v�A�\)A�K�A�;dA��A�oA�VA�  A���A��mA���A���AŮAŃA�XA�-A���A��
AąA�K�A�(�A�VA��TA��
A���A���A���A���A�ĜA�ƨAú^Aç�AÝ�AÉ7AÉ7A�jA�`BA�M�A�I�A�A�A�7LA�1'A�"�A��A�{A�
=A�A���A��A��`A��HA��
A�ƨA°!A¬A§�A¡�A�AuAA�t�A�hsA�dZA�ZA�ZA�^5A�O�A�I�A�C�A�A�A�=qA�;dA�9XA��A�oA�bA�JA���A���A��TA��/A��;A��/A���A�ƨA�ȴA�A��wA��jA��RA��RA��!A��uA��A�~�A��A��+A�|�A�ffA�XA�S�A�I�A�?}A�33A� �A�1A��`A��
A���A��FA���A��A�^5A��A�C�A�1A���A��jA��RA���A�XA�1'A��A��A��HA��A���A��wA���A��9A��-A�^5A�p�A�O�A�p�A�G�A�bNA�VA�5?A�1'A��A�1A�+A��A��A�A��A��jA�ƨA���A���A�ȴA���A��jA���A���A���A���A�ffA���A�~�A���A��uA��uA�z�A�p�A�XA�33A��A���A���A�^5A�/A���A��A���A���A�r�A��RA���A���A�x�A�C�A�-A��A��A���A���A��hA�1'A���A��A�$�A��A��A��A�{A�bA�JA�A���A��A��A���A���A��RA���A���A��PA��+A��A��A�|�A�|�A�x�A�v�A�n�A�l�A�l�A�dZA�^5A�O�A�C�A�33A�(�A�oA���A��A��mA��;A��A���A���A�ĜA��RA��A���A���A���A���A���A��uA��uA��hA��hA��\A��A�p�A�Q�A�=qA�1'A�$�A��A���A��A��FA���A��A�t�A�l�A�bNA�C�A���A��jA���A��7A��A�|�A�z�A�x�A�x�A�r�A�t�A�t�A�l�A�ffA�`BA�Q�A�~�A���A��9A���A���A��A���A�ĜA���A��mA��A��A��/A��yA��A���A���A��A�A��A�bA�+A��A� �A� �A�&�A�"�A�"�A�-A�(�A�/A�5?A�9XA�33A�33A�1'A�;dA�9XA�9XA�5?A�9XA�;dA�=qA�5?A�7LA�1'A�33A�7LA�E�A�7LA�=qA�1'A�G�A�/A�I�A�G�A�M�A�^5A�XA�ZA�S�A�O�A�hsA�^5A�n�A�hsA�XA�E�A�bNA�bNA�z�A��A�~�A��PA��+A���A��9A��-A��^A���A���A�A���A�A��A�  A��A�"�A�9XA�=qA�O�A�O�A�VA�dZA��+A�r�A�x�A��uA�ffA���A���A�ƨA��^A�ȴA��`A��A��A��/A��A�+A��A�5?A�1'A��7A���A��jA��!A��!A��A��A�|�A�~�A�~�A�|�A�z�A�x�A�x�A�x�A�x�A�x�A�x�A�v�A�x�A�v�A�z�A�x�A�x�A�x�AցA�~�AփAցAցAփAցAցAցAցAփAփAփAփAօAփAփAփAցAփAփAփAցAցAփAփAօAօAօAօAօAփA�~�A�~�A�|�AցA�~�AցA�~�A�|�A�~�A�~�A�~�A�|�A�|�A�~�A�|�A�|�A�~�A�z�A�|�A�|�AցA�~�A�|�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�|�A�|�A�|�A�~�A�~�A�~�AցAցAցAփAցAցAցAփAցAփAցAցAփAփAփAցAփAօAօAև+Aև+AօAև+AօAօAև+AօAօAօAև+AօAօAև+A։7Aև+A։7Aև+Aև+A։7Aև+Aև+A֋DAև+Aև+A։7A֋DA֍PA֋DA֋DA֋DA֋DA֋DA։7A։7Aև+Aև+Aև+A։7A։7A։7A։7A֋DAև+A։7A։7A։7A։7A։7A։7A։7A֋DA։7A֋DA։7A։7A։7A֋DA֋DA։7A֋DA։7A։7A։7A։7Aև+A։7A։7A֋DA֋DA֍PA֋DA֋DA֋DA֍PA֍PA֍PA֏\A֏\A֍PA֏\A֏\A֑hA֑hA֓uA֑hA֓uA֏\A֑hA֏\A֏\A֏\A֏\A֏\A֑hA֑hA֑hA֑hA֕�A֑hA֑hA֕�A֕�A֗�A֗�A֗�A֙�A֙�A֙�A֙�A֙�A֗�A֙�A֙�A֗�A֙�A֙�A֗�A֙�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֙�A֙�A֙�A֙�A֗�A֗�A֙�A֙�A֗�A֙�A֙�A֙�A֙�A֗�A֗�A֗�A֗�A֗�A֗�A֙�A֙�A֗�A֗�A֗�A֗�A֙�A֙�A֗�A֙�A֙�A֗�A֗�A֗�A֙�A֙�A֙�A֗�A֙�A֗�A֓uA֕�A֓uA֓uA֓uA֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֓uA֓uA֕�A֕�A֗�A֕�A֕�A֕�A֓uA֓uA֕�A֕�A֗�A֕�A֕�A֕�A֓uA֕�A֗�A֕�A֕�A֓uA֓uA֓uA֓uA֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֗�A֗�A֗�A֕�A֗�A֕�A֗�A֕�A֕�A֕�A֗�A֗�A֕�A֕�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֕�A֕�A֗�A֙�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֕�A֗�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֓uA֓uA֕�A֕�A֓uA֓uA֓uA֕�A֕�A֕�A֓uA֓uA֓uA֕�A֓uA֕�A֕�A֕�A֕�A֕�A֕�A֓uA֕�A֕�A֕�A֕�A֕�A֕�A֕�A֓uA֓uA֑hA֓uA֓uA֓uA֕�A֕�A֕�A֕�A֓uA֕�A֓uA֕�A֓uA֕�A֕�A֓uA֓uA֕�A֗�A֓uA֕�A֓uA֕�A֕�A֗�A֕�A֕�A֗�A֗�A֕�A֓uA֕�A֕�A֓uA֑hA֓uA֓uA֓uA֓uA֓uA֓uA֑hA֓uA֑hA֏\A֑hA֓uA֓uA֑hA֑hA֏\A֓uA֑hA֑hA֑hA֓uA֑hA֏\A֍PA֏\A֍PA֍PA֍PA֍PA֋DA֋DA։7A֋DA֍PA։7A։7AփAփA։7A֋DA։7AփAօAև+AօAօAփA�~�AցA�~�A�~�A�~�A�z�A�z�A�|�A�z�A�|�A�z�A�z�A�v�A�v�A�x�A�z�A�z�A�|�A�|�A�z�A�~�A�z�A�x�A�t�A�t�A�t�A�t�A�t�A�r�A�v�A�v�A�v�A�t�A�t�A�x�A�t�A�l�A�jA�n�A�l�A�bNA�S�A�I�A�E�A�G�A�K�A�K�A�I�A�I�A�E�A�G�A�E�A�C�A�I�A�E�A�I�A�C�A�?}A�=qA�=qA�9XA�?}A�=qA� �A�oA���A��A��A��A��`A�ƨAնFAգ�AՓuA�jA�hsA�dZA�bNA�\)A�XA�I�A��A�A��TA��AԬA�JAә�A�9XA�%A��A��TA���A�ĜAҺ^AҶFA҇+AҁA�|�A҃A҉7A�|�AҋDAґhA҇+A�p�A�l�A�jA�jA�jA�jA�jA�jA�hsA�ffA�jA�l�A�hsA�jA�hsA�jA�hsA�ffA�jA�jA�hsA�hsA�ffA�ffA�hsA�hsA�ffA�`BA�ffA�ffA�ffA�dZA�bNA�bNA�bNA�dZA�dZA�ffA�ffA�dZA�ffA�hsA�ffA�ffA�ffA�ffA�ffA�`BA�^5A�^5A�^5A�`BA�^5A�^5A�\)A�XA�XA�XA�ZA�XA�XA�ZA�VA�XA�XA�ZA�XA�XA�XA�XA�XA�VA�XA�VA�XA�XA�XA�VA�S�A�VA�XA�VA�Q�A�G�A�K�A�E�A�A�A�9XA�7LA�+A��A�JA�A��AсA�O�A�=qA�"�A���AЬA�jA�"�Aϕ�AσAρA�~�A�p�A�ffA�O�A�1A�ĜAΕ�A�`BA�O�A�I�A�C�A�?}A�=qA�9XA�"�A�A���A��Aʹ9Aͧ�A͡�A͉7ÁA�z�A�x�A�n�A�jA�hsA�dZA�`BA�O�A�E�A�;dA� �A�oA�
=A���A��A��A��/A��
A���A���A���A��#A��A��#A��#A��A��A���A���A�ĜA���A̾wA̺^A̸RA̶FA̴9A̮A̮A̮A̮A̮Ạ�A̙�A̓uA̋DA̅A�|�A�r�A�l�A�ffA�`BA�`BA�ZA�K�A�=qA�E�A�G�A�=qA�1'A�/A�+A�&�A� �A� �A�&�A�+A�(�A�(�A�(�A�&�A��A�bA���A���A�t�A���A�ZA�G�AȁA�C�A��AǼjA�C�A���AƩ�AƁA�t�A�^5A�M�A�;dA��A�{A�VA�A���A��A��
A�ĜAŶFAŏ\A�`BA�7LA�1A��HAġ�A�^5A�5?A��A��A��;A��
A���A���A���A�ƨA�ȴAú^AìAç�AÏ\A�t�A�l�A�dZA�Q�A�K�A�I�A�=qA�5?A�(�A� �A��A�oA�%A���A��A��yA��TA��/A���A¶FA®A¬A§�A�A�ADA�|�A�p�A�hsA�^5A�^5A�^5A�VA�M�A�G�A�C�A�A�A�=qA�;dA�(�A��A�{A�VA�A���A��mA��;A��TA��;A���A���A���A�ȴA�ĜA��^A��9A��!A��A��\A��+A��7A��DA��7A�x�A�ffA�\)A�S�A�M�A�E�A�9XA�&�A�A��HA��
A���A��FA��\A�~�A�`BA�7LA���A�ffA��A��A��
A���A���A��9A��A�VA�?}A� �A�VA���A��yA��HA��A�ĜA��RA���A���A���A��A�r�A�hsA�`BA�VA�M�A�G�A�?}A�;dA�33A�/A�+A�(�A�$�A��A��A�A��HA��HA��
A��
A���A���A��wA��^A��9A��A���A���A��PA�z�A�jA�S�A�9XA�{A��A���A�x�A�5?A��;A��\A�ffA�1'A��A�A��+A�G�A�(�A�%A��A�A���A�ZA�JA��FA�;dA� �A��A��A��A�{A�bA�1A�A���A��mA��A���A���A��A���A���A��PA��7A��+A��A��A�~�A�|�A�t�A�p�A�n�A�l�A�ffA�XA�K�A�?}A�33A� �A�
=A���A��A��mA��HA��#A���A���A�ĜA��FA��A���A���A���A���A���A���A���A���A���A��hA��DA�l�A�M�A�;dA�1'A�$�A�bA��A���A��!A���A��A�x�A�r�A�`BA�33A��`A���A��uA��7A��A��A�|�A�|�A�x�A�x�A�x�A�r�A�p�A�jA�dZA���A��wA��jA�A���A��#A��
A��mA���A���A���A���A���A��A���A�  A��A��A� �A�"�A� �A�$�A�&�A�(�A�&�A�(�A�&�A�+A�+A�+A�+A�+A�/A�(�A�-A�-A�/A�-A�/A�+A�-A�/A�1'A�/A�/A�1'A�5?A�1'A�7LA�7LA�=qA�A�A�A�A�A�A�?}A�A�A�E�A�O�A�Q�A�VA�XA�XA�ZA�Q�A�^5A�hsA�n�A�r�A�v�A�|�A�v�A�~�A��A��+A��hA���A��!A��FA��9A��A�ȴA��A��yA��A�A�bA�{A�oA��A�1A�"�A�?}A�E�A�VA�VA�ffA�z�A��7A���A��A��^A���A��HA��yA��mA��yA��A�A�(�A�1'A�A�A�M�A�bNA��A���A��FA��A��FA��#A��uG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�A�|�A�x�A�~�AփAփA�|�A�~�AփAև+A։7A։7A։7A։7A֏\A֑hA֗�A֗�A֗�A֗�A֕�A֕�A֕�A֕�A֗�A���A֏\A�n�A���Aә�A�jA�^5A�G�A���A͗�A���A�G�AǃAîA�?}A�G�A��A�^5A�ȴA�5?A�^5A��A�|�A�A���A���A�v�A��TA�\)A�VA���A��A�hsA�E�A�A�A�A�bNA��A��uA��A��-A��A���A�5?A��hA�7LA�S�A�~�A��A���A��FA�\)A��RA�9XA�5?A�
=A�+A�&�A��A�JA��HA�/A���A���A��DA�&�A��RA��A�7LA�v�A���A�^5A�(�A� �A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�ȴA�%A�|�AìAÅA�^5Aǥ�A�$�A��A�K�A�-A�33A��A�;dA�bAƝ�A�=qA�ƨA̬A�I�A�(�A�r�A���A�I�A�(�A�VA�?}A�9XA�I�A̬A��HA���A�%Aş�Aƕ�A�
=A�hsAѴ9A���A��`AÓuA�x�A���A�z�A�`BA�E�A�~�A�C�A���A�
=A�;dA�5?A�jAė�Aŏ\Aĩ�Aġ�A��A��mA��AʶFA���AҺ^A�Q�A��A�7LA� �AՕ�A�l�A�ƨA�K�A�I�A�bA��
A��
AՋDA�n�Aа!A̸RA��A�+AˍPAͩ�A�A�A�E�A�7LA�A��A�{A�oA�/A�^5A�O�Aϕ�A�bNAվwA���A�bA�ffA�z�AϮA�5?A�A�A�/A��mA�jA��mA�I�A��Aՙ�A�E�A�  A�M�A��AБhA�(�A�&�A��mA��HA���AǼjA�;dA�^5A���A�?}A��Aк^A�A�A�ȴA�JA��A�9XAҡ�A�(�AѸRA�bNAӬA�  AӼjA�S�A�-A˕�A���A��A�`BAҶFAŴ9AƇ+A���A��A�M�A�dZA��A�ĜA�ZA�bNA�XA��/A�"�A���A�;dAĶFA�33A�dZAӼjA�ZA��/A��TA���AǑhAϙ�A�XA�$�A�"�Aȕ�A�`BA�{A��/A�ZA�l�A��;AЏ\A���A�?}A�C�A��Aϥ�A�hsA�XA΁A�oA��A�/A���A�^5A�I�A�^5A�A���A�?}A��TAѲ-A�XA�XA�K�A�bA��;A�;dA�O�A�O�A�C�A�Q�A�K�A�z�A�G�A��TA�S�A�VA�S�A�O�A�O�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�O�A�ZA�K�A�M�A�O�A�Q�A�Q�A�O�A�\)A�Q�A�VA�Q�A�I�A�G�A�C�A�O�A�O�A�S�A�XA�S�A�O�A�S�A�O�A�O�A�ZA�S�A�VA�Q�A�XA�`BA�bNA�bNA�^5A�ZA���A�\)A�XA�ZA�\)A�\)A�ZA�VA�\)A�\)A�`BA�XA�`BA�^5A�XA�VA�^5A�\)A�ZA�XA�dZA�XA�XA�bNA�bNA�bNA�VA�bNA�`BA�dZA�r�A�ffA�jA�ZA�l�A�l�A�ffA�ffA�ffA�jA�l�A�l�A�l�A�ffA�hsA�jA�jA�hsA�^5A�C�A�n�A�l�A�l�A�n�A�n�A�jA�ffA�\)A�l�A�p�A�l�A�jA�jA�l�A�jA�ZA�\)A�\)A�n�A�jA�bNA�hsA�S�A�ffA�C�A�hsA�hsA�ffA�dZA�bNA�VA�ffA�jA�dZA�bNA�bNA�`BA�dZA�bNA�^5A��TA�\)A�`BA�bNA�`BA�`BA�`BA�bNA�bNA�\)A���A�G�A�bNA�^5A�ZA�\)A�\)A�bNA�VA�Q�A�XA�XA�\)A�XA�S�A�XA�XA�XA�VA�XA�Q�A�XA�\)A�G�A���A�VA�VA�G�A�bNA�`BA�dZA�^5A�ZA�\)A�hsA�hsA�hsA�dZA�bNA�VA�ZA�^5A�`BA�ffA�ffA�ffA�hsA�C�A�hsA�ffA�ffA�bNA�dZA�bNA�dZA�`BA�bNA�dZA�jA�ffA�dZA�dZA�`BA�`BA�\)A�`BA�`BA�dZA�^5A�l�A�hsA�^5A�jA�n�A�n�A�n�A�jA�n�A�jA�jA�hsA�jA�dZA�ffA�hsA�hsA�ffA�hsA�p�A�Q�A�p�A�r�A�p�A�l�AոRA�p�A�r�A�l�A�n�A�p�A�l�A�p�A�n�A�r�A�hsA�ffA�p�A�n�A�l�A�l�A�n�A�n�A�jA�hsA�bNA�hsA�bNA�p�A�hsA�ffA�\)A�ffA�bNA�bNA�jA�p�A�n�A�ffA�^5A�p�A�v�A�t�A�r�A�t�A�v�A�x�A�v�A�x�A�z�A�z�A�|�A�z�A�|�A�|�A�|�A�|�A�z�A�z�A�x�A�v�A�v�A�x�A�v�A�x�A�v�A�v�A�x�A�x�A�z�A�z�A�~�A�|�A�~�A�~�A�|�A�z�A�x�A�x�A�x�A�x�A�x�A�x�A�v�A�x�A�v�A�z�A�x�A�x�A�x�AցA�~�AփAցAցAփAցAցAցAցAփAփAփAփAօAփAփAփAցAփAփAփAցAցAփAփAօAօAօAօAօAփA�~�A�~�A�|�AցA�~�AցA�~�A�|�A�~�A�~�A�~�A�|�A�|�A�~�A�|�A�|�A�~�A�z�A�|�A�|�AցA�~�A�|�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�|�A�|�A�|�A�~�A�~�A�~�AցAցAցAփAցAցAցAփAցAփAցAցAփAփAփAցAփAօAօAև+Aև+AօAև+AօAօAև+AօAօAօAև+AօAօAև+A։7Aև+A։7Aև+Aև+A։7Aև+Aև+A֋DAև+Aև+A։7A֋DA֍PA֋DA֋DA֋DA֋DA֋DA։7A։7Aև+Aև+Aև+A։7A։7A։7A։7A֋DAև+A։7A։7A։7A։7A։7A։7A։7A֋DA։7A֋DA։7A։7A։7A֋DA֋DA։7A֋DA։7A։7A։7A։7Aև+A։7A։7A֋DA֋DA֍PA֋DA֋DA֋DA֍PA֍PA֍PA֏\A֏\A֍PA֏\A֏\A֑hA֑hA֓uA֑hA֓uA֏\A֑hA֏\A֏\A֏\A֏\A֏\A֑hA֑hA֑hA֑hA֕�A֑hA֑hA֕�A֕�A֗�A֗�A֗�A֙�A֙�A֙�A֙�A֙�A֗�A֙�A֙�A֗�A֙�A֙�A֗�A֙�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֙�A֙�A֙�A֙�A֗�A֗�A֙�A֙�A֗�A֙�A֙�A֙�A֙�A֗�A֗�A֗�A֗�A֗�A֗�A֙�A֙�A֗�A֗�A֗�A֗�A֙�A֙�A֗�A֙�A֙�A֗�A֗�A֗�A֙�A֙�A֙�A֗�A֙�A֗�A֓uA֕�A֓uA֓uA֓uA֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֓uA֓uA֕�A֕�A֗�A֕�A֕�A֕�A֓uA֓uA֕�A֕�A֗�A֕�A֕�A֕�A֓uA֕�A֗�A֕�A֕�A֓uA֓uA֓uA֓uA֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֗�A֗�A֗�A֕�A֗�A֕�A֗�A֕�A֕�A֕�A֗�A֗�A֕�A֕�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֕�A֕�A֗�A֙�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֕�A֗�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֓uA֓uA֕�A֕�A֓uA֓uA֓uA֕�A֕�A֕�A֓uA֓uA֓uA֕�A֓uA֕�A֕�A֕�A֕�A֕�A֕�A֓uA֕�A֕�A֕�A֕�A֕�A֕�A֕�A֓uA֓uA֑hA֓uA֓uA֓uA֕�A֕�A֕�A֕�A֓uA֕�A֓uA֕�A֓uA֕�A֕�A֓uA֓uA֕�A֗�A֓uA֕�A֓uA֕�A֕�A֗�A֕�A֕�A֗�A֗�A֕�A֓uA֕�A֕�A֓uA֑hA֓uA֓uA֓uA֓uA֓uA֓uA֑hA֓uA֑hA֏\A֑hA֓uA֓uA֑hA֑hA֏\A֓uA֑hA֑hA֑hA֓uA֑hA֏\A֍PA֏\A֍PA֍PA֍PA֍PA֋DA֋DA։7A֋DA֍PA։7A։7AփAփA։7A֋DA։7AփAօAև+AօAօAփA�~�AցA�~�A�~�A�~�A�z�A�z�A�|�A�z�A�|�A�z�A�z�A�v�A�v�A�x�A�z�A�z�A�|�A�|�A�z�A�~�A�z�A�x�A�t�A�t�A�t�A�t�A�t�A�r�A�v�A�v�A�v�A�t�A�t�A�x�A�t�A�l�A�jA�n�A�l�A�bNA�S�A�I�A�E�A�G�A�K�A�K�A�I�A�I�A�E�A�G�A�E�A�C�A�I�A�E�A�I�A�C�A�?}A�=qA�=qA�9XA�?}A�=qA� �A�oA���A��A��A��A��`A�ƨAնFAգ�AՓuA�jA�hsA�dZA�bNA�\)A�XA�I�A��A�A��TA��AԬA�JAә�A�9XA�%A��A��TA���A�ĜAҺ^AҶFA҇+AҁA�|�A҃A҉7A�|�AҋDAґhA҇+A�p�A�l�A�jA�jA�jA�jA�jA�jA�hsA�ffA�jA�l�A�hsA�jA�hsA�jA�hsA�ffA�jA�jA�hsA�hsA�ffA�ffA�hsA�hsA�ffA�`BA�ffA�ffA�ffA�dZA�bNA�bNA�bNA�dZA�dZA�ffA�ffA�dZA�ffA�hsA�ffA�ffA�ffA�ffA�ffA�`BA�^5A�^5A�^5A�`BA�^5A�^5A�\)A�XA�XA�XA�ZA�XA�XA�ZA�VA�XA�XA�ZA�XA�XA�XA�XA�XA�VA�XA�VA�XA�XA�XA�VA�S�A�VA�XA�VA�Q�A�G�A�K�A�E�A�A�A�9XA�7LA�+A��A�JA�A��AсA�O�A�=qA�"�A���AЬA�jA�"�Aϕ�AσAρA�~�A�p�A�ffA�O�A�1A�ĜAΕ�A�`BA�O�A�I�A�C�A�?}A�=qA�9XA�"�A�A���A��Aʹ9Aͧ�A͡�A͉7ÁA�z�A�x�A�n�A�jA�hsA�dZA�`BA�O�A�E�A�;dA� �A�oA�
=A���A��A��A��/A��
A���A���A���A��#A��A��#A��#A��A��A���A���A�ĜA���A̾wA̺^A̸RA̶FA̴9A̮A̮A̮A̮A̮Ạ�A̙�A̓uA̋DA̅A�|�A�r�A�l�A�ffA�`BA�`BA�ZA�K�A�=qA�E�A�G�A�=qA�1'A�/A�+A�&�A� �A� �A�&�A�+A�(�A�(�A�(�A�&�A��A�bA���A���A�t�A���A�ZA�G�AȁA�C�A��AǼjA�C�A���AƩ�AƁA�t�A�^5A�M�A�;dA��A�{A�VA�A���A��A��
A�ĜAŶFAŏ\A�`BA�7LA�1A��HAġ�A�^5A�5?A��A��A��;A��
A���A���A���A�ƨA�ȴAú^AìAç�AÏ\A�t�A�l�A�dZA�Q�A�K�A�I�A�=qA�5?A�(�A� �A��A�oA�%A���A��A��yA��TA��/A���A¶FA®A¬A§�A�A�ADA�|�A�p�A�hsA�^5A�^5A�^5A�VA�M�A�G�A�C�A�A�A�=qA�;dA�(�A��A�{A�VA�A���A��mA��;A��TA��;A���A���A���A�ȴA�ĜA��^A��9A��!A��A��\A��+A��7A��DA��7A�x�A�ffA�\)A�S�A�M�A�E�A�9XA�&�A�A��HA��
A���A��FA��\A�~�A�`BA�7LA���A�ffA��A��A��
A���A���A��9A��A�VA�?}A� �A�VA���A��yA��HA��A�ĜA��RA���A���A���A��A�r�A�hsA�`BA�VA�M�A�G�A�?}A�;dA�33A�/A�+A�(�A�$�A��A��A�A��HA��HA��
A��
A���A���A��wA��^A��9A��A���A���A��PA�z�A�jA�S�A�9XA�{A��A���A�x�A�5?A��;A��\A�ffA�1'A��A�A��+A�G�A�(�A�%A��A�A���A�ZA�JA��FA�;dA� �A��A��A��A�{A�bA�1A�A���A��mA��A���A���A��A���A���A��PA��7A��+A��A��A�~�A�|�A�t�A�p�A�n�A�l�A�ffA�XA�K�A�?}A�33A� �A�
=A���A��A��mA��HA��#A���A���A�ĜA��FA��A���A���A���A���A���A���A���A���A���A��hA��DA�l�A�M�A�;dA�1'A�$�A�bA��A���A��!A���A��A�x�A�r�A�`BA�33A��`A���A��uA��7A��A��A�|�A�|�A�x�A�x�A�x�A�r�A�p�A�jA�dZA���A��wA��jA�A���A��#A��
A��mA���A���A���A���A���A��A���A�  A��A��A� �A�"�A� �A�$�A�&�A�(�A�&�A�(�A�&�A�+A�+A�+A�+A�+A�/A�(�A�-A�-A�/A�-A�/A�+A�-A�/A�1'A�/A�/A�1'A�5?A�1'A�7LA�7LA�=qA�A�A�A�A�A�A�?}A�A�A�E�A�O�A�Q�A�VA�XA�XA�ZA�Q�A�^5A�hsA�n�A�r�A�v�A�|�A�v�A�~�A��A��+A��hA���A��!A��FA��9A��A�ȴA��A��yA��A�A�bA�{A�oA��A�1A�"�A�?}A�E�A�VA�VA�ffA�z�A��7A���A��A��^A���A��HA��yA��mA��yA��A�A�(�A�1'A�A�A�M�A�bNA��A���A��FA��A��FA��#A��uA�|�A�~�A�~�A�|�A�z�A�x�A�x�A�x�A�x�A�x�A�x�A�v�A�x�A�v�A�z�A�x�A�x�A�x�AցA�~�AփAցAցAփAցAցAցAցAփAփAփAփAօAփAփAփAցAփAփAփAցAցAփAփAօAօAօAօAօAփA�~�A�~�A�|�AցA�~�AցA�~�A�|�A�~�A�~�A�~�A�|�A�|�A�~�A�|�A�|�A�~�A�z�A�|�A�|�AցA�~�A�|�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�|�A�|�A�|�A�~�A�~�A�~�AցAցAցAփAցAցAցAփAցAփAցAցAփAփAփAցAփAօAօAև+Aև+AօAև+AօAօAև+AօAօAօAև+AօAօAև+A։7Aև+A։7Aև+Aև+A։7Aև+Aև+A֋DAև+Aև+A։7A֋DA֍PA֋DA֋DA֋DA֋DA֋DA։7A։7Aև+Aև+Aև+A։7A։7A։7A։7A֋DAև+A։7A։7A։7A։7A։7A։7A։7A֋DA։7A֋DA։7A։7A։7A֋DA֋DA։7A֋DA։7A։7A։7A։7Aև+A։7A։7A֋DA֋DA֍PA֋DA֋DA֋DA֍PA֍PA֍PA֏\A֏\A֍PA֏\A֏\A֑hA֑hA֓uA֑hA֓uA֏\A֑hA֏\A֏\A֏\A֏\A֏\A֑hA֑hA֑hA֑hA֕�A֑hA֑hA֕�A֕�A֗�A֗�A֗�A֙�A֙�A֙�A֙�A֙�A֗�A֙�A֙�A֗�A֙�A֙�A֗�A֙�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֙�A֙�A֙�A֙�A֗�A֗�A֙�A֙�A֗�A֙�A֙�A֙�A֙�A֗�A֗�A֗�A֗�A֗�A֗�A֙�A֙�A֗�A֗�A֗�A֗�A֙�A֙�A֗�A֙�A֙�A֗�A֗�A֗�A֙�A֙�A֙�A֗�A֙�A֗�A֓uA֕�A֓uA֓uA֓uA֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֓uA֓uA֕�A֕�A֗�A֕�A֕�A֕�A֓uA֓uA֕�A֕�A֗�A֕�A֕�A֕�A֓uA֕�A֗�A֕�A֕�A֓uA֓uA֓uA֓uA֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֗�A֗�A֗�A֕�A֗�A֕�A֗�A֕�A֕�A֕�A֗�A֗�A֕�A֕�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֕�A֕�A֗�A֙�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֗�A֕�A֗�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֕�A֓uA֓uA֕�A֕�A֓uA֓uA֓uA֕�A֕�A֕�A֓uA֓uA֓uA֕�A֓uA֕�A֕�A֕�A֕�A֕�A֕�A֓uA֕�A֕�A֕�A֕�A֕�A֕�A֕�A֓uA֓uA֑hA֓uA֓uA֓uA֕�A֕�A֕�A֕�A֓uA֕�A֓uA֕�A֓uA֕�A֕�A֓uA֓uA֕�A֗�A֓uA֕�A֓uA֕�A֕�A֗�A֕�A֕�A֗�A֗�A֕�A֓uA֕�A֕�A֓uA֑hA֓uA֓uA֓uA֓uA֓uA֓uA֑hA֓uA֑hA֏\A֑hA֓uA֓uA֑hA֑hA֏\A֓uA֑hA֑hA֑hA֓uA֑hA֏\A֍PA֏\A֍PA֍PA֍PA֍PA֋DA֋DA։7A֋DA֍PA։7A։7AփAփA։7A֋DA։7AփAօAև+AօAօAփA�~�AցA�~�A�~�A�~�A�z�A�z�A�|�A�z�A�|�A�z�A�z�A�v�A�v�A�x�A�z�A�z�A�|�A�|�A�z�A�~�A�z�A�x�A�t�A�t�A�t�A�t�A�t�A�r�A�v�A�v�A�v�A�t�A�t�A�x�A�t�A�l�A�jA�n�A�l�A�bNA�S�A�I�A�E�A�G�A�K�A�K�A�I�A�I�A�E�A�G�A�E�A�C�A�I�A�E�A�I�A�C�A�?}A�=qA�=qA�9XA�?}A�=qA� �A�oA���A��A��A��A��`A�ƨAնFAգ�AՓuA�jA�hsA�dZA�bNA�\)A�XA�I�A��A�A��TA��AԬA�JAә�A�9XA�%A��A��TA���A�ĜAҺ^AҶFA҇+AҁA�|�A҃A҉7A�|�AҋDAґhA҇+A�p�A�l�A�jA�jA�jA�jA�jA�jA�hsA�ffA�jA�l�A�hsA�jA�hsA�jA�hsA�ffA�jA�jA�hsA�hsA�ffA�ffA�hsA�hsA�ffA�`BA�ffA�ffA�ffA�dZA�bNA�bNA�bNA�dZA�dZA�ffA�ffA�dZA�ffA�hsA�ffA�ffA�ffA�ffA�ffA�`BA�^5A�^5A�^5A�`BA�^5A�^5A�\)A�XA�XA�XA�ZA�XA�XA�ZA�VA�XA�XA�ZA�XA�XA�XA�XA�XA�VA�XA�VA�XA�XA�XA�VA�S�A�VA�XA�VA�Q�A�G�A�K�A�E�A�A�A�9XA�7LA�+A��A�JA�A��AсA�O�A�=qA�"�A���AЬA�jA�"�Aϕ�AσAρA�~�A�p�A�ffA�O�A�1A�ĜAΕ�A�`BA�O�A�I�A�C�A�?}A�=qA�9XA�"�A�A���A��Aʹ9Aͧ�A͡�A͉7ÁA�z�A�x�A�n�A�jA�hsA�dZA�`BA�O�A�E�A�;dA� �A�oA�
=A���A��A��A��/A��
A���A���A���A��#A��A��#A��#A��A��A���A���A�ĜA���A̾wA̺^A̸RA̶FA̴9A̮A̮A̮A̮A̮Ạ�A̙�A̓uA̋DA̅A�|�A�r�A�l�A�ffA�`BA�`BA�ZA�K�A�=qA�E�A�G�A�=qA�1'A�/A�+A�&�A� �A� �A�&�A�+A�(�A�(�A�(�A�&�A��A�bA���A���A�t�A���A�ZA�G�AȁA�C�A��AǼjA�C�A���AƩ�AƁA�t�A�^5A�M�A�;dA��A�{A�VA�A���A��A��
A�ĜAŶFAŏ\A�`BA�7LA�1A��HAġ�A�^5A�5?A��A��A��;A��
A���A���A���A�ƨA�ȴAú^AìAç�AÏ\A�t�A�l�A�dZA�Q�A�K�A�I�A�=qA�5?A�(�A� �A��A�oA�%A���A��A��yA��TA��/A���A¶FA®A¬A§�A�A�ADA�|�A�p�A�hsA�^5A�^5A�^5A�VA�M�A�G�A�C�A�A�A�=qA�;dA�(�A��A�{A�VA�A���A��mA��;A��TA��;A���A���A���A�ȴA�ĜA��^A��9A��!A��A��\A��+A��7A��DA��7A�x�A�ffA�\)A�S�A�M�A�E�A�9XA�&�A�A��HA��
A���A��FA��\A�~�A�`BA�7LA���A�ffA��A��A��
A���A���A��9A��A�VA�?}A� �A�VA���A��yA��HA��A�ĜA��RA���A���A���A��A�r�A�hsA�`BA�VA�M�A�G�A�?}A�;dA�33A�/A�+A�(�A�$�A��A��A�A��HA��HA��
A��
A���A���A��wA��^A��9A��A���A���A��PA�z�A�jA�S�A�9XA�{A��A���A�x�A�5?A��;A��\A�ffA�1'A��A�A��+A�G�A�(�A�%A��A�A���A�ZA�JA��FA�;dA� �A��A��A��A�{A�bA�1A�A���A��mA��A���A���A��A���A���A��PA��7A��+A��A��A�~�A�|�A�t�A�p�A�n�A�l�A�ffA�XA�K�A�?}A�33A� �A�
=A���A��A��mA��HA��#A���A���A�ĜA��FA��A���A���A���A���A���A���A���A���A���A��hA��DA�l�A�M�A�;dA�1'A�$�A�bA��A���A��!A���A��A�x�A�r�A�`BA�33A��`A���A��uA��7A��A��A�|�A�|�A�x�A�x�A�x�A�r�A�p�A�jA�dZA���A��wA��jA�A���A��#A��
A��mA���A���A���A���A���A��A���A�  A��A��A� �A�"�A� �A�$�A�&�A�(�A�&�A�(�A�&�A�+A�+A�+A�+A�+A�/A�(�A�-A�-A�/A�-A�/A�+A�-A�/A�1'A�/A�/A�1'A�5?A�1'A�7LA�7LA�=qA�A�A�A�A�A�A�?}A�A�A�E�A�O�A�Q�A�VA�XA�XA�ZA�Q�A�^5A�hsA�n�A�r�A�v�A�|�A�v�A�~�A��A��+A��hA���A��!A��FA��9A��A�ȴA��A��yA��A�A�bA�{A�oA��A�1A�"�A�?}A�E�A�VA�VA�ffA�z�A��7A���A��A��^A���A��HA��yA��mA��yA��A�A�(�A�1'A�A�A�M�A�bNA��A���A��FA��A��FA��#A��uG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�ީ=�5+>U�"@�->�v�=�a=���=���=��@e�=���@q�@_�n>o
�@�@�@�9C=���>2ǹ?�Se>�P	@�@�@�6z>/s�@�i@�B�@���>6�o@�=�@�1�>�BF>��@YG�@�:�=���> $�@=��=���=ز�>��?Ĩ=�j=�y}=�$�=� �>H�+?B=�خ=���=�Qn>U �@�!B=�,(>���?��=w��=�3>U��=���=���> u�>�>���=�>J@�B@�B�>�@��@�EN=���=�2�>�P@�2M@�8q=��0>0��@�?�=��e>UW?��'>W�@�8�=�O�>2y>�)�@�DR@�C�=��,@���@�h>]E>���@�@�@�E9?{�4?U �@�G0?�Q�=��b?�I�=���>�>U� @�>B?�q=�x?^�|=��>c��>�c@�y�=�Mj@F�H@�CW=��f>&E=�(c=���>�@Tzc=ej�=�;�=��t>K�@��@�F_>P3>FԀ@�/�@B�>j�?r�>T�%@�M@�J8>nX=��>p�@�$5??�@�B@�@:=ˇ�=빌@�i@�E�@�L0=��=�u=���>=4Y@�G0@�0�=Ӹ�>Of�@�A�@�G�@�N�?�p�>FE@�F�>�=\=�H�?�;�=�zN?qm�@�F@�I(=�:�?ݫ�=�%�>M:�@�G�@�@�=���=���?G3�=⌽?�o*@�M�@�#?)V@��/>dZ@�r�@�D�?�=�=�q�?��@�I(@R��>>Q�@�K
>W9m@�Pr@�P	>.A@�P�>�(�@�M>��7>R @�N<@�P	@�J�@��=�t�>��@�L�@�M@�L�@�M@�Ks@�@�I{?r�@�N'@�N'@�M�@�M@�N'@�N'@�MU@�L�@�MU@�N{@�N�@�P	@�M�@�J�@�K
@�L0@�N'@�N{@�OL@�Q/@�MU@�N�@�M�@�K
@�K�@�Ks@�M�@�MU@�N�@�OL@�N{@�M�@�N{@�Ks@�N'@�M�@�N�@�Pr@�P	@�P	@�S&@�S�@�S�@�S�@�S�@SP	@�R�@�Q�@�R @�R @�P�@�P�@�N'@�S&@�S�@�TL@�R�@�R�@�S�@�Ri@�S�@�Ri@�S�@�S�@�Ta@�S&@�Ri@�Ta@�U�@�VC@�VC@�U�@�U@�Ri@�Wi@�Z�@�V�@�VC@�T�@�X�@�X%@�W @�W @�Wi@�W�@�X�@�X�@�W�@�W�@�X%@�X�@�X%@�V�@�U�@�U�@�X�@�X�@�X%@�X�@�W�@�W @�W @�X%@�X�@�X�@�Y`@�Wi@�W�@�W @�VC@�S&@�TL@�U�@�X�@�X%@�W @�V�@�U�@�V�?�w�@�V�@�W @�T�@�T�@�U�@YgM@�W @�W�@�U�@�S�@�U�@�Ri@�Ta@�TL@�Ri@�O@�S�@�S&@�S&@�Ri@�Q�@�TL@�U�@�S�@�Ri@d��@�͊@�U@�R�@�Ri@�Ri@�Ri@�U�@�S�@�Pr@�R�@�Pr@�T�@�R @�P�@�R @�P�@�S�@�Ta@�Q�@�N'@�O�@�Q/@�QD?D��@�R�@�R�@�S&@�U�@�T�@�U�@�S�@�U�@�VC@�V�@�W @�V�@�U@�TL@�R�@�S�@�V�@�U@�Wi@�VC@�VC@�VC@�V�@�U�@�U�@�U@�VC@�VC@�V�@�U�@�U@�V�@�VC@�W�@�W @�VC@�U�@�VC@�U�@�T�@�U�@�VC@�U�@�U@�X�@�Wi@�U@�Y`@�Z@�X�@�Y`@�Y�@�Y�@�Y�@�X%@�X�@�Y�@�Wi@�W @�W @�X�@�Z�@�Z�@�[B@�Z�@�Z�@�Z�@�[B@�Z@�Yu@�Y�@�Y�@�Y`@�[B@�Z�@�Z@�Y`@�Y�@�[B@�V�@�X�@�Z@�X�@�Y`@�Y`@�Z�@�X�@�X%@�W�@�V�@�W @�X%@�Z@�W�@�Wi@�Wi@�Wi@�W @�U�@�Y�@�W @�U@�V�@�Y�@�[B@�\�@�]�@�\�@�^@�^_@�_1@�^_@�_�@�_�@�_1@�_�@�_�@�_�@�_�@�_1@�_1@�_�@�^_@�^_@�^t@�^�@�^�@�^�@�^�@�_1@�_�@�_�@�_�@�`@�`W@�`�@�`W@�^t@�_1@�_F@�_1@�^�@�_F@�^�@�^�@�^�@�_�@�^�@�_F@�_�@�`@�a�@�a�@�b�@�b9@�bc@�b�@�b�@�c @�c @�b�@�c�@�c�@�c�@�c�@�c�@�dZ@�c�@�dZ@�c�@�dZ@�c�@�d0@�d0@�c�@�c�@�c�@�c�@�dZ@�c�@�d�@�d�@�c�@�c�@�d�@�c�@�c5@�c�@�c�@�c�@�c�@�c�@�c�@�cs@�c�@�c�@�dZ@�c�@�d@�d@�d@�d@�do@�do@�d@�do@�c�@�c�@�dE@�dE@�do@�d�@�e,@�d�@�e,@�e�@�e,@�]@�bx@�e,@�e�@�e�@�e�@�f�@�f�@�gb@�gb@�f�@�g�@�g�@�g�@�hI@�g�@�g�@�g�@�g#@�g�@�g�@�i�@�i@�i�@�i�@�i�@�j@@�i�@�j@�j@�i�@�i�@�i�@�jU@�jU@�j�@�k@�k@�k@�k@�k{@�k{@�k{@�k�@�k{@�k<@�k{@�k�@�k�@�lL@�l�@�m]@�l�@�m	@�m]@�m]@�l�@�l�@�k�@�lL@�lL@�l�@�l�@�m	@�m	@�l�@�mH@�mH@�m�@�m�@�mr@�mr@�mr@�m�@�m�@�m�@�n�@�m�@�m�@�m�@�nD@�nD@�nD@�nD@�n�@�n�@�n�@�n�@�n�@�n�@�o@�oi@�p;@�p;@�o�@�p;@�p�@�p�@�p�@�qa@�q�@�r2@�q�@�q�@�r�@�r�@�r�@�s@�s@�sm@�s@�r�@�q�@�q�@�r2@�r2@�r�@�s�@�s�@�s�@�t~@�t�@�t�@�t�@�u�@�v!@�v!@�v�@�v�@�v�@�v�@�wG@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�wG@�wG@�w�@�wG@�wG@�v�@�wG@�wG@�v�@�x@�w�@�w�@�x@�x@�x@�w�@�x@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�y>@�x�@�x�@�y>@�y>@�yS@�x�@�yS@�yS@�yS@�yS@�yS@�yS@�y�@�yS@�yS@�x�@�yS@�x�@�x�@�x�@�x�@�x�@�yS@�yS@�x�@�yS@�yS@�yS@�yS@�yS@�yS@�yS@�y�@�y�@�y�@�yS@�y�@�z:@�y�@�z:@�z%@�z%@�z�@�z�@�z%@�z�@�z�@�z%@�z�@�z�@�z�@�z�@�z�@�{J@�z�@�z�@�z�@�z�@�{J@�{J@�{J@�{�@�{�@�{�@�|@�|@�|@�{�@�|@�|@�|�@�|@�|�@�|�@�|1@�|�@�|�@�|�@�|�@�|�@�}V@�}V@�|�@�}V@�}V@�}V@�}�@�}�@�}V@�}�@�}�@�}�@�}�@�}�@�}V@�~@�~@�~@�~@�~@�~@�~@�~@�~�@�~�@�9@�~|@�~|@�~|@�~�@�~�@�}�@�~|@�~@�~@�~|@�~|@�~|@�~@�}�@�~@�~|@�~@�~|@�~@�~|@�~|@�~�@�~|@�~�@�~�@�~�@�N@�~�@�N@�N@�N@�N@�N@�N@�N@�N@�N@��@��@��@��
@��@��
@�N@��@��@�9@��
@��
@��
@��@��
@��
@��
@��@��
@��
@��@��@��@��@�N@��
@��@�N@�N@��@�N@�N@�~�@�~�@�~�@�9@�~�@�~�@�~�@�~@�~�@�}k@�}@�}@�}@�|�@�|1@�{�@�{t@�{@�zN@�y@�x�@�x�@�w�@�w2@�v@�ti@�sC@�r@�q�@�q@�p�@�q@�qa@�q�@�qa@�p�@�p�@�p;@�p;@�p;@�p;@�p;@�oi@�n�@�m�@�m@�l�@�l�@�l"@�l�@�l�@�k�@�kQ@�i�@�hs@�hs@�i�@�j@�j@�i/@�h
@�h
@�gM@�ek@�d�@�cs@�b�@�b$@�`�@�`B@�_�@�`�@�a@�a�@�b9@�b9@�b9@�a@�_�@�]�@�\@�Z�@�Z�@�Z@�Y`@�Z@�[�@�\@�[B@�Z�@�X�@�VC@�S�@�O7@�Ks@�I{@�HV@�G0@�G0@�G�@�G�@�D�@�@:@�;:@�8@�7L@�6&@�2M@�/0@�-�@�,�@�,|@�+V@�)�@�&�@�#�@�"@�&@��@�'@��@��@�}@��@�W@�}@��@�
(@��@��@�k@�t@�1@��X@��k@��@���@���@�خ@�˧@���@���@��D@��$@���@�m�@�T7@�C@�9@�4�@�3@�3	@�3]@�33@�2�@�2�@�28@�2�@�2�@�2�@�3	@�2�@�2�@�3	@�28@�2�@�1�@�1�@�0�@�0�@�/0@�.�@�-�@�-M@�,(@�*�@�'�@�$t@�!�@��@�!@��@� G@�!l@�"�@�"�@�")@�!@��@��@�m@��@�"@�@@�n@��@�H@��@��@��@��@��@��@��@�#@�H@�@�n@�n@�@@��@��@��@��@�!l@�"�@�#O@�$ @�#�@�"�@�!l@�!@� �@� G@��@��@�!@��@��@�!@��@��@��@��@�@�N@�O@� *@���@���@��@��@@���@���@��H@���@���@��@���@��@��@���@�ں@�Ԫ@��u@�Ɠ@���@���@��{@��l@���@�i�@�G�@�"}@�@��@@��\@��I@��1@��@��L@��p@���@��4@�p@�h4@�d@�\}@�V�@�Ri@�M�@�I�@�Cl@�7a@�.^@�+�@�'=@�H@��@��@�@���@���@��f@���@��@���@���@���@��@��/@��j@�ҳ@�ʂ@��C@���@���@���@��5@���@���@��G@���@��@���@��@��u@���@��G@���@��@���@���@��@���@��p@���@���@��"@���@���@��w@�|F@�s.@�mr@�k�@�j�@�j@�i�@�d�@�c^@�c^@�`�@�_�@�[-@�Zq@�X�@�U�@�Q�@�Mj@�Go@�D|@�@�@�=@�<�@�;�@�:i@�:i@�9X@�4@�.4@�(9@�!�@�r@��@���@��U@��>@��^@�F5@��@��N@��*@�t@�VX@�*�@�o@��f@���@���@��A@���@�Ë@��N@��1@��u@��<@���@���@���@���@�x@�h�@�V.@�D(@�2�@�!�@�x@��^@��K@��<@��1@���@��L@���@��]@��<@���@��@���@���@���@��N@��}@���@��@@���@�}�@�{@�v!@�q�@�m@�h�@�d�@�`W@�[�@�WT@�Ri@�N'@�J�@�F�@�AJ@�9�@�5@�2�@�/�@�,�@�)�@�%�@� �@��@�@�b@�Q@��@��@��@�
�@��@�O@��@�`@��P@���@��@�� @��@��6@��@�ۡ@�ڐ@��o@��=@���@��@�̸@���@�� @��z@���@���@���@��T@��"@��D@��{@���@���@��e@��@��k@�z�@�t�@�k�@�`�@�V@�M�@�H�@�B1@�9X@�-�@�%�@�/@� �@���@��!@���@���@�~�@�u�@�i@�V.@�I�@�Ex@�B�@�:�@�6�@�0�@�*�@�@��@��@�
�@��@�6@���@��@��<@��8@���@��}@��@���@��Z@���@���@���@��X@���@��m@��@@��b@��0@��g@���@��@���@���@��e@���@�� @���@��'@���@���@���@���@�@�s@�bx@�JM@�'�@���@��^@���@�{t@�v�@�i�@�[�@�Se@�>�@�'�@�b@��@��#@��q@���@�҉@��@���@���@�YK@�LD@�K�@�K4@�J8@�H�@�G�@�Ec@�C@�?�@�<6@�8q@�5~@�2M@�*@�%�@�"�@� G@��@��@��@��@��@��@�]@�Q@�@�s@�@�	l@��@���@���@��@��t@��P@�ߏ@��#@�ֶ@���@�ϖ@�̎@���@���@���@��M@���@��|@��@��h@��p@��d@��G@���@��@���@���@��@���@�z�@�r@�f<@�Y�@�Ks@�@y@�6;@�,|@�$�@��@�@���@���@��1@��y@���@�ƽ@��@��a@��U@��H@��<@���@���@���@��:@���@��k@��D@���@��6@���@���@��5@��$@���@���@��c@��@���@��P@���@���@��-@���@��@�ڐ@��/@���@��j@��&@��L@��@��7@��@���@���@��i@���@��z@��e@��@��@��@��6@���@��\@��@��.@���@��S@��S@��@��R@��@��@��@���@��0@��@��J@���@��$@��@���@���@��X@��C@��X@� �@��@��@�C@�h@��@�W@�2@�@��@�o@�|@��@�b@��@� �@�4�@�9C@�<�@�;�@�:*@�Ec@�U�@�b�@�g�@�o @�r2@�y>@��w@��~@���@��G@���@��7@���@��H@���@��|@��<@��b@��7@���@���@���@��5@��@�ٔ@��@��@��i@���@��@�6�@�L�@�\}@�_[@�Z�@�\}@�]�@��@@���@�ߏ@�ߤ@��@@���@��n@�݃@��@��/@���@�݃@�ݘ@���@��+@��+@��j@�݃@��@��@��	@���@���@���@��H@��@��.@��.@�ݭ@��C@��@��@��@��@��@���@��@��@���@��@���@���@��i@��@��@��@��@��;@��@��@��C@���@���@��C@��C@��@��@���@��C@��m@��.@��@���@��.@��@��.@��@��@��X@��m@��i@��@��X@��@��@���@���@��@���@��*@��*@��@��T@��@��@���@���@��@��@��6@��@��@��@���@��@���@��@��@��`@��`@���@��@��6@���@���@��)@���@���@��@��@���@��S@��@��@���@���@���@��S@���@��@��%@��%@��@��y@��y@��@��O@��:@��y@��@��y@��K@��F@���@��F@��@��@��p@��`@��@��@��K@��`@��@��@��@���@��@���@���@���@���@���@��1@��1@��p@��F@��1@���@��@��@���@��@��@��@��-@��-@��@���@���@���@���@���@��R@��)@��)@��)@��g@���@���@��N@��@��J@��5@��@��@��t@��t@��E@��E@��@���@��J@���@��@��@��J@��5@��@���@���@��,@��@��@��k@���@���@���@��@��8@��@��I@��s@��s@��4@��s@��@��I@��@��4@��@���@���@���@��@��4@��@��s@��s@��^@��@���@��@��E@��0@��@��@��@��Z@��o@��E@���@���@���@���@���@���@���@��@���@���@���@���@���@���@���@���@��@��@��U@��+@��+@���@��@@���@���@��j@��j@���@���@��@���@��E@��@��@��E@��Z@���@���@���@���@��@��+@��+@��+@��@@��@@��U@��j@��j@���@���@���@���@���@��@���@���@���@��@���@��@��@���@��f@���@��<@���@���@��Q@��Q@��Q@���@���@���@���@���@���@��@��#@��#@��M@��w@��w@��w@���@���@���@���@���@���@���@���@��	@��	@��3@��	@��3@��	@��]@��3@��r@��r@���@���@���@���@���@���@���@���@���@���@��@��@��/@��Y@���@��@���@���@���@��@��@��+@��@���@��@���@���@���@���@���@���@���@���@���@���@���@���@��Y@��n@���@���@���@��@��@��@���@��/@���@���@��@���@���@���@��@��+@��@@���@��@���@���@���@���@��@@��U@��U@��@���@��@���@���@���@���@���@��@���@���@���@���@���@���@���@��@��z@��@��;@��P@��;@��P@���@���@���@���@���@���@��@���@��;@��@��U@���@���@���@���@���@���@��@@��U@��@@���@���@��@��U@��U@��U@��@@��U@��@��@@���@��U@��@��Y@���@���@���@��@���@���@���@��]@��@��]@��@��@��8@���@���@��@���@��M@��U@��@��@���@���@��U@���@���@���@��@��I@��@��w@��M@��@��M@��M@��M@��@��g@���@��@��w@��@���@��@��@��#@��@���@��,@���@���@���@��o@��V@��,@��@���@���@���@��o@��N@��@���@���@��O@��@���@��v@��L@��	@���@��@��@��a@��"@�߹@��@��;@��@��7@���@��H@��b@��Q@��j@��'@���@�ϫ@��2@���@���@��@���@���@���@��\@���@���@��@�� @��/@��	@���@���@���@�n/@�i�@�[�@�V@�J�@�]@���@�ί@��<@��t@��@��1@���@���@��1@��@��@���@��-@��p@��@���@��p@��-@��-@��-@���@���@���@���@��}@��S@���@��@���@��N@��9@��@���@��$@���@��S@���@���@���@��>@���@��>@��@��@���@���@��>@��}@��S@��>@��S@��}@��}@���@��S@���@��>@��S@��S@���@��>@��S@��}@���@���@��S@��>@��@���@��)@���@���@��@��@��p@��F@��[@��[@��@��@��@���@���@��@��@���@���@��u@���@���@��`@��6@���@��6@��6@���@��:@���@���@��>@���@���@���@���@��r@���@���@��@���@��|@��@���@�rq@�c�@�[@�Oa@�<�@��@��@��k@���@�~=@�},@�{t@�v6@�q�@�g�@�IR@�(�@�Z@���@���@��-@��@��@��@��@���@��!@�õ@��=@���@��^@���@���@���@���@���@��k@���@���@�~g@�}k@�u�@�o�@�m�@�`�@�X%@�Uq@�N�@�I@�D�@�>�@�<6@�7@�3�@�9X@�>W@�>�@�>�@�?�@�@:@�A�@�>�@�;y@�7�@�5�@�3�@�3	@�2#@�0�@�/0@�,�@�,�@�+�@�.4@�0�@�+�@�&�@�#�@��@��@��@�3@��@��@��@��@�	B@��@���@�  @�C@���@��@��f@���@��@���@��@��R@��<@��@���@��@��@��|@��@���@�ѷ@��D@�q@�-�@���@�a�@�D�@�6@� @��'@���@���@��@���@�w�@�n�@�iY@�\)@�XO@�V@�QY@�M�@�G�@�>�@�6&@�0U@�!@��@��@���@��]@���@��s@��@��@�q@�do@�_�@�]�@�Z�@�\�@�U�@�U�@�O�@�G@�Dg@�:�@�,�@�&�@�#�@��@�@��@�@��@�	�@�W@�  @��	@���@���@��9@��@��`@���@�݃@�҉@���@��}@���@�ƨ@��]@��@���@���@���@��@��@��@���@���@���@���@��^@��w@���@��E@��@��q@���@��<@���@�|�@�u�@�wp@�v�@�o�@�l7@�m�@�k�@�i�@�e�@�b�@�\�@�_[@�P	@�J@�Jb@�K�@�K�@�D@�;:@�6z@�2�@�0+@�+�@�'(@�!@�@�h@���@��P@��M@��X@��z@���@��+@��5@�d�@�F@�6�@�,g@�*�@�&�@� G@�@��]@��j@��@��?@��@��$@��5@�ί@�Ǥ@���@��@���@��$@���@���@���@���@��x@���@���@��C@���@���@�� @��H@��v@��U@��#@���@�y�@�l�@�k�@�h@�h4@�do@�a|@�]�@�[�@�X�@�Vm@�Q�@�Nf@�H@�@d@�8�@�.�@�#�@��@���@���@��y@��:@���@�f�@�Ov@�<�@�!@�	�@��@��|@�Ɠ@��x@���@��@��~@�r�@�O"@�1{@���@���@���@��%@��@��)@��C@���@���@��@��@���@��@��[@�ɛ@��H@��j@��#@���@���@���@��
@��c@���@���@���@��h@���@���@���@��@���@��9@���@���@��@���@�@�{�@�x�@�uy@�r\@�o�@�j�@�f�@�d�@�b$@�_�@�^t@�]:@�]%@�\@�[W@�Z�@�Y�@�VX@�L@�<!@�3@�-�@�(�@� �@�Q@��@���@��y@�޾@��@�׈@�ҳ@���@��,@��s@�r�@�m	@�iD@�h
@�e�@�e�@�d�@�c�@�c�@�a�@�_F@�[�@�Y@�zc@�|�@�~�@��I@���@���@���@���@���@��S@��@��c@��@���@��1@��V@��@��	@���@��?@��T@��;@���@���@��K@���@���@��@���@��2@��@���@��.@���@��X@���@��@���@��)@��C@���@��@��%@���@���@��!@��@��K@��F@��h@��c@��@���@��g@���@��A@���@���@���@��H@���@��+@��U@��b@���@�Ǐ@���@�̸@���@��}@�э@�Ԫ@���@��@�۶@��L@��m@���@���@���@��@��@��@�d@�@��@�z@�U@�a@�"@� @�+�@�-�@�5�@�6P@�?>@�H,@�N�@�X@�`@�d�@�lL@�v@�{ @�|[@�z�@��@���@���@���@���@��2@���@��U@�Ҟ@���@��$@�۶@��@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                444344444443343344443344334334433444444444444444443444444444444433433444334434444344433433443344344444434444443443444443444433443444443444343344433444433443334434444433444433444443443433444334434334343443333443333343433333333333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�@q�@_�pG�O�@�@�@�9CG�O�G�O�G�O�G�O�@�@�@�6{G�O�G�O�@�B�@���G�O�@�=�@�1�G�O�G�O�@YG�@�:�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�!BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�B@�B�G�O�@��@�EMG�O�G�O�G�O�@�2K@�8vG�O�G�O�@�?�G�O�G�O�G�O�G�O�@�8�G�O�G�O�G�O�@�DN@�C�G�O�@���@�jG�O�G�O�@�@�@�E8G�O�G�O�@�G5G�O�G�O�G�O�G�O�G�O�G�O�@�>FG�O�G�O�G�O�G�O�G�O�G�O�@�y�G�O�G�O�@�CYG�O�G�O�G�O�G�O�G�O�@TzbG�O�G�O�G�O�G�O�@��@�FaG�O�G�O�@�/�G�O�G�O�G�O�G�O�G�O�@�J7G�O�G�O�G�O�@�$9G�O�@�B@�@=G�O�G�O�G�O�@�E�@�L2G�O�G�O�G�O�G�O�@�G2@�0�G�O�G�O�@�A�@�G�@�N�G�O�G�O�@�F�G�O�G�O�G�O�G�O�G�O�@�F
@�I&G�O�G�O�G�O�G�O�@�G�@�@�G�O�G�O�G�O�G�O�G�O�@�M�G�O�G�O�@��4G�O�@�r�@�D�G�O�G�O�G�O�@�I&@R��G�O�G�O�@�KG�O�@�Pm@�P
G�O�@�P�G�O�@�MG�O�G�O�@�N:@�P
@�J�@��G�O�G�O�@�L�@�M@�L�@�M|@�KtG�O�@�I�G�O�@�N&@�N)@�M�@�M}@�N%@�N&@�MV@�L�@�MT@�N{@�N�@�P@�M�@�J�@�K	@�L2@�N%@�N~@�ON@�Q,@�MW@�N�@�M�@�K@�K�@�Kt@�M�@�MR@�N�@�OM@�Nz@�M�@�N}@�Kt@�N)@�M�@�N�@�Pt@�P@�P
@�S)@�S�@�S�@�S�@�S�G�O�@�R�@�Q�@�R@�R@�P�@�P�@�N)@�S&@�S�@�TJ@�R�@�R�@�S�@�Ri@�S�@�Rj@�S�@�S�@�Tb@�S"@�Ri@�Tb@�U�@�VH@�VB@�U�@�U@�Rg@�Wl@�Z�@�V�@�V?@�T�@�X�@�X$@�V�@�V�@�Wl@�W�@�X�@�X�@�W�@�W�@�X&@�X�@�X'@�V�@�U�@�U�@�X�@�X�@�X"@�X�@�W�@�V�@�V�@�X*@�X�@�X�@�Yd@�Wj@�W�@�W@�VA@�S*@�TR@�U�@�X�@�X!@�W@�V�@�U�@�V�G�O�@�V�@�W@�T�@�T�@�U�@YgN@�V�@�W�@�U�@�S�@�U�@�Rj@�Ta@�TJ@�Rj@�O@�S�@�S+@�S'@�Rg@�Q�@�TM@�U�@�S�@�Rg@d��@�͊@�U!@�R�@�Rl@�Rg@�Rj@�U�@�S�@�Pt@�R�@�Pw@�T�@�R@�P�@�R@�P�@�S�@�Tb@�Q�@�N)@�O�@�Q2@�QEG�O�@�R�@�R�@�S!@�U�@�T�@�U�@�S�@�U�@�V?@�V�@�V�@�V�@�U @�TJ@�R�@�S�@�V�@�U @�Wf@�VB@�VB@�VB@�V�@�U�@�U�@�U@�VH@�VH@�V�@�U�@�U @�V�@�VB@�W�@�V�@�VH@�U�@�VH@�U�@�T�@�U�@�VH@�U�@�U@�X�@�Wj@�U@�Y`@�Z @�X�@�Y`@�Y�@�Y�@�Y�@�X$@�X�@�Y�@�Wl@�V�@�V�@�X�@�Z�@�Z�@�[?@�Z�@�Z�@�Z�@�[?@�Z@�Yv@�Y�@�Y�@�Yd@�[@@�Z�@�Z@�Yd@�Y�@�[F@�V�@�X�@�Z@�X�@�Y`@�Y`@�Z�@�X�@�X'@�W�@�V�@�W@�X&@�Z@�W�@�Wf@�Wj@�Wj@�W@�U�@�Y�@�W@�U @�V�@�Y�@�[?@�\�@�]�@�\�@�^@�^^@�_3@�^^@�_�@�_�@�_-@�_�@�_�@�_�@�_�@�_3@�_1@�_�@�^`@�^[@�^t@�^�@�^�@�^�@�^�@�_2@�_�@�_�@�_�@�`@�`U@�`�@��?@���@�ߕ@�ߥ@��?@�ݿ@��m@�݃@��@��4@���@�݃@�ݘ@���@��-@��,@��k@�݆@��@��@��@���@���@���@��G@��@��.@��2@�ݯ@��F@��@��@��@��@��@���@��@��@���@��@���@���@��j@��@��@��@��@��>@��@��@��E@���@���@��F@��I@��@��@���@��I@��q@��1@��@���@��4@��@��1@��@��@��Y@��m@��n@��@��R@��@��@���@���@��@���@��-@��&@��@��U@��@��@���@���@��@��@��6@��@��@��@���@��@���@��@��@��c@��e@���@��@��6@���@���@��'@�� @���@��@��@���@��R@��@��@���@���@��@��S@���@��@��'@��*@��@��}@��~@��@��N@��7@��w@��@��z@��N@��H@���@��C@��	@��@��n@��a@��@��@��J@��^@��@��@��@���@��@���@���@���@���@���@��3@��3@��q@��J@��6@���@��@��@���@��@��@��@��.@��.@��@���@���@���@���@���@��U@��*@��1@��*@��j@���@���@��L@��@��J@��6@��@��@��v@��y@��C@��I@�� @���@��H@���@��@��@��O@��6@��@���@���@��.@��@���@��n@���@���@���@��@��;@��"@��J@��w@��t@��7@��t@��"@��K@��@��4@��@���@���@���@��@��7@��&@��t@��q@��`@��@���@��@��H@��-@��@��@��@��^@��r@��E@���@���@���@���@���@���@���@��@���@���@���@���@���@���@���@���@��@��@��T@��+@��.@���@��A@���@���@��n@��j@���@���@��@���@��H@��@��@��C@��\@���@���@���@���@��@��.@��.@��.@��B@��D@��U@��n@��j@���@���@���@���@���@��@���@���@���@��@���@��@��@���@��g@���@��=@���@���@��R@��P@��W@���@���@���@���@���@���@��@��&@�� @��L@��|@��z@��z@���@���@���@���@���@���@���@���@��	@��@��7@��@��4@��@��a@��2@��s@��s@���@���@���@���@���@���@���@���@���@���@��@��@��/@��[@���@��@���@���@���@��@��@��+@��@���@��@���@���@���@���@���@���@���@���@���@���@���@���@��]@��r@���@���@���@��@���@��@���@��/@���@���@���@���@���@���@��@��/@��B@���@���@���@���@���@���@��B@��T@��V@��~@���@��~@���@���@���@���@���@��@���@���@���@���@���@���@��@��@��z@��@��A@��S@��:@��P@���@���@���@���@���@���@��@���@��>@���@��U@���@���@���@���@���@���@��B@��V@��A@���@���@���@��W@��R@��U@��B@��T@���@��A@���@��T@��@��Y@���@���@���@��@���@���@���@��^@��!@��a@��@��@��<@���@���@��@���@��P@��X@���@��@���@���@��W@���@���@���@��@��K@��@��w@��R@��@��N@��N@��N@��@��c@���@��@��w@��@���@��@��@��&@��@���@��1@���@���@���@��u@��Z@��,@�� @���@���@���@��r@��L@��@���@���@��N@��@���@��x@��K@��
@���@��@��@��f@��$@�߶@��{@��>@��@��9@���@��L@��c@��T@��k@��'@���@�Ϯ@��2@���@���@��@���@���@���@��_@���@���@��@��@��/@��
@���@���@���@�n+@�i�@�[�@�V@�J�@�[@���@�ΰ@��:@��s@��@��6@���@���@��.@��@��@���@��/@��s@��@���@��o@��-@��/@��+@��@���@��@��@��|@��S@���@��@���@��R@��:@��
@���@��#@���@��V@���@�� @���@��>@��@��>@��@��@��@���@��>@��z@��T@��:@��Q@��~@��~@���@��V@���@��A@��Q@��U@���@��B@��U@��~@���@���@��S@��@@��@���@��(@���@���@��@��@��o@��J@��\@��\@��@�� @��
@���@���@��@��@���@���@��r@���@���@��c@��9@���@��9@��8@���@��8@���@���@��>@���@���@���@���@��v@���@���@��@���@��~@��@���@�rw@�c�@�[@�Oh@�<�@��@��@��l@���@�~@@�}+@�{t@�v8@�q�@�g�@�IS@�(�@�V@���@���@��.@��@�� @��@��@���@��'@�õ@��?@���@��]@���@���@���@���@���@��n@���@���@�~g@�}l@�u�@�o�@�m�@�`�@�X%@�Up@�N�@�I@�D�@�>�@�<7@�7@�3�@�9W@�>X@�>�@�>�@�?�@�@:@�A�@�>�@�;z@�7�@�5�@�3�@�3@�2"@�0�@�/0@�,�@�,�@�+�@�.2@�0�@�+�@�&�@�#�@��@��@��@�3@��@��@��@��@�	F@��@���@� @�B@���@��@��j@���@��@���@��@��V@��<@��@���@��@��@��{@��@���@�ѹ@��C@�q@�-�@���@�a�@�D�@�6@�@��"@���@���@�� @���@�w�@�n�@�iZ@�\(@�XO@�V@�QW@�M�@�G�@�>�@�6&@�0Z@�!@��@��@���@��^@���@��u@��	@��@�q@�dr@�_�@�]�@�Z�@�\�@�U�@�U�@�O�@�G@�Dg@�:�@�,�@�&�@�#�@��@�@��@�"@��@�	�@�Y@� @��@���@���@��<@��@��`@���@�݆@�҆@���@�ˁ@���@�ƪ@��a@��@���@���@��@��@��
@��
@���@���@���@���@��Z@��y@���@��D@��@��r@���@��:@���@�|�@�u�@�wr@�v�@�o�@�l;@�m�@�k�@�i�@�ez@�b�@�\�@�_Z@�P@�J@�J_@�K�@�K�@�D@�;=@�6~@�2�@�0+@�,@�'#@�!@�@�i@���@��P@��Q@��Y@��@���@��,@��6@�d�@�F@�6�@�,f@�*�@�' @� J@�@��Z@��f@��@��B@��@��#@��2@�β@�Ǧ@���@��@���@��)@���@���@���@���@��y@���@���@��E@���@���@��@��F@��v@��Y@��$@���@�y�@�l�@�k�@�h@�h1@�dr@�a~@�]�@�[�@�X�@�Vo@�Q�@�Ni@�H@�@d@�8�@�.�@�#�@��@���@���@��|@��9@���@�f�@�Ot@�<�@�!@�
@��@��@�Ɩ@��{@���@��@���@�r�@�O"@�1|@���@���@���@��"@��@��+@��E@���@���@��@��@���@��
@��Z@�ɘ@��G@��j@��"@���@���@���@��@��f@���@���@���@��h@���@���@���@��@���@��9@���@���@��@���@�@�{�@�x�@�uy@�r]@�o�@�j�@�f�@�d�@�b#@�_�@�^x@�]:@�]#@�\@�[X@�Z�@�Y�@�V\@�L@�< @�3@�-�@�(�@� �@�R@��@���@��}@���@��@�׊@�ұ@���@��+@��t@�r�@�m@�iF@�h@�f@�e�@�d�@�c�@�c�@�a�@�_E@�[�@�Y@�ze@�|�@�~�@��I@���@���@���@���@��@��T@��@��i@��
@���@��2@��V@��@��@���@��B@��T@��<@���@���@��L@���@���@��@���@��1@��@���@��/@���@��Y@���@��@���@��.@��C@���@��@��$@���@���@��&@��@��J@��H@��i@��d@��@���@��f@���@��?@���@���@���@��J@���@��(@��V@��a@���@�ǒ@���@�̶@���@��}@�э@�Ԩ@���@��"@�۶@��N@��n@���@�� @���@��@��@��@�e@�@��@�z@�S@�_@�$@� 
@�+�@�-�@�5�@�6N@�?<@�H+@�N�@�X@�` @�d�@�lJ@�v@�{@�|^@�z�@��@���@���@���@���@��4@���@��W@�Ҝ@���@��!@�۶@��@�@��?@���@�ߕ@�ߥ@��?@�ݿ@��m@�݃@��@��4@���@�݃@�ݘ@���@��-@��,@��k@�݆@��@��@��@���@���@���@��G@��@��.@��2@�ݯ@��F@��@��@��@��@��@���@��@��@���@��@���@���@��j@��@��@��@��@��>@��@��@��E@���@���@��F@��I@��@��@���@��I@��q@��1@��@���@��4@��@��1@��@��@��Y@��m@��n@��@��R@��@��@���@���@��@���@��-@��&@��@��U@��@��@���@���@��@��@��6@��@��@��@���@��@���@��@��@��c@��e@���@��@��6@���@���@��'@�� @���@��@��@���@��R@��@��@���@���@��@��S@���@��@��'@��*@��@��}@��~@��@��N@��7@��w@��@��z@��N@��H@���@��C@��	@��@��n@��a@��@��@��J@��^@��@��@��@���@��@���@���@���@���@���@��3@��3@��q@��J@��6@���@��@��@���@��@��@��@��.@��.@��@���@���@���@���@���@��U@��*@��1@��*@��j@���@���@��L@��@��J@��6@��@��@��v@��y@��C@��I@�� @���@��H@���@��@��@��O@��6@��@���@���@��.@��@���@��n@���@���@���@��@��;@��"@��J@��w@��t@��7@��t@��"@��K@��@��4@��@���@���@���@��@��7@��&@��t@��q@��`@��@���@��@��H@��-@��@��@��@��^@��r@��E@���@���@���@���@���@���@���@��@���@���@���@���@���@���@���@���@��@��@��T@��+@��.@���@��A@���@���@��n@��j@���@���@��@���@��H@��@��@��C@��\@���@���@���@���@��@��.@��.@��.@��B@��D@��U@��n@��j@���@���@���@���@���@��@���@���@���@��@���@��@��@���@��g@���@��=@���@���@��R@��P@��W@���@���@���@���@���@���@��@��&@�� @��L@��|@��z@��z@���@���@���@���@���@���@���@���@��	@��@��7@��@��4@��@��a@��2@��s@��s@���@���@���@���@���@���@���@���@���@���@��@��@��/@��[@���@��@���@���@���@��@��@��+@��@���@��@���@���@���@���@���@���@���@���@���@���@���@���@��]@��r@���@���@���@��@���@��@���@��/@���@���@���@���@���@���@��@��/@��B@���@���@���@���@���@���@��B@��T@��V@��~@���@��~@���@���@���@���@���@��@���@���@���@���@���@���@��@��@��z@��@��A@��S@��:@��P@���@���@���@���@���@���@��@���@��>@���@��U@���@���@���@���@���@���@��B@��V@��A@���@���@���@��W@��R@��U@��B@��T@���@��A@���@��T@��@��Y@���@���@���@��@���@���@���@��^@��!@��a@��@��@��<@���@���@��@���@��P@��X@���@��@���@���@��W@���@���@���@��@��K@��@��w@��R@��@��N@��N@��N@��@��c@���@��@��w@��@���@��@��@��&@��@���@��1@���@���@���@��u@��Z@��,@�� @���@���@���@��r@��L@��@���@���@��N@��@���@��x@��K@��
@���@��@��@��f@��$@�߶@��{@��>@��@��9@���@��L@��c@��T@��k@��'@���@�Ϯ@��2@���@���@��@���@���@���@��_@���@���@��@��@��/@��
@���@���@���@�n+@�i�@�[�@�V@�J�@�[@���@�ΰ@��:@��s@��@��6@���@���@��.@��@��@���@��/@��s@��@���@��o@��-@��/@��+@��@���@��@��@��|@��S@���@��@���@��R@��:@��
@���@��#@���@��V@���@�� @���@��>@��@��>@��@��@��@���@��>@��z@��T@��:@��Q@��~@��~@���@��V@���@��A@��Q@��U@���@��B@��U@��~@���@���@��S@��@@��@���@��(@���@���@��@��@��o@��J@��\@��\@��@�� @��
@���@���@��@��@���@���@��r@���@���@��c@��9@���@��9@��8@���@��8@���@���@��>@���@���@���@���@��v@���@���@��@���@��~@��@���@�rw@�c�@�[@�Oh@�<�@��@��@��l@���@�~@@�}+@�{t@�v8@�q�@�g�@�IS@�(�@�V@���@���@��.@��@�� @��@��@���@��'@�õ@��?@���@��]@���@���@���@���@���@��n@���@���@�~g@�}l@�u�@�o�@�m�@�`�@�X%@�Up@�N�@�I@�D�@�>�@�<7@�7@�3�@�9W@�>X@�>�@�>�@�?�@�@:@�A�@�>�@�;z@�7�@�5�@�3�@�3@�2"@�0�@�/0@�,�@�,�@�+�@�.2@�0�@�+�@�&�@�#�@��@��@��@�3@��@��@��@��@�	F@��@���@� @�B@���@��@��j@���@��@���@��@��V@��<@��@���@��@��@��{@��@���@�ѹ@��C@�q@�-�@���@�a�@�D�@�6@�@��"@���@���@�� @���@�w�@�n�@�iZ@�\(@�XO@�V@�QW@�M�@�G�@�>�@�6&@�0Z@�!@��@��@���@��^@���@��u@��	@��@�q@�dr@�_�@�]�@�Z�@�\�@�U�@�U�@�O�@�G@�Dg@�:�@�,�@�&�@�#�@��@�@��@�"@��@�	�@�Y@� @��@���@���@��<@��@��`@���@�݆@�҆@���@�ˁ@���@�ƪ@��a@��@���@���@��@��@��
@��
@���@���@���@���@��Z@��y@���@��D@��@��r@���@��:@���@�|�@�u�@�wr@�v�@�o�@�l;@�m�@�k�@�i�@�ez@�b�@�\�@�_Z@�P@�J@�J_@�K�@�K�@�D@�;=@�6~@�2�@�0+@�,@�'#@�!@�@�i@���@��P@��Q@��Y@��@���@��,@��6@�d�@�F@�6�@�,f@�*�@�' @� J@�@��Z@��f@��@��B@��@��#@��2@�β@�Ǧ@���@��@���@��)@���@���@���@���@��y@���@���@��E@���@���@��@��F@��v@��Y@��$@���@�y�@�l�@�k�@�h@�h1@�dr@�a~@�]�@�[�@�X�@�Vo@�Q�@�Ni@�H@�@d@�8�@�.�@�#�@��@���@���@��|@��9@���@�f�@�Ot@�<�@�!@�
@��@��@�Ɩ@��{@���@��@���@�r�@�O"@�1|@���@���@���@��"@��@��+@��E@���@���@��@��@���@��
@��Z@�ɘ@��G@��j@��"@���@���@���@��@��f@���@���@���@��h@���@���@���@��@���@��9@���@���@��@���@�@�{�@�x�@�uy@�r]@�o�@�j�@�f�@�d�@�b#@�_�@�^x@�]:@�]#@�\@�[X@�Z�@�Y�@�V\@�L@�< @�3@�-�@�(�@� �@�R@��@���@��}@���@��@�׊@�ұ@���@��+@��t@�r�@�m@�iF@�h@�f@�e�@�d�@�c�@�c�@�a�@�_E@�[�@�Y@�ze@�|�@�~�@��I@���@���@���@���@��@��T@��@��i@��
@���@��2@��V@��@��@���@��B@��T@��<@���@���@��L@���@���@��@���@��1@��@���@��/@���@��Y@���@��@���@��.@��C@���@��@��$@���@���@��&@��@��J@��H@��i@��d@��@���@��f@���@��?@���@���@���@��J@���@��(@��V@��a@���@�ǒ@���@�̶@���@��}@�э@�Ԩ@���@��"@�۶@��N@��n@���@�� @���@��@��@��@�e@�@��@�z@�S@�_@�$@� 
@�+�@�-�@�5�@�6N@�?<@�H+@�N�@�X@�` @�d�@�lJ@�v@�{@�|^@�z�@��@���@���@���@���@��4@���@��W@�Ҝ@���@��!@�۶@��@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                444344444443343344443344334334433444444444444444443444444444444433433444334434444344433433443344344444434444443443444443444433443444443444343344433444433443334434444433444433444443443433444334434334343443333443333343433333333333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�:h�	:h�6:h��:h��:h�	:h�a:h��:h�:h��:h��:h�f:h�:h�.:h�N:h��:h��:h�C:h�:h�W:h�[:h��:h��:h��:h��:h�:h�[:h�6:h�;:h�L:h�U:h��:h��:h��:h��:h��:h��:h��:h��:h�2:h��:h��:h�.:h��:h��:h��:h��:h�':h��:h��:h�:h�T:h��:h��:h�U:h�Y:h��:h��:h��:h�Y:h��:h�::h��:h��:h�>:h�:h�::h�:h�:h�n:h��:h��:h�g:h�e:h�l:h�g:h�:h�2:h�k:h�2:h��:h�|:h�
:h��:h�:h�:h�[:h�e:h��:h��:h�2:h��:h��:h��:h��:h��:h�/:h��:h��:h�n:h�p:h��:h��:h�2:h��:h��:h�:h��:h��:h��:h��:h��:h�I:h��:h�@:h��:h��:h��:h�K:h��:h�F:h�a:h�e:h�>:h��:h��:h��:h��:h�v:h��:h�:h��:h��:h�.:h��:h�(:h��:h��:h�`:h��:h�3:h��:h��:h��:h��:h�i:h�?:h��:h�h:h��:h��:h��:h��:h��:h�:h�:h�d:h�1:h�:h��:h��:h��:h�:h�(:h�(:h�@:h�]:h�]:h�<:h��:h�
:h�:h��:h�:h��:h��:h��:h��:h��:h��:h��:h�%:h�|:h�t:h�Y:h�|:h�h:h��:h��:h��:h��:h��:h��:h�q:h�T:h�:h�":h�z:h�Y:h��:h�:h��:h��:h��:h��:h�D:h�g:h�L:h�:h�/:h��:h��:h�:h�B:h�>:h��:h�>:h��:h�:h�]:h��:h�W:h��:h��:h��:h�_:h��:h��:h�>:h�::h�$:h�[:h��:h�:h�U:h�2:h�:h�:h�:h�r:h��:h�Q:h��:h��:h��:h��:h��:h��:h��:h�:h�/:h��:h��:h�/:h��:h�:h�:h�:h�J:h�d:h��:h��:h��:h�:h��:h�:h�':h��:h��:h�:h�!:h�N:h��:h�U:h��:h�:h�O:h�o:h��:h��:h�3:h�2:h�k:h��:h��:h��:h��:h��:h��:h��:h��:h�:h�W:h�+:h�a:h�a:h��:h��:h�g:h�[:h��:h�v:h��:h��:h�a:h�:h�_:h��:h�T:h�S:h�:h�:h�
:h�T:h�r:h�X:h��:h��:h��:h��:h�:h�:h�L:h��:h��:h��:h��:h��:h��:h��:h��:h��:h�+:h�':h�E:h�H:h��:h�I:h�~:h�D:h��:h�{:h��:h��:h�:h�W:h�:h�W:h�#:h�:h�X:h�#:h�p:h�:h°:h­:h��:h�:h�O:h��:h�l:hæ:hã:h��:h��:h�:h­:h�^:h°:h�]:h�w:h�w:h�w:h�s:h�T:h�w:h�:h�	:h�	:h�:h�:h�:h� :h�z:hã:h�T:h��:hĄ:h�:h�z:h��:hâ:h��:hĄ:hĠ:h�t:hã:h��:h�:h�1:hĜ:hĄ:hĜ:h��:h��:hĜ:h�1:h�I:h�L:hĀ:hě:hĀ:h��:h��:hĠ:h��:h��:h�H:h��:h��:h��:h��:h�%:h��:h�-:h�>:h��:h�H:hŁ:hŘ:h�x:hŔ:h��:h�:h�:h�T:h�;:h��:h�F:h��:h�}:hĄ:h�J:hĜ:h��:h��:hĘ:hĘ:hĘ:h�1:h�L:h�0:h�l:hé:hĆ:h�M:h�F:h�J:h�1:h�I:hĄ:h�0:hġ:h�I:h��:h��:h�w:h�w:h�w:h©:h�w:h�]:h�]:h��:h�e:h��:h�a:h�]:h�7:h�a:h�F:h��:h��:h�R:h��:h��:h��:h�:h�A:h��:h��:h��:h�u:h�]:h�:h�:h��:h��:h�:h��:h��:h��:h�:h��:h�4:h�i:h��:h�:h�~:h�H:h�:h��:h�!:h��:h��:h��:h��:h�:h��:h�*:h��:h��:h��:h��:h�o:h��:h�%:h�):h��:h�T:h��:h��:h��:h��:h��:h��:h��:h�Y:h��:h��:h��:h��:h�X:h�Y:h�:h��:h�h:h�y:h�G:h��:h��:h��:h��:h��:h�Z:hwb:hs�:hq:hq�:ho0:h`�:h\q:hO�:hG�:h5�:h1�:h0�:h/J:h,f:h)u:h%�:h
�:h�:g�f:g��:g�T:g�$:ga�:g8�:g!�:g�:g�:g:g+:g�:g:g�:g�:g:ge:gn:gD:g�:gh:gb:ge:g`:g~:gC:gz:gz:g:g�:gv:g�:gW:g4:g:g�:g�:g�:g�:g�:g�:g�:go:g�:gy:g�:g�:g�:gz:gv:g�:g:g�:g�:g�:g:g:gt:g�:go:g�:g�:g�:g8:g�:g�:g:go:g4:g�:g�:g�:g?:g�:g@:g�:gH:g�:gh:g8:gO:gO:g�:g :g�:g�:g�:g�:g�:g�:g�:g:g::g7:g:g�:g\:g�:g�:g*:g:g.:g-:g6:gD:g�:g	�:g�:gK:g�:g 3:f��:f�\:f��:f�:f� :f��:f�f:f��:f�{:fx�:fI�:f6:e�:e�D:e~f:e|�:ez�:es�:em�:ea:e8�:e�:d�x:d��:d��:d�I:d��:d�S:d�_:d��:d�_:d��:d�:dy�:d]�:dV�:dP�:dC�:d<�:d9%:d7~:d34:d1B:d1%:d-�:d,�:d"e:d�:dE:d:c��:c�:c�6:c��:c��:c�:c��:c�:c��:c�:c٨:c��:c�4:c��:c�":c�+:c�:c��:c��:c�:c��:c��:cə:cǪ:cŹ:cµ:c:c�:c�k:c��:c�j:c��:c�o:c��:c��:c��:c��:c��:c�':c�}:c�L:c��:c�::c��:c��:c�C:c�@:c};:c|_:cx�:cv�:crw:cq`:cu�:c|":cyT:cx�:cz�:c}8:cu�:cn?:cc�:cJ�:cE:b˥:br�:aڣ:af�:a@�:a-f:an:`�z:`�i:`_�:`G$:`?:`2�:`']:` %:`�:`	�:`�:` �:_��:_�1:_�::_��:_�.:_�:_��:_�m:_u�:_f�:_F�:_�:^��:^��:^ٓ:^�:^�:^�C:^�{:^��:^�u:^��:^�:^�k:^��:^�B:^�:^w�:^t1:^hZ:^d�:^eA:^\�:^Y�:^Q�:^J�:^D�:^BR:^:?:^4�:^-�:^&O:^!�:^�:^�:^	:^�:]��:]��:]�}:]�{:]�:]�:]�E:]ߣ:]��:]�x:]�x:]�:]�:]�`:]��:]ɻ:]ȓ:]��:]�[:]�R:]��:]��:]�_:]�\:]��:]�@:]�M:]�8:]�/:]��:]�C:]�8:]~�:]y�:]va:]nG:]q�:]]u:]U�:]V:]W�:]W�:]M�:]B:];�:]7:]3�:].:]'�:]�:]�:\�R:\��:\�[:\�:\��:\�u:\�:\��:\k�:\'�:[��:[�c:[��:[��:[��:[��:[��:[�X:[��:[�:[}�:[u�:[lt:[gE:[b�:[Yf:[Q�:[J1:[B�:[?�:[7�:[/#:[)�:[$�:[ �:[�:[�:[�:[�:[v:[
�:[�:[�:[�:[ �:Z��:Z�:Z��:Z�:Z��:Z��:Z��:Z�:Z�-:Z˙:Zǝ:ZĀ:Z�t:Z��:Z��:Z��:Z�C:Z�v:Z��:ZjO:ZL':Z(�:Z:Y�:Y��:Y�":Yj�:YQ�:Y-�:YM:X�:X�::X��:X�b:X��:X|:Xj9:XH�:X�:W�:W��:W�:W�:W�]:W��:W�:W��:W�1:W��:W�s:W��:W}�:Wz@:Wv�:Wi�:Wb�:W_ :WZ�:WX�:WW�:WV�:WUj:WT�:WS�:WO�:WMT:WL�:WL(:WI#:WB�:W<�:W5�:W.6:W&�:W%:Wm:W�:W�:WT:V��:V�?:V�(:V�:V��:V��:V�:V��:V�{:V��:V�\:V�=:V��:V��:V�:V֜:V�S:Vĺ:V��:V��:V�2:V�:V��:V{y:Ve�:VR5:VC#:V5:V-u:V+�:V%(:V�:U�d:U��:U��:U�x:U��:U��:U�6:U��:U��:U�:U�:U�u:U�Z:U��:U�/:>�:>{:>
:>S:>.:>3:>:> �:>(�:>)Q:>*K:>*�:>,�:>)�:>-:>.�:>;j:>?I:>A�:>B6:>BN:>C:>D*:>DY:>D�:>EQ:>E2:>E�:>F�:>F:>G':>F�:>G`:>F�:>G�:>H@:>H�:>I3:>H�:>Gz:>G�:>H�:>I�:>Ii:>I�:>KF:>Lm:>Kv:>L�:>O�:>P�:>S :>U�:>V :>U.:>T�:>Vt:>ZV:>^l:>_0:>`�:>a�:>a�:>]�:>e:>j:>n�:>p�:>rt:>u�:>w5:>{J:>|�:>~�:>��:>��:>��:>��:>�O:>�6:>��:>�9:>��:>��:>��:>ϒ:>��:>�I:>��:>֭:>�o:>�':>��:>�:>��:?v:?6:?%:?(:?2�:?8�:?B�:?O�:?V8:?W�:?V:?\[:?d:?{�:?�:?�F:?��:?��:?��:?�J:?��:?��:?�B:?�O:@eG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bk�B�#B�)B�)B�#B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�#B�#B�#B�#B�#B�#B�)B�)B�)B�)B
�wB�5B�;B�TB�BI�BL�BN�BT�B7LB0!B0!B%�B�B+B��BBPBPBB��B��BBBB��B�B�NB�B��B��B��B��B��B�/BɺB�9B��B�Bs�Bl�BT�BB�B0!B"�B/B;dBF�Bq�B��B�B��BiyB>wB%B��B�B��B�Bq�B�^BdZB`BB^5BYBR�B>wB2-B�BuB+BB
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A!>4?��?�#�B�/?¼�>�V?�>��>���A?�?��A��?A�&�?�w�B3�B��>�S	?^�xA4s�?���B.B1�?Z&�AO�B05B6?a�B/PB&�@'�?%�uA��4B�>�G�? g�A�'>��?_5?�VXA">�M�>�ם>�Ġ?� ?x�@JZ>�ȟ>׻Q?% ?�~�B*Q?�=?��B@ξ>���>��?�k�>��d>�`�?K@�?;]?���?��?y}[B,[B��?2��A���Bw�>��	>�*�?9��B!JB<�?�<?Y �Bv�?M+?��/@̜\?�łB��?�?]�@"`B3B�#?~B��B��?��f?�PjB&�B/�@�=�@��NBi�@���?�A
bU>݂V?*�z?�F�B/(A
?��@�wG?�'?��@^tA�8?!lA��~B/ ?�?L?O>��? �q?9�A���>��>���>��t?06eA��eB7D?�V�?v��B"MAk�`?���@���?�WdAi�B��?���?\1?�)NB�}@��B+oB8�>��+?�8AV��B*B�>ȫ�?�?	��?k0@B2A�>��v?��B(�B*�B5n@��?t�]B��?�_>��@��+?�~@���B,�B_�?�A�>�;?��B/:B<3>��4?~�@�H?	�k@�TRB3�A<sg@_V&A�x1?=�B�[B4K@���?~9Aq�B0TA���? z?��7BA�?���B4vB;�?X
BXP?�8B=3?��r?=�SB4�B6tB6OB
�?/<@�2B6�B6�B;AB6�B7A_(�B6�@?�vB6]B5�B5�B7PB7�B7&B73B6�B6jB7aB7�B9�B3�B6GB5�B6+B7B7iB8�B5�B6aB5�B6�B7wB9B:BB7�B7+B6�B5�B6�B7�B6�B5yB7�B3�B7B7�B8�B6kB6B6B6B7BB93A��B7OB7�B7nB6�B5�B6gB5�B7�B8B7B8�B5�B7�B8�B:�B6BB8jB8�B:cB4�B8�B:cB7tB8%B8B<�B7B5yB8\B5�B6�B4�B9�B6>B5�B74B7<B7�B6fB6>B6�B5�B7�B7{B7B6�B6'B9RBC�B5�B6>B5�B5�B4�B5�B74B<3B6FB5
B6�B6 B6^B4�B4�B8~B8�B:B5�B6�B8�B6'B=IB6�A5�&B6'B6kB5,B5�B7tA�pB7<B6VB6�B5�B7kB5qB5�B6YB62B�8B8B6#B5QB5yB4�B7"B7tB6B7A�T�A���B7B6�B7�B7B7B7�B:gB96B8�B6�B9B88B8B8@B70B9�B;$B7�B7B6;B5�B=�@�o�B9�B9�B?�B7�B7�B6�B7�B:�B:�B6B6cB6B6TB6YB9�B8�B:B7�B7�B6�B6�B5�BDIB5cB5�B5�B8%B7\B8{B6�B7�B8{B7TB6VB74B7\B6�B8�B8�B9B8EB8�B7B8�B6FB6�B8�B7�B6�B5�B61B8"B6�B8"B6�B87B8"B8\B7<B6sB87B:^B9�B7BB=B6�B5�B7B7�B|gB5�B4�B6�B7�B6�B7�B5hB6�B6@B6B8�B6B5�B7B7B71B5�B6�B7'B8{B6kB9�B6B7B7�B;�B7�B8�B7�B8*B3NB2]B6�B<�B7B6B7�B7�B7�B7aB7TB7YB7�B6�B6rB5�B6�B5�B5�B5�B5�B6�B5�B6FB7B7^B6�B7VB6�B7�B7�B7(B7(B6�B6�B5�B6B5/B5�B6�B6yB6B8B6B6�B6�B7`B7wB8�B8	B8VB8�B6�B7B64B6JB6zB7;B6�B6�B70B7B6/B6'B6B76B7�B7B6�B64B7SB6�B6LB6DB6�B6�B5�B5�B6)B4�B6nB6fB6cB5�B7�B6�B6ZB7gB7WB7�B6�B6�B6wB7B7(B7kB7�B7B7^B8B7NB7>B8]B7�B6\B7{B6�B6�B75B7-B7KB7�B7�B7�B7B7YB7�B/�B56B7�B7�B8?B7nB7FB76B7�B7�B7NB7LB6�B6�B6�B7�B7�B7�B7"B7�B6�B9$B7,B7�B7B6�B79B7wB7B7ZB6�B7�B7wB7B6�B76B7�B7|B7tB7dB6�B6�B7�B7B8SB6�B7yB6�B6�B8B7�B7_B7oB6�B6mB7/B6}B7IB6�B6�B7�B7 B7B7SB8B7�B85B7[B7�B8XB7`B7PB7HB8hB7�B7�B8:B7nB7fB7UB7�B7�B7�B6�B7B7�B7�B7�B7�B7�B8�B8BB8�B7\B7�B8�B8PB8@B7�B8"B7�B8�B7�B7�B7zB7�B7bB7�B7�B7�B7�B8=B6eB7�B8=B85B8B9�B7B7�B8qB7�B7$B8�B7�B7UB7EB7�B7B6�B7�B7KB7�B7�B6�B6�B6�B7�B7xB6�B6�B78B6�B7�B8 B8;B7jB7B8B8iB7�B7%B7�B7B7oB7�B8B8B7�B7�B7xB7pB7hB6�B8B6~B77B7�B7�B6�B8#B7rB7iB8B7�B8B7�B7�B7�B7�B7B7�B7�B8B6�B7�B7�B7�B7�B7�B8BB8:B8�B9�B8B7�B8�B8�B7�B7�B7�B8�B7�B8*B8�B8B8uB8B94B7�B8ZB8?B9 B8�B8�B8B7�B8mB8�B8]B8UB9B8DB8�B8B8oB8gB8_B9 B9vB9nB8�B8�B8�B8�B99B9�B9)B8�B9B9B9�B9B9_B9VB9B9FB8uB9�B9�B9�B9�B:�B9lB9�B9�B9�B9,B:B9�B9.B9�B9B9�B9�B9qB:B8xB:
B99B91B9�B9!B9�B:�B:�B:�B9_B9_B: B9�B9�B9fB:B:kB9�B:�B:�B9&B:RB9>B:JB9�B9yB:�B9qB9�B;aB:�B:�B:�B:B:B;<B:�B:kB;4B;4B;4B:cB:cB;$B:[B;$B:�B;zB:�B;�B:�B:�B;B:�B:�B;�B;�B;�B;�B;ZB;�B:�B;�B:�B;�B;�B;JB:�B:xB;AB:�B:�B:pB:B:B;9B:
B:�B9�B8�B:mB:�B:!B9�B:B8�B9HB9
B8�B7�B9mB9B8�B8RB8�B6�B6&B5�B5�B4�B4�B4B3B3-B1TB2�B2JB0B0qB1�B1-B2:B1�B0�B1*B0�B1�B1�B1�B2iB1�B02B/*B0^B//B/�B1B2KB0�B1iB1�B0AB/9B/1B09B1`B2�B1eB.�B0UB1>B0VB-6B,.B+rB*�B*�B*B)�B(�B)�B*�B*�B*2B+�B-�B)�B'�B&�B'B'�B'�B&B&�B&�B'�B)IB$�B"B"hB!�B �B�BB%�B)�B&B AB!�B B�B�B�B�BhBEB�B%B$BDB�BsBB�B�BIB�B�B �BB�BqB9B �B �B(�B+;B�B,�B):B�B_BlB#�B#|B�B�B�BzB�B	�BMB�B�}BB>~BXHBl7BtB}uB�MB��B��B��B��B�	B�=B�&B��B�B��B��B�B�gB�~B�~B�lB�/B��B�^B��B�4B�^B��B�DB�B��B��B�uB� B�wB��B��B��B�QB�	B�~B�AB��B��B�QB��B��B�1B��B�9B��B�1B��B�4B��B�xB��B�(B��B��B�_B�B�JB��B��B�mB��B��B�^B�B�LB�nB��B��B��B�~B��B��B��B��B�HB�B�B�zB��B�^B�lB��B�B��B|�B|9B}(B��B~�B~�Bz�BvYBwnBw Bw�Bs�BpBpBm*Bi8BdCBa�B\7BTfBf�BuuBs|B_�BLB:�B?�BS�BA/BU�B��B~�Bx�BoBB_ABHBQ�BcsBtVB�^B�pB��B�B��B��Bu�BvB��B��B�vB��B��B�TB��B��B��B�tB��B�4B�B��B}�B|�B�B{B|RB�BkB�ZB��B�B�]B��B�cB�3B��B��B��B�SB�CB��B|�B|�B&Bz�B|�B~By]B{6BxXBv�Bx�Bv�Bu�Bk�Bl=Bc�Bc�Bc�Bo�Bl�Bl�Bl�Bl�Bo
BluBk�B�KBz�BpIBmBb�B_�Bb�B_#B^�B_WB_[B^�B_�B[ZBT�BQBK�BE�B;B1uB$�B1B�B��B�WB�UB]B:&B>_B0jB#�BDCBE�BW2Ba]B^EB` B_�B\�Bb�B_�B[�B[BW�BS�BS�BPBIBH�BH�BI@BL`BF�BQuBZB]vB\�Bf�Bh`Bj�BgBf�Bc�Bc>B]RB[�B\gBZcB]BV�BZ�BX�B]$B\bBZ�B[BX�BZ�BY�BXKBW�BVKBV�BV�BV�BT�BSfBR�BWXBV�BU�BU0BT�BTBV0BW�BYNBWuBZBX�BT�BXIBW�BX�BV�BV�BU�BP�BU�BWzBT�BQ�BR�BQqBTBUyBR�BPkBOpBR�BPxBO?BN$BK�BJ�BE�B@�BDBG�BF�BB6B=9B:�B;�B<0B8rB5|B3�B/�B-B,�B2hB3QB/�B1�B2sB3�B4�Be�Bg�B[�BX�BSzBL]BM�B^HB[�BZBf7Bi4Bd�Bj�Bf6BY�BY<BOhBi/BaBk�BX�BeBW�BX�BbB\�Ba�Bf(BR�BQGBK�BS�BW�BkDBg�B_�B\�BZ�BV BY�BgBc2Be BYvBn�BYRBc�BS�BW�BR�BUXBSBT4BY�BX�BV%B_B[�B3,B�B��BGfBfIBkBA�BB�B0'B)<B'�B%�B�B*iB&�B*�BwB$�B#2B-BE�BG�BG�BIbBH�BIeBH�BI�BJmBL�BP�BPRBP{BO�BT�BUBW�BX�BX�BY=BY�BX�BY8BV�BX0BW�BV#BV,BTBT�BT6BUZBSgBT�BV�BW7BWBVBU�BT�BTWBSBT,BU>BUGBTfBS�BSBQ.BPBNBK�BHrBD�BBwBABCBD"BBhB>�B9>B7�B8B;~B<SB<�B;;B7YB0NB(B-�B6aB?�BB B@�BBEBApB@=B>3B>�B<+B86B7�B5�B2B.�BT�BL�BFpBL.BL[BN�BF}BJBFLB:zB4�B8�BNBL|BN�BH{BH�BOTBRCBJxBP�BE�BL�BL?BL�BI�BK@BMRBI�BLVBJ@BHJBFxBI�BI�BJ�BF�BHUBH�BJqBI�BI�BI{BMBN�BTUBUWBT�BNBT�BRbBW�BMBVBBKBO�BS�BRBW�BV�BY�B\�BT�BY�BS�BWzB^�Be~BW�B\!BW�BT�BX�BUdB[�BO�BT�BkBl�Bm�Bc�BM�Bq�B�0B�^B��B��B}IB{6B��B�LB��B��B~�Bp�B��B��B��B�SB��B��B�xB��B�B��B�(B��B�%B��B��B��B��B��B�=B��BɺB�/B�XB�.B��B�vB�'B��BܞB�B�dB�B�BۭB۸B�,BܲB��B�BےB�QB�zBۣB�B��BڿB�'B�B�eBۆB۷B�=B�5B�SB�hBۑBہB��BێBۆBۉB�5B�fBێBگB�B�6B��B�]BۆB�~B�BۋB�^BۤB��BۅB�<B�B��B�IB�B�1BۥB��BۂB�B��B�bB�B�B�/BܶB�B�*B�mB��B��B��BܐBۈBۋB۩B�{B۫B�jBܹB�xB��B��B�B�2B�DB�4BۊB�$BܘBܣB��B�mB��B�zB��B�XB�HBڼB�B�B܍BܗB��B��B�\B��B܅B�DB��B�dB��B�B�EB�!B�eB��B�gB�1B�bB�B�\BۍB�WB�B�mB�$B�TB�EB�1B�DBۊB��B۫BۣB��BۻB��B�'B�PB�[B�B��BۑB��B��B�pBۡBۑBۯBیB��B��B��B��BۧB�SB��B��B� B�cB�SB�%B�iB�(B�B��B��B܇BۥBۯB� B��B�B��B�
B�XBۜB��B�"B��B��B�B܅B��B�#B��B��BۙBۆBیB�SB��B�3B�dB�ABۄB��B��B�B��B��B�%B�B��BۮBۛB�-B��B�JB�`B�XB�B�@B۬B�B�2B۞B�B�0B��B�B۱B�^B�;B�~B�nB�TB�iBۚB�B�+B�B��BۚBےB�B�B۪B�DB�B�!B��B��B��BۥB�,B��B۲B��B��BےBەBۍBۅB��B��B��B��B��B�EBۛB�5B�"B��B��B۶B��B�#B�CB�(BۯBۺB��B�B�^B�VBۇB�BۜBۧB۟BۗBۢBےBۜBۧB�gB܄B��B��B�B��B�B��B܄B�|B��B۸B�B��BۍB�B�<B��B�GB�B��B܅B�}BܭBܸB��B��B��B�	B�'B�2B�)B�GB�eB�]B�]B�hB�_B�}B܈B�mB۱B��B��BܘB��BܮB۹BܞB�pBܻB��B��BܶB��B�=B�B�-B��B��B�B��B��BܦB�5B�nB�@B�]B܎B��BܙBܶBܮB��BݦB��BܴB�|BܬB�tB�~B�wB�wB�nB�\B�gB�B��B��B�B�B�|B݆B�B�B��B�NB�kB�B�B�wB��B��BݔBݦB��B��B�B�B�KBݖB݄BݏBݴBݴBݏB�B�B��B�3B�FB�3BݯBݯB�vBݯB�nB��B�fB݌B�SBݟBݼB�^BބB��B�^BގB��B޿B��B��B�iB�NB�NBݬBݑB� B�vB� B��B��BތB��B�.B�.B��B��B��B�iBݭB�bBޥB�B��BݥB�lB�dB�BݝBފB�IBޕBݕB�	B�:BޣB��BޛB��BޛBހB�GB��B�]B��B��B�UBއBߝB߂B�YB�(BމB�B�zB�.BޅBުB�B�"B�nB�B��B޼BߙB߆BޚBߑBޒB�YB�QB��B�B߲B�B�oB޲B��BߗB��B�B��B�RBߖB�]B�JB��B߬B��B޾BޙB�2B�2B݉B��B��B�B��BްB�YB�B��B�@B�SB�eB�SB�B�8B�B�B�vB�$B�oB��B�IB�bB��B��BߒB�RBޝB�`B��B�wB��B�B�]B�B�cB�CB�sB�B�B��B��B�B�jB�B�FB�B�\B�'B�B��B��BwB�BYB%B�BCB!�B'�B+kB+�B=�B?BBMB@"B=BBbB<UB9�B>wBGBH�BJ&BJ BJBJBJ�BJbBJ�BK�BJ�BJlBK�BJ�BK|BJ�BKtBK�BJ�BJ�BKQBJ�BKgBK�BJ�BJ�BKWBM�BK�BK�BK�BLABMBM;BM;BK�BL<BK�BKWBL3BKbBJ�BKGBKZBKxBK�BK�BM�BNTBN&BM�BMgBM�BM�BM�BN�BN�BN�BM�BN�BNnBM�BO/BN8BNBBM�BN`BNBNBM�BM�BN�BM�BN0BM�BM_BMVBM�BM�BL�BL!BL7BMBM�BMBMJBM�BN�BN4BP�BT�BWHBWGB[�Bg�Bm0BlJBk�BlBg�B]�B^�BR�BM�BMaBL�BMJBM!BL�BL�BH�BF�BD�B@�B@�B@ B?GB>�B>KB?-B:�B;bB<�B7OB7B5_B5�B3�B3�B3}B4kB4�B5VB4�B5JB4wB37B5MB3�B1B1�B0_B/�B.4B.<B.\B,�B)�B,mB.�B/�B.�B0	B1B2tB1&B1dB1B0�B/�B0�B0�B0B/yB/�B/�B.�B0�B3CB2�B1�B1TB1B1�B1�B1�B1B0NB0B/EB/6B.�B/B.�B1�B0�B/(B/YB.XB.xB-�B-!B-�B0�B/vB/B09B2�B3OB1B0�B0 B.�B*�B+)B*�B&�B#�B$�B3�B$�B(/B$�B#�B"�B"�B!B#B"�B"iB"�B"NB"B"PB"�B!�B!�B"�B"}B"�B"uB&�B)	B$2B!(B!&B�B�B�B�B�BkB�B	BBtBB,BxB�B�BaBBNBBBB,B\B�BBB
uB
BB]B]BBUB+B�B�B�B<B�BLBIB+B�B�B�B�B�B�BaB�B}B
DB~BvB�BMB7B�BuBhBABBB�B�B�BBXB`B�+B%B B��B�B�qB�B��B�fB��B��B��B��B�B��B��B��B��B�jB��B�B �B�B tB_B�[B�DB |B��B��B��BB�B(B�B�B�B
B
�B
VB
5B�B
�BBvB
�B�B_B�B%B$B�BlBWB,B�B�B�B�B0B�BlB�B�B�B4B7B�B�B�B�B/BUB/B�B�B�B�B9BB�B
iB�BPB9B�B��B��B�B��B��B��B�0B�B�_B�|B�"B�B�B��B��B�
B�vB�7B�B�XB�{B�0B�-B��B�vB��B�&B�B��B��B�SB��B �B �B �B^BIBhB{B�B�B BeB�B�B�B�B#BBzB�B�B$B6B�B�B,B�B�BaB�B�B�B�B�B�B�B�B0B�B�B?B!B��B��B��B��B��B��B�?B��B��B�?B�BB��B��B�GB�MB��B�B�HB��B�wB�1B�B�B�B�!B�B��B�eB
��B
�@B
�?B
��B
�aB
��B
�'B
�xB
�JB
��B
��B
�'B
��B
��B
�UB
��B
��B
�B
�bB
�	B
�B
�UB
�B
�\B
��B
�;B
�B
� B
�zB
�*B
�B
��B
��B
��B
��B
�[B
��B
�DB
��B
��B
�\B
�B
�fB
��B
�-B
��B
��B
��B
�^B
��B
�4B
�AB iB �BB
��B
��B
�vB �B
��B |B$B �B �B|B<BYB�B"BOB BB�BBB�B\BlB�B	�B	"BB�B�BBiBsB�B�BB�BB�BB�BIB�B�BB�B�B�B�B�B�B!�B�B!VB �B#lB#B$�B)�B"�B*�B.�B+/B.B0�B+�B	�9G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�422222222222222222222222242222222222222222222222222222222222222222222222222222222222242222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999444344444443343344443344334334433444444444444444443444444444444433433444334434444344433433443344344444434444443443444443444433443444443444343344433444433443334434444433444433444443443433444334434334343443333443333343433333333333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�B�%B�,B�*B�#B�,B�*B�*B�*B�,B�'B�*B�*B�*B�*B�&B�&B�&B�&B�&B�)B�,B�*B�,B�,G�O�B�8B�>B�UB�BI�BL�BN�BU B7NB0%B0B%�B�B-B��BBRBRBB��B��BBBB��B�B�QB�B��B��B��B��B� B�/BɺB�;B��B�	Bs�Bl�BT�BB�B0"B"�B/B;eBF�Bq�B��B�B��BiyB>vB$B�B�B��B�Bq�G�O�Bd]B`CB^6BYBR�B>vB2.B�ByB0B
B
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��@A�&�G�O�B3�B��G�O�G�O�G�O�G�O�B.B1�G�O�G�O�B06B9G�O�B/SB&�G�O�G�O�A��6B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B*QG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B,]B��G�O�A���Bw�G�O�G�O�G�O�B!NB<�G�O�G�O�Bv�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�B3B�%G�O�B��B��G�O�G�O�B&�B/�G�O�G�O�Bi�G�O�G�O�G�O�G�O�G�O�G�O�B/*G�O�G�O�G�O�G�O�G�O�G�O�A�7G�O�G�O�B/G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�A��jB7GG�O�G�O�B"RG�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�B��G�O�B+qB8�G�O�G�O�G�O�B*B�G�O�G�O�G�O�G�O�B2�A�	G�O�G�O�B(�B*�B5nG�O�G�O�B��G�O�G�O�G�O�G�O�G�O�B,�B_�G�O�G�O�G�O�G�O�B/>B<6G�O�G�O�G�O�G�O�G�O�B3�G�O�G�O�A�x5G�O�B�]B4KG�O�G�O�G�O�B0WA���G�O�G�O�BA�G�O�B4sB;�G�O�BXRG�O�B=2G�O�G�O�B4�B6uB6OB
�G�O�G�O�B6�B6�B;AB6�B7G�O�B6�G�O�B6\B5�B6B7OB7�B7&B74B6�B6iB7cB7�B9�B3�B6JB5�B6-B7B7kB8�B5�B6dB5�B6�B7zB9B:DB7�B7*B6�B5�B6�B7�B6�B5|B7�B3�B7B7�B8�B6nB6B6B6B7CB95G�O�B7SB7�B7pB6�B5�B6fB5�B7�B8
B7B8�B5�B7�B8�B:�B6DB8kB8�B:eB4�B8�B:eB7tB8)B8B<�B7B5xB8`B5�B6�B4�B9�B6AB5�B78B7=B7�B6fB6AB6�B5�B7�B7}B7B6�B6)B9SBC�B5�B6AB5�B5�B4�B5�B78B<6B6IB5B6�B6B6bB4�B4�B8�B8�B:B5�B6�B8�B6)B=HB6�G�O�B6)B6nB5+B5�B7tA�pB7=B6UB6�B5�B7nB5rB5�B6ZB65B�7B8B6%B5UB5xB4�B7$B7tB6	B7A�T�A���B7B6�B7�B7B7B7�B:iB97B8�B6�B9B8:B8B8DB72B9�B;&B7�B7B6?B5�B=�G�O�B9�B9�B?�B7�B7�B6�B7�B:�B:�B6B6dB6!B6UB6ZB9�B8�B:B7�B7�B6�B6�B5�BDJB5eB5�B5�B8)B7`B8~B6�B7�B8~B7VB6UB78B7`B6�B8�B8�B9B8FB8�B7B8�B6IB6�B8�B7�B6�B5�B63B8$B6�B8$B6�B8:B8$B8`B7=B6rB8:B:`B9�B7BB>B6�B5�B7B7�B|iB5�B4�B6�B7�B6�B7�B5lB6�B6DB6B8�B6B5�B7B7B71B5�B6�B7'B8~B6nB9�B6B7B7�B;�B7�B8�B7�B8,B3PB2_B6�B<�B7B6B7�B7�B7�B7`B7VB7XB7�B6�B6qB6B6�B5�B5�B5�B5�B6�B5�B6EB7B7`B6�B7VB6�B7�B7�B7+B7+B6�B6�B5�B�xB�*B��BܞB�B�dB�B�B۰B۽B�,BܴB��B�BۖB�SB�zBۦB�B��BھB�*B�B�iBۈB۹B�=B�8B�UB�kBےBۄB��BېBۆBیB�8B�gBېBڰB�B�5B��B�`BۈB�B�BێB�aBۦB� BیB�=B�B��B�LB� B�1BۨB��BۄB�B��B�gB�B�B�2BܶB� B�*B�pB��B��B��BܒBیBێB۫B�BۮB�jBܹB�yB��B��B�B�7B�GB�8BۈB�'BܘBܦB��B�lB��B�}B��B�ZB�LBھB�B�B܏BܘB��B��B�`B��B܈B�GB��B�gB��B�B�IB�$B�gB� B�iB�3B�gB�B�`BےB�YB�B�lB�$B�SB�FB�3B�FBیB��B۫BۦB��B۽B��B�'B�QB�[B�B��BۘB��B��B�rBۤBۖB۳BێB��B��B��B��B۫B�UB� B��B�"B�gB�UB�'B�kB�*B�B��B��B܋BۦB۳B�"B��B�"B��B�B�YB۟B��B�"B��B��B�B܅B��B�&B��B�BۜBۈBیB�UB��B�6B�iB�BBۆB��B��B�B��B��B�'BۂB��B۱B۟B�0B��B�LB�dB�ZB�B�BBۯB�B�6B۟B�B�3B��B�B۶B�aB�BBۂB�pB�WB�kBۜB�B�.B�B��B۟BۖB�B�B۬B�FB�B�$B��B��B��B۩B�.B��B۱B��B��BۖBۚBېBۈB��B��B��B��B��B�IBۜB�6B�$B��B��B۶B��B�&B�BB�,B۱B۾B��B�B�aB�WBۈBۂB۟B۫BۡBۚBۤBۖBۜB۫B�gB܅B��B��B�B��B�B��B܋B�}B��B۸B�B��BېB�B�=B��B�IB�B��B܈B܃BܬBܹB��B��B��B�B�*B�5B�*B�HB�kB�aB�aB�gB�^B܀B܋B�mB۱B��B��BܘB��BܱB۾BܠB�oBܾB��B��BܶB��B�=B�B�,B� B��B�B��B��BܨB�8B�pB�CB�aB܏B��BܜBܹBܯB��BݦB��BܶB܀BܯB�wB܀B�yB�yB�oB�\B�kB�B��B��B�B�B݀B݉B�B�B��B�PB�oB�B�	B�yB��B��BݙBݩB��B��B�B�B�MBݙB݇BݎBݱBݱBݎB�B�B��B�4B�FB�4BݱBݱB�{BݱB�oB��B�iBݎB�SBݟBݽB�`BއB��B�_BޒB��B��B��B��B�kB�OB�OBݮBݓB�"B�yB�"B��B��BލB��B�-B�-B��B��B��B�lBݱB�cBިB�B��BݦB�lB�eB�BݝBލB�LBޘBݕB�B�:BަB��BޝB��BޝBނB�JB��B�aB��B��B�UBފBߟB߅B�[B�$BލB�B�{B�/BއBުB�B�$B�oB�	B��B޾BߛB߇BޝBߒBޔB�[B�RB��B�B߶B�B�pB޵B� BߛB��B�B��B�UBߛB�`B�OB��B߰B��B��BޘB�4B�4BݐB��B��B�B��B޳B�[B�B��B�@B�UB�fB�TB�B�8B�B�B�vB�#B�rB��B�KB�dB�B��BߓB�RBޟB�bB��B�zB��B�B�_B�B�eB�CB�vB�B�B��B��B�B�lB��B�EB�B�\B�'B�B��B��BwB�B[B%B�BFB!�B'�B+oB+�B=�B?BBMB@%B=!BBcB<UB9�B>yBG BH�BJ)BJBJ BJ BJ�BJcBJ�BK�BJ�BJoBK�BJ�BK�BJ�BKuBK�BJ�BJ�BKSBJ�BKiBK�BJ�BJ�BKZBM�BK�BK�BK�BL>BMBM>BM>BK�BL>BK�BKZBL3BKdBJ�BKJBK\BKyBK�BK�BM�BNUBN'BM�BMiBM�BM�BM�BN�BN�BN�BM�BN�BNmBM�BO1BN9BNDBM�BN_BN"BN"BM�BM�BN�BM�BN2BM�BMbBMYBM�BM�BL�BLBL9BMBM�BMBMKBM�BN�BN4BP�BT�BWLBWKB[�Bg�Bm3BlMBk�BlBg�B]�B^�BR�BM�BMcBL�BMMBM!BL�BL�BH�BF�BD�B@�B@�B@B?JB>�B>MB?/B:�B;dB<�B7PB7B5aB5�B3�B3�B3}B4mB4�B5YB4�B5KB4zB3:B5PB3�B1B1�B0aB/�B.3B.;B._B,�B)�B,nB.�B/�B.�B0B1B2uB1)B1gB1B0�B/�B0�B0�B0B/yB/�B/�B.�B0�B3CB2�B1�B1VB1B1�B1�B1�B1B0QB0B/GB/9B.�B/B.�B1�B0�B/*B/[B.XB.yB-�B-$B-�B0�B/wB/B0=B2�B3OB1B1 B0"B.�B*�B+-B+ B&�B#�B$�B3�B$�B(1B$�B#�B"�B"�B!B# B"�B"hB"�B"MB"B"QB"�B!�B!�B"�B"�B"�B"qB&�B)B$4B!(B!*B�B�B�B�B�BmB�B
BBuB�B/BzB�B�BdBBRBBDB.B^B�BB B
vB
BB_B]BBTB0B�B�B�B?B�BMBMB-B�B�B�B�B�B�BfB�B�B
DB�BzB�BPB9B�BxBiBDBBB�B�B�BBWBeB�.B'B B��B�B�rB�B��B�iB��B��B��B��B�B��B��B��B��B�mB��B�B �B�B tB`B�^B�FB ~B��B��B��BB�B(B�B�B�B
B
�B
TB
8B�B
�BByB
�B�B_B�B'B&B�BpBYB/B�B�B�B�B3B�BqB�B�B�B6B6B�B�B�B�B/BWB0B�B�B�B�B<BB�B
lB�BSB:B�B��B��B�B��B��B��B�3B�B�cB�|B�"B�B�B��B��B�B�yB�9B�B�ZB�~B�4B�2B��B�uB��B�)B�B��B��B�VB��B �B �B �B]BHBkB}B�B�BBhB�B�B�B�B&BB}B�B�B&B6B�B�B-B B�BcB�B�B�B�B�B�B�B�B0B�B�BBB#B��B��B��B��B��B��B�@B��B��B�?B�DB��B��B�FB�QB��B�B�KB��B�zB�3B��B�B�B� B�B��B�hB
��B
�@B
�BB
��B
�cB
��B
�)B
�wB
�MB
��B
��B
�*B
��B
��B
�WB
��B
��B
�B
�bB
�B
�B
�XB
�	B
�\B
��B
�<B
�B
�B
��B
�*B
�B
��B
��B
��B
��B
�^B
��B
�FB
�B
��B
�^B
�B
�gB
��B
�,B
��B
��B
��B
�`B
��B
�6B
�AB hB �BB
��B
��B
�xB �B
��B �B%B �B �B|BABYB�B"BRB!BB�BGB�BaBnB�B	�B	&B B�B�BBlBuB�B�BB B	B�B
B�BJB�B�BB�B�B�B�B�B�B!�B�B!UB �B#lB#B$�B)�B"�B*�B.�B+1B.B0�B+�B	�;B�xB�*B��BܞB�B�dB�B�B۰B۽B�,BܴB��B�BۖB�SB�zBۦB�B��BھB�*B�B�iBۈB۹B�=B�8B�UB�kBےBۄB��BېBۆBیB�8B�gBېBڰB�B�5B��B�`BۈB�B�BێB�aBۦB� BیB�=B�B��B�LB� B�1BۨB��BۄB�B��B�gB�B�B�2BܶB� B�*B�pB��B��B��BܒBیBێB۫B�BۮB�jBܹB�yB��B��B�B�7B�GB�8BۈB�'BܘBܦB��B�lB��B�}B��B�ZB�LBھB�B�B܏BܘB��B��B�`B��B܈B�GB��B�gB��B�B�IB�$B�gB� B�iB�3B�gB�B�`BےB�YB�B�lB�$B�SB�FB�3B�FBیB��B۫BۦB��B۽B��B�'B�QB�[B�B��BۘB��B��B�rBۤBۖB۳BێB��B��B��B��B۫B�UB� B��B�"B�gB�UB�'B�kB�*B�B��B��B܋BۦB۳B�"B��B�"B��B�B�YB۟B��B�"B��B��B�B܅B��B�&B��B�BۜBۈBیB�UB��B�6B�iB�BBۆB��B��B�B��B��B�'BۂB��B۱B۟B�0B��B�LB�dB�ZB�B�BBۯB�B�6B۟B�B�3B��B�B۶B�aB�BBۂB�pB�WB�kBۜB�B�.B�B��B۟BۖB�B�B۬B�FB�B�$B��B��B��B۩B�.B��B۱B��B��BۖBۚBېBۈB��B��B��B��B��B�IBۜB�6B�$B��B��B۶B��B�&B�BB�,B۱B۾B��B�B�aB�WBۈBۂB۟B۫BۡBۚBۤBۖBۜB۫B�gB܅B��B��B�B��B�B��B܋B�}B��B۸B�B��BېB�B�=B��B�IB�B��B܈B܃BܬBܹB��B��B��B�B�*B�5B�*B�HB�kB�aB�aB�gB�^B܀B܋B�mB۱B��B��BܘB��BܱB۾BܠB�oBܾB��B��BܶB��B�=B�B�,B� B��B�B��B��BܨB�8B�pB�CB�aB܏B��BܜBܹBܯB��BݦB��BܶB܀BܯB�wB܀B�yB�yB�oB�\B�kB�B��B��B�B�B݀B݉B�B�B��B�PB�oB�B�	B�yB��B��BݙBݩB��B��B�B�B�MBݙB݇BݎBݱBݱBݎB�B�B��B�4B�FB�4BݱBݱB�{BݱB�oB��B�iBݎB�SBݟBݽB�`BއB��B�_BޒB��B��B��B��B�kB�OB�OBݮBݓB�"B�yB�"B��B��BލB��B�-B�-B��B��B��B�lBݱB�cBިB�B��BݦB�lB�eB�BݝBލB�LBޘBݕB�B�:BަB��BޝB��BޝBނB�JB��B�aB��B��B�UBފBߟB߅B�[B�$BލB�B�{B�/BއBުB�B�$B�oB�	B��B޾BߛB߇BޝBߒBޔB�[B�RB��B�B߶B�B�pB޵B� BߛB��B�B��B�UBߛB�`B�OB��B߰B��B��BޘB�4B�4BݐB��B��B�B��B޳B�[B�B��B�@B�UB�fB�TB�B�8B�B�B�vB�#B�rB��B�KB�dB�B��BߓB�RBޟB�bB��B�zB��B�B�_B�B�eB�CB�vB�B�B��B��B�B�lB��B�EB�B�\B�'B�B��B��BwB�B[B%B�BFB!�B'�B+oB+�B=�B?BBMB@%B=!BBcB<UB9�B>yBG BH�BJ)BJBJ BJ BJ�BJcBJ�BK�BJ�BJoBK�BJ�BK�BJ�BKuBK�BJ�BJ�BKSBJ�BKiBK�BJ�BJ�BKZBM�BK�BK�BK�BL>BMBM>BM>BK�BL>BK�BKZBL3BKdBJ�BKJBK\BKyBK�BK�BM�BNUBN'BM�BMiBM�BM�BM�BN�BN�BN�BM�BN�BNmBM�BO1BN9BNDBM�BN_BN"BN"BM�BM�BN�BM�BN2BM�BMbBMYBM�BM�BL�BLBL9BMBM�BMBMKBM�BN�BN4BP�BT�BWLBWKB[�Bg�Bm3BlMBk�BlBg�B]�B^�BR�BM�BMcBL�BMMBM!BL�BL�BH�BF�BD�B@�B@�B@B?JB>�B>MB?/B:�B;dB<�B7PB7B5aB5�B3�B3�B3}B4mB4�B5YB4�B5KB4zB3:B5PB3�B1B1�B0aB/�B.3B.;B._B,�B)�B,nB.�B/�B.�B0B1B2uB1)B1gB1B0�B/�B0�B0�B0B/yB/�B/�B.�B0�B3CB2�B1�B1VB1B1�B1�B1�B1B0QB0B/GB/9B.�B/B.�B1�B0�B/*B/[B.XB.yB-�B-$B-�B0�B/wB/B0=B2�B3OB1B1 B0"B.�B*�B+-B+ B&�B#�B$�B3�B$�B(1B$�B#�B"�B"�B!B# B"�B"hB"�B"MB"B"QB"�B!�B!�B"�B"�B"�B"qB&�B)B$4B!(B!*B�B�B�B�B�BmB�B
BBuB�B/BzB�B�BdBBRBBDB.B^B�BB B
vB
BB_B]BBTB0B�B�B�B?B�BMBMB-B�B�B�B�B�B�BfB�B�B
DB�BzB�BPB9B�BxBiBDBBB�B�B�BBWBeB�.B'B B��B�B�rB�B��B�iB��B��B��B��B�B��B��B��B��B�mB��B�B �B�B tB`B�^B�FB ~B��B��B��BB�B(B�B�B�B
B
�B
TB
8B�B
�BByB
�B�B_B�B'B&B�BpBYB/B�B�B�B�B3B�BqB�B�B�B6B6B�B�B�B�B/BWB0B�B�B�B�B<BB�B
lB�BSB:B�B��B��B�B��B��B��B�3B�B�cB�|B�"B�B�B��B��B�B�yB�9B�B�ZB�~B�4B�2B��B�uB��B�)B�B��B��B�VB��B �B �B �B]BHBkB}B�B�BBhB�B�B�B�B&BB}B�B�B&B6B�B�B-B B�BcB�B�B�B�B�B�B�B�B0B�B�BBB#B��B��B��B��B��B��B�@B��B��B�?B�DB��B��B�FB�QB��B�B�KB��B�zB�3B��B�B�B� B�B��B�hB
��B
�@B
�BB
��B
�cB
��B
�)B
�wB
�MB
��B
��B
�*B
��B
��B
�WB
��B
��B
�B
�bB
�B
�B
�XB
�	B
�\B
��B
�<B
�B
�B
��B
�*B
�B
��B
��B
��B
��B
�^B
��B
�FB
�B
��B
�^B
�B
�gB
��B
�,B
��B
��B
��B
�`B
��B
�6B
�AB hB �BB
��B
��B
�xB �B
��B �B%B �B �B|BABYB�B"BRB!BB�BGB�BaBnB�B	�B	&B B�B�BBlBuB�B�BB B	B�B
B�BJB�B�BB�B�B�B�B�B�B!�B�B!UB �B#lB#B$�B)�B"�B*�B.�B+1B.B0�B+�B	�;G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�422222222222222222222222242222222222222222222222222222222222222222222222222222222222242222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999444344444443343344443344334334433444444444444444443444444444444433433444334434444344433433443344344444434444443443444443444433443444443444343344433444433443334434444433444433444443443433444334434334343443333443333343433333333333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No adjustment applied; profiles too shallow to allow a reliable calibration/quality control check.                                                                                                                                                              Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202009011546432020090115464320200901154643202009011546432020090115464320200901154643202009011546432020090115464320200901154643202009011546432020090115464320200901154643AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201811202122082018112021220820181120212208    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202122082018112021220820181120212208  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202122082018112021220820181120212208  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4000            0               4000            UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202009011546432020090115464320200901154643  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                