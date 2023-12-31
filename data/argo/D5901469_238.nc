CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  g   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-20T21:22:11Z creation      
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
resolution        =���   axis      Z        X�  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X�  �   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8 �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X� #(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X� {�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8 ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X� �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8 C�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X� Z   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     X� ��   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8 �   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     X� !�   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8 z�   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     X� �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X� ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8 B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X� X�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8 ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X� ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   !�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   -�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   9�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � E�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   F(   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   F4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   F@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   FL   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � FX   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , G   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   GD   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 GP   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        G�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        G�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       G�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 G�Argo profile    3.1 1.2 19500101000000  20181120212211  20200901154658  5901469 5901469 5901469 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  2688                            2688                            2688                            2C  2B  2C  DAD APEX                            APEX                            APEX                            2730                            2730                            2730                            112607                          112607                          112607                          846 846 846 @�Yb�'��@�Yb�'��@�Yb�'��111 @�Yc^ЩN@�Yc^ЩN@�Yc^ЩN@6�1&�x�@6�1&�x�@6�1&�x��dW;dZ��dW;dZ��dW;dZ�111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ACA BCA  CA BCA >���@@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bv��B�  B�ffB���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2�C3�fC6  C8  C:  C<� C>�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�        =���    =���        =���=���    =���>L��                =���=���        =���    =���        >L��=���    =���    >L��                        >L��>L��        >L��>L��        =���        >���>L��            =���                        =���        >���        >���>���                        =���    >���>L��    =���        =���=���    =���    =���        =���            >L��            =���            =���=���            =���=���    >���    =���    >L��                        =���    =���=���>L��    =���>L��        >L��                =���        >L��>���>L��    =���>L��>���>L��            >���            >L��=���=���        =���>L��=���    =���>���>���>���            =���=���    =���=���    =���    =���>L��=���=���    =���>���>���>L��            =���>���>���>L��    =���    >L��>���=���=���    =���>L��=���=���=���>L��=���=���=���>L��>L��>L��=���>���>L��>L��>���>L��>L��>L��>L��>���>���>L��>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>L��>L��>���>L��>���>���=���>���>���>���>���>���>���>���>���>���>���>���>L��>L��>���>L��>���>���>���>���>���>���>���?   ?   >���=���>���>���>���>���>���>���>L��>���>���>���>���?   >���>L��>���>L��>���>L��>���>���>���>���>���>���>���>���>���>���>���>L��>���>L��>���>���>L��>���>���>���>���>���>���>���>���=���>���>���>L��>���>���>���>���>���>���>���>L��>���>���>���>���>L��>���>���>���>���>���>���=���>���>���>���>���>L��>L��>���>L��>L��>���>���>���>���>���>���>���>���>���>���>L��>L��>���>���>���?   >���>L��>L��>���>L��>L��>���>L��>L��>���>���>���>���>���>���>���>L��>���>L��>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���?   ?   >���>L��>���>L��>L��>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>L��>L��>���>���>���>���>���>���>���>���>���>���>���>L��>L��>���>L��>���>���>���>���>L��>L��>���>���>���?   ?   ?��?��?333?333?L��?L��?L��?L��?fff?fff?���?�  ?���?���?���?���?�ff?�ff?�ff?���?���?�33?�33?�  ?�  ?���?�  ?���?ٙ�?�ff?ٙ�?ٙ�?�ff?�33?�33@   @   @   @ff@ff@��@��@��@��@33@��@��@   @   @   @&ff@&ff@,��@,��@333@333@9��@@  @@  @Fff@L��@S33@S33@Y��@`  @fff@l��@s33@s33@y��@�  @�33@�ff@���@���@�  @�ff@���@���@���@�33@�ff@���@���@�  @�ff@���@���@�  @�33@ə�@���@�33@�ff@ٙ�@�  @�33@�ff@陚@�  @�ff@���@���A   A33A��AffA  A33A��AffA��A33A��A  A��A��AffA   A#33A$��A&ffA)��A+33A.ffA0  A1��A4��A8  A9��A;33A>ffAA��AC33AD��AH  AK33AL��ANffAP  AS33AVffAX  A[33A\��A^ffAa��Ad��AfffAh  Ak33AnffAp  As33At��AvffAy��A|��A~ffA���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A�ffA�33A���A���A�33A�  A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A���A�33A���A���A�33A�  A���A�33A�  A���A�ffA�  A���A�ffA�33A���Aə�A�33A�  A���A�ffA�33A���Aљ�A�ffA�  A���A�ffA�33A���Aٙ�A�ffA�  A���A�ffA�33A�  AᙚA�ffA�  A���A�ffA�33A�  A陚A�ffA�  A���A홚A�33A�  A�A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33B   B ffB33B��B  B��B33B��BffB��B33B  BffB��B��B  BffB	33B	��B
  B
ffB33B��B  BffB��B��B  BffB��B��B  BffB��B��B  BffB��B33B  BffB��B33B��BffB��B33B��B  BffB��B33B��B  BffB��B33B��B  BffB��B33B��B  BffB��B33B��B   B ffB ��B!33B!��B"  B"  B"ffB"��B#33B#��B$  B$ffB$��B%33B%��B&  B&ffB&��B'33B'��B(  B(  B(ffB(��B)33B)��B*  B*ffB*��B+33B+��B,  B,ffB,��B-33B-��B.  B.ffB.��B/33B/��B0  B0ffB0��B133B1��B2  B2ffB2��B333B3��B4  B4ffB4��B533B5��B6  B6ffB6��B733B7��B8  B8ffB8��B933B9��B:  B:ffB:��B:��B;33B;��B<  B<ffB<��B=33B=��B=��B>  B>ffB>��B?33B?33B?��B@  B@  B@ffB@��B@��BA33BA��BA��BB  BB  BBffBB��BB��BC33BC33BC��BD  BD  BDffBDffBD��BD��BE33BE��BE��BF  BF  BFffBFffBF��BG33BG33BG��BH  BH  BHffBHffBH��BI33BI33BI��BI��BJ  BJffBJffBJ��BK33BK33BK��BK��BL  BLffBLffBL��BL��BM33BM��BM��BN  BN  BNffBN��BN��BO33BO33BO��BP  BP  BPffBPffBP��BQ33BQ33BQ��BR  BR  BRffBRffBR��BS33BS33BS��BT  BT  BTffBTffBT��BU33BU33BU��BU��BV  BVffBVffBV��BV��BW33BW33BW��BX  BX  BXffBXffBX��BY33BY33BY��BY��BZ  BZffBZffBZ��B[33B[33B[��B[��B\  B\ffB\ffB\��B\��B]33B]��B]��B^  B^ffB^ffB^��B_33B_33B_��B_��B`  B`ffB`ffB`��B`��Ba33Ba��Ba��Bb  BbffBbffBb��Bb��Bc33Bc��Bc��Bd  Bd  BdffBd��Bd��Be33Be��Be��Bf  Bf  BfffBf��Bf��Bg33Bg33Bg��Bh  Bh  BhffBh��Bh��Bi33Bi33Bi��Bj  BjffBjffBj��Bj��Bk33Bk��Bk��Bl  BlffBl��Bl��Bm33Bm��Bm��Bn  BnffBnffBn��Bo33Bo33Bo��Bp  Bp  BpffBp��Bp��Bq33Bq��Bq��Br  BrffBrffBr��Br��Bs33Bs��Bs��Bt  Bt  BtffBtffBt��Bt��Bu33Bu33Bu��Bu��Bu��Bv  Bv  Bv  Bv  BvffBvffBvffBvffBv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��BvffBvffBvffBvffBvffBvffBvffBvffBv  Bv  Bv  Bv  Bv  Bu��Bu��Bu��Bu��Bu��Bu��Bu��Bu33Bu33Bu33Bu33Bu33Bu33Bu33Bu33Bu33Bu33Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bu33Bu33Bu33Bu33Bu33Bu��Bu��Bu��Bu��Bu��Bv  Bv  Bv  BvffBvffBvffBvffBv��Bv��Bw33Bw33Bw33Bw��Bw��Bx  Bx  BxffBxffBx��Bx��By33By33By��By��Bz  Bz  BzffBzffBz��B{33B{33B{��B{��B|  B|  B|ffB|��B|��B}33B}33B}��B~  B~  B~ffB~ffB~��B~��B33B��B��B�  B�33B�33B�ffB�ffB���B���B���B�  B�  B�33B�ffB�ffB���B���B���B���B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�  B�  B�33B�33B�33B�ffB�ffB���B���B���B���B���B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�ffB�ffB�ffB���B���B���B���B���B���B���B���B���B�  B�  B�  B�33B�33B�33B�33B�ffB�ffB���B���B���B���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�33B�33B�ffB�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B�  B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�  B�ffB�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�33B�33B�ffB���B���B�  B�  B�33B�ffB�ffB���B���B�  B�  B�33B�33B�ffB���B���B���B���B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�33B�33B�ffB�ffB���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B�  B�  B�33B�ffB���B���B���B���B�  B�33B�33B�ffB���B���B���B�  B�  B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB�ffB���B���B�  B�33B�ffB�ffB���B���B�  B�  B�33B�ffB���B���B�  B�33B�33B�ffB���B���B�  B�33B�ffB�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B�  B�33B�ffBÙ�B�  B�33B�ffBę�B�  B�33B�ffBř�B���B�33B�ffBƙ�B���B�  B�ffBǙ�B���B�  B�33B�ffB���B�  B�33B�ffBə�B���B�  B�33B�ffBʙ�B�  B�33B�33B�ffB˙�B���B�  B�33B�ffB̙�B���B�  B�33B�ffB͙�C>  C>  C>  C>  C>  C>  C>  C>  C>  C>  C>  C>  C=�fC=�fC=�fC=�fC=�fC=��C=��C=��C=�3C=�3C=�3C=��C=��C=��C=� C=� C=ffC=ffC=L�C=L�C=33C=33C=�C=  C=  C<�fC<�fC<��C<�3C<�3C<��C<� C<� C<ffC<L�C<L�C<33C<�C<�C<  C;�fC;��C;�3C;�3C;��C;� C;ffC;L�C;L�C;33C;�C;  C:�fC:�fC:��C:�3C:��C:� C:ffC:L�C:33C:�C:  C9�fC9��C9�3C9��C9� C9ffC9L�C933C9�C9  C8�fC8��C8�3C8��C8� C8L�C833C8�C8  C7�fC7��C7�3C7��C7ffC7L�C733C7�C7  C6�fC6�3C6��C6� C6ffC633C6�C6  C5�fC5��C5��C5� C5ffC5L�C5�C5  C4�f@@  @@  @Fff@L��@S33@S33@Y��@`  @fff@l��@s33@s33@y��@�  @�33@�ff@���@���@�  @�ff@���@���@���@�33@�ff@���@���@�  @�ff@���@���@�  @�33@ə�@���@�33@�ff@ٙ�@�  @�33@�ff@陚@�  @�ff@���@���A   A33A��AffA  A33A��AffA��A33A��A  A��A��AffA   A#33A$��A&ffA)��A+33A.ffA0  A1��A4��A8  A9��A;33A>ffAA��AC33AD��AH  AK33AL��ANffAP  AS33AVffAX  A[33A\��A^ffAa��Ad��AfffAh  Ak33AnffAp  As33At��AvffAy��A|��A~ffA���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A�ffA�33A���A���A�33A�  A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A���A�33A���A���A�33A�  A���A�33A�  A���A�ffA�  A���A�ffA�33A���Aə�A�33A�  A���A�ffA�33A���Aљ�A�ffA�  A���A�ffA�33A���Aٙ�A�ffA�  A���A�ffA�33A�  AᙚA�ffA�  A���A�ffA�33A�  A陚A�ffA�  A���A홚A�33A�  A�A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33B   B ffB33B��B  B��B33B��BffB��B33B  BffB��B��B  BffB	33B	��B
  B
ffB33B��B  BffB��B��B  BffB��B��B  BffB��B��B  BffB��B33B  BffB��B33B��BffB��B33B��B  BffB��B33B��B  BffB��B33B��B  BffB��B33B��B  BffB��B33B��B   B ffB ��B!33B!��B"  B"  B"ffB"��B#33B#��B$  B$ffB$��B%33B%��B&  B&ffB&��B'33B'��B(  B(  B(ffB(��B)33B)��B*  B*ffB*��B+33B+��B,  B,ffB,��B-33B-��B.  B.ffB.��B/33B/��B0  B0ffB0��B133B1��B2  B2ffB2��B333B3��B4  B4ffB4��B533B5��B6  B6ffB6��B733B7��B8  B8ffB8��B933B9��B:  B:ffB:��B:��B;33B;��B<  B<ffB<��B=33B=��B=��B>  B>ffB>��B?33B?33B?��B@  B@  B@ffB@��B@��BA33BA��BA��BB  BB  BBffBB��BB��BC33BC33BC��BD  BD  BDffBDffBD��BD��BE33BE��BE��BF  BF  BFffBFffBF��BG33BG33BG��BH  BH  BHffBHffBH��BI33BI33BI��BI��BJ  BJffBJffBJ��BK33BK33BK��BK��BL  BLffBLffBL��BL��BM33BM��BM��BN  BN  BNffBN��BN��BO33BO33BO��BP  BP  BPffBPffBP��BQ33BQ33BQ��BR  BR  BRffBRffBR��BS33BS33BS��BT  BT  BTffBTffBT��BU33BU33BU��BU��BV  BVffBVffBV��BV��BW33BW33BW��BX  BX  BXffBXffBX��BY33BY33BY��BY��BZ  BZffBZffBZ��B[33B[33B[��B[��B\  B\ffB\ffB\��B\��B]33B]��B]��B^  B^ffB^ffB^��B_33B_33B_��B_��B`  B`ffB`ffB`��B`��Ba33Ba��Ba��Bb  BbffBbffBb��Bb��Bc33Bc��Bc��Bd  Bd  BdffBd��Bd��Be33Be��Be��Bf  Bf  BfffBf��Bf��Bg33Bg33Bg��Bh  Bh  BhffBh��Bh��Bi33Bi33Bi��Bj  BjffBjffBj��Bj��Bk33Bk��Bk��Bl  BlffBl��Bl��Bm33Bm��Bm��Bn  BnffBnffBn��Bo33Bo33Bo��Bp  Bp  BpffBp��Bp��Bq33Bq��Bq��Br  BrffBrffBr��Br��Bs33Bs��Bs��Bt  Bt  BtffBtffBt��Bt��Bu33Bu33Bu��Bu��Bu��Bv  Bv  Bv  Bv  BvffBvffBvffBvffBv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bw33Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��BvffBvffBvffBvffBvffBvffBvffBvffBv  Bv  Bv  Bv  Bv  Bu��Bu��Bu��Bu��Bu��Bu��Bu��Bu33Bu33Bu33Bu33Bu33Bu33Bu33Bu33Bu33Bu33Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bu33Bu33Bu33Bu33Bu33Bu��Bu��Bu��Bu��Bu��Bv  Bv  Bv  BvffBvffBvffBvffBv��Bv��Bw33Bw33Bw33Bw��Bw��Bx  Bx  BxffBxffBx��Bx��By33By33By��By��Bz  Bz  BzffBzffBz��B{33B{33B{��B{��B|  B|  B|ffB|��B|��B}33B}33B}��B~  B~  B~ffB~ffB~��B~��B33B��B��B�  B�33B�33B�ffB�ffB���B���B���B�  B�  B�33B�ffB�ffB���B���B���B���B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�  B�  B�33B�33B�33B�ffB�ffB���B���B���B���B���B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�ffB�ffB�ffB���B���B���B���B���B���B���B���B���B�  B�  B�  B�33B�33B�33B�33B�ffB�ffB���B���B���B���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�33B�33B�ffB�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B�  B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�  B�ffB�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�33B�33B�ffB���B���B�  B�  B�33B�ffB�ffB���B���B�  B�  B�33B�33B�ffB���B���B���B���B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�33B�33B�ffB�ffB���B���B���B���B�  B�33B�33B�ffB�ffB���B���B���B�  B�  B�33B�33B�ffB�ffB���B���B���B�  B�  B�33B�ffB���B���B���B���B�  B�33B�33B�ffB���B���B���B�  B�  B�33B�ffB���B���B���B�  B�33B�33B�ffB���B���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB�ffB���B���B�  B�33B�ffB�ffB���B���B�  B�  B�33B�ffB���B���B�  B�33B�33B�ffB���B���B�  B�33B�ffB�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B�  B�33B�ffBÙ�B�  B�33B�ffBę�B�  B�33B�ffBř�B���B�33B�ffBƙ�B���B�  B�ffBǙ�B���B�  B�33B�ffB���B�  B�33B�ffBə�B���B�  B�33B�ffBʙ�B�  B�33B�33B�ffB˙�B���B�  B�33B�ffB̙�B���B�  B�33B�ffB͙�C>  C>  C>  C>  C>  C>  C>  C>  C>  C>  C>  C>  C=�fC=�fC=�fC=�fC=�fC=��C=��C=��C=�3C=�3C=�3C=��C=��C=��C=� C=� C=ffC=ffC=L�C=L�C=33C=33C=�C=  C=  C<�fC<�fC<��C<�3C<�3C<��C<� C<� C<ffC<L�C<L�C<33C<�C<�C<  C;�fC;��C;�3C;�3C;��C;� C;ffC;L�C;L�C;33C;�C;  C:�fC:�fC:��C:�3C:��C:� C:ffC:L�C:33C:�C:  C9�fC9��C9�3C9��C9� C9ffC9L�C933C9�C9  C8�fC8��C8�3C8��C8� C8L�C833C8�C8  C7�fC7��C7�3C7��C7ffC7L�C733C7�C7  C6�fC6�3C6��C6� C6ffC633C6�C6  C5�fC5��C5��C5� C5ffC5L�C5�C5  C4�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   >��@=p�@}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B@=pBG�
BO�
BW�
B_�
Bg�
Bo�
Bv��B�
B�Q�B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C]C��C��C��C��C��C��C��C��C��C"]C#��C%��C'��C)��C+��C-��C/��C2]C3�)C5��C7��C9��C<u�C>]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��#�
�#�
=u�#�
=u�#�
�#�
=u=u�#�
=u>#�
�#�
�#�
�#�
�#�
=u=u�#�
�#�
=u�#�
=u�#�
�#�
>#�
=u�#�
=u�#�
>#�
�#�
�#�
�#�
�#�
�#�
�#�
>#�
>#�
�#�
�#�
>#�
>#�
�#�
�#�
=u�#�
�#�
>��>#�
�#�
�#�
�#�
=u�#�
�#�
�#�
�#�
�#�
�#�
=u�#�
�#�
>���#�
�#�
>�Q�>���#�
�#�
�#�
�#�
�#�
�#�
=u�#�
>��>#�
�#�
=u�#�
�#�
=u=u�#�
=u�#�
=u�#�
�#�
=u�#�
�#�
�#�
>#�
�#�
�#�
�#�
=u�#�
�#�
�#�
=u=u�#�
�#�
�#�
=u=u�#�
>���#�
=u�#�
>#�
�#�
�#�
�#�
�#�
�#�
�#�
=u�#�
=u=u>#�
�#�
=u>#�
�#�
�#�
>#�
�#�
�#�
�#�
�#�
=u�#�
�#�
>#�
>��>#�
�#�
=u>#�
>�Q�>#�
�#�
�#�
�#�
>���#�
�#�
�#�
>#�
=u=u�#�
�#�
=u>#�
=u�#�
=u>�Q�>�Q�>���#�
�#�
�#�
=u=u�#�
=u=u�#�
=u�#�
=u>#�
=u=u�#�
=u>��>��>#�
�#�
�#�
�#�
=u>��>��>#�
�#�
=u�#�
>#�
>��=u=u�#�
=u>#�
=u=u=u>#�
=u=u=u>#�
>#�
>#�
=u>��>#�
>#�
>��>#�
>#�
>#�
>#�
>��>��>#�
>��>�Q�>��>��>��>�Q�>�Q�>#�
>��>�Q�>��>��>��>��>��>��>��>#�
>#�
>��>#�
>��>��=u>��>��>��>��>�Q�>�Q�>��>�Q�>�Q�>�Q�>��>#�
>#�
>��>#�
>�Q�>��>��>��>�Q�>��>�Q�>�>�>��=u>��>�Q�>��>��>�Q�>�Q�>#�
>��>��>�Q�>��>�>�Q�>#�
>��>#�
>��>#�
>��>��>�Q�>�Q�>��>�Q�>�Q�>�Q�>�Q�>�Q�>��>#�
>��>#�
>��>��>#�
>�Q�>��>��>��>��>��>��>��=u>�Q�>��>#�
>��>��>�Q�>�Q�>��>��>��>#�
>�Q�>��>��>�Q�>#�
>��>��>��>��>�Q�>��=u>��>��>��>��>#�
>#�
>��>#�
>#�
>��>�Q�>��>��>��>��>�Q�>��>�Q�>��>#�
>#�
>�Q�>��>�Q�>�>��>#�
>#�
>��>#�
>#�
>��>#�
>#�
>��>��>��>��>��>��>�Q�>#�
>��>#�
>��>�Q�>�Q�>��>��>��>��>��>��>��>��>#�
>��>��>��>�Q�>��>�>�>��>#�
>��>#�
>#�
>#�
>��>��>��>��>��>�Q�>��>��>�Q�>�Q�>��>�Q�>��>#�
>#�
>#�
>��>��>��>��>��>��>��>��>��>��>��>#�
>#�
>��>#�
>��>�Q�>��>��>#�
>#�
>��>�Q�>�Q�>�>�?\)?\)?(��?(��?B�\?B�\?B�\?B�\?\(�?\(�?��?u?��?��?��?��?�G�?�G�?�G�?�z�?�z�?�{?�{?��H?��H?Ǯ?��H?Ǯ?�z�?�G�?�z�?�z�?�G�?�{?�{?��H?��H?��H@�
@�
@
=q@
=q@
=q@
=q@��@
>@
>@p�@p�@p�@#�
@#�
@*=q@*=q@0��@0��@7
>@=p�@=p�@C�
@J=q@P��@P��@W
>@]p�@c�
@j=q@p��@p��@w
>@}p�@��@��@�Q�@��@��R@��@�Q�@��@��@��@��@��@��@��R@��@�Q�@��@��R@��@�Q�@˅@��@��@�Q�@޸R@��@��@�Q�@�R@��@�Q�@��@��RA�\A(�AA\)A
�\A(�AA��A�\A(�A\)A��A(�AA\)A"�\A$(�A%A(��A*�\A-A/\)A0��A4(�A7\)A8��A:�\A=A@��AB�\AD(�AG\)AJ�\AL(�AMAO\)AR�\AUAW\)AZ�\A\(�A]A`��Ad(�AeAg\)Aj�\AmAo\)Ar�\At(�AuAx��A|(�A}A�z�A�G�A��GA��A�G�A�zA��A�z�A�zA��GA�z�A�zA��GA�z�A�G�A��GA��A�G�A��GA��A�G�A�zA��A�G�A�zA��A�z�A�zA��GA�z�A�G�A��GA��A�G�A�zA��A�z�A�zA��A�z�A�zA��GA�z�A�G�A��GA�z�A�G�A��GA��A�G�A��GA��A�G�A�zAîA�z�A�zA��GA�z�A�G�A��GAˮA�z�A�zA��GA�z�A�G�A�zAӮA�z�A�zA��GA�z�A�G�A�zAۮA�z�A�zA��GA߮A�G�A�zA�A�z�A�zA��GA�A�G�A�zA�A�z�A�G�A��GA�A�G�A�zA�A�z�A�G�A��GA��A�z�A�zA��GA�z�A�G�A��GA��B =pB
=Bp�B�
B��B
=Bp�B=pB��B
=B�
B=pB��Bp�B�
B=pB	
=B	p�B	�
B
=pB
=Bp�B�
B=pB��Bp�B�
B=pB��Bp�B�
B=pB��Bp�B�
B=pB��B
=B�
B=pB��B
=Bp�B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B =pB ��B!
=B!p�B!�
B!�
B"=pB"��B#
=B#p�B#�
B$=pB$��B%
=B%p�B%�
B&=pB&��B'
=B'p�B'�
B'�
B(=pB(��B)
=B)p�B)�
B*=pB*��B+
=B+p�B+�
B,=pB,��B-
=B-p�B-�
B.=pB.��B/
=B/p�B/�
B0=pB0��B1
=B1p�B1�
B2=pB2��B3
=B3p�B3�
B4=pB4��B5
=B5p�B5�
B6=pB6��B7
=B7p�B7�
B8=pB8��B9
=B9p�B9�
B:=pB:��B:��B;
=B;p�B;�
B<=pB<��B=
=B=p�B=p�B=�
B>=pB>��B?
=B?
=B?p�B?�
B?�
B@=pB@��B@��BA
=BAp�BAp�BA�
BA�
BB=pBB��BB��BC
=BC
=BCp�BC�
BC�
BD=pBD=pBD��BD��BE
=BEp�BEp�BE�
BE�
BF=pBF=pBF��BG
=BG
=BGp�BG�
BG�
BH=pBH=pBH��BI
=BI
=BIp�BIp�BI�
BJ=pBJ=pBJ��BK
=BK
=BKp�BKp�BK�
BL=pBL=pBL��BL��BM
=BMp�BMp�BM�
BM�
BN=pBN��BN��BO
=BO
=BOp�BO�
BO�
BP=pBP=pBP��BQ
=BQ
=BQp�BQ�
BQ�
BR=pBR=pBR��BS
=BS
=BSp�BS�
BS�
BT=pBT=pBT��BU
=BU
=BUp�BUp�BU�
BV=pBV=pBV��BV��BW
=BW
=BWp�BW�
BW�
BX=pBX=pBX��BY
=BY
=BYp�BYp�BY�
BZ=pBZ=pBZ��B[
=B[
=B[p�B[p�B[�
B\=pB\=pB\��B\��B]
=B]p�B]p�B]�
B^=pB^=pB^��B_
=B_
=B_p�B_p�B_�
B`=pB`=pB`��B`��Ba
=Bap�Bap�Ba�
Bb=pBb=pBb��Bb��Bc
=Bcp�Bcp�Bc�
Bc�
Bd=pBd��Bd��Be
=Bep�Bep�Be�
Be�
Bf=pBf��Bf��Bg
=Bg
=Bgp�Bg�
Bg�
Bh=pBh��Bh��Bi
=Bi
=Bip�Bi�
Bj=pBj=pBj��Bj��Bk
=Bkp�Bkp�Bk�
Bl=pBl��Bl��Bm
=Bmp�Bmp�Bm�
Bn=pBn=pBn��Bo
=Bo
=Bop�Bo�
Bo�
Bp=pBp��Bp��Bq
=Bqp�Bqp�Bq�
Br=pBr=pBr��Br��Bs
=Bsp�Bsp�Bs�
Bs�
Bt=pBt=pBt��Bt��Bu
=Bu
=Bup�Bup�Bup�Bu�
Bu�
Bu�
Bu�
Bv=pBv=pBv=pBv=pBv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv=pBv=pBv=pBv=pBv=pBv=pBv=pBv=pBu�
Bu�
Bu�
Bu�
Bu�
Bup�Bup�Bup�Bup�Bup�Bup�Bup�Bu
=Bu
=Bu
=Bu
=Bu
=Bu
=Bu
=Bu
=Bu
=Bu
=Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bu
=Bu
=Bu
=Bu
=Bu
=Bup�Bup�Bup�Bup�Bup�Bu�
Bu�
Bu�
Bv=pBv=pBv=pBv=pBv��Bv��Bw
=Bw
=Bw
=Bwp�Bwp�Bw�
Bw�
Bx=pBx=pBx��Bx��By
=By
=Byp�Byp�By�
By�
Bz=pBz=pBz��B{
=B{
=B{p�B{p�B{�
B{�
B|=pB|��B|��B}
=B}
=B}p�B}�
B}�
B~=pB~=pB~��B~��B
=Bp�Bp�B�
B��B��B�Q�B�Q�B��B��RB��RB��B��B��B�Q�B�Q�B��B��B��RB��RB��B��B��B�Q�B�Q�B��B��B��RB��RB��B��B��B��B�Q�B�Q�B��B��B��RB��RB��B��B��B��B��B��B�Q�B�Q�B��B��B��B��B��RB��RB��RB��RB��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B�Q�B�Q�B�Q�B�Q�B��B��B��B��B��B��B��RB��RB��RB��B��B��B��B��B��B��B�Q�B�Q�B��B��B��B��RB��RB��RB��B��B��B��B�Q�B�Q�B��B��B��B��RB��B��B��B��B�Q�B�Q�B��B��B��RB��RB��B��B��B�Q�B�Q�B��B��RB��RB��B��B��B�Q�B��B��B��RB��B��B��B�Q�B��B��B��RB��B��B��B�Q�B��B��RB��B��B��B�Q�B��B��RB��RB��B��B�Q�B��B��B��RB��B��B�Q�B��B��B��RB��B��B�Q�B��B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��RB��B��B�Q�B��B��RB��RB��B��B�Q�B��B��RB��B��B��B�Q�B�Q�B��B��RB��B��B��B��B�Q�B��B��B��RB��RB��B��B��B�Q�B�Q�B��B��B��RB��RB��B��B��B�Q�B�Q�B��B��B��RB��RB��B��B��B�Q�B�Q�B��B��B��RB��B��B��B��B�Q�B�Q�B��B��RB��RB��B��B��B�Q�B��B��B��RB��RB��B��B��B�Q�B��B��B��RB��B��B��B�Q�B��B��B��RB��B��B��B�Q�B��B��RB��RB��B��B�Q�B��B��B��RB��B��B�Q�B�Q�B��B��RB��B��B�Q�B�Q�B��B��RB��B��B��B�Q�B��B��RB��B��B��B�Q�B��B��RB��B��B�Q�B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B�Q�B��B��RB��B��B�Q�B��B��RB��B�Q�B��B��RB��B��B�Q�B��B��B��B�Q�B��B��RB��B�Q�B��B��RB��B��B�Q�B��RB��B��B�Q�B��B��RB��B�Q�B��B��RB��B��B��B��RB��B��B�Q�B¸RB��B��B�Q�BÅB��B��B�Q�BąB��B��B�Q�BŅBŸRB��B�Q�BƅBƸRB��B�Q�BǅBǸRB��B��B�Q�BȸRB��B��B�Q�BɅBɸRB��B��B�Q�BʅB��B��B��B�Q�B˅B˸RB��B��B�Q�B̅B̸RB��B��B�Q�BͅC=��C=��C=��C=��C=��C=��C=��C=��C=��C=��C=��C=��C=�)C=�)C=�)C=�)C=�)C=C=C=C=��C=��C=��C=�]C=�]C=�]C=u�C=u�C=\)C=\)C=B�C=B�C=(�C=(�C=]C<��C<��C<�)C<�)C<C<��C<��C<�]C<u�C<u�C<\)C<B�C<B�C<(�C<]C<]C;��C;�)C;C;��C;��C;�]C;u�C;\)C;B�C;B�C;(�C;]C:��C:�)C:�)C:C:��C:�]C:u�C:\)C:B�C:(�C:]C9��C9�)C9C9��C9�]C9u�C9\)C9B�C9(�C9]C8��C8�)C8C8��C8�]C8u�C8B�C8(�C8]C7��C7�)C7C7��C7�]C7\)C7B�C7(�C7]C6��C6�)C6��C6�]C6u�C6\)C6(�C6]C5��C5�)C5C5�]C5u�C5\)C5B�C5]C4��C4�)@=p�@=p�@C�
@J=q@P��@P��@W
>@]p�@c�
@j=q@p��@p��@w
>@}p�@��@��@�Q�@��@��R@��@�Q�@��@��@��@��@��@��@��R@��@�Q�@��@��R@��@�Q�@˅@��@��@�Q�@޸R@��@��@�Q�@�R@��@�Q�@��@��RA�\A(�AA\)A
�\A(�AA��A�\A(�A\)A��A(�AA\)A"�\A$(�A%A(��A*�\A-A/\)A0��A4(�A7\)A8��A:�\A=A@��AB�\AD(�AG\)AJ�\AL(�AMAO\)AR�\AUAW\)AZ�\A\(�A]A`��Ad(�AeAg\)Aj�\AmAo\)Ar�\At(�AuAx��A|(�A}A�z�A�G�A��GA��A�G�A�zA��A�z�A�zA��GA�z�A�zA��GA�z�A�G�A��GA��A�G�A��GA��A�G�A�zA��A�G�A�zA��A�z�A�zA��GA�z�A�G�A��GA��A�G�A�zA��A�z�A�zA��A�z�A�zA��GA�z�A�G�A��GA�z�A�G�A��GA��A�G�A��GA��A�G�A�zAîA�z�A�zA��GA�z�A�G�A��GAˮA�z�A�zA��GA�z�A�G�A�zAӮA�z�A�zA��GA�z�A�G�A�zAۮA�z�A�zA��GA߮A�G�A�zA�A�z�A�zA��GA�A�G�A�zA�A�z�A�G�A��GA�A�G�A�zA�A�z�A�G�A��GA��A�z�A�zA��GA�z�A�G�A��GA��B =pB
=Bp�B�
B��B
=Bp�B=pB��B
=B�
B=pB��Bp�B�
B=pB	
=B	p�B	�
B
=pB
=Bp�B�
B=pB��Bp�B�
B=pB��Bp�B�
B=pB��Bp�B�
B=pB��B
=B�
B=pB��B
=Bp�B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B =pB ��B!
=B!p�B!�
B!�
B"=pB"��B#
=B#p�B#�
B$=pB$��B%
=B%p�B%�
B&=pB&��B'
=B'p�B'�
B'�
B(=pB(��B)
=B)p�B)�
B*=pB*��B+
=B+p�B+�
B,=pB,��B-
=B-p�B-�
B.=pB.��B/
=B/p�B/�
B0=pB0��B1
=B1p�B1�
B2=pB2��B3
=B3p�B3�
B4=pB4��B5
=B5p�B5�
B6=pB6��B7
=B7p�B7�
B8=pB8��B9
=B9p�B9�
B:=pB:��B:��B;
=B;p�B;�
B<=pB<��B=
=B=p�B=p�B=�
B>=pB>��B?
=B?
=B?p�B?�
B?�
B@=pB@��B@��BA
=BAp�BAp�BA�
BA�
BB=pBB��BB��BC
=BC
=BCp�BC�
BC�
BD=pBD=pBD��BD��BE
=BEp�BEp�BE�
BE�
BF=pBF=pBF��BG
=BG
=BGp�BG�
BG�
BH=pBH=pBH��BI
=BI
=BIp�BIp�BI�
BJ=pBJ=pBJ��BK
=BK
=BKp�BKp�BK�
BL=pBL=pBL��BL��BM
=BMp�BMp�BM�
BM�
BN=pBN��BN��BO
=BO
=BOp�BO�
BO�
BP=pBP=pBP��BQ
=BQ
=BQp�BQ�
BQ�
BR=pBR=pBR��BS
=BS
=BSp�BS�
BS�
BT=pBT=pBT��BU
=BU
=BUp�BUp�BU�
BV=pBV=pBV��BV��BW
=BW
=BWp�BW�
BW�
BX=pBX=pBX��BY
=BY
=BYp�BYp�BY�
BZ=pBZ=pBZ��B[
=B[
=B[p�B[p�B[�
B\=pB\=pB\��B\��B]
=B]p�B]p�B]�
B^=pB^=pB^��B_
=B_
=B_p�B_p�B_�
B`=pB`=pB`��B`��Ba
=Bap�Bap�Ba�
Bb=pBb=pBb��Bb��Bc
=Bcp�Bcp�Bc�
Bc�
Bd=pBd��Bd��Be
=Bep�Bep�Be�
Be�
Bf=pBf��Bf��Bg
=Bg
=Bgp�Bg�
Bg�
Bh=pBh��Bh��Bi
=Bi
=Bip�Bi�
Bj=pBj=pBj��Bj��Bk
=Bkp�Bkp�Bk�
Bl=pBl��Bl��Bm
=Bmp�Bmp�Bm�
Bn=pBn=pBn��Bo
=Bo
=Bop�Bo�
Bo�
Bp=pBp��Bp��Bq
=Bqp�Bqp�Bq�
Br=pBr=pBr��Br��Bs
=Bsp�Bsp�Bs�
Bs�
Bt=pBt=pBt��Bt��Bu
=Bu
=Bup�Bup�Bup�Bu�
Bu�
Bu�
Bu�
Bv=pBv=pBv=pBv=pBv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bw
=Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv��Bv=pBv=pBv=pBv=pBv=pBv=pBv=pBv=pBu�
Bu�
Bu�
Bu�
Bu�
Bup�Bup�Bup�Bup�Bup�Bup�Bup�Bu
=Bu
=Bu
=Bu
=Bu
=Bu
=Bu
=Bu
=Bu
=Bu
=Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bt��Bu
=Bu
=Bu
=Bu
=Bu
=Bup�Bup�Bup�Bup�Bup�Bu�
Bu�
Bu�
Bv=pBv=pBv=pBv=pBv��Bv��Bw
=Bw
=Bw
=Bwp�Bwp�Bw�
Bw�
Bx=pBx=pBx��Bx��By
=By
=Byp�Byp�By�
By�
Bz=pBz=pBz��B{
=B{
=B{p�B{p�B{�
B{�
B|=pB|��B|��B}
=B}
=B}p�B}�
B}�
B~=pB~=pB~��B~��B
=Bp�Bp�B�
B��B��B�Q�B�Q�B��B��RB��RB��B��B��B�Q�B�Q�B��B��B��RB��RB��B��B��B�Q�B�Q�B��B��B��RB��RB��B��B��B��B�Q�B�Q�B��B��B��RB��RB��B��B��B��B��B��B�Q�B�Q�B��B��B��B��B��RB��RB��RB��RB��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B�Q�B�Q�B�Q�B�Q�B��B��B��B��B��B��B��RB��RB��RB��B��B��B��B��B��B��B�Q�B�Q�B��B��B��B��RB��RB��RB��B��B��B��B�Q�B�Q�B��B��B��B��RB��B��B��B��B�Q�B�Q�B��B��B��RB��RB��B��B��B�Q�B�Q�B��B��RB��RB��B��B��B�Q�B��B��B��RB��B��B��B�Q�B��B��B��RB��B��B��B�Q�B��B��RB��B��B��B�Q�B��B��RB��RB��B��B�Q�B��B��B��RB��B��B�Q�B��B��B��RB��B��B�Q�B��B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��RB��B��B�Q�B��B��RB��RB��B��B�Q�B��B��RB��B��B��B�Q�B�Q�B��B��RB��B��B��B��B�Q�B��B��B��RB��RB��B��B��B�Q�B�Q�B��B��B��RB��RB��B��B��B�Q�B�Q�B��B��B��RB��RB��B��B��B�Q�B�Q�B��B��B��RB��B��B��B��B�Q�B�Q�B��B��RB��RB��B��B��B�Q�B��B��B��RB��RB��B��B��B�Q�B��B��B��RB��B��B��B�Q�B��B��B��RB��B��B��B�Q�B��B��RB��RB��B��B�Q�B��B��B��RB��B��B�Q�B�Q�B��B��RB��B��B�Q�B�Q�B��B��RB��B��B��B�Q�B��B��RB��B��B��B�Q�B��B��RB��B��B�Q�B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B��B�Q�B��B��RB��B�Q�B��B��RB��B��B�Q�B��B��RB��B�Q�B��B��RB��B��B�Q�B��B��B��B�Q�B��B��RB��B�Q�B��B��RB��B��B�Q�B��RB��B��B�Q�B��B��RB��B�Q�B��B��RB��B��B��B��RB��B��B�Q�B¸RB��B��B�Q�BÅB��B��B�Q�BąB��B��B�Q�BŅBŸRB��B�Q�BƅBƸRB��B�Q�BǅBǸRB��B��B�Q�BȸRB��B��B�Q�BɅBɸRB��B��B�Q�BʅB��B��B��B�Q�B˅B˸RB��B��B�Q�B̅B̸RB��B��B�Q�BͅC=��C=��C=��C=��C=��C=��C=��C=��C=��C=��C=��C=��C=�)C=�)C=�)C=�)C=�)C=C=C=C=��C=��C=��C=�]C=�]C=�]C=u�C=u�C=\)C=\)C=B�C=B�C=(�C=(�C=]C<��C<��C<�)C<�)C<C<��C<��C<�]C<u�C<u�C<\)C<B�C<B�C<(�C<]C<]C;��C;�)C;C;��C;��C;�]C;u�C;\)C;B�C;B�C;(�C;]C:��C:�)C:�)C:C:��C:�]C:u�C:\)C:B�C:(�C:]C9��C9�)C9C9��C9�]C9u�C9\)C9B�C9(�C9]C8��C8�)C8C8��C8�]C8u�C8B�C8(�C8]C7��C7�)C7C7��C7�]C7\)C7B�C7(�C7]C6��C6�)C6��C6�]C6u�C6\)C6(�C6]C5��C5�)C5C5�]C5u�C5\)C5B�C5]C4��C4�)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�Q�A�M�A�ZA�ffA�x�AփAփAփAփAփAփAօAօAցAցAփAփAփAփAցAփA�|�A�Q�A�C�A�7LA� �A�VA��AնFA�ĜA�bNA�v�A�1'A�VAȬA�~�A�(�AŅA�ZA�hsA� �A�5?A���A�bA��\A�^5A���A�;dA�{A��HA���A�\)A�E�A��mA�A�A�I�A�v�A��9A�VA��7A���A���A�x�A��;A�\)A��yA�M�A���A�ffA�hsA�M�A�=qA��!A��PA�%A�33A�t�A�jA�-A��A���A��jA�~�A�ȴA��uA��A�"�A��^A�{A�1A�5?A��A��!A�K�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�A�1'AƮAάA��A�l�AԑhA��A�Q�A�~�A�/A�`BAǛ�A��A�?}A���A��A��A�$�Aԡ�AɍPAͩ�A�x�A��AՁA�/A̰!A�dZAͰ!A�A��;A�z�A�hsA�=qA�;dA�E�A�ZA�/A��AǅA�\)A��A��Aˏ\A�33A�-A�"�A��A�oA���A�A��Aé�A��`Aå�A�~�A7A��yA�\)A�z�AΑhA�n�A�ȴA� �A�%Aհ!A�G�A�C�A�A��HA��A��`A�I�Aї�AϼjAԃA�A�A�ȴA�|�A�?}A�^5A�z�A�t�A�^5A�ffA���A�`BA��A�=qA�l�Aե�A��TA�^5A�XA��HAΛ�A�I�A�G�A���A���A�z�A˸RA���A�ZA���AžwA�1'AՕ�A�p�A���A�$�A��#Aɺ^Aϝ�A�ZA��A�+A�bA�VA�I�Aβ-A���A�hsAΧ�A�O�Aɕ�A�A�A�A�5?A˰!A�A�A�ȴAÁA�
=A�
=A��
A�A���A�A�XA�/A��/A���A�VA�M�A�(�A�E�Aʧ�A�z�A�v�A��`A���A�dZA�ƨA�G�A�1'A���A�1'Aˏ\A�O�A�O�A�5?A�?}A�Q�A�\)A�C�AˬA�JA�XA���A�VA��yA���A�  A��mAΙ�A�  A�bNA��A��A��AŰ!A��
A�9XA�E�A��Aɩ�A�ZA��A�{A�E�A�A�A�A�A�ƨA���A�JA�A�7LA���A�%AΑhA��A��A��yA���A�bA���A�oA�K�A�oA�$�A�n�A�"�A�{A�bA�(�A�&�A�(�A�(�A�"�A��mA�+A�+A�$�A�$�A�$�A�&�A�+A�"�A�"�A�"�A�"�A� �A��A��A�"�Aէ�A�$�A� �A��A��A� �A�oA��A��A��A� �A��A�oAգ�A�+A�(�A�+A�&�A�&�A�&�A�+A�$�A� �A� �A�(�A�z�A�+A�33A�(�A� �A�"�A��A�"�A�"�A�"�A� �A��A��A�bA�bA�&�A�+A�-A�+A��A�"�A� �A�7LA�=qA�(�A�oA��A�oA�JA��A�VA�+A�33A�$�A��A��A��A�"�A�$�A��A��A��A��A��A�oA��A�bA�{A�JA��A��A��A��A��A��A��A�{A���A��A�"�A� �A�"�A��A�"�A�&�A� �A��A� �A��A��A�$�A��A�(�A��A�{A�+A�(�A�-A�1'A�/A�VA�I�A�1'A�1'A�33A�/A�+A�/A�/A�5?A�33A�33A�=qA�;dA�;dA�?}A�=qA�;dA�&�A�;dA�7LA�$�A�;dA�=qA�7LA�-A�&�A�&�A�"�A��A��A�"�A�+A�(�A� �A� �A�+A�/A�/A�/A�-A�/A�-A�-A� �A�&�A�1'A�33A�/A�-A�(�A�"�A�/A�+A�1'A�-A�&�A�"�A�&�A�&�A�$�A�"�A�1'A�1'A�1'A�33A�33A�5?A�7LA�33A�33A�-A�7LA�5?A�33A�1'A�/A�33A�33A�1'A�33A�/A�/A�(�A�33A�33A�33A�1'A�33A�5?A�1'A�-A�5?A�7LA�5?A�(�A�-A�7LA�/A�+A�(�A�/A�1'A�7LA�9XA�;dA�;dA�33A�7LA�5?A�/A�5?A�?}A�A�A�G�A�?}A�A�A�G�A�G�A�C�A�5?A�M�A�M�A�Q�A�Q�A�Q�A�Q�A�Q�A�M�A�I�A�K�A�K�A�M�A�O�A�K�A�I�A�K�A�I�A�G�A�K�A�K�A�M�A�I�A�O�A�O�A�M�A�O�A�I�A�M�A�O�A�O�A�O�A�M�A�O�A�O�A�K�A�M�A�M�A�M�A�K�A�M�A�K�A�K�A�M�A�K�A�K�A�M�A�O�A�O�A�Q�A�O�A�M�A�O�A�M�A�K�A�M�A�Q�A�O�A�K�A�K�A�K�A�K�A�K�A�G�A�E�A�I�A�I�A�K�A�M�A�M�A�M�A�M�A�M�A�G�A�Q�A�VA�VA�S�A�ZA�\)A�`BA�^5A�^5A�\)A�XA�ZA�S�A�S�A�VA�VA�XA�n�A�z�A�dZA�ffA�dZA�ffA�jA�bNA�bNA�dZA�dZA�`BA�ffA�`BA�jA�v�A�t�A�t�A�r�A�t�A�v�A�z�A�|�A�~�A�~�A�~�AցA�~�A�~�A�~�A�~�AցA�~�AցAցAփAցAցAցAցAցAցAցAցAցAցAցAցAցAցAցA�~�AցAցAցAցAցA�~�A�~�AցAցAցAցAցAփAցAցAցAցAցAցAփA�~�AփAփAցA�~�A�~�AցAցAցAցA�~�A�~�A�~�A�~�AցAցAցAցA�~�AցAցAցA�~�AցA�~�A�~�A�~�AցAցAցA�~�A�~�A�~�AցAցAփAցAցAփAփAցAփAփAփAփAփAցAփAցAփAփAփAփAփAցAցAփAփA�~�A�~�AցA�~�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�|�A�~�A�|�A�~�AցA�~�AցAցA�~�A�~�AցAցAցA�~�A�~�A�~�AցAցAցA�~�AցAցA�~�AցA�~�A�~�A�~�A�~�AցAցA�~�AցA�~�A�~�AցAցAցA�~�AցA�~�AցAցAփAցAցAփAփAփA�~�AցAփAփAցAփA�~�AցAցAփAցAցAցAցAցA�~�A�~�AցA�~�A�~�A�~�AցAցAցA�~�A�~�A�~�A�~�AցAցAցAցA�~�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�~�A�~�AցA�~�AցAցAցAցAցAցAցA�~�AցAցAցAցAցAցA�~�AցAցA�~�AցAցAցAցA�~�AցAցA�~�AցA�~�A�~�A�|�A�|�A�z�A�x�A�x�A�z�A�z�A�x�A�x�A�v�A�x�A�x�A�x�A�x�A�r�A�v�A�x�A�r�A�n�A�n�A�n�A�n�A�ffA�^5A�M�A�I�A�G�A�I�A�E�A�C�A�I�A�G�A�G�A�E�A�E�A�?}A�A�A�?}A�C�A�A�A�C�A�?}A�A�A�E�A�C�A�C�A�E�A�C�A�A�A�A�A�C�A�?}A�E�A�G�A�E�A�E�A�=qA�A�A�?}A�C�A�A�A�9XA�?}A�;dA�9XA�;dA�9XA�?}A�;dA�;dA�=qA�;dA�9XA�9XA�7LA�7LA�5?A�7LA�9XA�33A�1'A�5?A�5?A�1'A�5?A�33A�33A�/A�/A�33A�/A�1'A�-A�/A�33A�7LA�5?A�1'A�33A�/A��A��A��A��A��A�&�A�$�A�$�A�+A�&�A�&�A�&�A�+A��A� �A��A�{A�&�A�&�A�+A��A��A�oA�oA�oA�oA�oA�oA�bA�{A�oA�{A�oA�{A�oA�VA�bA�bA�bA�oA�oA�bA�bA�VA�VA�
=A�
=A�VA�VA�
=A�
=A�%A�A�1A�1A�A�  A�  A���A�  A�  A�A�  A�  A���A���A�  A���A���A���A���A���A��A���A���A��A��A��A��TA��`A��TA��#A��;A��/A��#A��/A��TA��A��
A���A��
A���A���A���A���A��
A���A���A���A���A���A�ƨA�ȴA���A�ĜA�A�A���AռjAռjAռjAպ^AնFAղ-Aէ�AլAլAթ�Aէ�Aե�Aե�Aե�Aե�A՛�Aՙ�AՕ�AՓuAՕ�A՗�AՕ�AՓuAՕ�AՍPAՁA�r�A�n�A�\)A�;dA�1A���A��#A�ƨAԼjAԲ-Aԗ�A�v�A�r�A�dZA�\)A�G�A�E�A�/A�-A�+A� �A�bA�1A�  A���A���A���A���A���A���A��A��A��HA��;A��#A��
A��#A��
A��A��
A��;A��
A��A��
A��A��
A��
A��
A��
A��
A��
A���A��A��
A��
A��A��#A��A��#A��
A��#A��A��
A��
A��#A��A��#A��
A��#A��A��#A��/A��#A��A��A��#A��A��A��
A��
A���A��#A��#A��
A��#A��#A��#A��/A��HA��HA���A���A���A���A���A��#A��A��A��A��
A��A��A��A��
A���A��A���A��/A��/A��/A��HA��;A��HA��;A��HA��HA��;A��;A��/A��;A��;A��;A��
A��#A��TA��`A��TA��/A���Aӧ�AӬAӏ\A�`BAӇ+Aӟ�AӓuA�hsA���A��
A�z�A��/A��`A��mA��HA��#A��A��A��
A���A���A�ȴA�ƨA�ȴAӶFAӬAӥ�A�~�AӉ7Aӛ�AӁA�ZA�C�A�JA�1A��HA��A��A���A�AҸRAҾwAғuA�hsA�33A���AѶFAѥ�Aѝ�A�t�A�oA��#A�ȴA��
A���A�Aк^A�~�A�33A��Aϲ-A�x�A�?}A�JA�A���A��/A��`A���A�Aκ^AάAΕ�A�r�A�Q�A�9XA�"�A�  A��;A���Aͣ�A͏\ÁA�n�A�S�A�1'A�JA��A���A̋DA�|�A�x�A�l�A�jA�jA�ffA�ffA�dZA�ffA�ffA�dZA�dZA�ffA�dZA�dZA�dZA�dZA�`BA�bNA�XA�VA�S�A�S�A�S�A�S�A�Q�A�O�A�K�A�9XA�G�A�E�A�;dA�"�A�/A�{A�VA�oA�oA�%A�
=A�  A���A�A���A�A�%A�%A�1A�%A�JA�VA�bA�oA�bA�JA�1A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��yA��yA��A��mA��`A��mA��yA��A��`A��/A��
A���A���A���A�ȴA�ȴA�ƨA˸RAˬAˡ�A��A�JA���AʶFAʸRAʗ�Aʇ+Aʉ7A�\)A�=qA�-A�1Aɴ9AɃA�hsA�1'A�"�A�"�A�{A���A��yA��`A��TA��/A���AȺ^Aȴ9AȸRAȩ�A�~�A�S�A�bNA�l�A�bA�1A���A���A���A���A���A�A�  A�A��TA��;AǮAǲ-Aǝ�AǅAǉ7AǇ+AǁAǁAǁAǁAǁAǁA�~�AǁA�~�AǁA�x�A�v�A�t�A�x�A�t�A�p�A�r�A�n�A�XA�Q�A�O�A�^5A�`BA�ZA�?}A�?}A�7LA�33A�;dA�+A�7LA�+A�"�A�&�A�%A���A�1A���A�A��A��`A���A���A���A��
AƧ�AƁA�7LA��Aź^Aş�AőhA�v�A�n�A�ffA�^5A�\)A�M�A�9XA�"�A�oA�1A�  A���A��A��yA��
AĶFAģ�Aĉ7A�r�A�n�A�jA�bNA�\)A�S�A�M�A�=qA�+A�(�A�(�A�(�A� �A� �A� �A��A��A��A�{A�oA�JA�
=A�A���A���A��A���AöFAé�A×�AÇ+A�^5A�33A�bA�ȴA�AA�ZA�1A��;A��DA�I�A�$�A�%A��mA���A��-A��A���A�~�A�jA�\)A�ZA�VA�Q�A��A��A�VA��yA��9A��hA��PA�jA�A�A��A��A�A��PA�M�A�33A�+A�&�A�(�A�(�A�+A�+A�+A�+A�+A�+A�-A�-A�-A�1'A�1'A�33A�5?A�9XA�9XA�9XA�9XA�9XA�=qA�=qA�?}A�9XA�7LA�5?A�5?A�9XA�;dA�=qA�7LA�"�A� �A�bA�{A�%A��;A��A��/A���A�ȴA���A���A���A���A���A��FA�ƨA���A��+A��7A��DA��A��A�r�A�7LA��A�
=A��
A��FA���A�z�A�hsA�dZA�bNA�XA�?}A�-A�$�A��A���A��A��mA��HA��;A��/A��#A���A���A���A���A���A���A���A���A�ȴA�ĜA�ĜA�A��jA��^A��^A��9A���A���A��DA��A�z�A�l�A�hsA�ffA�`BA�Q�A�/A���A��jA�n�A�/A���A�ĜA�t�A�K�A�7LA�"�A��A�1A���A���A���A��mA��/A��
A���A���A�ĜA���A��^A��A���A���A���A���A���A���A��uA��+A�x�A�hsA�ZA�I�A�E�A�C�A�C�A�A�A�C�A�;dA�=qA�7LA�5?A�33A�5?A�33A�33A�33A�33A�33A�33A�1'A�-A� �A��A��A��A��A�oA�VA�JA�JA�JA�1A�%A�A���A���A���A��TA��#A��A��#A��A��#A��/A��/A��/A��;A��;A��;A��;A��;A��HA��;A��;A��HA��`A��mA��yA��HA���A���A���A��7A��7A��A�z�A�x�A�dZA�dZA�bNA�dZA�dZA�bNA�
=A�oA��A��A�
=A��A��A��A�
=A�A�%A�A�  A�A�1A�
=A��A�7LA�S�A�S�A�O�A�K�A�Q�A�S�A�ffA�l�A�p�A�l�A�r�A�p�A�x�A��A�~�A��PA��DA��PA��A�x�A��A��PA��A��PA��uA��\A�z�A�|�A��DA��7A���A��PA��DA���A���A���A���A���A���A��DA��PA��7A��PA��A���A��9A��-A���A��jA���A��RA��FA�ƨA���A�ĜA�ƨA�ĜA��wA��jA��
A��;A��A��
A��HA��A���A���A���A��HA��HA��
A��
A��A��/A��A��A��A��yA��A���A���A�JA��A�oA�%A���A�A���A��A�5?A��A�=qA�oA�7LA�E�A�dZA�l�A��+A��DA��uA��7A���A�S�A�M�A�Q�A�M�A�Q�A�M�A�O�A�O�A�S�A�K�A�K�A�M�A�M�A�K�A�G�A�I�A�M�A�M�A�M�A�O�A�O�A�O�A�M�A�K�A�Q�A�^5A�^5A�VA�XA�XA�^5A�`BA�bNA�^5A�dZA�^5A�ZA�XA�XA�VA�ZA�z�A�p�A�hsA�ffA�ffA�r�A�dZA�bNA�dZA�ffA�^5A�bNA�jA�^5A�ffA�p�A�|�A�r�A�p�A�x�A�v�A�|�A�~�AցAփAփAփAցAցAցAփAցAփAցAփAփAփAցAցAօAփAօAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAօAփAփAփAփAօAօAփAօAօAօAցAցAցAցAփAփAփAփAցAփAցAփAփAցAփAփAփAփAփAփAցAփAցAցAփAօAօAցAցAփAփAփAփAփAփAփAփAօAօAփAօAօAօAօAփAօAօAփAօAօAօAօAօAօAօAփAցAցAցA�~�AցAցA�~�AցAցAցA�~�AցAցAցAցAցA�~�AցAցAցA�~�AցAցAփAփAցAցAցAփAփAփAփAփAցAփAցAփAցAփAփAցAցAցAփAցAցAփAփAցAփAփAցAցAփAփAփAփAօAօAօAօAօAփAօAօAցAցAցAփAօAօAօAցAցAփAցAցAցAցAփAփAփAցAփAփAցAփAցAցAփAփAցAցAցAփAցAցA�~�AցA�|�AցAցAցAցAփAցAցAփAցAփAփAփAփAփAփAփAցAփAփAփAփAփAցAփAցAփAփAցAօAփAցAփAփAփAփAցAփAփAցAցAցAցA�z�A�z�A�z�A�|�A�|�A�x�A�z�A�x�A�z�A�x�A�x�A�z�A�z�A�x�A�x�A�x�A�r�A�r�A�n�A�p�A�ZA�M�A�M�A�I�A�I�A�I�A�K�A�K�A�G�A�G�A�E�A�E�A�G�A�C�A�?}A�A�A�E�A�E�A�E�A�A�A�A�A�G�A�I�A�E�A�E�A�E�A�G�A�E�A�C�A�G�A�G�A�G�A�G�A�G�A�E�A�C�A�?}A�=qA�;dA�=qA�A�A�A�A�A�A�A�A�?}A�A�A�=qA�;dA�=qA�9XA�9XA�9XA�9XA�;dA�9XA�9XA�;dA�9XA�5?A�5?A�7LA�9XA�5?A�9XA�9XA�7LA�7LA�9XA�9XA�5?A�1'A�5?A�5?A�7LA�/A�7LA�1'A�"�A��A��A��A��A��A��A�&�A�&�A�$�A�&�A�$�A�&�A�+A�+A��A� �A�-A�/A�$�A�5?A�9XA�+A��A�oA�{A��A��A�{A�{A�{A�{A��A��A��A�{A��A�{A�{A�{A�{A�{A�{A��A�{A�{A�oA�oA�oA�oA�oA�bA�bA�VA�bA�bA�VA�
=A�A�A�A�A�A�A�A�%A�1A�A�A�A�%A�A�A�A�A���A�A���A�  A���A��A��A��yA��`A��mA��mA��`A��`A��`A��mA��`A��HA��#A��A��
A��
A��
A��A��A���A���A���A���A���A���A���A���A�ƨA�A���AվwAվwAվwAպ^AոRAնFAղ-Aհ!AծAծAթ�Aթ�Aէ�Aե�Aգ�Aե�A՟�A՟�A՝�A՟�A՗�A՗�A՗�AՑhAՇ+A�~�A�v�A�r�A�bNA�E�A�{A��A��#A�ƨAԼjAԴ9Aԗ�A�|�A�t�A�dZA�ZA�I�A�G�A�7LA�/A�-A�"�A�oA�JA���A���A���A���A���A���A���A��A��A��A��HA��;A��/A��#A��/A��TA��HA��HA��/A��/A��/A��HA��#A��#A��#A��/A��;A��;A��A��#A��#A��#A��/A��;A��HA��;A��;A��/A��/A��HA��;A��;A��HA��;A��;A��;A��HA��HA��;A��HA��TA��HA��HA��HA��;A��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��`A��TA��`A��TA��`A��;A��HA��HA��HA��HA��HA��;A��HA��`A��`A��`A��mA��mA��yA��mA��mA��yA��mA��A��A��A��A��A��A��A��A��A���A���A���A��A��A��A���A���A��A��A��A��A��yA��yA��`A��HA��`A��`A��mA��yA��yA��yA��yA��yA��A��HA��/A��#A��A��A��A��
A���A��
A���A�ȴA�ĜAӾwAӬAӅA�r�A�S�A�/A���A��`A���A�ƨA�ĜA�ĜAҴ9AҮAҋDA�33A��HA���AѮAћ�Aщ7A�1'A��A��;A��
A���A���AУ�A�hsA��A��Aϛ�A�^5A�7LA� �A�%A���A��A��HA���A�AμjAΩ�AΏ\A�jA�K�A�5?A��A���A��
Aͺ^A͛�A͋DA�~�A�l�A�O�A�&�A�  A��;Ḁ�A�~�A�|�A�x�A�p�A�l�A�hsA�hsA�hsA�ffA�ffA�hsA�hsA�jA�hsA�ffA�ffA�dZA�`BA�`BA�^5A�\)A�XA�ZA�VA�VA�VA�S�A�Q�A�Q�A�VA�XA�Q�A�O�A�K�A�M�A�I�A�G�A�G�A�E�A�A�A�=qA�7LA�5?A�1'A�+A�$�A� �A��A��A��A��A��A��A��A��A��A��A��A�{A�{A��A��A��A��A��A��A��A�oA���A���A���A���A���A���A���A���A��A��A��A��A��A��yA��yA��yA��yA��yA��yA��A��mA��TA��#A��A���A���A���A�ȴA�ĜA˾wA˺^A˰!A˥�A˝�A˛�A˟�A˟�A˛�AˍPA�r�A�7LA��
A�`BA���Aɛ�A�z�A�ZA�A�A�VA���A��yA��yA��A��mA��HA��AȼjAȇ+A�x�Aȉ7A�n�A�+A�1'A�VA�A�1A�A�VA�VA�1A�A���A��A��yA���A���AǑhAǑhAǑhAǋDAǅAǅAǃAǃAǃAǅAǃAǁAǁAǃAǃA�z�A�z�A�z�A�z�A�z�A�|�A�|�A�|�A�~�A�x�A�t�A�v�A�l�A�jA�bNA�XA�O�A�K�A�I�A�K�A�=qA�;dA�9XA�"�A��A�"�A��A��A�{A���A��HA���A���AƶFAƗ�A�l�A�{A���Aũ�Aŕ�Aŉ7A�v�A�jA�dZA�`BA�VA�C�A�1'A��A�VA�%A�  A���A��A��yA���AĲ-Aě�AăA�v�A�r�A�jA�ffA�\)A�Q�A�I�A�=qA�/A�-A�+A�$�A�$�A�"�A� �A��A��A��A�{A�bA�JA�1A�A���A��A��AüjAð!AÛ�AÍPA�hsA�?}A��A��A¥�APA�bNA��A��A��A�\)A�5?A�{A���A��TA���A��wA��!A���A�x�A�jA�`BA�\)A�ZA�I�A��A��A�VA��/A��^A���A��PA�dZA�A�A��A�1A��#A��hA�O�A�5?A�-A�+A�+A�+A�-A�+A�-A�-A�-A�-A�/A�1'A�/A�33A�1'A�5?A�7LA�9XA�=qA�;dA�;dA�=qA�?}A�A�A�=qA�9XA�7LA�7LA�9XA�=qA�?}A�=qA�9XA�5?A�1'A�/A�1'A�/A�(�A��A�{A�VA�VA�VA�bA�JA���A��A��TA��HA��A�ƨA���A���A���A��A�hsA�?}A��A��;A��RA���A�|�A�l�A�ffA�dZA�ZA�A�A�1'A�(�A��A�A��A��yA��TA��;A��;A��/A��A��
A���A���A���A���A���A���A���A�ƨA�ƨA�A���A��wA��jA��FA���A���A��DA��A�z�A�n�A�l�A�hsA�\)A�I�A�(�A�A�A�l�A�+A��A��!A�ffA�G�A�5?A��A��A�{A�%A���A���A��A��;A��#A���A���A�ƨA��wA��RA��A���A���A���A���A���A���A��uA��A�v�A�ffA�XA�K�A�G�A�E�A�G�A�E�A�A�A�?}A�=qA�9XA�9XA�7LA�7LA�5?A�7LA�7LA�5?A�5?A�5?A�1'A�33A�/A� �A��A��A��A�oA�bA�VA�bA�VA�JA�1A�A�A���A���A��mA��;A��/A��#A��/A��/A��/A��;A��HA��HA��TA��TA��TA��TA��TA��TA��TA��`A��yA��yA��A��TA���A��!A���A��uA��\A��DA��+A��A�t�A�hsA�dZA�dZA�hsA�  A�A�A�%A�1A�A�1A�1A�1A�
=A�1A�
=A�$�A��A�/A�C�A�K�A�K�A�M�A�Q�A�K�A�ZA�hsA�hsA�n�A�l�A�jA�jA�l�A�p�A�r�A�v�A�x�A�z�A�~�A�~�A��A��A��A��A��7A��+A��DA��PA��PA��DA��DA��PA��7A��DA��uA���A���A���A���A���A���A���A���A���A��A���A��!A��FA��-A��^A��^A��wA�A��jA���A�ƨA�ĜA�ĜA�A�ȴA�ƨA���A���A���A��
A��#A��/A��A��#A��HA��/A��/A��HA��mA��TA��;A��A��mA��A��A��A���A���A���A���A�  A���A�JA�JA�{A�oA� �A�$�A�?}A�A�A�I�A�dZA�v�A��A��DA���A���A��!A�G�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�A�Q�A�M�A�ZA�ffA�x�AփAփAփAփAփAփAօAօAցAցAփAփAփAփAցAփA�|�A�Q�A�C�A�7LA� �A�VA��AնFA�ĜA�bNA�v�A�1'A�VAȬA�~�A�(�AŅA�ZA�hsA� �A�5?A���A�bA��\A�^5A���A�;dA�{A��HA���A�\)A�E�A��mA�A�A�I�A�v�A��9A�VA��7A���A���A�x�A��;A�\)A��yA�M�A���A�ffA�hsA�M�A�=qA��!A��PA�%A�33A�t�A�jA�-A��A���A��jA�~�A�ȴA��uA��A�"�A��^A�{A�1A�5?A��A��!A�K�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�A�1'AƮAάA��A�l�AԑhA��A�Q�A�~�A�/A�`BAǛ�A��A�?}A���A��A��A�$�Aԡ�AɍPAͩ�A�x�A��AՁA�/A̰!A�dZAͰ!A�A��;A�z�A�hsA�=qA�;dA�E�A�ZA�/A��AǅA�\)A��A��Aˏ\A�33A�-A�"�A��A�oA���A�A��Aé�A��`Aå�A�~�A7A��yA�\)A�z�AΑhA�n�A�ȴA� �A�%Aհ!A�G�A�C�A�A��HA��A��`A�I�Aї�AϼjAԃA�A�A�ȴA�|�A�?}A�^5A�z�A�t�A�^5A�ffA���A�`BA��A�=qA�l�Aե�A��TA�^5A�XA��HAΛ�A�I�A�G�A���A���A�z�A˸RA���A�ZA���AžwA�1'AՕ�A�p�A���A�$�A��#Aɺ^Aϝ�A�ZA��A�+A�bA�VA�I�Aβ-A���A�hsAΧ�A�O�Aɕ�A�A�A�A�5?A˰!A�A�A�ȴAÁA�
=A�
=A��
A�A���A�A�XA�/A��/A���A�VA�M�A�(�A�E�Aʧ�A�z�A�v�A��`A���A�dZA�ƨA�G�A�1'A���A�1'Aˏ\A�O�A�O�A�5?A�?}A�Q�A�\)A�C�AˬA�JA�XA���A�VA��yA���A�  A��mAΙ�A�  A�bNA��A��A��AŰ!A��
A�9XA�E�A��Aɩ�A�ZA��A�{A�E�A�A�A�A�A�ƨA���A�JA�A�7LA���A�%AΑhA��A��A��yA���A�bA���A�oA�K�A�oA�$�A�n�A�"�A�{A�bA�(�A�&�A�(�A�(�A�"�A��mA�+A�+A�$�A�$�A�$�A�&�A�+A�"�A�"�A�"�A�"�A� �A��A��A�"�Aէ�A�$�A� �A��A��A� �A�oA��A��A��A� �A��A�oAգ�A�+A�(�A�+A�&�A�&�A�&�A�+A�$�A� �A� �A�(�A�z�A�+A�33A�(�A� �A�"�A��A�"�A�"�A�"�A� �A��A��A�bA�bA�&�A�+A�-A�+A��A�"�A� �A�7LA�=qA�(�A�oA��A�oA�JA��A�VA�+A�33A�$�A��A��A��A�"�A�$�A��A��A��A��A��A�oA��A�bA�{A�JA��A��A��A��A��A��A��A�{A���A��A�"�A� �A�"�A��A�"�A�&�A� �A��A� �A��A��A�$�A��A�(�A��A�{A�+A�(�A�-A�1'A�/A�VA�I�A�1'A�1'A�33A�/A�+A�/A�/A�5?A�33A�33A�=qA�;dA�;dA�?}A�=qA�;dA�&�A�;dA�7LA�$�A�;dA�=qA�7LA�-A�&�A�&�A�"�A��A��A�"�A�+A�(�A� �A� �A�+A�/A�/A�/A�-A�/A�-A�-A� �A�&�A�1'A�33A�/A�-A�(�A�"�A�/A�+A�1'A�-A�&�A�"�A�&�A�&�A�$�A�"�A�1'A�1'A�1'A�33A�33A�5?A�7LA�33A�33A�-A�7LA�5?A�33A�1'A�/A�33A�33A�1'A�33A�/A�/A�(�A�33A�33A�33A�1'A�33A�5?A�1'A�-A�5?A�7LA�5?A�(�A�-A�7LA�/A�+A�(�A�/A�1'A�7LA�9XA�;dA�;dA�33A�7LA�5?A�/A�5?A�?}A�A�A�G�A�?}A�A�A�G�A�G�A�C�A�5?A�M�A�M�A�Q�A�Q�A�Q�A�Q�A�Q�A�M�A�I�A�K�A�K�A�M�A�O�A�K�A�I�A�K�A�I�A�G�A�K�A�K�A�M�A�I�A�O�A�O�A�M�A�O�A�I�A�M�A�O�A�O�A�O�A�M�A�O�A�O�A�K�A�M�A�M�A�M�A�K�A�M�A�K�A�K�A�M�A�K�A�K�A�M�A�O�A�O�A�Q�A�S�A�M�A�Q�A�M�A�Q�A�M�A�O�A�O�A�S�A�K�A�K�A�M�A�M�A�K�A�G�A�I�A�M�A�M�A�M�A�O�A�O�A�O�A�M�A�K�A�Q�A�^5A�^5A�VA�XA�XA�^5A�`BA�bNA�^5A�dZA�^5A�ZA�XA�XA�VA�ZA�z�A�p�A�hsA�ffA�ffA�r�A�dZA�bNA�dZA�ffA�^5A�bNA�jA�^5A�ffA�p�A�|�A�r�A�p�A�x�A�v�A�|�A�~�AցAփAփAփAցAցAցAփAցAփAցAփAփAփAցAցAօAփAօAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAօAփAփAփAփAօAօAփAօAօAօAցAցAցAցAփAփAփAփAցAփAցAփAփAցAփAփAփAփAփAփAցAփAցAցAփAօAօAցAցAփAփAփAփAփAփAփAփAօAօAփAօAօAօAօAփAօAօAփAօAօAօAօAօAօAօAփAցAցAցA�~�AցAցA�~�AցAցAցA�~�AցAցAցAցAցA�~�AցAցAցA�~�AցAցAփAփAցAցAցAփAփAփAփAփAցAփAցAփAցAփAփAցAցAցAփAցAցAփAփAցAփAփAցAցAփAփAփAփAօAօAօAօAօAփAօAօAցAցAցAփAօAօAօAցAցAփAցAցAցAցAփAփAփAցAփAփAցAփAցAցAփAփAցAցAցAփAցAցA�~�AցA�|�AցAցAցAցAփAցAցAփAցAփAփAփAփAփAփAփAցAփAփAփAփAփAցAփAցAփAփAցAօAփAցAփAփAփAփAցAփAփAցAցAցAցA�z�A�z�A�z�A�|�A�|�A�x�A�z�A�x�A�z�A�x�A�x�A�z�A�z�A�x�A�x�A�x�A�r�A�r�A�n�A�p�A�ZA�M�A�M�A�I�A�I�A�I�A�K�A�K�A�G�A�G�A�E�A�E�A�G�A�C�A�?}A�A�A�E�A�E�A�E�A�A�A�A�A�G�A�I�A�E�A�E�A�E�A�G�A�E�A�C�A�G�A�G�A�G�A�G�A�G�A�E�A�C�A�?}A�=qA�;dA�=qA�A�A�A�A�A�A�A�A�?}A�A�A�=qA�;dA�=qA�9XA�9XA�9XA�9XA�;dA�9XA�9XA�;dA�9XA�5?A�5?A�7LA�9XA�5?A�9XA�9XA�7LA�7LA�9XA�9XA�5?A�1'A�5?A�5?A�7LA�/A�7LA�1'A�"�A��A��A��A��A��A��A�&�A�&�A�$�A�&�A�$�A�&�A�+A�+A��A� �A�-A�/A�$�A�5?A�9XA�+A��A�oA�{A��A��A�{A�{A�{A�{A��A��A��A�{A��A�{A�{A�{A�{A�{A�{A��A�{A�{A�oA�oA�oA�oA�oA�bA�bA�VA�bA�bA�VA�
=A�A�A�A�A�A�A�A�%A�1A�A�A�A�%A�A�A�A�A���A�A���A�  A���A��A��A��yA��`A��mA��mA��`A��`A��`A��mA��`A��HA��#A��A��
A��
A��
A��A��A���A���A���A���A���A���A���A���A�ƨA�A���AվwAվwAվwAպ^AոRAնFAղ-Aհ!AծAծAթ�Aթ�Aէ�Aե�Aգ�Aե�A՟�A՟�A՝�A՟�A՗�A՗�A՗�AՑhAՇ+A�~�A�v�A�r�A�bNA�E�A�{A��A��#A�ƨAԼjAԴ9Aԗ�A�|�A�t�A�dZA�ZA�I�A�G�A�7LA�/A�-A�"�A�oA�JA���A���A���A���A���A���A���A��A��A��A��HA��;A��/A��#A��/A��TA��HA��HA��/A��/A��/A��HA��#A��#A��#A��/A��;A��;A��A��#A��#A��#A��/A��;A��HA��;A��;A��/A��/A��HA��;A��;A��HA��;A��;A��;A��HA��HA��;A��HA��TA��HA��HA��HA��;A��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��`A��TA��`A��TA��`A��;A��HA��HA��HA��HA��HA��;A��HA��`A��`A��`A��mA��mA��yA��mA��mA��yA��mA��A��A��A��A��A��A��A��A��A���A���A���A��A��A��A���A���A��A��A��A��A��yA��yA��`A��HA��`A��`A��mA��yA��yA��yA��yA��yA��A��HA��/A��#A��A��A��A��
A���A��
A���A�ȴA�ĜAӾwAӬAӅA�r�A�S�A�/A���A��`A���A�ƨA�ĜA�ĜAҴ9AҮAҋDA�33A��HA���AѮAћ�Aщ7A�1'A��A��;A��
A���A���AУ�A�hsA��A��Aϛ�A�^5A�7LA� �A�%A���A��A��HA���A�AμjAΩ�AΏ\A�jA�K�A�5?A��A���A��
Aͺ^A͛�A͋DA�~�A�l�A�O�A�&�A�  A��;Ḁ�A�~�A�|�A�x�A�p�A�l�A�hsA�hsA�hsA�ffA�ffA�hsA�hsA�jA�hsA�ffA�ffA�dZA�`BA�`BA�^5A�\)A�XA�ZA�VA�VA�VA�S�A�Q�A�Q�A�VA�XA�Q�A�O�A�K�A�M�A�I�A�G�A�G�A�E�A�A�A�=qA�7LA�5?A�1'A�+A�$�A� �A��A��A��A��A��A��A��A��A��A��A��A�{A�{A��A��A��A��A��A��A��A�oA���A���A���A���A���A���A���A���A��A��A��A��A��A��yA��yA��yA��yA��yA��yA��A��mA��TA��#A��A���A���A���A�ȴA�ĜA˾wA˺^A˰!A˥�A˝�A˛�A˟�A˟�A˛�AˍPA�r�A�7LA��
A�`BA���Aɛ�A�z�A�ZA�A�A�VA���A��yA��yA��A��mA��HA��AȼjAȇ+A�x�Aȉ7A�n�A�+A�1'A�VA�A�1A�A�VA�VA�1A�A���A��A��yA���A���AǑhAǑhAǑhAǋDAǅAǅAǃAǃAǃAǅAǃAǁAǁAǃAǃA�z�A�z�A�z�A�z�A�z�A�|�A�|�A�|�A�~�A�x�A�t�A�v�A�l�A�jA�bNA�XA�O�A�K�A�I�A�K�A�=qA�;dA�9XA�"�A��A�"�A��A��A�{A���A��HA���A���AƶFAƗ�A�l�A�{A���Aũ�Aŕ�Aŉ7A�v�A�jA�dZA�`BA�VA�C�A�1'A��A�VA�%A�  A���A��A��yA���AĲ-Aě�AăA�v�A�r�A�jA�ffA�\)A�Q�A�I�A�=qA�/A�-A�+A�$�A�$�A�"�A� �A��A��A��A�{A�bA�JA�1A�A���A��A��AüjAð!AÛ�AÍPA�hsA�?}A��A��A¥�APA�bNA��A��A��A�\)A�5?A�{A���A��TA���A��wA��!A���A�x�A�jA�`BA�\)A�ZA�I�A��A��A�VA��/A��^A���A��PA�dZA�A�A��A�1A��#A��hA�O�A�5?A�-A�+A�+A�+A�-A�+A�-A�-A�-A�-A�/A�1'A�/A�33A�1'A�5?A�7LA�9XA�=qA�;dA�;dA�=qA�?}A�A�A�=qA�9XA�7LA�7LA�9XA�=qA�?}A�=qA�9XA�5?A�1'A�/A�1'A�/A�(�A��A�{A�VA�VA�VA�bA�JA���A��A��TA��HA��A�ƨA���A���A���A��A�hsA�?}A��A��;A��RA���A�|�A�l�A�ffA�dZA�ZA�A�A�1'A�(�A��A�A��A��yA��TA��;A��;A��/A��A��
A���A���A���A���A���A���A���A�ƨA�ƨA�A���A��wA��jA��FA���A���A��DA��A�z�A�n�A�l�A�hsA�\)A�I�A�(�A�A�A�l�A�+A��A��!A�ffA�G�A�5?A��A��A�{A�%A���A���A��A��;A��#A���A���A�ƨA��wA��RA��A���A���A���A���A���A���A��uA��A�v�A�ffA�XA�K�A�G�A�E�A�G�A�E�A�A�A�?}A�=qA�9XA�9XA�7LA�7LA�5?A�7LA�7LA�5?A�5?A�5?A�1'A�33A�/A� �A��A��A��A�oA�bA�VA�bA�VA�JA�1A�A�A���A���A��mA��;A��/A��#A��/A��/A��/A��;A��HA��HA��TA��TA��TA��TA��TA��TA��TA��`A��yA��yA��A��TA���A��!A���A��uA��\A��DA��+A��A�t�A�hsA�dZA�dZA�hsA�  A�A�A�%A�1A�A�1A�1A�1A�
=A�1A�
=A�$�A��A�/A�C�A�K�A�K�A�M�A�Q�A�K�A�ZA�hsA�hsA�n�A�l�A�jA�jA�l�A�p�A�r�A�v�A�x�A�z�A�~�A�~�A��A��A��A��A��7A��+A��DA��PA��PA��DA��DA��PA��7A��DA��uA���A���A���A���A���A���A���A���A���A��A���A��!A��FA��-A��^A��^A��wA�A��jA���A�ƨA�ĜA�ĜA�A�ȴA�ƨA���A���A���A��
A��#A��/A��A��#A��HA��/A��/A��HA��mA��TA��;A��A��mA��A��A��A���A���A���A���A�  A���A�JA�JA�{A�oA� �A�$�A�?}A�A�A�I�A�dZA�v�A��A��DA���A���A��!A�G�A�S�A�M�A�Q�A�M�A�Q�A�M�A�O�A�O�A�S�A�K�A�K�A�M�A�M�A�K�A�G�A�I�A�M�A�M�A�M�A�O�A�O�A�O�A�M�A�K�A�Q�A�^5A�^5A�VA�XA�XA�^5A�`BA�bNA�^5A�dZA�^5A�ZA�XA�XA�VA�ZA�z�A�p�A�hsA�ffA�ffA�r�A�dZA�bNA�dZA�ffA�^5A�bNA�jA�^5A�ffA�p�A�|�A�r�A�p�A�x�A�v�A�|�A�~�AցAփAփAփAցAցAցAփAցAփAցAփAփAփAցAցAօAփAօAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAփAօAփAփAփAփAօAօAփAօAօAօAցAցAցAցAփAփAփAփAցAփAցAփAփAցAփAփAփAփAփAփAցAփAցAցAփAօAօAցAցAփAփAփAփAփAփAփAփAօAօAփAօAօAօAօAփAօAօAփAօAօAօAօAօAօAօAփAցAցAցA�~�AցAցA�~�AցAցAցA�~�AցAցAցAցAցA�~�AցAցAցA�~�AցAցAփAփAցAցAցAփAփAփAփAփAցAփAցAփAցAփAփAցAցAցAփAցAցAփAփAցAփAփAցAցAփAփAփAփAօAօAօAօAօAփAօAօAցAցAցAփAօAօAօAցAցAփAցAցAցAցAփAփAփAցAփAփAցAփAցAցAփAփAցAցAցAփAցAցA�~�AցA�|�AցAցAցAցAփAցAցAփAցAփAփAփAփAփAփAփAցAփAփAփAփAփAցAփAցAփAփAցAօAփAցAփAփAփAփAցAփAփAցAցAցAցA�z�A�z�A�z�A�|�A�|�A�x�A�z�A�x�A�z�A�x�A�x�A�z�A�z�A�x�A�x�A�x�A�r�A�r�A�n�A�p�A�ZA�M�A�M�A�I�A�I�A�I�A�K�A�K�A�G�A�G�A�E�A�E�A�G�A�C�A�?}A�A�A�E�A�E�A�E�A�A�A�A�A�G�A�I�A�E�A�E�A�E�A�G�A�E�A�C�A�G�A�G�A�G�A�G�A�G�A�E�A�C�A�?}A�=qA�;dA�=qA�A�A�A�A�A�A�A�A�?}A�A�A�=qA�;dA�=qA�9XA�9XA�9XA�9XA�;dA�9XA�9XA�;dA�9XA�5?A�5?A�7LA�9XA�5?A�9XA�9XA�7LA�7LA�9XA�9XA�5?A�1'A�5?A�5?A�7LA�/A�7LA�1'A�"�A��A��A��A��A��A��A�&�A�&�A�$�A�&�A�$�A�&�A�+A�+A��A� �A�-A�/A�$�A�5?A�9XA�+A��A�oA�{A��A��A�{A�{A�{A�{A��A��A��A�{A��A�{A�{A�{A�{A�{A�{A��A�{A�{A�oA�oA�oA�oA�oA�bA�bA�VA�bA�bA�VA�
=A�A�A�A�A�A�A�A�%A�1A�A�A�A�%A�A�A�A�A���A�A���A�  A���A��A��A��yA��`A��mA��mA��`A��`A��`A��mA��`A��HA��#A��A��
A��
A��
A��A��A���A���A���A���A���A���A���A���A�ƨA�A���AվwAվwAվwAպ^AոRAնFAղ-Aհ!AծAծAթ�Aթ�Aէ�Aե�Aգ�Aե�A՟�A՟�A՝�A՟�A՗�A՗�A՗�AՑhAՇ+A�~�A�v�A�r�A�bNA�E�A�{A��A��#A�ƨAԼjAԴ9Aԗ�A�|�A�t�A�dZA�ZA�I�A�G�A�7LA�/A�-A�"�A�oA�JA���A���A���A���A���A���A���A��A��A��A��HA��;A��/A��#A��/A��TA��HA��HA��/A��/A��/A��HA��#A��#A��#A��/A��;A��;A��A��#A��#A��#A��/A��;A��HA��;A��;A��/A��/A��HA��;A��;A��HA��;A��;A��;A��HA��HA��;A��HA��TA��HA��HA��HA��;A��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��`A��TA��`A��TA��`A��;A��HA��HA��HA��HA��HA��;A��HA��`A��`A��`A��mA��mA��yA��mA��mA��yA��mA��A��A��A��A��A��A��A��A��A���A���A���A��A��A��A���A���A��A��A��A��A��yA��yA��`A��HA��`A��`A��mA��yA��yA��yA��yA��yA��A��HA��/A��#A��A��A��A��
A���A��
A���A�ȴA�ĜAӾwAӬAӅA�r�A�S�A�/A���A��`A���A�ƨA�ĜA�ĜAҴ9AҮAҋDA�33A��HA���AѮAћ�Aщ7A�1'A��A��;A��
A���A���AУ�A�hsA��A��Aϛ�A�^5A�7LA� �A�%A���A��A��HA���A�AμjAΩ�AΏ\A�jA�K�A�5?A��A���A��
Aͺ^A͛�A͋DA�~�A�l�A�O�A�&�A�  A��;Ḁ�A�~�A�|�A�x�A�p�A�l�A�hsA�hsA�hsA�ffA�ffA�hsA�hsA�jA�hsA�ffA�ffA�dZA�`BA�`BA�^5A�\)A�XA�ZA�VA�VA�VA�S�A�Q�A�Q�A�VA�XA�Q�A�O�A�K�A�M�A�I�A�G�A�G�A�E�A�A�A�=qA�7LA�5?A�1'A�+A�$�A� �A��A��A��A��A��A��A��A��A��A��A��A�{A�{A��A��A��A��A��A��A��A�oA���A���A���A���A���A���A���A���A��A��A��A��A��A��yA��yA��yA��yA��yA��yA��A��mA��TA��#A��A���A���A���A�ȴA�ĜA˾wA˺^A˰!A˥�A˝�A˛�A˟�A˟�A˛�AˍPA�r�A�7LA��
A�`BA���Aɛ�A�z�A�ZA�A�A�VA���A��yA��yA��A��mA��HA��AȼjAȇ+A�x�Aȉ7A�n�A�+A�1'A�VA�A�1A�A�VA�VA�1A�A���A��A��yA���A���AǑhAǑhAǑhAǋDAǅAǅAǃAǃAǃAǅAǃAǁAǁAǃAǃA�z�A�z�A�z�A�z�A�z�A�|�A�|�A�|�A�~�A�x�A�t�A�v�A�l�A�jA�bNA�XA�O�A�K�A�I�A�K�A�=qA�;dA�9XA�"�A��A�"�A��A��A�{A���A��HA���A���AƶFAƗ�A�l�A�{A���Aũ�Aŕ�Aŉ7A�v�A�jA�dZA�`BA�VA�C�A�1'A��A�VA�%A�  A���A��A��yA���AĲ-Aě�AăA�v�A�r�A�jA�ffA�\)A�Q�A�I�A�=qA�/A�-A�+A�$�A�$�A�"�A� �A��A��A��A�{A�bA�JA�1A�A���A��A��AüjAð!AÛ�AÍPA�hsA�?}A��A��A¥�APA�bNA��A��A��A�\)A�5?A�{A���A��TA���A��wA��!A���A�x�A�jA�`BA�\)A�ZA�I�A��A��A�VA��/A��^A���A��PA�dZA�A�A��A�1A��#A��hA�O�A�5?A�-A�+A�+A�+A�-A�+A�-A�-A�-A�-A�/A�1'A�/A�33A�1'A�5?A�7LA�9XA�=qA�;dA�;dA�=qA�?}A�A�A�=qA�9XA�7LA�7LA�9XA�=qA�?}A�=qA�9XA�5?A�1'A�/A�1'A�/A�(�A��A�{A�VA�VA�VA�bA�JA���A��A��TA��HA��A�ƨA���A���A���A��A�hsA�?}A��A��;A��RA���A�|�A�l�A�ffA�dZA�ZA�A�A�1'A�(�A��A�A��A��yA��TA��;A��;A��/A��A��
A���A���A���A���A���A���A���A�ƨA�ƨA�A���A��wA��jA��FA���A���A��DA��A�z�A�n�A�l�A�hsA�\)A�I�A�(�A�A�A�l�A�+A��A��!A�ffA�G�A�5?A��A��A�{A�%A���A���A��A��;A��#A���A���A�ƨA��wA��RA��A���A���A���A���A���A���A��uA��A�v�A�ffA�XA�K�A�G�A�E�A�G�A�E�A�A�A�?}A�=qA�9XA�9XA�7LA�7LA�5?A�7LA�7LA�5?A�5?A�5?A�1'A�33A�/A� �A��A��A��A�oA�bA�VA�bA�VA�JA�1A�A�A���A���A��mA��;A��/A��#A��/A��/A��/A��;A��HA��HA��TA��TA��TA��TA��TA��TA��TA��`A��yA��yA��A��TA���A��!A���A��uA��\A��DA��+A��A�t�A�hsA�dZA�dZA�hsA�  A�A�A�%A�1A�A�1A�1A�1A�
=A�1A�
=A�$�A��A�/A�C�A�K�A�K�A�M�A�Q�A�K�A�ZA�hsA�hsA�n�A�l�A�jA�jA�l�A�p�A�r�A�v�A�x�A�z�A�~�A�~�A��A��A��A��A��7A��+A��DA��PA��PA��DA��DA��PA��7A��DA��uA���A���A���A���A���A���A���A���A���A��A���A��!A��FA��-A��^A��^A��wA�A��jA���A�ƨA�ĜA�ĜA�A�ȴA�ƨA���A���A���A��
A��#A��/A��A��#A��HA��/A��/A��HA��mA��TA��;A��A��mA��A��A��A���A���A���A���A�  A���A�JA�JA�{A�oA� �A�$�A�?}A�A�A�I�A�dZA�v�A��A��DA���A���A��!A�G�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>-��@��z>A�{>�
@�9�=��?��a?w>>��>�6@�1�@;d=�=@�>8	?��@KZ2=�]>k'@(XO=��[>�D@?d>YK?�f�@�%�>��'>L�l>��~>B8�@�@:=pO�=a=2=��=�>	 ??�&�@� \@�.�=��>h�@�%>��0>#h�@�Z\=ж1=��?^��@�;�>ҕ�=j@=0j+=>q=�]O=h��=��<=��?d�L=鷿>R��?��7>D�@ �R@�CW>	^�@mv6@�E�?�L�?^�=�!l=���=��=�L0>Hr�?��[?txB@�(9>]>4��>�7�>�=�[@1L�>9�*?��?F�>Q/@�6&>��>���@�4�=��>�)�>L: @�j=�.?���?��1=���>1��=��;=�#�@zHk?s-M=�s�>���>}��@e{J>��c?(~|@�T"=�o�>=x�>H�i@��=�~�=�<K?@t >0?@>(�\@�K�>W@@�5�?��7>U�X@fʂ@�I{>'g>�-�@�N�=���>�H�=�!�?X�v=�~�>v�@��@�B[@�L0>�H>d@H�<@�H,@�B�?P_>	�@W~=?��@�G0=��>
�R?���@�G�@&j�=��Q=�U>O@�N'@�9�@�@�>:�j@�Pr@�T�@�D|@h=�C�=���>��@�8�>0�?�	B@� �@Dj>@x�@�6�>?�a@�/�?�ʬ>-��>M>o*Z@�=�@�;%@�=�?�Y�>��=���>`��@�B1@�B�@�>B@�F�>�>b�\>���@�?@�=@/4Y@[\>�@�'@�;�?&�@�:i@*C-@�8�@���@��B@�9C@�5�@�:�@�;�@�C@�<`@�;�@�;�@�;%@�;�@B@�<�@�<`@�;�@�<K@�<�@�@�@�A_@�:T@�9�@�9C@�: @�8�@�8	@�6�@�: @�:i@�:i@�:i@�9�@�8	@�7�@�6&@�6�@�6z@�7L@�8�@�8q@�8q@� q@�;�@�;�@�:�@�:i@�9.@�9C@�9C@�9�@�: @�;%@�;�@��#@�;%@�;%@�;%@�9C@�8�@�8q@�8	@�8	@�6�@�8	@�8	@�8q@�6z@�3�@�;�@�=@�>-@�9�@�9C@�<`@�;�@�<`@�A_@�: @�9�@�9C@�5�@�1�@�6z@�4�@�9C@�9�@�5i@�6z@�6z@�8	@�8q@�9C@�6�@�6z@�6�@�7L@�4/@�3r@�6z@�4�@�4�@�3	@�7�@�8	@�6z@�6z@�7L@�7L@�5i@�4�@�5�@�[�@�9�@�9C@�9.@�8	@�: @�:i@�9�@�9C@�9C@�8	@�9C@�:�@�9C@�<K@�9�@�:i@�=�@�=@�>B@�>B@�>B@�=�@pJ�@�?S@�?@�?}@�>�@�>�@�?@�@�@�CW@�C�@�CW@�C�@�C-@�C�@�D@�CW@�A�@�>�@�B�@�A_@�A_@�B�@�A_@�>�@�;�@�=�@�<�@�8	@�6�@�6�@�;%@�<`@�<`@�;�@�<�@�=@�>B@�=�@�=�@�>B@�?@�?�@�>B@�9�@�=@�>�@�>�@�=�@�=@�=@�<�@�=�@�=@�>�@�=�@�<`@�:�@�=@�<�@�;�@�<�@�@:@�?}@�?}@�?}@�@�@�@�@�@�@�@:@�?@�@�@�@�@�@:@�?�@�?}@�?�@�@:@�?�@�@@�@:@�>�@�?}@�?}@�?�@�?@�?�@�?�@�?�@�?}@�?S@�>B@�@�@�A�@�>B@�=@�A�@�@�@�?�@�=�@�?@�?�@�@�@�A�@�A_@�?}@�@�@�@:@�A5@�@�@�A_@�CW@�F5@�G0@�G�@�F@�I{@�I(@�G�@�H,@�I�@�LD@�Ks@�K
@�M@�LD@�M�@�LD@�Ks@�K�@�K
@�K
@�K
@�K�@�JM@�I�@�I{@�IR@�I{@�JM@�K
@�K�@�K�@�Ks@�K4@�K
@�K@�K@�Ks@�Ks@�K�@�K�@�K�@�K�@�K�@�K�@�LD@�K@�J�@�K@�J�@�K@�K@�K@�Ks@�K@�K�@�K�@�LD@�LY@�K�@�K�@�K@�Ks@�K@�L�@�L�@�K@�K�@�K@�K�@�K�@�J�@�I�@�I�@�J�@�Ks@�K@�K�@�K�@�K�@�K@�K�@�K@�P@�P�@�Oa@�O�@�P3@�T@�T�@�T�@�S�@�Q�@�R~@�P�@�PH@�Q�@�R*@�W�@�a�@�a�@�dZ@�[@�\�@�[�@�ZG@�\)@�[@�[�@�[�@�[l@�^@�`@�^�@�g�@�g�@�g�@�fQ@�e�@�f�@�i�@�kQ@�mH@�l�@�mH@�m�@�m�@�mr@�m�@�mr@�n@�m�@�nn@�n�@�n@�n@�n@�n�@�n@�n�@�n�@�n�@�n�@�n�@�n�@�n�@�n�@�n�@�o @�oi@�o?@�oi@�o�@�o?@�o�@�p@�o�@�p@�p@�p@�pz@�p@�pz@�pz@�pz@�p�@�pz@�p�@�p�@�p�@�p�@�q7@�qa@�q7@�p�@�pz@�p�@�pz@�pz@�qL@�p�@�qL@�qL@�p�@�qL@�qL@�q�@�qL@�q�@�qL@�r@�r@�r2@�q�@�qv@�q�@�r@�q�@�rq@�rq@�rq@�rq@�rq@�rq@�rq@�sC@�sC@�sC@�sC@�t@�t@�s�@�s�@�t*@�t*@�t*@�t�@�t~@�t�@�t~@�t~@�t~@�t~@�t~@�t~@�t�@�t~@�t~@�t~@�t~@�t@�t@�sX@�sX@�sX@�sX@�t@�s�@�t?@�sm@�t@�t@�t@�t@�t@�t@�t~@�t~@�t�@�t�@�uO@�t~@�t�@�t�@�t�@�uO@�uO@�uO@�u�@�ud@�v!@�ud@�uO@�u�@�u�@�v!@�uO@�u�@�v!@�v!@�v�@�v!@�v�@�v�@�v�@�v�@�v!@�v�@�v�@�v�@�v�@�wG@�v�@�v�@�wG@�w�@�w\@�w�@�w�@�x-@�x�@�x@�x@�x@�x�@�x@�x@�w�@�x@�x�@�x�@�x�@�x�@�x@�x@�w�@�w�@�x@�w�@�x-@�w�@�x�@�x@�x�@�x�@�x@�x-@�w�@�x�@�x�@�x�@�x�@�x�@�x�@�x-@�x-@�x�@�x-@�x�@�x�@�x�@�x�@�x�@�x�@�yS@�x�@�y�@�x�@�yS@�yS@�y�@�y�@�y�@�z%@�y�@�z%@�z%@�z%@�y�@�y�@�y�@�y�@�y�@�z%@�y�@�z%@�z%@�z�@�y�@�y�@�y�@�y�@�z:@�y�@�y�@�yh@�yS@�yS@�x�@�x�@�x�@�x-@�v�@�v�@�w@�w@�w@�w@�vK@�u�@�vK@�vK@�u�@�u%@�t@�u:@�t~@�r�@�qL@�qa@�p&@�n�@�n�@�jU@�d�@�b$@�b�@�b�@�b�@�b�@�a�@�b$@�b�@�b$@�`�@�_�@�`-@�`B@�`�@�`�@�`B@�`�@�ag@�a�@�a�@�`�@�`�@�`�@�`B@�`�@�`�@�`�@�`B@�_�@�_�@�_�@�_�@�_@�_@�]�@�]�@�]%@�]%@�]%@�\�@�[�@�]�@�]%@�[�@�\S@�\h@�[�@�[�@�[�@�[B@�Z�@�Z�@�Z�@�Y�@�Z@�Z@�Zq@�Z@�Z@�Y�@�X�@�X�@�YK@�X�@�X%@�X%@�X%@�X�@�X�@�X�@�W�@�Wi@�VC@�T�@�S&@�RT@�Q�@�Q�@�RT@�S&@�S�@�S�@�S�@�S�@�S&@�R�@�R�@�P�@�O�@�O7@�Pr@�Q/@�RT@�Q�@�N�@�L�@�K�@�K�@�K�@�K4@�K�@�Ks@�Ks@�Ks@�L0@�L0@�K�@�K�@�K
@�J�@�JM@�Jw@�J�@�J�@�J8@�I�@�I{@�H�@�G�@�F�@�F�@�G�@�G�@�Ex@�D�@�D@�D@�D|@�D@�B1@�@:@�?�@�?}@�?�@�?�@�?�@�>�@�>B@�=2@�<`@�;�@�;:@�:i@�:@�:@�9�@�9C@�8�@�8@�8@�5�@�4D@�1'@�0j@�0@�/�@�.�@�.s@�.
@�-M@�+�@�)�@�)�@�)@�(�@�)5@�)5@�(N@�'@�&�@�&W@�%�@�%�@�$�@�%@�$_@�#:@�!�@�@��@�@��@�a@�@�j@��@��@��@�j@�E@�@�=@�E@� @�N@�
(@�	@�t@��@�l@�u@���@��a@��@��b@��@��=@��@��@��?@�ܱ@�Ҟ@��@��@���@���@��2@���@��@���@���@���@���@���@���@��^@�|�@�{@�x�@�t�@�r2@�pP@�o*@�n�@�n�@�nn@�m�@�lv@�kQ@�iY@�g�@�gw@�fQ@�f�@�e�@�e�@�e,@�d�@�d�@�c�@�c�@�b�@�bx@�bx@�bx@�bx@�b�@�b�@�b�@�b�@�b�@�b�@�cI@�c�@�cI@�c�@�c�@�c�@�d@�d@�c�@�c�@�c�@�c�@�c�@�d@�c�@�d@�d@�d@�d@�d@�do@�d@�d@�c�@�c�@�cI@�b�@�b�@�cI@�cI@�b�@�b$@�a�@�`�@�`�@�`�@�`�@�`B@�_�@�_p@�_@�]�@�]�@�\�@�\S@�[-@�Zq@�YK@�X�@�X%@�VC@�U�@�U@�S�@�R�@�QD@�P@�M�@�L0@�I�@�H@�E�@�D(@�A�@�@d@�>l@�;d@�8�@�4�@�2#@�0�@�.�@�+�@�(9@�#�@�O@�@�g@��@��3@��4@���@��Q@��K@��Y@���@��;@��,@���@��`@��@���@���@���@��@���@��@��7@���@��@��}@��@��O@���@���@���@���@��8@���@���@���@���@���@���@��*@��1@��!@��]@�|[@�k'@�Z�@�R*@�L@�D�@�6z@�#�@��@��@��@���@��#@��\@��,@��8@��@���@�~�@�m�@�a�@�[�@�Sz@�LD@�Dg@�;�@�7L@�2a@�+V@�!�@�8@��@��z@��@��@���@���@�Ȋ@���@���@���@���@���@��@@�|�@�r�@�ek@�^@�[l@�Y!@�W�@�WT@�V�@�V.@�U�@�U�@�Uq@�U@�T�@�S�@�S�@�R�@�R�@�Q�@�Q@�O�@�N@�L0@�K@�JM@�I=@�H�@�H@�F�@�B�@�>�@�=\@�<�@�: @�8@�5~@�1�@�-b@�)�@�$t@� \@��@��@��@��@�E@��@�	@��@��@��@���@���@���@��<@��<@���@���@��/@��U@��@���@���@���@� �@�;@�;@� �@�;@��@��@�;@� *@���@���@��b@��@��I@���@��t@��@��@��	@��f@�Ց@��x@���@���@��@���@��A@��_@���@��E@���@���@��x@��@��x@��x@���@���@���@��@��?@��N@���@�wp@�`B@�C�@�6@���@��R@��@��k@���@��`@��D@���@���@���@��@���@���@��I@���@��}@��Y@��U@���@�|�@�t*@�n/@�la@�j+@�g�@�ek@�d@�b�@�b9@�b�@�b�@�`�@�]�@�Z�@�X:@�Uq@�S�@�SP@�R*@�P�@�OL@�M@�KI@�HV@�F5@�C�@�@d@�=�@�<�@�:�@�:�@�:�@�:*@�9�@�9@�7�@�5@�-�@�)�@�'R@�%�@�%p@�%�@�$�@�!�@�m@��@�f@�g@�0@��@�@��@�@� �@���@��j@���@��b@���@��K@��@�܇@��1@��@��n@��D@�z�@�oi@�h�@�c5@�\�@�Vm@�Q�@�M@@�E�@�=@�3H@�+�@�%�@� 2@�v@�<@��@��@��/@���@��>@��@�ޔ@���@�ײ@���@�Б@��O@��P@���@��@���@���@���@��k@��5@���@��O@��q@���@���@���@��N@���@���@���@��b@�x�@�k�@�a@�UG@�F�@�3�@��@�	�@���@���@���@��@��J@�~|@�c�@�L@�;%@�,�@� �@�]@�Z@�2@���@��|@��r@��@��1@�ɛ@���@��"@���@���@��@�w�@�m�@�e@�[�@�T�@�I=@�=\@�-�@�O@��@��@�8@��@�@��@��@��@��@�f@�#@��@�n@�D@�@��@�7@�q@�?@��@� �@�!�@�"�@�%�@�($@�)�@�*E@�*E@�)�@�*�@�,g@�/0@�0�@�2@�1�@�0U@�.�@�,@�($@�&@�#�@� �@��@��@��@��@��@�u@� q@�!�@�">@� �@�u@��@��@�Y@��@�c@�G@���@��P@���@�ɛ@��x@��>@���@���@���@��x@���@���@���@���@�{�@�t@�m3@�jj@�h�@�f�@�eA@�d@�bc@�`B@�_F@�]y@�\>@�[@�YK@�Wi@�Uq@�SP@�P�@�N{@�K�@�H�@�E�@�A�@�<�@�6e@�.�@�(x@�"}@��@��@��@�p@��H@��N@�ی@�ƨ@��@��$@��@�j�@�S�@�=�@�4Y@�-w@�&�@� \@��@�j@�@�@@�8@�0@�$@�	@��@��@� �@���@��r@��@���@��f@��U@��U@��@���@���@���@��@��&@��]@��a@���@���@��P@��@@�܇@�ی@���@��+@��+@���@��@���@���@���@���@��$@��@�Ѣ@��@���@��K@�̣@��%@���@��S@���@���@�ɰ@��\@�ȴ@�Ǥ@���@���@���@���@��@��@���@��@��'@��'@��7@���@��z@��@��j@��@��+@���@��@���@���@���@��r@���@��[@��@���@��@��S@���@��7@���@��@��@���@��
@�~�@�|�@�{�@�~R@��V@���@���@��@��L@��@���@���@��[@��@���@��S@���@���@��@���@���@���@��@��h@���@���@���@��@���@��l@��@��@���@��1@��@���@��u@��1@���@���@��)@���@��1@���@��g@��0@���@��M@��@��^@���@��@��U@���@���@��	@��@��6@��q@��m@��C@��C@���@���@��}@��y@��u@��B@���@��1@���@��@���@���@���@��@�¹@��7@��L@��v@��@��e@���@���@���@���@���@���@��@�ı@��!@��q@���@���@��!@�͟@��!@��`@��N@���@��0@��E@���@��E@��'@�ۡ@��/@��;@��6@��@���@��5@��@��,@��b@��@��@�Y@��@��@� G@�!@�#�@��,@��o@��F@��@�ӄ@��[@�ә@���@���@���@���@�ә@���@���@��S@���@��_@���@��5@���@�ҳ@�Ѣ@�� @��N@�ә@��@@��8@�ֶ@��^@�ջ@��M@���@�܇@���@��]@���@��@��w@�ם@��@���@��|@���@��@���@��@��C@��i@��	@��@���@���@���@��m@���@��@���@��@��`@��h@���@��W@��@��=@��8@��I@��^@��I@��s@��^@��@���@���@��0@��0@��0@��Z@��Z@���@��@��Z@��Z@���@���@���@���@���@���@���@���@���@��@��@@��U@���@���@���@���@��@@���@���@���@��Q@��f@��f@���@��Q@��f@���@���@���@���@���@���@���@��@��'@��Q@��<@��'@���@��{@���@���@���@���@���@���@��8@��@��8@��M@��w@���@���@���@��8@��8@��M@��w@���@��]@��Y@��M@��M@���@��@���@���@���@���@��@��@��Y@��Y@��n@���@���@���@��n@���@��@@��@@��@���@��@��@@��@@��@��@��U@���@���@���@���@���@���@��@��@��@��D@��D@��D@��Y@���@��@���@���@���@���@��@���@��Y@��@��@��@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@��&@��;@��&@��e@��z@��e@���@���@���@��"@���@��@��a@���@��"@��a@���@��@���@��	@���@��H@��3@��H@���@��	@���@��"@��a@���@��H@���@���@��@���@��3@���@��a@���@���@���@���@��@��	@��@��H@��H@��H@��]@��r@���@���@���@��	@���@��3@��@��@��	@���@��v@��3@��H@��H@��	@��H@���@���@���@��@��@��C@��m@��m@���@��m@��m@���@���@���@���@���@���@���@���@���@���@���@���@�  @���@���@���@���@���@���@���@���@���@��m@��C@��X@���@��"@��L@���@��	@���@��@���@��@���@���@���@���@���@���@��P@���@��@��3@���@��w@��@��K@��!@���@���@��)@���@���@��@��q@��@���@���@��u@��!@��@��2@��q@��@��u@��u@��@���@��@���@��@��.@���@��\@���@��@���@��@��@��@��u@��@���@���@��T@���@��6@��@��`@���@���@�� @�� @��@��@��]@��@���@���@��@���@��@��@���@�߹@��@��@���@��]@��]@���@��@��@��]@�ߏ@�޾@��e@��;@��@���@���@���@���@��@��1@���@��,@��N@��R@���@��@@��@�ؙ@�خ@��@�ٔ@��r@��|@��
@���@���@��0@��+@���@�ܱ@��F@��>@��@���@���@���@�Ѣ@��x@��5@�Ӯ@�ԕ@��V@��5@��x@���@��$@�Ѣ@���@�э@���@���@�э@��N@���@��>@��>@��)@���@�ϖ@���@��W@���@��)@�ϖ@��@��@���@���@�Ɇ@���@���@�ʬ@�ʬ@�˧@�Ɇ@���@���@�ʂ@��q@��q@��q@��q@��P@��@��i@�ƨ@��*@��@���@���@��@���@���@��@��^@��4@��Z@���@��c@���@���@���@��@���@���@��9@���@���@��1@���@���@��p@���@��>@���@���@���@��H@��3@��v@���@��@���@��M@��{@���@��Z@���@���@���@���@��R@��,@���@��t@���@���@���@��F@���@���@��@���@��a@���@��f@�w�@�f�@�W�@�M�@�D�@�A�@�=�@�4�@�'(@�%p@��@�@�'@��@��@�
�@��@��@���@���@���@��<@��Q@��f@��@��<@���@��@��Z@��4@��A@��t@�� @��_@��@��V@��@��@���@��t@��t@��@��_@��@���@�� @��Z@��@��5@��5@�� @��5@��5@��@��@��@��@��@��@��E@��E@��Z@��Z@��Z@��Z@��Z@���@��@���@���@��@��@��@���@���@���@���@���@��@��@��A@��@��@��@��A@��V@��@��@��@��V@��k@��Z@���@���@��@��@��@��@���@��@��@��@���@���@��|@��@��@��@��R@��@���@���@���@���@���@���@���@���@��Z@���@���@��Z@���@���@���@���@��E@���@��|@���@���@��R@��@��@��@��E@��@��@��@��@���@��@��@��c@��B@��B@���@��@��@��K@��%@���@��m@��&@��.@���@�٩@��S@��@��I@���@�� @���@��s@���@���@��A@�{@�y�@�m3@�O�@�/@�%F@�+@�|@�
g@��-@��x@���@���@�¤@��@���@���@�y�@�_�@�G@�/E@� @��@�
@�S@� �@��/@��R@���@��@��C@��Z@���@���@���@���@���@��{@�{ @�m�@�eV@�`@�X@�M@@�;�@�)�@�3@�W@�� @��x@���@���@��@��@��@��@��u@��`@��@��`@���@��u@��u@��6@��@��i@���@���@���@���@��@��"@���@�߹@�ߤ@��@�޾@��P@�߹@��@�ޔ@��U@��@��@���@�޾@��@@��3@�ں@��j@�ם@�զ@�Ҟ@��h@�ρ@��@�Κ@��[@���@�ͳ@��:@���@��y@�̣@��d@���@�˼@��)@�̸@��`@��%@��:@��y@��%@�ɰ@�ȟ@��a@��'@�@�¹@���@�¤@��P@��+@���@���@���@��8@��@��x@���@��Z@��@���@���@���@��@��@��[@��_@���@��1@��:@���@���@��v@��@��f@��=@��F@��-@���@��@��O@���@�z�@�a@�8�@�	l@��.@��[@��n@��l@���@�t�@�j�@�do@�b�@�d�@�d@�a|@�]:@�R�@�<�@�4/@�;�@�8�@� @�?@�<@��@�V@�@�|@��@�V@�@��@�l@��@��e@��]@��x@���@���@��B@���@��@��t@��t@��t@��K@��K@��@��@��6@���@��S@��@��}@��}@��}@��@��@��S@��}@��G@���@���@���@��@��"@��@�ؙ@��M@���@��g@�� @�΅@���@�Ŭ@���@���@���@��b@���@��`@���@���@��
@���@��'@�u�@�Q@�1�@�*@�'@�=@��@�;@��7@��n@��+@��>@��@�ٔ@��@��F@�ʬ@��!@���@���@��t@���@��c@���@��L@���@��<@���@�|@�x-@�sC@�m�@�g�@�dE@�e@�b$@�a�@�`�@�`@�^�@�^5@�]%@�[�@�Z2@�X%@�U�@�R�@�O�@�K�@�B�@�5~@�.�@�%F@��@��@���@��:@�ʬ@���@���@���@�pz@�Z�@�:i@�@��C@���@��@��R@��!@���@��b@���@���@��@��R@��@��J@��O@�}@�z�@�v`@�d@�SP@�H�@�<�@�*�@�P@��@���@��1@���@��@��P@���@���@���@��T@���@��@��G@��G@��\@��@���@��y@��y@��u@���@���@��$@���@��s@��@��@��Y@��@���@��!@��i@��X@��u@���@���@��5@�զ@���@�؄@�ײ@���@��@��3@��]@��Q@��<@���@���@�ݭ@�݃@���@�ۡ@�ܱ@��U@��@��@��V@�ȟ@��a@��r@���@���@���@��@�v@�d@�W@�K�@�C�@�A�@�@@�<�@�2�@�*�@�(@�$ @�7@��@��@�c@�	�@��@�1@��@��@��@�l@��@�\@��@�!@� �@���@��H@��@��U@��Y@���@���@��k@��`@���@��@���@��=@�ԕ@�ӄ@�΅@��z@���@��/@��y@�i�@�K�@�/�@�b@���@��H@�پ@�Б@��>@��m@���@��@��Y@���@��@��F@���@��@��i@��a@���@��0@���@���@��V@��@���@���@��@���@���@���@���@���@���@��@��Z@���@�c@�~�@�}�@�|@�z�@�z�@�z%@�yS@�y@�y�@�x�@�x�@�x�@�w�@�x@�v�@�p�@�m�@�m3@�l�@�l�@�l�@�l@�k�@�k{@�j�@�j@�iY@�h4@�f�@�e�@�a(@�^�@�_�@�`�@�a|@�b�@�cI@�do@�e@�g@�g�@�i@�i@�j@�j@@�k�@�k�@�l�@�n�@�q"@�s@�p�@�f{@�Z�@�Q@�K�@�H@�F�@�E@�A@�<�@�6;@�2�@�4n@�6&@�3@�2�@�3�@�5i@�6;@�6;@�6�@�6�@�8@�9.@�:@�<�@�B1@�D|@�K@�R�@�Se@�V.@�W�@�X�@�Xy@�\)@�_�@�`@�b�@�bN@�bN@�b�@�b�@�dE@�d@�eA@�ff@�g�@�i@�i@�j@�j�@�k'@�k{@�m	@�m�@�m�@�nY@�nY@�n�@�n�@�n�@�n@�o @�q�@�r�@�sX@�t�@�u�@�v�@�y}@�xl@�y�@�{�@�{�@�{�@�},@�x@�x@���@��,@���@��
@��M@���@��U@���@��^@���@���@��f@��v@���@��&@���@���@��\@���@���@���@��.@��S@��%@��`@��%@���@��W@���@��@��)@���@���@��(@���@���@��N@���@��f@��#@��X@���@��"@��X@���@���@���@��i@�ί@��g@��|@��f@��&@��6@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       4344344444344444444444444344443444444334434434443444444444444443433444444444344444444443443444344444443444434434443444444344344334434444443334443344343444344443334333444434434434344443334444333344433434334343333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��}G�O�G�O�@�9�G�O�G�O�G�O�G�O�G�O�@�1�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�%�G�O�G�O�G�O�G�O�@�@=G�O�G�O�G�O�G�O�G�O�G�O�@� ]@�.�G�O�G�O�@�%G�O�G�O�@�Z]G�O�G�O�G�O�@�;�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�CVG�O�@mv8@�E�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�(:G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�6$G�O�G�O�@�4�G�O�G�O�G�O�@�jG�O�G�O�G�O�G�O�G�O�G�O�G�O�@zHkG�O�G�O�G�O�G�O�@e{PG�O�G�O�@�T"G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�@�K�G�O�G�O�@�5�G�O�G�O�@fʈ@�IzG�O�G�O�@�N�G�O�G�O�G�O�G�O�G�O�G�O�@��@�B]@�L2G�O�G�O�G�O�@�H1@�B�G�O�G�O�@W~CG�O�@�G5G�O�G�O�G�O�@�G�G�O�G�O�G�O�G�O�@�N(@�9�@�@�G�O�@�Pt@�T�@�D�G�O�G�O�G�O�G�O�@�8�G�O�G�O�@� G�O�G�O�@�6�G�O�@�/�G�O�G�O�G�O�G�O�@�=�@�;'@�=�G�O�G�O�G�O�G�O�@�B2@�B�@�>@@�F�G�O�G�O�G�O�@�?@�=G�O�@[]G�O�@�'@�;�G�O�@�:jG�O�@�8�@���@��B@�9@@�5�@�:�@�;�@�C@�<d@�;�@�;�@�;#@�;�G�O�@�<�@�<_@�;�@�<L@�<�@�@�@�Ab@�:U@�9�@�9B@�: @�8�@�8@�6�@�: @�:l@�:j@�:j@�9�@�8@�7�@�6&@�6�@�6~@�7J@�8�@�8r@�8o@� p@�;�@�;�@�:�@�:i@�92@�9F@�9F@�9�@�9�@�;)@�;�@��(@�;$@�;&@�;&@�9E@�8�@�8r@�8	@�8
@�6�@�8@�8@�8n@�6z@�3�@�;�@�=!@�>-@�9�@�9E@�<^@�;�@�<b@�A_@�:@�9�@�9C@�5�@�1�@�6y@�4�@�9F@�9�@�5d@�6x@�6{@�8@�8p@�9E@�6�@�6|@�6�@�7Q@�4/@�3u@�6w@�4�@�4�@�3@�7�@�8@�6z@�6x@�7L@�7L@�5p@�4�@�5�@�[�@�9�@�9C@�95@�8@�9�@�:j@�9�@�9B@�9C@�8@�9E@�:�@�9E@�<J@�9�@�:f@�=�@�=@�>B@�>C@�>E@�=�@pJ�@�?V@�?@�?}@�>�@�>�@�?@�@�@�CV@�C�@�CY@�C�@�C*@�C�@�D@�C[@�A�@�>�@�B�@�A^@�Ab@�B�@�Ab@�>�@�;�@�=�@�<�@�8	@�6�@�6�@�;!@�<_@�<d@�;�@�<�@�=@�>B@�=�@�=�@�>B@�?@�?�@�>B@�9�@�=@�>�@�>�@�=�@�=!@�=@�<�@�=�@�=@�>�@�=�@�<_@�:�@�=#@�<�@�;�@�<�@�@9@�?|@�?|@�?}@�@�@�@�@�@�@�@:@�?@�@�@�@�@�@6@�?�@�?|@�?�@�@:@�?�@�@@�@7@�>�@�?z@�?�@�?�@�?@�?�@�?�@�?�@�?|@�?V@�>B@�@�@�A�@�>?@�=@�A�@�@�@�?�@�=�@�?@�?�@�@�@�A�@�A^@�?�@�@�@�@:@�A5@�@�@�Ab@�CZ@�F6@�G6@�G�@�F@�Ix@�I%@�G�@�H/@�I�@�LF@�Kr@�K@�M@�LB@�M�@�LB@�Kr@�K�@�K	@�K	@�K@�K�@�JP@�I�@�I}@�IQ@�I~@�JK@�K
@�K�@�K�@�Kr@�K4@�K@�K@�K@�Kr@�Kr@�K�@�K�@�K�@�K�@�K�@�K�@�LF@�K@�J�@�K!@�J�@�K @�K @�K@�Kr@�K!@�K�@�K�@�LE@�L^@��+@��p@��F@��@�ӂ@��]@�Ә@���@���@���@���@�Ӛ@���@���@��O@���@��`@���@��8@���@�Ҷ@�ѣ@��@��R@�Ӛ@��A@��6@�ּ@��`@�պ@��M@���@�܆@��@��b@���@��@��|@�מ@��"@���@��x@���@��@���@��@��B@��j@��@��@���@���@���@��r@���@��@���@��@��`@��l@���@��W@��@��=@��8@��J@��c@��J@��s@��]@��@���@���@��0@��.@��1@��Y@��^@���@��@��Z@��^@���@���@���@���@���@���@���@���@���@�� @��B@��R@���@���@���@���@��A@���@���@���@��R@��a@��f@���@��M@��j@���@���@���@���@���@���@���@��@��(@��O@��?@��+@���@��|@���@���@���@���@���@���@��9@��@��;@��O@��q@���@���@���@��5@��9@��O@��w@���@��^@��Y@��R@��V@���@��@���@���@���@���@��@��@��Y@��Y@��l@���@���@���@��s@���@��A@��A@��@���@��@��A@��A@��~@���@��U@���@���@���@���@���@���@��@��@��@��F@��F@��F@��Z@���@��@���@���@���@���@��@���@��V@��@��@���@���@��~@��~@���@���@���@��@���@���@���@���@���@���@���@���@��.@��:@��*@��c@��z@��f@���@���@���@��!@���@��@��b@���@��#@��g@���@��@���@��@���@��M@��4@��J@���@��@���@��$@��b@���@��K@���@���@��@���@��2@���@��d@���@���@���@���@��$@��	@��!@��H@��H@��J@��^@��r@���@���@���@��@���@��4@��@��@��@���@��v@��4@��E@��N@��@��B@���@���@���@��@��@��F@��r@��n@���@��n@��r@���@���@���@���@���@���@���@���@���@���@���@���@�  @���@���@���@���@���@���@���@���@���@��n@��G@��]@���@��&@��H@���@��	@���@��@���@��@���@���@���@���@���@���@��T@���@��@��.@���@��x@��@��M@��#@��@���@��)@���@���@��@��s@��@���@���@��u@��@��@��2@��n@��@��y@��v@��@���@��@���@��@��-@���@��\@���@��@���@��@��@��@��z@���@���@���@��R@���@��3@��@��b@���@���@��@�� @��@��@��[@��@���@���@��@���@��@��@���@�߸@��@��@���@��^@��^@���@��@��@��b@�ߒ@�޽@��f@��=@�� @���@���@���@���@��@��3@���@��.@��N@��P@���@��A@��@�؟@�ج@��@�ٓ@��t@�Հ@��@���@���@��.@��-@���@�ܲ@��B@��?@��@���@���@���@�Ѣ@��x@��2@�ӱ@�Ԛ@��W@��7@��{@���@��&@�Ѣ@���@�ы@���@���@�ѐ@��Q@���@��?@��?@��*@���@�ϖ@���@��Z@���@��+@�Ϛ@��@��@���@���@�ɂ@���@���@�ʯ@�ʬ@�˦@�Ɇ@���@���@��~@��t@��w@��t@��t@��L@��@��k@�Ƨ@��,@��@���@���@��@���@���@��@��c@��7@��Z@���@��b@���@���@���@��@���@���@��;@���@���@��4@���@���@��o@���@��<@���@���@���@��I@��5@��y@���@��@���@��P@��{@���@��Y@���@���@���@���@��U@��/@���@��s@���@���@���@��I@���@���@��@���@��c@���@��d@�w�@�f�@�W�@�M�@�D�@�A�@�=�@�4�@�',@�%u@��@�!@�&@��@��@�
�@��@��@���@���@���@��>@��S@��f@��@��;@���@��@��[@��6@��B@��z@��@��c@��@��[@�� @��@���@��r@��r@��@��`@��@���@��"@��Z@��@��6@��5@��!@��5@��6@��@�� @��@��@��@��@��I@��B@��Z@��_@��Z@��Z@��Z@���@��@���@���@��@���@��@���@���@���@���@���@���@���@��B@��@���@��@��B@��Z@��@��@��@��^@��m@��Z@���@���@��@��@�� @��@���@��@���@���@���@���@��|@��@��@��@��Q@��@���@���@���@���@���@���@���@���@��Y@���@���@��[@���@���@���@���@��F@���@��}@���@���@��P@��@��@��@��F@��@��@��@��@���@��@��@��e@��A@��B@���@�� @��@��H@��&@���@��g@��&@��0@���@�٭@��V@��@��H@���@�� @���@��v@���@���@��I@�{@�y�@�m4@�O�@�/@�%F@�)@�}@�
i@��/@��v@�ɾ@���@�¦@��@���@���@�y�@�_�@�G@�/F@�@��@�
@�S@� �@��0@��R@���@��@��H@��Z@���@���@���@���@���@��y@�{@�m�@�eV@�`@�X@�M>@�;�@�)�@�5@�^@��@��v@�� @���@��@��@��@��@��w@��a@��@��`@���@��s@��t@��;@��@��j@���@���@���@���@��@��@���@�ߺ@�ߣ@��@���@��R@�߶@��@�ޕ@��T@��@�ހ@���@���@��=@��6@�ڽ@��l@�מ@�ժ@�Ҟ@��h@�ρ@��@�Κ@��\@���@�ͳ@��9@���@��v@�̟@��b@���@�˻@��.@�̷@��^@��$@��=@��z@��%@�ɮ@�Ȟ@��g@��'@�@�»@���@�¡@��O@��.@���@���@���@��<@���@��t@���@��Y@��@���@���@���@���@��"@��Z@��b@���@��6@��:@���@���@��q@��@��f@��>@��I@��*@��@��@��R@���@�z�@�a@�8�@�	n@��-@��^@��n@��o@���@�t�@�j�@�dr@�b�@�d�@�d
@�a}@�]:@�R�@�<�@�41@�;�@�8�@�@�>@�;@��@�\@�@�}@��@�X@�
@��@�i@��@��h@��^@��z@���@���@��C@���@��@��y@��v@��v@��H@��J@��@��
@��6@���@��R@��@��~@��~@��{@��@��@��Z@��{@��E@���@���@���@��@�� @��@�ؗ@��M@���@��n@��"@�Ά@���@�Ů@���@���@���@��h@���@��`@���@���@��@���@��'@�u�@�Q@�1�@�(@�&@�?@��@�=@��8@��r@��+@��?@��@�ٔ@��@��H@�ʪ@��!@���@���@��s@���@��b@���@��Q@���@��A@���@�|@�x-@�sB@�m�@�g�@�dD@�e@�b'@�a�@�`�@�`@�^�@�^8@�]&@�[�@�Z2@�X&@�U�@�R�@�O�@�K�@�B�@�5�@�.�@�%H@��@��@���@��@@�ʲ@���@���@���@�p{@�Z�@�:h@� @��@@���@��@��Q@��#@���@��c@���@���@��@��V@��@��J@��O@�}@�z�@�vc@�d
@�SX@�H�@�<�@�*�@�O@��@���@��6@���@��@��V@���@���@���@��V@���@��@��D@��D@��[@��@���@��z@��z@��t@���@���@��$@���@��t@��@��@��Z@��	@���@��!@��f@��X@��w@���@���@��2@�թ@���@�؆@�ׯ@���@��@��6@��^@��P@��;@���@���@�ݮ@�݂@���@�ۢ@�ܶ@��[@��@��@��V@�ț@��`@��t@���@���@���@��@�v@�d
@�W@�K�@�C�@�A�@�@@�<�@�2�@�*�@�(@�$@�4@��@��@�^@�	�@��@�6@��@��@��@�m@��@�Z@��@�%@� �@���@��F@��@��X@��\@���@���@��n@��`@���@��@���@��A@�Ԗ@�ӆ@�΋@��w@���@��2@��v@�i�@�K�@�/�@�f@���@��F@���@�Ж@��>@��n@���@��@��^@���@��@��G@���@��@��l@��a@���@��.@���@���@��W@��@���@���@��@���@���@���@���@���@���@��	@��Z@���@�b@�~�@�}�@�|@�z�@�z�@�z'@�yQ@�y@�y�@�x�@�x�@�x~@�w�@�x@�v�@�p�@�m�@�m3@�l�@�l�@�l�@�l@�k�@�k{@�j�@�j@�iY@�h7@�f�@�e�@�a'@�^�@�_�@�`�@�a~@�b�@�cI@�do@�e@�g@�g�@�i@�i@�j@�j?@�k�@�k�@�l�@�n�@�q&@�s@�p�@�f@�Z�@�Q@�K�@�H@�F�@�E@�A@�<�@�6;@�2�@�4n@�6&@�3@�2�@�3�@�5k@�6;@�6;@�6�@�6�@�8@�9.@�:@�<�@�B6@�D{@�K"@�R�@�Sd@�V-@�W�@�X�@�Xz@�\*@�_�@�`@�b�@�bK@�bQ@�b�@�b�@�dE@�d@�eB@�fj@�g�@�i@�i
@�j@�j�@�k%@�k�@�m@�m�@�m�@�nY@�nY@�n�@�n�@�n�@�n@�o@�q�@�r�@�sT@�t�@�u�@�v�@�y@�xl@�y�@�{�@�{�@�{�@�}+@�v@�z@���@��+@���@��@��N@���@��W@���@��Z@���@���@��f@��y@���@��(@���@���@��]@���@���@���@��.@��V@��'@��b@��%@���@��W@���@��@��)@���@���@��*@���@���@��N@���@��e@��(@��Z@���@��#@��V@���@���@���@��g@�β@��f@��~@��g@��$@��8@��@��+@��p@��F@��@�ӂ@��]@�Ә@���@���@���@���@�Ӛ@���@���@��O@���@��`@���@��8@���@�Ҷ@�ѣ@��@��R@�Ӛ@��A@��6@�ּ@��`@�պ@��M@���@�܆@��@��b@���@��@��|@�מ@��"@���@��x@���@��@���@��@��B@��j@��@��@���@���@���@��r@���@��@���@��@��`@��l@���@��W@��@��=@��8@��J@��c@��J@��s@��]@��@���@���@��0@��.@��1@��Y@��^@���@��@��Z@��^@���@���@���@���@���@���@���@���@���@�� @��B@��R@���@���@���@���@��A@���@���@���@��R@��a@��f@���@��M@��j@���@���@���@���@���@���@���@��@��(@��O@��?@��+@���@��|@���@���@���@���@���@���@��9@��@��;@��O@��q@���@���@���@��5@��9@��O@��w@���@��^@��Y@��R@��V@���@��@���@���@���@���@��@��@��Y@��Y@��l@���@���@���@��s@���@��A@��A@��@���@��@��A@��A@��~@���@��U@���@���@���@���@���@���@��@��@��@��F@��F@��F@��Z@���@��@���@���@���@���@��@���@��V@��@��@���@���@��~@��~@���@���@���@��@���@���@���@���@���@���@���@���@��.@��:@��*@��c@��z@��f@���@���@���@��!@���@��@��b@���@��#@��g@���@��@���@��@���@��M@��4@��J@���@��@���@��$@��b@���@��K@���@���@��@���@��2@���@��d@���@���@���@���@��$@��	@��!@��H@��H@��J@��^@��r@���@���@���@��@���@��4@��@��@��@���@��v@��4@��E@��N@��@��B@���@���@���@��@��@��F@��r@��n@���@��n@��r@���@���@���@���@���@���@���@���@���@���@���@���@�  @���@���@���@���@���@���@���@���@���@��n@��G@��]@���@��&@��H@���@��	@���@��@���@��@���@���@���@���@���@���@��T@���@��@��.@���@��x@��@��M@��#@��@���@��)@���@���@��@��s@��@���@���@��u@��@��@��2@��n@��@��y@��v@��@���@��@���@��@��-@���@��\@���@��@���@��@��@��@��z@���@���@���@��R@���@��3@��@��b@���@���@��@�� @��@��@��[@��@���@���@��@���@��@��@���@�߸@��@��@���@��^@��^@���@��@��@��b@�ߒ@�޽@��f@��=@�� @���@���@���@���@��@��3@���@��.@��N@��P@���@��A@��@�؟@�ج@��@�ٓ@��t@�Հ@��@���@���@��.@��-@���@�ܲ@��B@��?@��@���@���@���@�Ѣ@��x@��2@�ӱ@�Ԛ@��W@��7@��{@���@��&@�Ѣ@���@�ы@���@���@�ѐ@��Q@���@��?@��?@��*@���@�ϖ@���@��Z@���@��+@�Ϛ@��@��@���@���@�ɂ@���@���@�ʯ@�ʬ@�˦@�Ɇ@���@���@��~@��t@��w@��t@��t@��L@��@��k@�Ƨ@��,@��@���@���@��@���@���@��@��c@��7@��Z@���@��b@���@���@���@��@���@���@��;@���@���@��4@���@���@��o@���@��<@���@���@���@��I@��5@��y@���@��@���@��P@��{@���@��Y@���@���@���@���@��U@��/@���@��s@���@���@���@��I@���@���@��@���@��c@���@��d@�w�@�f�@�W�@�M�@�D�@�A�@�=�@�4�@�',@�%u@��@�!@�&@��@��@�
�@��@��@���@���@���@��>@��S@��f@��@��;@���@��@��[@��6@��B@��z@��@��c@��@��[@�� @��@���@��r@��r@��@��`@��@���@��"@��Z@��@��6@��5@��!@��5@��6@��@�� @��@��@��@��@��I@��B@��Z@��_@��Z@��Z@��Z@���@��@���@���@��@���@��@���@���@���@���@���@���@���@��B@��@���@��@��B@��Z@��@��@��@��^@��m@��Z@���@���@��@��@�� @��@���@��@���@���@���@���@��|@��@��@��@��Q@��@���@���@���@���@���@���@���@���@��Y@���@���@��[@���@���@���@���@��F@���@��}@���@���@��P@��@��@��@��F@��@��@��@��@���@��@��@��e@��A@��B@���@�� @��@��H@��&@���@��g@��&@��0@���@�٭@��V@��@��H@���@�� @���@��v@���@���@��I@�{@�y�@�m4@�O�@�/@�%F@�)@�}@�
i@��/@��v@�ɾ@���@�¦@��@���@���@�y�@�_�@�G@�/F@�@��@�
@�S@� �@��0@��R@���@��@��H@��Z@���@���@���@���@���@��y@�{@�m�@�eV@�`@�X@�M>@�;�@�)�@�5@�^@��@��v@�� @���@��@��@��@��@��w@��a@��@��`@���@��s@��t@��;@��@��j@���@���@���@���@��@��@���@�ߺ@�ߣ@��@���@��R@�߶@��@�ޕ@��T@��@�ހ@���@���@��=@��6@�ڽ@��l@�מ@�ժ@�Ҟ@��h@�ρ@��@�Κ@��\@���@�ͳ@��9@���@��v@�̟@��b@���@�˻@��.@�̷@��^@��$@��=@��z@��%@�ɮ@�Ȟ@��g@��'@�@�»@���@�¡@��O@��.@���@���@���@��<@���@��t@���@��Y@��@���@���@���@���@��"@��Z@��b@���@��6@��:@���@���@��q@��@��f@��>@��I@��*@��@��@��R@���@�z�@�a@�8�@�	n@��-@��^@��n@��o@���@�t�@�j�@�dr@�b�@�d�@�d
@�a}@�]:@�R�@�<�@�41@�;�@�8�@�@�>@�;@��@�\@�@�}@��@�X@�
@��@�i@��@��h@��^@��z@���@���@��C@���@��@��y@��v@��v@��H@��J@��@��
@��6@���@��R@��@��~@��~@��{@��@��@��Z@��{@��E@���@���@���@��@�� @��@�ؗ@��M@���@��n@��"@�Ά@���@�Ů@���@���@���@��h@���@��`@���@���@��@���@��'@�u�@�Q@�1�@�(@�&@�?@��@�=@��8@��r@��+@��?@��@�ٔ@��@��H@�ʪ@��!@���@���@��s@���@��b@���@��Q@���@��A@���@�|@�x-@�sB@�m�@�g�@�dD@�e@�b'@�a�@�`�@�`@�^�@�^8@�]&@�[�@�Z2@�X&@�U�@�R�@�O�@�K�@�B�@�5�@�.�@�%H@��@��@���@��@@�ʲ@���@���@���@�p{@�Z�@�:h@� @��@@���@��@��Q@��#@���@��c@���@���@��@��V@��@��J@��O@�}@�z�@�vc@�d
@�SX@�H�@�<�@�*�@�O@��@���@��6@���@��@��V@���@���@���@��V@���@��@��D@��D@��[@��@���@��z@��z@��t@���@���@��$@���@��t@��@��@��Z@��	@���@��!@��f@��X@��w@���@���@��2@�թ@���@�؆@�ׯ@���@��@��6@��^@��P@��;@���@���@�ݮ@�݂@���@�ۢ@�ܶ@��[@��@��@��V@�ț@��`@��t@���@���@���@��@�v@�d
@�W@�K�@�C�@�A�@�@@�<�@�2�@�*�@�(@�$@�4@��@��@�^@�	�@��@�6@��@��@��@�m@��@�Z@��@�%@� �@���@��F@��@��X@��\@���@���@��n@��`@���@��@���@��A@�Ԗ@�ӆ@�΋@��w@���@��2@��v@�i�@�K�@�/�@�f@���@��F@���@�Ж@��>@��n@���@��@��^@���@��@��G@���@��@��l@��a@���@��.@���@���@��W@��@���@���@��@���@���@���@���@���@���@��	@��Z@���@�b@�~�@�}�@�|@�z�@�z�@�z'@�yQ@�y@�y�@�x�@�x�@�x~@�w�@�x@�v�@�p�@�m�@�m3@�l�@�l�@�l�@�l@�k�@�k{@�j�@�j@�iY@�h7@�f�@�e�@�a'@�^�@�_�@�`�@�a~@�b�@�cI@�do@�e@�g@�g�@�i@�i@�j@�j?@�k�@�k�@�l�@�n�@�q&@�s@�p�@�f@�Z�@�Q@�K�@�H@�F�@�E@�A@�<�@�6;@�2�@�4n@�6&@�3@�2�@�3�@�5k@�6;@�6;@�6�@�6�@�8@�9.@�:@�<�@�B6@�D{@�K"@�R�@�Sd@�V-@�W�@�X�@�Xz@�\*@�_�@�`@�b�@�bK@�bQ@�b�@�b�@�dE@�d@�eB@�fj@�g�@�i@�i
@�j@�j�@�k%@�k�@�m@�m�@�m�@�nY@�nY@�n�@�n�@�n�@�n@�o@�q�@�r�@�sT@�t�@�u�@�v�@�y@�xl@�y�@�{�@�{�@�{�@�}+@�v@�z@���@��+@���@��@��N@���@��W@���@��Z@���@���@��f@��y@���@��(@���@���@��]@���@���@���@��.@��V@��'@��b@��%@���@��W@���@��@��)@���@���@��*@���@���@��N@���@��e@��(@��Z@���@��#@��V@���@���@���@��g@�β@��f@��~@��g@��$@��8@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       4344344444344444444444444344443444444334434434443444444444444443433444444444344444444443443444344444443444434434443444444344344334434444443334443344343444344443334333444434434434344443334444333344433434334343333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�:�{�:�{9:�{:�z�:�{I:�{(:�{]:�{�:�|y:�y�:�y�:�{_:�y�:�{�:�xf:�y:�zC:�{�:�z:�z�:�z�:�y�:�z:�yP:�{_:��x:��<:�~2:�~�:�}I:��P:�� :��k:��%:��J:���:��?:�}�:�~�:�~�:�5:���:���:���:��:���:��:���:��{:���:��	:���:���:���:���:���:���:���:���:��&:��#:���:��Y:�� :���:���:���:���:���:���:��:��b:��L:���:���:���:���:���:��_:���:���:���:��:��C:��D:��:��C:��!:��.:��D:��V:��e:���:���:���:��:���:���:���:���:��(:��7:���:���:���:���:���:���:���:��:���:��
:��:��:��1:��C:��p:���:���:��s:���:���:��:��:���:��	:��:��,:��f:��>:��h:��z:���:���:���:���:��b:��f:��z:���:���:��n:��Q:��}:���:��:��?:���:���:���:���:��:��:��Q:��Q:��b:���:���:���:��h:���:��":��":���:���:���:��":��":��Y:��_:��4:���:���:��v:��x:���:���:��:��:��:��@:��@:��@:��R:��x:��:��z:��x:���:���:��:���:��N:���:���:��\:��]:��Y:��Y:��]:���:���:���:���:���:���:���:���:���:���:���:���:��:���:��(:��<:��*:��v:���:���:���:��h:���:��:���:���:��:��n:���:��k:���:���:���:���:���:��k:���:���:���:��:��m:���:��v:��v:���:��j:���:��5:��:��X:��m:��p:���:���:���:���:���:���:���:���:��:��P:��':���:���:��k:���:���:���:���:��|:�� :���:���:���:���:���:��:��b:��a:���:���:���:���:���:���:���:���:��!:�� :��4:��4:��:��2:��.:��E:��1:��2:��Y:��Y:��j:��Y:��T:��C:�� :��X:��E:��C:��V:��[:���:���:���:��:���:���:��V:���:��2:���:���:���:���:���:���:���:���:��z:��:��N:��:��C:���:���:��Y:���:���:���:���:���:���:���:���:��^:��S:���:���:��y:��+:���:��$:��Z:���:��}:��z:��V:���:���:���:���:��:���:��J:���:��L:���:���:��e:��{:��~:���:���:��:���:���:��>:���:��h:���:���:��G:��B:��W:��:���:��:��5:��N:��:��i:���:���:��`:��L:��0:��:��`:���:���:��[:��2:��:���:��*:��j:��:���:���:��:��u:���:�":�y�:�{:�}�:�{�:�yL:�|�:��):��x:�n:��:��:��>:���:��Z:�}:�~w:���:���:��:���:��':���:�{:�xX:�y:�z�:�y�:�y�:�y�:�yr:�z:�{s:�|F:�|	:�z:�yu:�y�:�y(:�y�:�y�:�y�:�y�:�y�:�y�:�yO:�x�:�xX:�xX:�xE:�x�:�w�:�w�:�w�:�x:�xF:�w�:�uw:�q�:�q�:�r:�rD:�r�:�r�:�sT:�sQ:�t2:�rH:�r�:�r�:�s':�r8:�r::�r8:�r8:�pF:�p�:�o{:�o�:�oB:�i�:�kj:�e�:�eG:�e�:�e�:�e6:�d�:�dy:�e�:�d�:�c�:�`|:�`i:�^{:�^�:�_p:�_�:�_:�]:�\�:�\X:�\�:�]:�\�:�\:�Y�:�V�:�U�:�T�:�Sf:�ST:�R�:�Q�:�P�:�OV:�N:�MC:�K|:�KW:�I�:�J:�H�:�H*:�H�:�G�:�FP:�F:�E�:�E�:�C�:�BD:�A�:�@�:�=�:�;_:�8o:�7,:�3:�(�:�T:��:��:��[:���:��:��:���:��O:��f:���:�β:�͘:���:��J:��q:��s:���:���:��:���:���:���:��s:���:��B:���:���:���:��::���:��M:���:���:��Q:���:���:���:���:���:��2:���:���:��:��P:��i:���:��b:��a:��O:��a:��b:��2:��5:��:��:��:��:��Z:��S:��i:��n:��i:��i:��i:���:���:���:���:��:���:���:���:���:���:���:���:���:���:��::��:���:��:��::��P:���:���:��r:��T:��a:��i:���:���:���:���:���:���:���:��r:���:���:���:���:��V:���:���:���:��/:��t:���:���:��@:��A:��T:��z:��Q:��|:���:��Y:��X:���:��:��7:��O:��::���:���:��V:���:���:��.:���:���:��:��W:��:���:��:��:���:��:��u:���:���:���:���:���:��=:���:���:���:��W:��g:���:���:���:��:���:�}�:�p�:�^�:�X�:�N:�K�:�Kh:�K$:�D�:�C�:�8 :��:� :��@:��m:��:��:���:���:���:��-:��M:���:���:�|:�\b:�E:�.�:�b:��:�J:���:��:��:��:��h:��d:��:���:���:��:��g:��c:��m:���:���:�v�:�k:�c@:�^s:�WJ:�M�:�=�:�-Q:� e:��:���:��
:���:���:��:��:��:��:���:���:��':���:��E:���:���:��:��.:��:���:���:��:��:��:��:���:���:��:��,:���:��b:��:��,:��:��}:��G:��:���:���:��h:��:��@:��:��o:��:���:���:��:���:��N:��:�ڴ:��}:��(:���:��_:�ل:��M:���:�ط:��7:�ٚ:��1:��:��,:��c:��:���:���:��4:��:��t:�Й:�о:�Ё:��7:��3:���:���:��
:�̋:���:��;:��{:���:�ɻ:�Ɂ:��+:��8:���:���:��n:�ď:���:���:��:���:��!:��:���:���:���:���:���:��C:��_:���:���:���:�x�:�T:�)y:��l:��:��z:���:��:��8:���:���:��1:���:��N:��:��):���:�p�:�i&:�o�:�mq:�T?:�Ts:�LS:�D�:�G:�E�:�H:�HF:�G:�E�:�A�:�>:�=C:�6�:�4:�*F:�)�:�(�:�(F:�&�:�&�:�&�:�&�:�&�:�&}:�&:�&L:�&E:�&m:�&:�$�:�$�:�$�:�$�:�$�:�$�:�$�:�$�:�$�:�"�:�"�:�"�:� �:��:�T:��:��:�x:�	:��:��:�t:�:�z:��:��:�3:���:���:��C:��:��*:���:�׬:���:��G:��R:��:�m|:�e\:�`�:�Yz:�TN:�P�:�N.:�JR:�BF:�:�:�0�:�)�:�&Y:�#:� �:��:�2:��:�:��X:���:��%:��:��:��I:��7:�ذ:��@:��Q:���:�ƻ:��g:���:��r:���:���:���:��G:��O:��4:���:���:���:���:��k:���:���:���:���:���:��:�y�:�f�:�W�:�<9:�#�:�7:�:���:��4:��:���:���:�u�:�h�:�^�:�W�:�P�:�J;:�E9:�5�:�/�:�*�:�)m:�'�:�"z:�H:�X:�	K:���:��:���:��X:���:��Y:���:���:���:�pk:�Y':�QB:�O�:�O�:�O�:�P[:�Q�:�R�:�S :�S :�S:�S�:�Us:�U�:�U�:�V�:�W&:�X�:�Z:�[�:�_�:�`P:�`_:�e:�iW:�k�:�l!:�j�:�i�:�lo:�m�:�s:�u5:�xU:�zA:�z�:�z(:�z=:�|U:�~>:�~b:�|�:�|u:�~�:��:��:�i:�~�:�}�:�~�:��-:��:�}3:�w$:�l�:�g�:�dN:�]�:�TU:�D�:�5�:�":��:�,:��$:���:���:��m:��p:��p:��E:���:��4:��C:�Ŭ:��:���:��D:���:��	:���:���:���:���:��:���:�� :���:��t:���:��:���:��n:���:��4:��[:��}:�� :��;:���:��:��:��z:���:��:���:�w�:�hp:�T�:�/�:��:���:��
:�� :��:��>:���:��):��m:��{:�� :��Z:���:��A:���:��U:��:���:��:�}h:�x�:�w:�u�:�u:�t�:�t�:�t�:�t�:�v<:�p,:�j�:�e=:�`�:�^�:�^;:�^�:�^�:�\�:�\_:�[A:�Y�:�X�:�X�:�X:�WE:�W:�W�:�V�:�V�:�V�:�V:�V:�T�:�Op:�L�:�LW:�L :�L:�K�:�KQ:�K:�J�:�JX:�I�:�H�:�G�:�F�:�E�:�Az:�? :�@:�A:�A�:�B�:�Cf:�Do:�D�:�F�:�G�:�H�:�H�:�I�:�I�:�J�:�K:�L:�M�:�O�:�Q�:�O^:�FK:�;}:�3 :�.H:�*�:�)�:�(&:�$�:� �:��:��:�$:��:�,:��:��:�>:��:��:�X:��:��:��:�s:�:��:��:�#�:�*�:�+G:�-�:�/j:�0M:�/�:�30:�6J:�6�:�8�:�8�:�8�:�8�:�9;:�:�:�:I:�;d:�<o:�=�:�>�:�>�:�?�:�@/:�@�:�A:�Bk:�C :�C(:�C�:�C�:�C�:�C�:�D:�CJ:�D.:�F�:�Go:�H:�I7:�Ju:�KZ:�M�:�L�:�N :�O�:�O�:�O�:�P�:�S:�S
:�TY:�Uw:�U�:�X:�Wd:�W�:�Z!:�X�:�XV:�Y�:�Y�:�[:�\:�^�:�_`:�`:�_�:�a^:�`�:�`�:�a�:�b:�c%:�c�:�d�:�c�:�c�:�f�:�f�:�fx:�g:�i�:�j�:�l:�ls:�j�:�m
:�m�:�p�:�qj:�w�:�w�:�zj:�|f:���:��:��s:��:��|:���:���:��:��Q:���:���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
=B�NB�NB�NB�`B�sB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�yB�sB�mB�`B�HB	0!B��BbBbBPBJ�B�B�BoBB��B��B��B	7B�BoBBB
=BDB�B�B �B'�B!�B�B�B�;B��B~�Bp�Bp�B�1B�9B�LB��B��B��B�BhsBe`BS�B<jB-B\B�mBĜB�?B��B��B�oB�=B��BiyBN�B:^B-B!�B�BhBBBBB
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?X��B��?v��?:�@B9&?e�@�wn@�ď?ur�?@ B*�Ah�?;yAP=[?#��A %�A��`?C�?8?Ax�b>�,R?@U�A�H�?$�A�B�?���?}A�?�x)?oŽBV�>���>�$�>���>���?)ޭAbUBZB��?e�?���B&�@à?K��A��^?�?��@�~B>�@��>4$�>W��>h�>�2�>��>��(>� �@��k?x?�OA
�_?3k�Ak�HB@G?*A��sB3\AY�@�`;>�R/>���>�L�?` ?xyA�o@��{BP?6|?cK�@_�?%I�>�w�A���?jĂ@K��@�ҏ?D��B6�?#0?���Bbj>�N�@M�?}2B��?�@��\@��>ݛ�?c?�F?@�A�Q@��A>�UT?��?��A�O�?�Or@^2HB�s>�Q?n�?y�A�8�>��?
��@�eg?#2�@0��?P��B_M?�l@AP>uB"@��3?�F A���B=�?!�_@�;BlH>��@�V>�@���>�o3?;�A��B*0BB�@�?5N�A��sB3YB<�@��T?*�VA��hA�BZ�?#S?+��@�>B��Au�>�Ĝ? 6�?&�B7�B%tB_'?i%�B9>B9B3�A`p�>��?A_?��%B=�?!:�AC�B+!A��x?o��BA�?o(�B/`AN_?]�1?%�?��B1�B*�B?G@�%@"�,?_1?�A�3`B3RB/ B��?.[?���?���B3�BZ�A�=�A���?$WB)IBN�@\nB>�Az��B<dA�e�B�B5�Bx�B7�B>gA�B6�B7RB6*B5�B8�A��B6iB6B7�B8_B8�B;jB:�B7cB6�B6nB7B6�B6�B6�B7Bf�B6�B8@B9�B6�B5�B9�B9B5�B8�B6�B8B;�B ~B5YB6"B4�B5�B4�B4�B3HB5�B7�B8�B6�B�rB5B1�B5�B7/B6B8B5SB5KB4KB6B6�B7�B;B8�B6�B6�B6�B3�B7�B93B9�B1]B3B4�B=B9�B9�B8�B7�B:5B3HB0vB2.B7B7B:B5�B5�B6�B7�B7iB8�B6�B7�B8�B9XB7�B9}B9�B7�B6IB7B7�B6<B4�B7�BD�BXGB6�B78B6dB7�B7B5�B7{B:]B78B:B8	B7B9�B6�B;jB= B7B7�B6�B5jB6,BB�A��mB6`B6'B5�B6�B8B6�B8DB8qB9�B92B5�B5�B6XB5#B5CB4�B9�B5�B5�B<�B5XB3wB3ZB4�B8�B7�B5\B6�B7qB8(B6B6�B9PB:EB6�B64B5�B5�B6�B6�B8\B7B7�B8ZB5�B4�B5�B5�B7�B9�B5�B6�B5�B6TB7�B7�B8RB7�B7�B9~B7/B6uB6uB5�B6�B6FB5rB6nB5fB9B5}B5�B6B6�B7�B6fB6B7B6]B6�B7GB9�B6B5fB6B6�B6B4�B6`B6�B6FB69B3�B7�B:(B5B7�B7qB9MB7�B7�B61B5B2�B3�B6nB5�B5�B8�B8XB6�B7B5\B6�B9B6fB5 B7B>%B6�B6B4B5�B5+B6�B5+B6B7�B6\B6\B5�B5VB5�B6B4�B5�B6qB5�B6DB6/B7�B5B4�B5jB4�B7B5�B4�B5MB5B6B5=B5=B6�B6eB5UB5B6B4�B6B6B5<B6QB5�B5�B5B5kB4�B4�B5�B4KB5WB5�B6jB4�B4*B6&B5�B6B6NB5>B6B6�B5�B6�B5tB5LB4�B4�B4�B5B6�B7UB6B5B6'B4B6�B5�B6�B5~B4sB6�B4AB67B7�B7B<9BDcB;iB9B9@B9�B9�B7�B7�B9�B:�B9�B9MB=3B<�B=�BA�B=&B=�B<�B<�B<�B>�B>uB?hB>8B>�B>�B>B>�B>�B>sB>�B=�B?5B>nB> B=.B=�B>=B=�B>pB>hB>`B>PB>?B>7B>'B>B>B>-B>zB>MB?4B>�B>$B>zB>�B>bB?�B?qB>�B>�B>B>�B>�B=�B?B>�B>�B>�B>�B>�B>DB?�B>,B=�B>3B?KB>�B>B>�B>QB>�B?`B>�B??B?7B>�B>VB>�B>=B?�B>�B>�B>~B>�B>eB?jB?B?�B>�B>�B>�B?lB?\B?TB?7B?'B>UB?B?�B>�B>{B?<B>�B>�B>�B?B>�B?�B>�B?�B>�B>�B>�B>�B>�B?NB?FB>uB>dB?�B?�B>B>�B>�B?xB?HB?B?UB>�B?B?B?B?�B>�B?�B?5B>cB?{B>�B>�B>�B?RB>yB>pB>�B?�B?pB?�B>�B?BB>�B??B>�B>�B?�B>EB?\B?�B?�B?�B>�B?B?�B?B?�B?YB?DB?<B?,B?�B?gB?�B?B?GB>�B?AB?�B>�B?B?WB@�B?�B>�B?/B?�B>�B?�B?qB?�B>�B@B?�B?AB?9B>�B?�B?�B>�B?�B?~B@B>�B?>B?6B?�B?�B?=B?�B?B?dB?[B>�B?�B?cB@$B?�B?JB?�B?�B?}B@?B?�B?eB@B?�B@hB?�B?�B?(B@GB?vB?vB?�B?fB?�B?�B?�B@B?=B?5B?-B?%B?{B?B@4B?cB?�B?�B>�B>�B>�B?MB?�B>�B>xB?'B>UB>mB>�B?�B>�B>.B>�B?2B>aB>YB?B>hB>�B>XB>PB=�B=8B>�B>B<�B=QB=�B=�B<qB;NB>B=iB>rB=�B>�B>�B?�B@tB=fB>rB>�B?,B=�B?hB>�B?�B>�B?�B>B@GB?�B>�B?aB>�B=�B>AB>�B?MB>0B@B=B;�B<�B<GB?dB=kB>4B;�B;�B>�B<XB=�B>NB<�B?B<@B<�B=B<WB<�B=�B={B=�B=5B=�B=-B;SB>B>�B=�B=!B>�B<�B<�B<�B>�B>UB<B=�B<�B?B>=B<�B:B:}B; B8�B8�B?MB=�B?\B>sB?�B<`B=mB=B:�B;�B;�B;�B8$B?B:�B?�BAB;B:[B6RB< B:�B<�B=B<ZB<�B<�B<�B=LB<[B=B;�B<�B;:B;�B<�B<AB<�B<VB;/B:�B;FB:�B:;B9�B;B;�B:+B8SB94B8�B:(B<B9WB7�B8,B8�B8DB9YB8�B8lB6�B7B6B9FB6�B4GB4LB3�B4�B5�B4�B6qB2B3�B3�B3MB/�B5PB4 B4�B7B5B5qB5�B3YB/0B3B36B3�B3TB4�B5�B4B1�B0�B2RB3B3 B4B1'B4B1�B,LB/�B/�B.�B.LB/�B.B,XB+�B,�B-rB0PB-�B+�B*�B*�B*�B)�B(�B'7B)�B)$B(�B'EB$XB!vB�B�B9B#BSB
B�B�B�B ~B �B&�B*lB*~B(:B*�B3 B1B2�B1�B6�B4]B9�B8�B7�B8B;�B=jB?�BBKBCBB�BA�B@�B?B>�B@�BDDBC�BE�BF�BD�BFBD�BE�BA{BD�BC'BC�BB�BC�BC�BC�BC�BC�BC�BDgBCBDGBD�BCuBB�BC�BC
BD�BCUBC�BD�BD�BB�BC�BCUBD�BCUBD BCUBB�BCUBD~BD BCUBC�BC�BD?BC�BF?BB�BB�BC�BA�BACB@�B?�B>:B=�BD�BE�BC�BDtB@�B=�B=�B=2B<)B<HB:uB9�B9kB8�B9~B6�B9�B2�B1�B0�B,�B,FB)B(�B%�B$CB#B!�B �B*B�BQB�BB0B�B
LB�BqB�B	)B�B�B��B�PB�B�rB�B��B��B��B�-B�RB�XB�B�1B�1B��B��B�^B�rB��B�B�rB��B�zB��B��B�B�*B��B�LB��BݓB��B�?B�B�B�B��B�2B�B�B��B�B�BrB|B"B'�B4�B4�B(B"6BWB�B�B B!�B&�B- B8sBF�BCHB?BC�B8�B;�B=�B:TB6�B4gB5�B8B7�B5�B:B=�B@�BC�BD�B@[B;�B9�B<�BA�BEEBI�BW�BZ�BZ]B]�B^GB]�B^�B^uB_:B^B]�B^#B]mB\8B\WB\OB[TBZ�B[*BX�BZ�BZ�BZ�BY�BYdBX�BXBUBSUBY&BR�BQ9BS~BZ�BRuBX�BW�BQYBM�BPBK�BM�BMGBF&BDzB=�B9B5�B2�B1�B-kB*�B(mB'�B)B+bB.WB5�B6{B6SB5�B8=B:B;3B:fB:B:]B9�B:�B;�B<�B;B8:B6�B4�B1�B2�B0VB-�B'�B#�B�BEB�BtB6B�B	�B~B B�B�BMBB�BBoBM�BUHBn�BtABn�Bs�BnB]�B\�BSyB?HB,-B/6B22B/iB;BB;�B5�B3�B;�B<�B:�B6)B3`B1*B6�B3�B,hB,WB9~BFYB:�B.�BM�BOBR�BOwBL�BLBK�BG�BINBGcBR�BQlBa�B]�BckBk�BisBi(Bj@Bh�Bg*BeBbLB`HB^�BZ�BYBWWBX�BYTBZ#BXBYbBZHBXBWIBYbBXBV�BOwBN>BQBZ}BWhBU�BT@BL�BN~BG�BJ�BJ�BH&BQaBSSBMtBMjBIfBM#BI�BL�BG�BCB2dB1<B,bB2�BADBJ-BN�BN�BSXBP�BO�BN}BH$BE�BD�BF�BGIBF+BDBAdB?�B9XB5�B9?B9CB>oBC�BA�B@^B?�B?LB>�B<;B=PBC4BB�B@�B=�B?B<�B:VB8�B7�B5/B3B0B-mB)5B&�B#HB�BB[B�BzB
�B��B�;B��B��B��B��B�:B�B��B�B��B��B��B��B�B��B B��B�=B�|B��B��B�B��B��B؈B�)B��B��B�AB�B�B��B�qB�B�$B�PB��BB%B�B�B�B�B�B�BCB�B �B"B!B!�B"�B"4B#_B$<B$�B%B%�B'B)�B,
B,B,eB+�B-�B/B1�B4@B46B4sB3qB4kB:�B9ZB<#B8pB<;BH�B?~BD�BI�BN�B\!BM�BNBObB` BX�BQB`�Bg�Ba�BZ�BXBP`BK�BRBU>BJ�BO�BRBV?BZWB^nB].BZ5BV�BY�B]�B\%BX�B]�BbuBbmBc-BbeBbBa<Ba�Ba|B`�B__B^�B]NB\JBZ`BY�BYJBV�BUBT�BR�BN�BLCBL	BJfBIXBG�BCyBC{B?qB9TB2B(uB$tB$�B&�B0>B4�B3�B3�B?"BFkBHBI�BG�BIBJ�BL�BH2BK�BMBM�BLXBKzBM�BLGBKBM�BO�BQ�BR,BQBQ�BRYBS�BU�BV�BXdBY�B]�B^^B^�B^�B^�B\�B^�B\�B]�B]�B^�B]YB]eB] B]B\B[�B[JBZBY\B[�B[�B\B]}B\�B^^B_pB_�B_�B^�B_�B_�B`�B`�Ba�B`�BeCBg�Bi�BinBj�Bj�Bi�Bj�Bj_Bi1Bh�BhBg�Bg�Bf�Bf�BgUBfwBd�Bb�B]WBWTBUB_"B^�B`�B]�BZ�BYmBV�B[�BZ�BZ�BX�BV�BVdB]�B^aB\�Ba�BlyBe�Bl�Bk�Bu�BxBu�BvBv�Bu3BsVBtBnBc{BXdBX{BY�BZ�BWBUBMNBKqBJkBK�BI BI�BE�BA�BB�B<B=�B=jBB_BG�BD.BB[BIMBD�BE,BI�BS5BS>BMXBOxBI�BN�BP*BK�BJ�BM�BQ�BP|BP�BYlBX�BZ�BY;BL�BN�BKXBN7BX"BNBM�BT*BW�BR�BQnBW�BWpBX�B[�B\�BP�BL�BN�BO�BKZBM�BSBQ�BR�BN�BRMBX/BY�BZ'BY�BQ_BS�BS�BYB[mBX�BY�BR0BLcBQBXB_B]�Bf�B_�BWrBc�BXCBj^B\�BbCBc�Bl�BgHBhvBe�Bj�Bg�B��B�B��B�QB�B�B��B�B�eB��B��B�B��B�nB��B�B�OB�B�B��B�B�B��B��B�B��B�B�B�uB��B�B�B�B�IB�B��B��B�MB�EB�B�B�B�pB�B�B�B��B��B�kB��B�XB�B�qB�^B��B�B��B��B�)B�B��B�=B�xB�RB�dB�B�B�iB�OB�4B�IB�B�jB��B�B��B��B��B�4B�IB��B�B�9B�B�B��B��B��B��B��B��B��B�B�B�IB�AB�B�)B��B��B�'B�1B�B�B�zB��B�OB�ZB�pB�B�B��B�xB�B�B��B�yB�B�sB�PB��B�B��B��B�B��B�B��B�B�B��B��B�B�+B�B� B�tB�B�oB�B��B�B�~B�.B�B��B�B�nB�fB�|B�B�B�B��B��B�B�B��B�B��B��B�dB�TB��B��B��B�+B�B�LB�DB�B�JB��B��B�B�B�
B�2B��B�B�8B�0B��B�*B�@B��B�(B� B�B�SB�B�(B�B�QB�SB��B��B�B�B�yB��B��B�B�B�B�qB�B�B�B�gB�B��B�B�wB�B��B�B��B�4B�B�B��B�B�!B�B�B�9B�B��B�B��B�B�	B��B��B�xB�B�B�XB�B�B�B�?B�/B��B�B�5B�^B�(B�kB�vB�B��B��B�B��B��B�B��B�B�B�0B�B�+B�6B��B�B�1B�)B��B��B�	B�B�&B�B��B�GB�>B�B�B�B��B��B�B�B�!B�B��B��B�&B�1B�)B�B�B��B�B��B��B�B��B�ZB�B��B��B�B��B��B�B��B��B�B��B��B�"B�<B�YB�B�+B�B��B��B�B��B�B�B�B�B�=B��B�B�B�JB�XB�XB�B��B�B�B�#B�TB�.B�&B�B�pB�$B�B�B�B�@B�B��B��B�{B�B�B�B�B�nB�-B�^B�B��B�lB�NB��B�>B�vB��B�B�}B�xB�RB�B��B�B��B�hB�	B�EB�}B�fB�-B�qB�B�mB�B��B�B�B��B�B�TB�nB�[B�mB�mB�^B�,B�,B�{B�MB�UB�B�B��B��B�B�B�B�iB��B��B�B�B��B�<B��B�B�4B�B�7B��B�B�/B�$B�B��B�B�_B�OB�B�=B��B�B�MB��B��B��B��B�B�CB�B�B�{B�B�BB�B�6B�AB�B�B�1B��B�OB�lB��B�B�B�rB�rB�XB��B�B��B�B��B��B�CB�B�B��B��B�B��B��B�B��B��B�B��B��B�B�\B�B�LB�LB�B��B��B�BB�B��B�B��B��B�B�B��B�3B�B�=B�HB��B��B�B�dB��B�PB�B�#B�B��B�B�B�B��B�MB�B�B�B��B�dB�QB��B�_B��B�^B��B��BߘB�rBߓBߦB�KB�NB�BޯB߾B�rB߮B��B� B޷B�QB߅B�.B�!B�FB�B�B�B�B�uB��B�XB�}B��B�B��B�lB��B�?B�0B��B�SB��B��B�B�B�B�\B�rB�}B�B�B�bB��B�B��B�GB�GB�oB��B��B�UB�B�B�B�B�'B�'B�&B��B�=B�PB��B�'B�}B�wB�B�B�B��B��B�B��B��B�B�B�DB�B�B�WB�B�B�B�}B�B�B��B�&B��B�B�B�EB��B�B�B��B��B�'B�B��B�B�'B�:B��B��B��B�zB��B�'B�B�B�B�B��B�`B�B��B�B�B��B��B�2B� B�*B��B��B��B��B��B��B�B��B�FB� B�~B�'B�B�B��B�B�CB��B�{B��B��B�B�B�B�,B�/B��B�B��B��B�B�B�B�B�B��B��B�fB�.B��B��B�sB��B�B�B��B��B�B��B�TB�AB��B��B��B�/B�+B��B��B�B��B�wB��B��B�B|B�BJBtB�BAB�BoBB�B<B3BB�B�B�B�BBB�BUB>BeBKB�B�B�B BBB�BAB�BB�B�BB�B�B�B{BB�B�BHBpB�BB
�B.B%B%B�B�B(B
�B
|B
�B�BgB�BB�BgB%B�B�B�B�B�BCBB2BB�BwB�B"BBB$B	B`BBPBuB�B`B�BQBBvBB�B3B�B5BB:B�B�B�BPB�B�B�BB�B�B{B�B6BTB�BVB}B�B�BB�B�B�B�B@BvBBNB�B�BFB�B<B�B�B�B�BrBB�B�B�BBnB�B3B�B�B�B�B�BOBBmB�B	BaB&B�B�BIB�B�B4B �B�B�B�B�BcB�B:B�B	�B	�B�B�B
B	�B
=B~B�B	�B
$B
�B�B�BdB$BbB�B�B�B�B�BBBBB�BbBYB�BJBBHB,B$BBbBZBBiB�B�B'B8B�B�B�B�B8B�BZB�BKB�B�B�B�B~B�B�B7BZB5B&ByBxBBmB�B�B8BjB�BPB�B�B�B�BOBeB#B
�B	�B	�B�BPB	B�B�BbBEBQB[B yB >B �B�"B��B��B�IB��B�qB�B�B�B�B��B�cB��B��B�oB��B��B�!B��B�aB�,B��B��B�;B��B�?B�B�B��B�]B�xB�B�B߼B�MB��B�4B�}B�hB�#B�MB��B��B�eB͘B�0B̑B˷B̈B�'BʈBɂBˉB�zB�aB��B��B�8B�bB��B��B�\B��B� BħB�cB�oB�BƜBȫB��B��B�BȱBɫB�JB�B�aBʔBʣB�1B��BηB�B�B�IB��B��B۝BۑB�^B�KB��B��B�=B�JB��B�:B�
B��B�DB�B��B�/B��B�BtBWBWBXB�B"B�BjB�B�B�B�BTBsBUBIB�B�BB�B�B�B�BB�B0BB�B�BB�B�B�B�B�B:BWBB�B�BBWBjB�BVBBnB�B�B�B6B�B/B"B�B=BB�BB�B�BXBoBBvB�BB
�B
BoB�B�B�B B B tB�B�AB��B��B��B��B�/B�B�kB�)B��B��B��B �B zBB �BzB�B
�B�BxBdBBBB
�B�B/B�B.B"B	�B
�B
 B	�B�B	XB	lB	2B	B
B	HB	�B	wB	6B	^B	�B�B�B3BB�B�B�BB-BqB�B�B�BeBJB0B/B�BB�B�B�B�B�B�B�BB=BiB�B�B�B �B�B�BVB-B&B~BrB B�B]B�B3B/B
�B
��B
��B
��B
��B
��B
�WB
��B
��B
�4B BFB
��BMB�BB
�qB|B�B�B B�B
�fB
��B
��B �B�B�BKB
B
��B
��B
��B �B 9B ,B
��BB�B EB ;B�B FB
��B
��BSB\B �B{B�BB ]BB�B9B�B�B �BPB�B�B�B�BsB>BSB�B�BXB9B B.B�BB�B�B�B�B"B~B�B�B�B�B�B$B�B�BB�BQB�BTBcBBsB#B�B�BB�BqB�B�B�B�BOB�B[B:B
�BrBQBiB�B
BzB	BVB
\oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�422222222222222222222222222222422224222222222222222222222222222222222222222222222242222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994344344444344444444444444344443444444334434434443444444444444443433444444444344444444443443444344444443444434434443444444344344334434444443334443344343444344443334333444434434434344443334444333344433434334343333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�B�QB�QB�SB�cB�uB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�|B�yB�rB�fB�LG�O�B��BhBeBVG�O�B�B�BsBB��B��B��B	8B�BqBBB
?BFB�B�B �B'�B!�B�B�B�@B��B~�Bp�Bp�B�6B�?B�NB��B��B��B�BhvBebBS�B<pB-B^B�tBĠB�EB��B��B�qB�AG�O�Bi}BN�B:cB-B!�B�BnBBBB	B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�B9(G�O�G�O�G�O�G�O�G�O�B*�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B G�O�G�O�G�O�G�O�BV�G�O�G�O�G�O�G�O�G�O�G�O�B^B��G�O�G�O�B&�G�O�G�O�A��cG�O�G�O�G�O�B>�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B@KG�O�A��uB3`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BRG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B6�G�O�G�O�BbqG�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q"G�O�G�O�G�O�G�O�A�O�G�O�G�O�B�uG�O�G�O�G�O�A�8�G�O�G�O�G�O�G�O�G�O�G�O�B_OG�O�G�O�B"G�O�G�O�A���B=�G�O�G�O�BlLG�O�G�O�G�O�G�O�G�O�G�O�A��B*5BB�G�O�G�O�G�O�B3]B<�G�O�G�O�A��nG�O�BZ�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�B7�B%uB_*G�O�B9DB9B3�G�O�G�O�G�O�G�O�B>G�O�G�O�B+'G�O�G�O�BA�G�O�B/cG�O�G�O�G�O�G�O�B1�B*�B?KG�O�G�O�G�O�G�O�A�3eB3TB/B��G�O�G�O�G�O�B3�BZ�G�O�A���G�O�B)OBN�G�O�B>�G�O�B<gA�e�B�B5�Bx�B7�B>iA�%B6�B7TB6,B5�B8�G�O�B6lB6B7�B8bB8�B;nB:�B7eB6�B6pB7B6�B6�B6�B7Bf�B6�B8BB9�B6�B5�B9�B9B5�B8�B6�B8B;�B �B5[B6!B4�B5�B4�B4�B3LB5�B7�B8�B6�B�vB5B1�B5�B72B6B8B5UB5QB4NB6B6�B7�B;B8�B6�B6�B6�B3�B7�B96B9�B1_B3�B4�B=B9�B9�B8�B7�B::B3LB0{B2-B7B7B:B5�B5�B6�B7�B7nB8�B6�B7�B8�B9ZB7�B9�B9�B7�B6LB7B7�B6?B4�B7�BD�BXIB6�B7:B6jB7�B7B5�B7}B:`B7:B:B8B7	B9�B6�B;mB=B7B7�B6�B5mB61BB�A��vB6eB6)B5�B6�B8B6�B8EB8tB9�B96B5�B5�B6[B5%B5GB4�B9�B5�B5�B<�B5]B3{B3`B4�B8�B7�B5_B6�B7vB8*B6B6�B9SB:GB6�B65B5�B5�B6�B6�B8`B7B7�B8]B5�B4�B5�B5�B7�B9�B5�B6�B5�B6VB7�B7�B8WB7�B7�B9�B71B6wB6wB5�B6�B6LB5wB6qB5jB9B5�B5�B6B6�B7�B6hB6B7B6aB6�B7JB9�B6B5jB6B6�B6B4�B6eB6�B6LB6=B3�B7�B:*B5 B7�B7tB9OB7�B7�B65B5B2�B3�B6qB5�B5�B8�B8^B7B7B5^B6�B9B6eB5B7B>(B6�B6B4B5�B5-B6�B5-B6B7�B6^B6^B5�B5\B5�B6B4�B5�B6wB5�B6HB62B7�B5
B4�B5nB4�B7
B5�B4�B5SB5B6B5?B5?B6�B6hB5WB5B6B4�B6B6B5?B6TB6B5�B5B5nB4�B�B�B��B�SB�B�B��B�B�gB��B��B�B�B�pB��B�B�SB�B�B��B�B�B��B��B�B��B�B�B�{B��B�B�B�B�NB�B��B��B�UB�JB�B�B�B�sB�B�B�B��B��B�nB��B�ZB�B�xB�cB��B�	B��B��B�.B�"B��B�AB�~B�SB�gB�B�B�lB�QB�6B�NB��B�nB��B�B��B��B��B�8B�LB�B��B�=B�B�B��B��B��B��B��B��B��B�B�B�NB�EB�$B�+B��B�B�-B�4B�B�B�|B��B�NB�^B�rB�B�B��B�zB�B��B��B�|B�B�wB�UB��B�B��B��B�B��B�B��B�B�B��B��B�B�/B�B�B�tB�B�rB�B��B�B�B�4B�(B��B�B�tB�iB�|B�B�B�B��B��B�B�	B� B�B��B��B�gB�YB��B��B� B�/B�B�NB�JB�B�LB��B��B�B�B�B�6B��B�B�:B�6B��B�/B�CB��B�-B�"B�B�UB�B�-B�B�SB�YB��B��B�B�B�~B��B��B�B�B�B�wB��B�B�B�iB�B��B�B�|B�B��B�B��B�8B�B�B��B�$B�$B�B�B�AB�B��B�B��B�B�B��B��B�zB�B� B�[B�B�B�B�CB�4B��B�B�8B�`B�-B�nB�zB�B��B��B�B��B��B�B��B�B�B�4B�B�-B�:B��B�B�4B�-B��B��B�B�B�(B�$B��B�FB�CB�B�B�B��B��B�$B�B�(B�	B�B��B�+B�6B�-B�B�B��B�B��B��B�B��B�\B�	B��B��B�B��B��B�B��B��B�B��B��B�(B�CB�YB�B�/B�B��B��B�B��B�B�B�B�B�CB��B�	B�B�HB�[B�[B�B��B�B�B�&B�WB�0B�&B�B�vB�'B�B�B�B�BB�B��B� B�B�B�B�B�B�rB�0B�dB�B�B�mB�SB��B�BB�xB��B�B�B�B�UB�"B��B�B��B�oB�B�EB�B�lB�/B�vB�B�qB�B��B�B�B��B�B�WB�qB�]B�oB�qB�aB�0B�0B�B�SB�XB�B�B��B��B�B�B�B�lB��B��B�B�B��B�?B��B� B�7B�B�9B��B�B�2B�&B�B��B�B�eB�SB�B�AB��B��B�OB��B��B��B��B�B�EB�B��B�B�B�EB�B�:B�CB�B�B�4B��B�QB�pB��B�B�B�uB�uB�\B��B�B��B�B��B�B�GB�B�B��B��B�B��B��B�B��B��B�B��B��B�B�_B�!B�QB�QB�B��B��B�GB�B��B�B�B��B�B�B��B�8B�	B�@B�JB��B��B�B�fB��B�SB�B�&B�B��B�B�B�B��B�QB�B�B� B��B�hB�UB�B�eB��B�bB��B��BߚB�rBߘBߨB�MB�SB��B޶B��B�tB߰B��B�B޻B�SBߊB�1B�#B�KB�B�B�B�B�zB��B�YB�B��B� B��B�rB��B�EB�3B��B�ZB��B��B�B�B�B�`B�wB�B�B�B�fB��B�B��B�KB�LB�tB��B��B�XB�B�B�B�B�*B�*B�*B��B�?B�TB��B�,B�B�{B�B�B�B��B��B�#B��B��B�B�B�KB�B�!B�[B�!B�!B�!B�B�B�B��B�*B��B�B�B�IB��B�B�B��B��B�*B�B��B�B�*B�?B��B��B��B�B��B�,B�B��B�B�B��B�eB��B��B�B�B��B��B�3B�&B�.B��B��B��B��B��B��B�B��B�HB�!B�B�)B�B�B��B�B�FB��B�B��B��B�B�B��B�.B�3B��B�B��B��B�B�B�B�B�B��B��B�iB�3B��B��B�uB��B�B�B��B��B�B��B�XB�DB��B��B��B�1B�-B��B��B�B��B�zB��B��B�B}B�BLBxB�BEB�BnBB�B?B8BB�B�B�B�B BB�BYB@BiBMB�B�B�BBBB�BEB�BB�B�BB�B�B�B}BB�B�BKBsB�B	B
�B4B+B+B�B�B-B
�B
{B
�B�BlB�BB�BiB(B�B�B�B�B�BEB�B7BB�B~B�B%B!BB'BBbBBVBwB�BdB�BUBByBB�B7B�B6BB=B�B�B�BSB�B�B�BB�B�B}B�B8B[B�B[B�B�B�BB�B�B�B�BDB|BBNB�B�BJB�B>B�B�B�B�BvBB�B�B�BBrB�B6B�B�B�B�B�BQBBqB�B	BdB*BB�BMB�B�B7B �B�B�B�B�BgB�B>B�B	�B	�B�BB
B	�B
?B�B�B	�B
)B
�B�B�BiB&BhB�B�B�B�B�BBBBB�BgB[B�BLBBKB/B(BBeB^BBhB�B�B,B;B�B�B�B�B;B�B_B�BOB�B�B�B�B�B�B�B:B\B<B'B~B{B	BlB�B�B:BnB�BTB�B�B�B�BSBgB'B
�B	�B	�B�BTB	"B�B�BdBJBVB_B {B =B �B�$B��B��B�KB��B�tB�B� B�B�B��B�iB��B��B�tB��B��B�'B��B�cB�0B��B��B�?B��B�BB�B�"B��B�_B�yB�B�B߿B�PB��B�6BтB�lB�&B�OB��B��B�hB͚B�5B̓B˹B̌B�+BʊBɆBˏBɀB�cB��B��B�:B�hB��B��B�^B��B�BĩB�cB�tB�BơBȮB��B��B�	BȳBɯB�OB�B�bBʘBʦB�3B��BνB�B� B�NB��B��B۟BۓB�bB�MB��B��B�@B�PB��B�?B�B��B�LB�B��B�1B��B�BwBYB[B_B�B(B�BpB�B�B�B�BWByBWBNB�B�BB�B�B�B�BB�B4BB�B�BB�B�B�B�BB@B[BB�B�BBYBoB�B[BBqB�BB�B8B�B5B%B�B?B�B�BB�B�BZBtB"BuB�BB
�B
BrB�B�B�B 
B 
B zB�B�FB��B��B��B��B�6B�B�nB�/B��B��B��B �B }BB �B}B�B
�B�BzBjBBGB
�B�B1B�B1B!B
B
�B
B
B�B	]B	qB	4B	 B
B	JB	�B	yB	9B	`B	�B�B�B8BB�B�B�B�B1BvB�B�B�BjBLB5B4B�BB�B�B�B�B�B�B�B BBBkB�B�B�B �B�B�B[B1B)B�BwBB�BaB�B7B1B
�B
��B
��B
��B
��B
��B
�[B
��B
��B
�7B BIB
��BPB�BB
�uB}B�B�BB�B
�hB
��B  B �B�B�BMBB
��B
��B
��B �B >B 1B
��BB�B KB AB�B KB
��B
��BVB_B �B~B�BB `BB�B>B�B�B �BRB�B�B�B�BvBCBUB�B�B]B;B$B3B�BB�B�B�B�B'B�B�B�B�B�B�B'B�B�BB�BTB�BXBiBBwB'B�B�BB�BuB�B�B�B�BQB�B_B?B
�BvBSBnB�BB�BB[B
\rB�B�B��B�SB�B�B��B�B�gB��B��B�B�B�pB��B�B�SB�B�B��B�B�B��B��B�B��B�B�B�{B��B�B�B�B�NB�B��B��B�UB�JB�B�B�B�sB�B�B�B��B��B�nB��B�ZB�B�xB�cB��B�	B��B��B�.B�"B��B�AB�~B�SB�gB�B�B�lB�QB�6B�NB��B�nB��B�B��B��B��B�8B�LB�B��B�=B�B�B��B��B��B��B��B��B��B�B�B�NB�EB�$B�+B��B�B�-B�4B�B�B�|B��B�NB�^B�rB�B�B��B�zB�B��B��B�|B�B�wB�UB��B�B��B��B�B��B�B��B�B�B��B��B�B�/B�B�B�tB�B�rB�B��B�B�B�4B�(B��B�B�tB�iB�|B�B�B�B��B��B�B�	B� B�B��B��B�gB�YB��B��B� B�/B�B�NB�JB�B�LB��B��B�B�B�B�6B��B�B�:B�6B��B�/B�CB��B�-B�"B�B�UB�B�-B�B�SB�YB��B��B�B�B�~B��B��B�B�B�B�wB��B�B�B�iB�B��B�B�|B�B��B�B��B�8B�B�B��B�$B�$B�B�B�AB�B��B�B��B�B�B��B��B�zB�B� B�[B�B�B�B�CB�4B��B�B�8B�`B�-B�nB�zB�B��B��B�B��B��B�B��B�B�B�4B�B�-B�:B��B�B�4B�-B��B��B�B�B�(B�$B��B�FB�CB�B�B�B��B��B�$B�B�(B�	B�B��B�+B�6B�-B�B�B��B�B��B��B�B��B�\B�	B��B��B�B��B��B�B��B��B�B��B��B�(B�CB�YB�B�/B�B��B��B�B��B�B�B�B�B�CB��B�	B�B�HB�[B�[B�B��B�B�B�&B�WB�0B�&B�B�vB�'B�B�B�B�BB�B��B� B�B�B�B�B�B�rB�0B�dB�B�B�mB�SB��B�BB�xB��B�B�B�B�UB�"B��B�B��B�oB�B�EB�B�lB�/B�vB�B�qB�B��B�B�B��B�B�WB�qB�]B�oB�qB�aB�0B�0B�B�SB�XB�B�B��B��B�B�B�B�lB��B��B�B�B��B�?B��B� B�7B�B�9B��B�B�2B�&B�B��B�B�eB�SB�B�AB��B��B�OB��B��B��B��B�B�EB�B��B�B�B�EB�B�:B�CB�B�B�4B��B�QB�pB��B�B�B�uB�uB�\B��B�B��B�B��B�B�GB�B�B��B��B�B��B��B�B��B��B�B��B��B�B�_B�!B�QB�QB�B��B��B�GB�B��B�B�B��B�B�B��B�8B�	B�@B�JB��B��B�B�fB��B�SB�B�&B�B��B�B�B�B��B�QB�B�B� B��B�hB�UB�B�eB��B�bB��B��BߚB�rBߘBߨB�MB�SB��B޶B��B�tB߰B��B�B޻B�SBߊB�1B�#B�KB�B�B�B�B�zB��B�YB�B��B� B��B�rB��B�EB�3B��B�ZB��B��B�B�B�B�`B�wB�B�B�B�fB��B�B��B�KB�LB�tB��B��B�XB�B�B�B�B�*B�*B�*B��B�?B�TB��B�,B�B�{B�B�B�B��B��B�#B��B��B�B�B�KB�B�!B�[B�!B�!B�!B�B�B�B��B�*B��B�B�B�IB��B�B�B��B��B�*B�B��B�B�*B�?B��B��B��B�B��B�,B�B��B�B�B��B�eB��B��B�B�B��B��B�3B�&B�.B��B��B��B��B��B��B�B��B�HB�!B�B�)B�B�B��B�B�FB��B�B��B��B�B�B��B�.B�3B��B�B��B��B�B�B�B�B�B��B��B�iB�3B��B��B�uB��B�B�B��B��B�B��B�XB�DB��B��B��B�1B�-B��B��B�B��B�zB��B��B�B}B�BLBxB�BEB�BnBB�B?B8BB�B�B�B�B BB�BYB@BiBMB�B�B�BBBB�BEB�BB�B�BB�B�B�B}BB�B�BKBsB�B	B
�B4B+B+B�B�B-B
�B
{B
�B�BlB�BB�BiB(B�B�B�B�B�BEB�B7BB�B~B�B%B!BB'BBbBBVBwB�BdB�BUBByBB�B7B�B6BB=B�B�B�BSB�B�B�BB�B�B}B�B8B[B�B[B�B�B�BB�B�B�B�BDB|BBNB�B�BJB�B>B�B�B�B�BvBB�B�B�BBrB�B6B�B�B�B�B�BQBBqB�B	BdB*BB�BMB�B�B7B �B�B�B�B�BgB�B>B�B	�B	�B�BB
B	�B
?B�B�B	�B
)B
�B�B�BiB&BhB�B�B�B�B�BBBBB�BgB[B�BLBBKB/B(BBeB^BBhB�B�B,B;B�B�B�B�B;B�B_B�BOB�B�B�B�B�B�B�B:B\B<B'B~B{B	BlB�B�B:BnB�BTB�B�B�B�BSBgB'B
�B	�B	�B�BTB	"B�B�BdBJBVB_B {B =B �B�$B��B��B�KB��B�tB�B� B�B�B��B�iB��B��B�tB��B��B�'B��B�cB�0B��B��B�?B��B�BB�B�"B��B�_B�yB�B�B߿B�PB��B�6BтB�lB�&B�OB��B��B�hB͚B�5B̓B˹B̌B�+BʊBɆBˏBɀB�cB��B��B�:B�hB��B��B�^B��B�BĩB�cB�tB�BơBȮB��B��B�	BȳBɯB�OB�B�bBʘBʦB�3B��BνB�B� B�NB��B��B۟BۓB�bB�MB��B��B�@B�PB��B�?B�B��B�LB�B��B�1B��B�BwBYB[B_B�B(B�BpB�B�B�B�BWByBWBNB�B�BB�B�B�B�BB�B4BB�B�BB�B�B�B�BB@B[BB�B�BBYBoB�B[BBqB�BB�B8B�B5B%B�B?B�B�BB�B�BZBtB"BuB�BB
�B
BrB�B�B�B 
B 
B zB�B�FB��B��B��B��B�6B�B�nB�/B��B��B��B �B }BB �B}B�B
�B�BzBjBBGB
�B�B1B�B1B!B
B
�B
B
B�B	]B	qB	4B	 B
B	JB	�B	yB	9B	`B	�B�B�B8BB�B�B�B�B1BvB�B�B�BjBLB5B4B�BB�B�B�B�B�B�B�B BBBkB�B�B�B �B�B�B[B1B)B�BwBB�BaB�B7B1B
�B
��B
��B
��B
��B
��B
�[B
��B
��B
�7B BIB
��BPB�BB
�uB}B�B�BB�B
�hB
��B  B �B�B�BMBB
��B
��B
��B �B >B 1B
��BB�B KB AB�B KB
��B
��BVB_B �B~B�BB `BB�B>B�B�B �BRB�B�B�B�BvBCBUB�B�B]B;B$B3B�BB�B�B�B�B'B�B�B�B�B�B�B'B�B�BB�BTB�BXBiBBwB'B�B�BB�BuB�B�B�B�BQB�B_B?B
�BvBSBnB�BB�BB[B
\rG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�422222222222222222222222222222422224222222222222222222222222222222222222222222222242222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994344344444344444444444444344443444444334434434443444444444444443433444444444344444444443443444344444443444434434443444444344344334434444443334443344343444344443334333444434434434344443334444333344433434334343333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No adjustment applied; profiles too shallow to allow a reliable calibration/quality control check.                                                                                                                                                              Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202009011546592020090115465920200901154659202009011546592020090115465920200901154659202009011546592020090115465920200901154659202009011546592020090115465920200901154659AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201811202122112018112021221120181120212211    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202122112018112021221120181120212211  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202122112018112021221120181120212211  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4000            0               4000            UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202009011546592020090115465920200901154659  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                