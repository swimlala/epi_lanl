CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  
   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:59Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7d   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8D   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9`   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           9h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9x   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            9|   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        P  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  P8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  t�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �<   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �P   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   Ҹ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   $   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   @   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20180724220259  20210722161418  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�kp��8@�kp��811  @�kp��P@�kp��P@*m��L�@*m��L��cLrq6��cLrq6�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?333@   @@  @�  @�  @�33@�  A��A��A#33AA��Ac33A���A�  A�33A�  A�  A�ffA�33A�  B ffB  B��BffB ��B(  B/��B8  B@ffBH��BP��BXffB`ffBh��BpffBw��B�  B�ffB���B���B�ffB�  B���B�ffB�33B�  B�33B�ffB�33B���B���B���B�  B�33BǙ�B˙�B���B�33B�ffB�33B�ffB㙚B�ffB�ffBB�33B���B�33B�ffC��C�fC  C33C
  C��C33C33C  C  C�fCL�C33C33C33C �C"33C$�C&�C(  C)�fC+��C-�fC0�C1�fC3��C5��C7�fC:  C<�C>�C@33CB�CD  CE�fCG�fCI�fCK��CM�fCP  CR  CT�CV33CXL�CZffC\�C]�3C`�CbL�CdL�CfffChffCj�Cl  CnffCpL�CrL�Ct�Cv  Cw�fCz�C|33C~  C�fC��C��C�  C��C��C��3C�  C��C��C�&fC��C�  C��fC�  C��C�  C��C�&fC�  C��C�&fC��C��3C�  C��C��3C��3C�  C��C��C�  C��3C��fC�  C��C��C�  C��fC��3C�ٚC�ٚC��C�&fC�&fC�&fC��C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C��C��C�&fC�&fC�33C��C��3C��3C�  C�  C�  C�  C��C��C�&fC�&fC�&fC�  C��C�&fC�&fC��C��C��C��C��C��C��3C��fC�  C�33C�&fC�33C��C��C��C�  C��3C��C�&fC��C��C��3C��fC��C�&fC��C�  C�  C��fC�ٚC�ٚC�  C��C��C��C�  C��3C�  C��C�  C��3C�  C��C�  C��3C�ٚC��C��C��C��3D l�D�D��D�3DffD� D9�D�fD�Ds3D�fD` D�fDL�D� D!l�D#ٚD&Y�D(�fD+y�D.  D0��D3�D5��D8�D:s3D<ٚD?33DA��DC��DFY�DH�3DK@ DM�3DP  DRy�DT��DW9�DY��D[�fD^S3D`� Dc�De� Dg�3DjffDl�fDoY�Dq�fDt9�Dv��Dy&fD{9�D}�fD�fD�6fD�l�D��fD�� D��3D�	�D�33D�Y�D�vfD���D�� D��D�fD�&fD�L�D�i�D��3D���D���D�ٚD��fD��D�<�D�Y�D�y�D��3D��3D�ɚD��3D�  D�&fD�@ D�VfD�c3D�y�D���D���D���D���D�ٚD��fD���D� D�&fD�@ D�VfD�vfD��fD���D���D��fD��fD��3D���D�fD�0 D�I�D�VfD�i�D�vfD�|�DĐ Dţ3Dư Dǹ�Dȼ�D�� D��3D���D��3D�� D��3D��3D���D���D�fD�fD�#3D�0 D�0 D�9�D�I�D�L�D�P D�VfD�` D�p D�s3D�y�D�vfD�|�D�y�D�y�D�|�D�|�D�vfD�vfD�p D�` D�S3D�L�D�C3D�33D�&fD��D�3D��3D��fD���D��fD���D��fD�� D�|�D�i�D�\�D�@ D�0 D�&fD�fD�  D�� E �fEP E<�E� E3E�fEnfE	�fE
�3EC3E>fE��E( EfE�fE�fE� Ea�ENfE� E3E�EFfE�3E�fE @ E!�fE"� E#�fE$�fE&.fE'nfE)	�E*>fE+p E,� E-�fE.��E0t�E1�3E2��E40 E5@ E6� E7�fE9$�E:.fE;�fE<��E?��EC EF3EI^fELP EOx ER��EU�fEX� E\fE_K3Eb$�Ee>fEh�3Ek��En��EoK3Eo� Ep�fEq<�Eq�fEr�fEsfEs��Etk3Et�3Eu� Ev&fEv� EwY�Ex3Ex��Eyi�Ey�3>���?��?333?   ?��?333?333?��?��?   ?   ?   ?   ?   ?��?333?��?333?��?333?�  ?L��?�  ?���?�ff?�33?���?�ff@   @��@&ff@9��@Fff@Y��@l��@y��@���@�33@�  @���@���@�33@ə�@�33@�33@���@�ffA��A  AffA��A��A#33A)��A1��A;33AA��AH  AP  AVffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114114444444411414114111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ?fff?���@   @`  @�  @�  @�33@�  A	��A��A+33AI��Ak33A���A�  A�33A�  A�  A�ffA�33A�  BffB
  B��BffB"��B*  B1��B:  BBffBJ��BR��BZffBbffBj��BrffBy��B�  B�ffB���B���B�ffB�  B���B�ffB�33B�  B�33B�ffB�33B���B���B���B�  B�33Bș�B̙�B���B�33B�ffB�33B�ffB䙚B�ffB�ffB�B�33B���B�33C 33CL�CffC� C�3C
� CL�C�3C�3C� C� CffC��C�3C�3C�3C ��C"�3C$��C&��C(� C*ffC,L�C.ffC0��C2ffC4L�C6L�C8ffC:� C<��C>��C@�3CB��CD� CFffCHffCJffCLL�CNffCP� CR� CT��CV�3CX��CZ�fC\��C^33C`��Cb��Cd��Cf�fCh�fCj��Cl� Cn�fCp��Cr��Ct��Cv� CxffCz��C|�3C~� C�33C�L�C�Y�C�@ C�L�C�Y�C�33C�@ C�L�C�Y�C�ffC�Y�C�@ C�&fC�@ C�L�C�@ C�L�C�ffC�@ C�Y�C�ffC�L�C�33C�@ C�L�C�33C�33C�@ C�Y�C�L�C�@ C�33C�&fC�@ C�Y�C�L�C�@ C�&fC�33C��C��C�L�C�ffC�ffC�ffC�Y�C�Y�C�L�C�L�C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�Y�C�ffC�ffC�s3C�L�C�33C�33C�@ C�@ C�@ C�@ C�L�C�Y�C�ffC�ffC�ffC�@ C�L�C�ffC�ffC�Y�C�Y�C�Y�C�L�C�L�C�L�C�33C�&fC�@ C�s3C�ffC�s3C�Y�C�Y�C�L�C�@ C�33C�L�C�ffC�Y�C�L�C�33C�&fC�L�C�ffC�Y�C�@ C�@ C�&fC��C��C�@ C�Y�C�L�C�L�C�@ C�33C�@ C�Y�C�@ C�33C�@ C�Y�C�@ C�33C��C�Y�C�L�C�L�D �D ��D,�D��D�3D�fD� DY�D�fD,�D�3DfD� D�fDl�D  D!��D#��D&y�D)fD+��D.  D0��D39�D5��D8,�D:�3D<��D?S3DA��DD�DFy�DH�3DK` DM�3DP@ DR��DT��DWY�DY��D\fD^s3D`� Dc9�De� Dh3Dj�fDmfDoy�Dq�fDtY�Dv��DyFfD{Y�D}�fD�fD�FfD�|�D��fD�� D��3D��D�C3D�i�D��fD���D�� D���D�fD�6fD�\�D�y�D��3D���D�ɚD��D�fD�)�D�L�D�i�D���D��3D��3D�ٚD��3D� D�6fD�P D�ffD�s3D���D���D���D���D���D��D��fD�	�D�  D�6fD�P D�ffD��fD��fD���D���D��fD��fD��3D��D�&fD�@ D�Y�D�ffD�y�DfDÌ�DĠ Dų3D�� D�ɚD���D�� D��3D���D��3D�� D��3D��3D���D�	�D�fD�&fD�33D�@ D�@ D�I�D�Y�D�\�D�` D�ffD�p Dހ D߃3D���D�fD��D㉚D䉚D��D��D�fD�fD� D�p D�c3D�\�D�S3D�C3D�6fD�)�D�3D�3D��fD���D��fD���D��fD�� D���D�y�D�l�D�P D�@ D�6fD�&fD� E   E �fEX ED�E� E3EfEvfE	�fE
�3EK3EFfE��E0 E&fE�fE�fE  Ei�EVfE� E3E��ENfE�3E�fE H E!�fE"� E#�fE$�fE&6fE'vfE)�E*FfE+x E,� E-�fE.��E0|�E1�3E2��E48 E5H E6� E7�fE9,�E:6fE;�fE<��E@�EC EF3EIffELX EO� ER��EU�fEX� E\fE_S3Eb,�EeFfEh�3Ek��En��EoS3Eo� Ep�fEqD�Eq�fEr�fEs&fEs��Ets3Et�3Eu� Ev.fEv� Ewa�Ex3Ex��Eyq�Ey�3?fff?���G�O�?�  ?���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�  ?���G�O�?���G�O�?���?���G�O�?�ff?�  ?ٙ�?�ff?�33@ff@33@   @9��@Fff@Y��@fff@y��@�ff@���@���@�33@�  @���@ə�@�33@ٙ�@�33@�33@���A33A	��A  AffA��A$��A+33A1��A9��AC33AI��AP  AX  A^ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114114444444411414114111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              @ �@ �@ �@ {@ O@ ""@ )�@ /�@ 7L@ >@ E�@ R�@ `�@ m�@ z�@ ��@ �0@ ��@ �!@ �w@ ��@ �#@ �@ �@@o@�@+�@:@H]@V�@dZ@qS@~�@�P@�H@��@��@��@�C@��@��@�,@%@�@"�@/�@>@Lu@Yn@e�@s_@�@�\@��@�M@��@�J@�O@�@�@��@�@�@#�@1�@A�@Q=@\�@g�@v@�p@��@��@�@�^@��@�h@�@�Y@�Q@�@�@*S@7�@D�@SI@`B@m�@z�@��@��@�(@�-@�w@�o@�@�m@��@@�@ @-@:@F�@T�@bN@oF@}�@��@��@��@�F@Ĝ@��@ލ@�y@��@�@�@$�@2�@>@K@[z@hs@v@�d@�\@�U@�Y@��@��@��@��@�L@��@
�@B@$�@33@A�@O�@^5@k.@ww@��@��@�@�@�k@�o@�
@�@�e@ �@�@O@)�@5�@C�@Q�@`�@m�@z�@��@��@��@��@��@��@�@�m@�@	^@	�@	 �@	.l@	<@	I@	V�@	c�@	qS@	~�@	��@	��@	�A@	��@	@	�7@	��@	�4@	��@
�@
�@
$�@
0x@
<�@
Ji@
X�@
ff@
t@
��@
�@
�a@
��@
�^@
�@
Ӡ@
��@
��@
��@�@B@&�@3�@A�@O0@[z@hs@ww@��@��@�(@�r@�@�@�
@�@�@@@�@(G@5?@D�@S�@`�@m:@z�@�+@�$@��@�~@��@�|@�#@�@�@j@o@�@+�@:@I@UU@bN@n�@�@��@�H@��@��@��@є@�@V@�0@��@�@]�@�@��@%�@g@�@�Y@7�@z3@�w@@Ji@�\@��@�@]�@��@�T@$�@e	@��@�m@(G@k�@�@��@33@s_@��@�@5�@t�@��@�,@9X@z�@��@ �@D�@��@�@�@O�@�u@�@V@O�@��@��@o@Q�@�@��@V@M$@��@�@�@FQ@�@��@�Q@<@x&@��@��@ .l@ k.@ �M@ �m@!$/@!a�@!��@!�#@"�@"R�@"�\@"�*@#
=@#E�@#~�@#�^@#�@$.l@$i!@$�(@$��@%B@%S�@%�\@%��@&�@&B8@&�@&��@&�@'/@'k.@'�4@'ލ@(�@(V�@(��@(��@)1@)B�@)|?@)�9@)��@*)�@*b�@*��@*��@+
=@+A�@+z3@+�-@+�@,"�@,Yn@,�@,�o@-�@->�@-x&@-�~@-�@. �@.Z�@.�@.�c@/]@/:@/t@/�Y@/�T@0B@0Q=@0�+@0��@0�@1+�@1`�@1��@1��@2  @23�@2i!@2��@2��@3�@38�@3j@3��@3є@4%@47�@4l�@4�a@4�7@5�@55@@5i!@5��@5��@6]@64�@6ff@6��@7�@7��@7��@8��@91'@9��@:5�@:��@;;d@;�h@<C�@<�T@=��@=��@>�C@>�@?�\@@)�@@��@A%�@A��@B�@B�f@C>�@Cψ@D_�@D�@@Eww@E��@F^�@F�l@Go�@Hg@H�(@I%�@I�A@J$�@J�z@KI�@K�W@L?}@L�H@MUU@M�Y@Ne�@N��@Oo�@P%@P��@Q�(@S6�@T�@U��@W,`@X�@Y�@[9X@\~K@]�@_E�@`|�@aψ@c>�@d�W@e�#@fB@fUU@f��@f�@g(G@g�@g�@g�e@hI@h�@h�7@i%@iUU@i�7@i��@j �@jj@j��@ �@ G�O�@ j@ G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ j@ G�O�@ G�O�@ @ �G�O�@ v@ �@ 1@ �@ 	�@ 
�@ J@ �@ b@ �@ �@ *@ 6@ B@ �@ [@ g@ ""@ $.@ '�@ )�@ +@ -@ 0x@ 2�@ 4�@ 7L@ :@ <�@ ?}@ B�@ E�@ H]@ K�@ O�@ R�@ UU@ X�@ [zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AڮAڧ�AڬAڰ!AڶFAڶFAڴ9AڶFAڶFAڶFAڸRAڸRAڴ9AڶFAڴ9AڶFAڸRAڴ9AڶFAڶFAڴ9AڶFAڲ-AڶFAں^AھwA���A���A�A�ĜA�ĜA�ĜA�ƨA�ĜA�ƨA�ȴA�ȴA�ȴA�ƨA�ȴA�ȴA�x�Aϲ-A�K�A�K�A�~�A�A�9XA�JA���A�ZA�%A��A�$�A�oA���A��yA���A�r�A�r�A�p�A���A��A���A�S�A���A���A�G�A��`A�~�A�=qA�`BA���A�Q�A�?}A�bA��A��A���A{��AwO�Ar�/Ao�wAl�Ae�#Abv�A`$�A^��A\I�AZ(�AX��AUƨAP�AM|�AKAI��AFĜAE&�AC7LA@�\A>9XA<I�A;x�A;/A:�A9x�A8ZA7��A6��A6�A5�hA5VA4�/A4�`A4��A4jA4=qA3l�A2ĜA2^5A1��A1+A0n�A/%A-�A,bNA+p�A*�uA*I�A)\)A(�+A'�;A'O�A'A%��A$ȴA$I�A#�PA"VA!�A!�A �RA ��A VA��A�A�HA�A^5A(�A��A\)AVA�mAt�A��A�AS�A"�A�AZA�
A|�A%A�AA�A��A�A��AS�A�`A�DAZA��A�wAhsA�A��AI�A��A�wAt�A��AVA��A�AȴA�DA$�A�AO�A+A�A�DA�Al�A&�A�AĜAz�A{A�#AhsAoAA
��A
�!A
�DA
bNA
�A	��A	l�A	�A�jA5?A��A"�AĜA�\AQ�A$�A�TA�A?}A+A��A�9A^5A��AO�A�`A�A�DAZAbA��AhsA ��A �A �@�C�@��R@��@��T@�`B@��D@��@�@�v�@�$�@���@�?}@�z�@��
@�33@���@�^5@�{@���@���@�dZ@���@��`@�b@��@��@�G�@噚@��@�|�@ە�@���@��@��m@ԛ�@�n�@�M�@�G�@�t�@�X@�o@�I�@���@�v�@��;@�&�@��@��@�hs@���@���@���@���@��@��7@��F@��^@��u@�"�@�X@���@��\@�O�@�S�@�V@�/@�t�@��7@��
@�{@��/@�;d@���@��u@�dZ@�~�@���@��@�V@�?}@���@�33@��y@��#@��@���@���@��#@�z�@;d@}��@{��@z~�@w��@vV@sdZ@pQ�@n�@l��@kC�@j�@h�u@g+@f$�@e�@d�D@b=q@` �@_|�@]�T@\j@Z��@Y&�@W+@Up�@T�D@SdZ@R��@QX@OK�@N��@L�D@Ko@I�@G�;@Fv�@D�@Dz�@Cƨ@Co@B�@@Ĝ@@b@?
=@=?}@;�F@9��@8Ĝ@8�@7�P@6v�@4��@4�j@4�@3o@2�\@1��@0��@/�;@/K�@.V@-/@,I�@*��@*J@)&�@(��@'\)@&��@%�@$�/@$�D@#�
@"~�@!�@ Ĝ@��@��@��@V@��@C�@�@%@A�@�@��@@��@1@C�@�H@~�@��@7L@��@�@+@�R@��@?}@j@ƨ@S�@
��@
^5@
�@	��@	hs@1'@�+@�@ƨ@�@J@ ��?��w?�p�?�^5?�x�?���?�ff?��/?�?��?�J?��?��?�|�?�v�?��?�/?�1?���?��?�X?�b?�P?��y?��T?�`B?��
?���?��?�&�?��?�  ?ޗ�?�{?�/?��m?�"�?ڟ�?�=q?���?�Q�?׍P?��y?�9X?�o?щ7?ϝ�?θR?̋D?��H?ɺ^?�1'?�+?�?��/?�S�?�-?��?�;d?���?��?�V?�5??�{?��?���?�p�?�V?��?�I�?�(�?��m?��m?��m?���?��?�C�AڮAڮAڬAڬAڥ�Aک�Aڲ-AڬAڥ�Aڥ�Aڣ�Aک�Aک�Aڥ�Aڣ�Aڧ�Aک�AڬAک�Aڥ�Aڥ�Aڧ�Aڧ�Aڧ�Aڥ�Aڥ�Aک�Aک�Aک�AڮAڮAڮAڲ-Aڴ9Aڴ9AڶFAں^AڶFAڴ9AڶFAڴ9Aڴ9AڶFAڶFAڶFAڶFAڶFAڸRAڶFAڶFAڶFAڶFAڸRAڸRAڸRAڸRAڶFAڸRAڸRAڴ9G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              AڮAڧ�AڬAڰ!AڶFAڶFAڴ9AڶFAڶFAڶFAڸRAڸRAڴ9AڶFAڴ9AڶFAڸRAڴ9AڶFAڶFAڴ9AڶFAڲ-AڶFAں^AھwA���A���A�A�ĜA�ĜA�ĜA�ƨA�ĜA�ƨA�ȴA�ȴA�ȴA�ƨA�ȴA�ȴA�x�Aϲ-A�K�A�K�A�~�A�A�9XA�JA���A�ZA�%A��A�$�A�oA���A��yA���A�r�A�r�A�p�A���A��A���A�S�A���A���A�G�A��`A�~�A�=qA�`BA���A�Q�A�?}A�bA��A��A���A{��AwO�Ar�/Ao�wAl�Ae�#Abv�A`$�A^��A\I�AZ(�AX��AUƨAP�AM|�AKAI��AFĜAE&�AC7LA@�\A>9XA<I�A;x�A;/A:�A9x�A8ZA7��A6��A6�A5�hA5VA4�/A4�`A4��A4jA4=qA3l�A2ĜA2^5A1��A1+A0n�A/%A-�A,bNA+p�A*�uA*I�A)\)A(�+A'�;A'O�A'A%��A$ȴA$I�A#�PA"VA!�A!�A �RA ��A VA��A�A�HA�A^5A(�A��A\)AVA�mAt�A��A�AS�A"�A�AZA�
A|�A%A�AA�A��A�A��AS�A�`A�DAZA��A�wAhsA�A��AI�A��A�wAt�A��AVA��A�AȴA�DA$�A�AO�A+A�A�DA�Al�A&�A�AĜAz�A{A�#AhsAoAA
��A
�!A
�DA
bNA
�A	��A	l�A	�A�jA5?A��A"�AĜA�\AQ�A$�A�TA�A?}A+A��A�9A^5A��AO�A�`A�A�DAZAbA��AhsA ��A �A �@�C�@��R@��@��T@�`B@��D@��@�@�v�@�$�@���@�?}@�z�@��
@�33@���@�^5@�{@���@���@�dZ@���@��`@�b@��@��@�G�@噚@��@�|�@ە�@���@��@��m@ԛ�@�n�@�M�@�G�@�t�@�X@�o@�I�@���@�v�@��;@�&�@��@��@�hs@���@���@���@���@��@��7@��F@��^@��u@�"�@�X@���@��\@�O�@�S�@�V@�/@�t�@��7@��
@�{@��/@�;d@���@��u@�dZ@�~�@���@��@�V@�?}@���@�33@��y@��#@��@���@���@��#@�z�@;d@}��@{��@z~�@w��@vV@sdZ@pQ�@n�@l��@kC�@j�@h�u@g+@f$�@e�@d�D@b=q@` �@_|�@]�T@\j@Z��@Y&�@W+@Up�@T�D@SdZ@R��@QX@OK�@N��@L�D@Ko@I�@G�;@Fv�@D�@Dz�@Cƨ@Co@B�@@Ĝ@@b@?
=@=?}@;�F@9��@8Ĝ@8�@7�P@6v�@4��@4�j@4�@3o@2�\@1��@0��@/�;@/K�@.V@-/@,I�@*��@*J@)&�@(��@'\)@&��@%�@$�/@$�D@#�
@"~�@!�@ Ĝ@��@��@��@V@��@C�@�@%@A�@�@��@@��@1@C�@�H@~�@��@7L@��@�@+@�R@��@?}@j@ƨ@S�@
��@
^5@
�@	��@	hs@1'@�+@�@ƨ@�@J@ ��?��w?�p�?�^5?�x�?���?�ff?��/?�?��?�J?��?��?�|�?�v�?��?�/?�1?���?��?�X?�b?�P?��y?��T?�`B?��
?���?��?�&�?��?�  ?ޗ�?�{?�/?��m?�"�?ڟ�?�=q?���?�Q�?׍P?��y?�9X?�o?щ7?ϝ�?θR?̋D?��H?ɺ^?�1'?�+?�?��/?�S�?�-?��?�;d?���?��?�V?�5??�{?��?���?�p�?�V?��?�I�?�(�?��m?��m?��m?���?��?�C�AڮAڮAڬAڬAڥ�Aک�Aڲ-AڬAڥ�Aڥ�Aڣ�Aک�Aک�Aڥ�Aڣ�Aڧ�Aک�AڬAک�Aڥ�Aڥ�Aڧ�Aڧ�Aڧ�Aڥ�Aڥ�Aک�Aک�Aک�AڮAڮAڮAڲ-Aڴ9Aڴ9AڶFAں^AڶFAڴ9AڶFAڴ9Aڴ9AڶFAڶFAڶFAڶFAڶFAڸRAڶFAڶFAڶFAڶFAڸRAڸRAڸRAڸRAڶFAڸRAڸRAڴ9G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�!B	�'B	�'B	�'B	�!B	�!B	�'B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�'B	�'B	�!B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�!B	�'B	�!B	�!B	�'B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�7B	u�B	�HB
7LB
G�B
D�B
�B
�B
�wB
�5B
�/B
��B
��B
��B
�wB
�/B{BL�B!�B
�B
�ZB
��B
Q�B
 �B
PB
	7B
\B
jB
�uB
�=B
t�B
@�B
  B	�sB	��B	�-B	��B	n�B	R�B	E�B	0!B	#�B	hB��B��B�B�TB�B�B	1B��B�B�yB�B��B	VB	�B	,B	%�B	#�B	M�B	s�B	�B	��B	��B	�?B	�}B	��B	�B	�HB	�mB	�B	�B	��B
B
+B
�B
!�B
'�B
-B
8RB
A�B
F�B
D�B
J�B
J�B
L�B
K�B
M�B
P�B
Q�B
P�B
Q�B
R�B
Q�B
P�B
O�B
P�B
Q�B
R�B
P�B
Q�B
Q�B
R�B
R�B
Q�B
Q�B
P�B
P�B
S�B
T�B
R�B
W
B
T�B
VB
VB
VB
T�B
S�B
R�B
S�B
Q�B
O�B
N�B
M�B
M�B
O�B
O�B
N�B
M�B
M�B
O�B
O�B
P�B
O�B
O�B
P�B
O�B
O�B
O�B
L�B
M�B
Q�B
Q�B
Q�B
Q�B
P�B
O�B
N�B
K�B
L�B
L�B
J�B
I�B
I�B
I�B
H�B
H�B
G�B
F�B
F�B
D�B
D�B
C�B
B�B
C�B
B�B
A�B
B�B
@�B
A�B
?}B
?}B
>wB
<jB
;dB
;dB
;dB
;dB
:^B
9XB
9XB
9XB
8RB
7LB
7LB
5?B
6FB
5?B
7LB
8RB
:^B
9XB
9XB
8RB
7LB
7LB
6FB
5?B
33B
33B
1'B
2-B
/B
/B
1'B
0!B
0!B
1'B
0!B
0!B
/B
/B
-B
.B
-B
-B
,B
)�B
'�B
#�B
#�B
"�B
!�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
oB
�B
�B
oB
uB
bB
bB
bB
\B
bB
bB
\B
bB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
%�B
&�B
%�B
'�B
&�B
(�B
+B
,B
-B
/B
0!B
1'B
2-B
2-B
49B
49B
6FB
7LB
7LB
9XB
9XB
;dB
<jB
=qB
?}B
@�B
B�B
D�B
D�B
F�B
F�B
G�B
H�B
I�B
I�B
J�B
J�B
M�B
N�B
N�B
N�B
O�B
P�B
R�B
S�B
T�B
S�B
VB
VB
XB
YB
YB
ZB
[#B
\)B
]/B
^5B
_;B
_;B
`BB
_;B
`BB
bNB
aHB
bNB
dZB
dZB
e`B
ffB
ffB
ffB
gmB
hsB
gmB
gmB
hsB
iyB
jB
k�B
k�B
l�B
l�B
n�B
n�B
o�B
p�B
p�B
p�B
q�B
p�B
r�B
s�B
s�B
t�B
u�B
u�B
v�B
w�B
x�B
y�B
y�B
z�B
{�B
|�B
|�B
}�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�%B
�+B
�+B
�+B
�1B
�1B
�7B
�=B
�7B
�DB
�PB
�VB
�\B
�hB
�hB
�oB
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�'B
�'B
�-B
�-B
�3B
�9B
�9B
�?B
�?B
�FB
�LB
�LB
�XB
�XB
�^B
�XB
�^B
�XB
�^B
�XB
�XB
�^B
�XB
�XB
�^B
�XB
�XB
�^B
�^B
�^B
�^B	�!B	�'B	�'B	�!B	�'B	�'B	�B	�B	�'B	�-B	�-B	�'B	�!B	�'B	�'B	�-B	�!B	�!B	�!B	�'B	�'B	�'B	�'B	�!B	�'B	�'B	�'B	�'B	�'B	�'B	�!B	�'B	�'B	�'B	�!B	�'B	�!B	�!B	�'B	�!B	�'B	�'B	�'B	�!B	�!B	�'B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�'B	�!B	�!B	�'G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              B	��B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�	B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�	B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	u�B	�6B
7;B
G�B
D�B
�B
��B
�gB
�&B
� B
��B
��B
�uB
�iB
�"BnBL�B!�B
�B
�OB
��B
Q�B
 �B
EB
	-B
RB
juB
�lB
�4B
t�B
@{B	��B	�kB	��B	�&B	�zB	n�B	R�B	E�B	0B	#�B	bB��B��B�B�PB�B�B	.B��B�B�wB�B��B	UB	�B	,B	%�B	#�B	M�B	s�B	�B	��B	��B	�BB	��B	��B	�"B	�NB	�sB	�B	�B	��B
B
4B
�B
!�B
'�B
-B
8^B
A�B
F�B
D�B
J�B
J�B
L�B
K�B
M�B
P�B
Q�B
P�B
Q�B
SB
Q�B
P�B
O�B
P�B
RB
SB
P�B
RB
RB
S
B
S
B
RB
RB
P�B
Q B
TB
UB
SB
W'B
UB
V"B
V"B
V#B
UB
TB
SB
TB
RB
PB
N�B
M�B
M�B
PB
PB
N�B
M�B
M�B
PB
PB
QB
PB
P	B
QB
P
B
P
B
PB
L�B
N B
RB
RB
RB
RB
QB
PB
O
B
K�B
L�B
M B
J�B
I�B
I�B
I�B
H�B
H�B
G�B
F�B
F�B
D�B
D�B
C�B
B�B
C�B
B�B
A�B
B�B
@�B
A�B
?�B
?�B
>�B
<�B
;�B
;�B
;�B
;�B
:�B
9�B
9�B
9�B
8�B
7�B
7�B
5�B
6�B
5�B
7�B
8�B
:�B
9�B
9�B
8�B
7�B
7�B
6�B
5�B
3�B
3�B
1vB
2}B
/kB
/lB
1xB
0sB
0sB
1zB
0uB
0uB
/pB
/pB
-dB
.jB
-eB
-fB
,`B
*UB
(IB
$1B
$1B
#,B
"&B
"'B
B
B
B
	B
B
B
�B
�B
�B
B
�B
�B
B
B
�B
 B
�B
�B
�B
�B
�B
�B
�B
B
B
)B
=B
@B
CB
FB
CB
KB
NB
XB
aB
cB
xB
 �B
 �B
 �B
!�B
"�B
#�B
&�B
'�B
&�B
(�B
'�B
)�B
+�B
,�B
-�B
0B
1B
2 B
3(B
3+B
5:B
5=B
7LB
8UB
8XB
:fB
:iB
<xB
=�B
>�B
@�B
A�B
C�B
E�B
E�B
G�B
G�B
H�B
I�B
J�B
J�B
K�B
K�B
OB
PB
PB
PB
Q'B
R0B
T@B
UHB
VQB
UMB
W\B
W^B
YmB
ZvB
ZyB
[�B
\�B
]�B
^�B
_�B
`�B
`�B
a�B
`�B
a�B
c�B
b�B
c�B
e�B
e�B
f�B
g�B
g�B
g�B
iB
jB
iB
i
B
jB
kB
l#B
m,B
m.B
n6B
n9B
pIB
pKB
qTB
r\B
r_B
raB
siB
rfB
ttB
u}B
uB
v�B
w�B
w�B
x�B
y�B
z�B
{�B
{�B
|�B
}�B
~�B
~�B
�B
��B
��B
��B
��B
�B
�	B
�B
�B
�B
�B
�B
�)B
�+B
�.B
�6B
�2B
�:B
�=B
�?B
�GB
�IB
�RB
�ZB
�VB
�hB
�zB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
� B
�B
�B
�*B
�+B
�8B
�<B
�OB
�[B
�ZB
�mB
�tB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�$B
�+B
�6B
�=B
�BB
�OB
�UB
�jB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�.B
�<B
�RB
�iB
�wB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B	��B	�B	�B	��B	�B	�B	��B	��B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	�B	��B	��B	�B	��B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202592021061413572320210614135723202107221611242021072216112420210722161124201807242202592021061413572320210614135723202107221611242021072216112420210722161124PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422025920180724220259  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025920180724220259QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025920180724220259QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�40000           0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216141820210722161418IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                