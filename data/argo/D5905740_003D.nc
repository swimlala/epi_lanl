CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  e   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:57Z creation      
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
resolution        =���   axis      Z        (  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  F�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (  I�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  T�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (  W�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  b�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  p�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ~�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ք   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �  ֌   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                     �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                     �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ɬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  լArgo profile    3.1 1.2 19500101000000  20180724220257  20210722161417  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�j�0?J@�j�0?J11  @�j�  �@�j�  �@*l�27��@*l�27���cI� ����cI� ���11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff@ff@Fff@�  @���@���@�  A��A33A(  AC33A`  A�  A�  A�  A�  A�33A�  A�  A�33B ffB��BffBffB ��B(  B/��B7��B@ffBHffBP  BXffB`��Bh��BpffBx  B�  B�33B�  B�  B�  B�33B�33B���B�33B�33B�  B�ffB�33B�33B�  B�  B�33B�  B���B�33B�  B�  B�  B���Bߙ�B�33B�  B뙚B�  B�ffB���B���C 33C  C�C33C  C
  C�C33C�C  C�C�C  C  C�C�C�fC"�C$  C%�fC(�C*�C,33C.�C/�fC1�fC4  C6�C8  C:  C<  C>�C@�CB�CD�CF33CH�CJ  CL  CN  CP  CR�CT  CV�CX�CZ  C\�C^  C_�fCa�fCd  Cf�Ch�Cj�Cl  Cn�Cp33Cr33Ct�Cv  Cx  Cz  C|�C~�C�fC�  C��C��C�  C�  C�  C��C��C�  C��C��C�  C�  C��3C�  C�  C��3C�  C�  C�  C��C��C��C��C��C�  C��3C��3C�  C��C��C��C��C��C��C�  C��3C�  C��C�  C�  C�  C��C�  C�  C��C��C��C�  C�  C��C��C�  C�  C��C��C��3C��3C��C��C��C��3C�  C�  C��C��C�  C��C��C��C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�  C�  C��C��C��C�  C�  C�  C��C��C��C�  C��C��C�  C��3C�  C��C��C��C��C��C��C��C��C�  C��3C��C�  C�  C��C�  C��3C�  C�  C��fC�33D ��D�fD` D,�D��D  D9�D	ffD
�3DٚDFfD�D��D��D�3D� D� D�fD� D�fD� Dy�D` DL�D  D��D!33D"�3D#,�D$l�D%��D'FfD(Y�D)s3D*��D,&fD-��D.��D0y�D1` D3FfD433D5  D7  D7��D8��D:�3D;l�D<FfD=��D>� D@L�DA�3DB�fDD�DE��DFL�DG�3DIL�DJfDK�fDM  DM��DO&fDP�fDQ� DSFfDS��DUFfDV�fDX  DYffDZ� D[��D]  D^��D_l�Da3Da�3Dcs3DefDe�fDg` Dh�Di��Dj��DlFfDmy�Dn�fDo��Dp�3Dq��Ds�DtS3DuٚDv� DxFfDyl�Dz�3>���>���>���?   >���>���>���>���>���>���>���>���>���>���>���>���>���?   >���?   ?��>���?��?   ?333?��?L��?fff?���?�33?�  ?�ff@ff@33@,��@9��@L��@`  @s33@�ff@�33@���@���@�ff@�33@�33@�  @�  @���A��AffAffA   A(  A0  A6ffA>ffAFffANffAT��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444414441444414114141411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           ?fff?�33@&ff@fff@�  @���@���@�  A	��A33A0  AK33Ah  A�  A�  A�  A�  A�33A�  A�  A�33BffB
��BffBffB"��B*  B1��B9��BBffBJffBR  BZffBb��Bj��BrffBz  B�  B�33B�  B�  B�  B�33B�33B���B�33B�33B�  B�ffB�33B�33B�  B�  B�33B�  B���B�33B�  B�  B�  B���B���B�33B�  B왚B�  B�ffB���B���C �3C� C��C�3C� C
� C��C�3C��C� C��C��C� C� C��C��C ffC"��C$� C&ffC(��C*��C,�3C.��C0ffC2ffC4� C6��C8� C:� C<� C>��C@��CB��CD��CF�3CH��CJ� CL� CN� CP� CR��CT� CV��CX��CZ� C\��C^� C`ffCbffCd� Cf��Ch��Cj��Cl� Cn��Cp�3Cr�3Ct��Cv� Cx� Cz� C|��C~��C�33C�@ C�L�C�L�C�@ C�@ C�@ C�L�C�L�C�@ C�Y�C�Y�C�@ C�@ C�33C�@ C�@ C�33C�@ C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�@ C�33C�33C�@ C�L�C�L�C�L�C�L�C�Y�C�L�C�@ C�33C�@ C�L�C�@ C�@ C�@ C�Y�C�@ C�@ C�L�C�Y�C�L�C�@ C�@ C�L�C�L�C�@ C�@ C�L�C�L�C�33C�33C�L�C�L�C�L�C�33C�@ C�@ C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�Y�C�L�C�L�C�L�C�@ C�@ C�L�C�Y�C�L�C�@ C�@ C�@ C�Y�C�Y�C�Y�C�@ C�L�C�L�C�@ C�33C�@ C�Y�C�Y�C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�@ C�33C�L�C�@ C�@ C�L�C�@ C�33C�@ C�@ C�&fC�s3D ��D�fD� DL�D��D  DY�D	�fD
�3D��DffD,�DٚD��D�3D� D� D�fD� D�fD� D��D� Dl�D  DٚD!S3D"�3D#L�D$��D%��D'ffD(y�D)�3D*ٚD,FfD-��D.��D0��D1� D3ffD4S3D5@ D7  D8�D8��D:�3D;��D<ffD>�D>� D@l�DA�3DB�fDD9�DE��DFl�DG�3DIl�DJ&fDK�fDM  DM��DOFfDP�fDR  DSffDT�DUffDV�fDX  DY�fD[  D[��D]@ D^��D_��Da33Da�3Dc�3De&fDe�fDg� Dh,�Di��Dk�DlffDm��Dn�fDo��Dp�3Dq��Ds9�Dts3Du��Dw  DxffDy��Dz�3G�O�G�O�?L��G�O�G�O�G�O�G�O�?L��G�O�G�O�G�O�?L��G�O�G�O�G�O�G�O�?fffG�O�?fff?�  G�O�?L��G�O�?�  G�O�?���?�ff?�33?���?�33@   @33@&ff@333@L��@Y��@l��@�  @���@�ff@�33@���@���@�ff@�33@�33@�  A   AffA��AffAffA(  A0  A8  A>ffAFffANffAVffA\��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444414441444414114141411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           @ �@ %@ V@ *@ O@ !s@ (G@ /�@ 7L@ >�@ G�@ SI@ _�@ m:@ z�@ ��@ �0@ �(@ �~@ �&@ �@ �#@ �y@ �q@@o@�@+�@9X@H]@V@b�@qS@�@�P@�H@�A@��@�>@�7@��@�@��@�@�@"�@0x@=q@Lu@Yn@g@t@��@�@�@��@�@��@Ӡ@�H@�@@�9@
�@�@$.@33@B8@P�@^5@k.@ww@��@�#@�m@�@�k@��@׹@�@�@ �@�@O@)�@7L@C�@R�@_�@l�@{�@�7@��@��@��@�w@��@�#@�@��@j@�@g@-@:�@I@V@b�@p�@~K@��@�H@�A@��@�>@�7@ލ@�@�~@%@{@"�@0x@>@K@Yn@g�@uk@�d@�\@�@��@�@ƨ@��@�H@�@�E@
=@�@%�@3�@A�@N�@]�@k.@ww@�@�@�m@�@�@�c@�
@�@�@ �@V@�@)�@6�@C�@Q=@_�@m�@{�@�7@��@�5@�-@�&@�@�t@��@��@	j@	@	 @	,`@	:@	H]@	V�@	c�@	p�@	~K@	��@	�H@	�A@	��@	�>@	��@	�/@	��@	��@
�@
*@
!s@
/�@
=q@
K�@
Yn@
ff@
t�@
�d@
�@
��@
�Y@
�@
ƨ@
Ӡ@
�H@
��@
��@
=@�@%�@33@A�@O�@\�@j@x&@�@��@�@�r@�k@�c@�
@�@�@^@@O@)�@7L@DD@Q=@_�@n�@|?@�7@��@�5@��@��@�*@�t@�m@�q@j@@g@,`@9X@G�@UU@a�@�U@��@�4@j@B@B8@ff@��@��@�@��@�@&�@T�@l�@�y@��@�
@V@(�@D�@_�@��@�@�W@��@	�@1�@Wb@g�@��@��@׹@�@@5�@\�@��@��@��@�@g@8�@Q�@�@�a@�F@�@��@*@C�@X�@�@��@��@��@�@&�@P�@x�@��@��@��@�L@�@>@b�@�7@��@��@�`@
=@0x@X�@k.@�0@��@��@@�@B�@m�@�d@�@��@�y@b@3�@T�@t�@�$@�!@��@�@@�@9X@UU@{�@��@�^G�O�G�O�@ G�O�G�O�G�O�G�O�@ G�O�G�O�G�O�@ G�O�G�O�G�O�G�O�@ �G�O�@ �@ jG�O�@ G�O�@ jG�O�@ @ v@ %@ �@ 	�@ 
=@ J@ V@ �@ o@ �@ �@ �@ �@ �@ g@ !s@ $.@ &�@ )�@ -@ /�@ 33@ 5�@ 8�@ <�@ @,@ DD@ G�@ K@ M�@ Q=@ T�@ X@ Z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�  A���A��A���A���A���A�  A�A���A�A�1A��A�(�A�(�A�+A�+A�-A�-A�/A�/A�/A�33A�33A�33A�5?A�7LA�9XA�9XA�9XA�5?A�7LA�5?A�7LA�9XA�9XA�;dA�=qA�A�A�A�A�A�A�A�A�7LA�jAёhA��A�jAá�A�dZA�^5A�t�A�;dA���A�C�A���A�K�A���A���A�9XA�  A���A�ffA��mA�
=A��A��A���A��A��-A�`BA���A�"�A��A�l�A��A�t�A�hsA��A�bNA�bA��A�/A|A�Au�TAr(�Ah��Ag�^Ab$�A\I�AY��AU�AQ�AM�AK%AGAD(�AA&�A?hsA>VA=S�A<{A:A�A9|�A8ȴA8$�A6��A5"�A3��A3�A3x�A3&�A3K�A3C�A2ĜA2jA2$�A1��A1K�A0��A/�A-�A-��A,�/A+�A+l�A*�A*1A)\)A'l�A&ĜA&��A&~�A%�A%"�A$�A$bNA#A#\)A#�A"��A!�A!`BA ��A �A+A�A�HA��A5?A��AG�A|�AO�A�jAjA��A+A�uA=qA�
A|�A�A�Az�A�mA�^A�AG�A�A(�A��A�
A?}A�AĜAjA5?A��A�-A�A��A�\AbNA5?A{A�
A�hAp�A%Ar�A�At�A"�A�RA5?At�A��A�/AȴA�A�FA&�A
=A
��A
VA	A	K�A�`A^5A$�A�wA�AȴAn�A1'A�A�hA;dA�jA�AE�A{A�A��A��AhsA"�A�/A�9A�A��AVA��A7LA ��A �jA �uA J@�
=@�V@�@��T@���@�X@�r�@�1@��m@��F@�"�@�E�@�/@��@�;d@��@��\@�v�@��@��@�G�@��H@��@���@�dZ@���@�ȴ@�(�@�S�@�~�@�t�@�v�@ݙ�@�S�@�M�@؃@ם�@֧�@�9X@�C�@���@Ѻ^@У�@��m@�"�@̓u@�1'@�{@�(�@�M�@�V@ģ�@��@��@��y@�@�Z@�
=@�@�ff@�J@��D@��u@�bN@��@��-@�r�@�+@�@�V@�p�@���@�|�@�O�@�Z@�K�@�~�@�$�@��@�  @���@�V@��#@�X@�Z@��P@�@��h@��@�bN@���@���@��@�p�@�hs@�V@��9@�j@��m@��@���@��-@�7L@�Ĝ@���@� �@��P@�ȴ@�-@���@�?}@�j@��w@��y@��@��/@�t�@�{@���@��A�%A�A�A�A�A�A���A���A���A���A�A�A�%A�A���A���A���A�  A�%A�A�1A�A���A��A���A���A���A���A��A���A���A��A��A��A��A��A���A���A���A���A���A�  A���A���A�A�A�A���A���A�  A�  A�A�%A�
=A�
=A�1A�{A�"�A�&�A�(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           A�  A���A��A���A���A���A�  A�A���A�A�1A��A�(�A�(�A�+A�+A�-A�-A�/A�/A�/A�33A�33A�33A�5?A�7LA�9XA�9XA�9XA�5?A�7LA�5?A�7LA�9XA�9XA�;dA�=qA�A�A�A�A�A�A�A�A�7LA�jAёhA��A�jAá�A�dZA�^5A�t�A�;dA���A�C�A���A�K�A���A���A�9XA�  A���A�ffA��mA�
=A��A��A���A��A��-A�`BA���A�"�A��A�l�A��A�t�A�hsA��A�bNA�bA��A�/A|A�Au�TAr(�Ah��Ag�^Ab$�A\I�AY��AU�AQ�AM�AK%AGAD(�AA&�A?hsA>VA=S�A<{A:A�A9|�A8ȴA8$�A6��A5"�A3��A3�A3x�A3&�A3K�A3C�A2ĜA2jA2$�A1��A1K�A0��A/�A-�A-��A,�/A+�A+l�A*�A*1A)\)A'l�A&ĜA&��A&~�A%�A%"�A$�A$bNA#A#\)A#�A"��A!�A!`BA ��A �A+A�A�HA��A5?A��AG�A|�AO�A�jAjA��A+A�uA=qA�
A|�A�A�Az�A�mA�^A�AG�A�A(�A��A�
A?}A�AĜAjA5?A��A�-A�A��A�\AbNA5?A{A�
A�hAp�A%Ar�A�At�A"�A�RA5?At�A��A�/AȴA�A�FA&�A
=A
��A
VA	A	K�A�`A^5A$�A�wA�AȴAn�A1'A�A�hA;dA�jA�AE�A{A�A��A��AhsA"�A�/A�9A�A��AVA��A7LA ��A �jA �uA J@�
=@�V@�@��T@���@�X@�r�@�1@��m@��F@�"�@�E�@�/@��@�;d@��@��\@�v�@��@��@�G�@��H@��@���@�dZ@���@�ȴ@�(�@�S�@�~�@�t�@�v�@ݙ�@�S�@�M�@؃@ם�@֧�@�9X@�C�@���@Ѻ^@У�@��m@�"�@̓u@�1'@�{@�(�@�M�@�V@ģ�@��@��@��y@�@�Z@�
=@�@�ff@�J@��D@��u@�bN@��@��-@�r�@�+@�@�V@�p�@���@�|�@�O�@�Z@�K�@�~�@�$�@��@�  @���@�V@��#@�X@�Z@��P@�@��h@��@�bN@���@���@��@�p�@�hs@�V@��9@�j@��m@��@���@��-@�7L@�Ĝ@���@� �@��P@�ȴ@�-@���@�?}@�j@��w@��y@��@��/@�t�@�{@���@��A�%A�A�A�A�A�A���A���A���A���A�A�A�%A�A���A���A���A�  A�%A�A�1A�A���A��A���A���A���A���A��A���A���A��A��A��A��A��A���A���A���A���A���A�  A���A���A�A�A�A���A���A�  A�  A�A�%A�
=A�
=A�1A�{A�"�A�&�A�(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	v�B	w�B	w�B	v�B	w�B	w�B	w�B	v�B	w�B	w�B	w�B	w�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	u�B	jB	+B	ffB	�+B	�B	�B
�B
W
B
hsB
H�B
n�B
��B�B
�5B
�B
��B
�BH�B�Bt�Bw�Bm�B`BBR�BD�B(�B	7BB
�B
�LB
w�B
A�B
B	�HB	��B	B	��B	|�B	[#B	1'B	�B	hB	B	B��B	  B��B��B��B��B�B�fB�B	uB	8RB	B�B	T�B	v�B	��B	��B	�3B	�B	�HB	�NB	�B	��B	��B
B
	7B
\B
�B
%�B
+B
33B
6FB
:^B
C�B
E�B
L�B
P�B
P�B
O�B
N�B
N�B
M�B
P�B
O�B
O�B
O�B
O�B
P�B
Q�B
P�B
O�B
O�B
P�B
P�B
Q�B
Q�B
R�B
Q�B
O�B
P�B
P�B
Q�B
R�B
P�B
O�B
XB
YB
ZB
YB
T�B
S�B
Q�B
P�B
Q�B
S�B
T�B
T�B
R�B
R�B
S�B
VB
T�B
S�B
R�B
T�B
T�B
Q�B
P�B
P�B
P�B
P�B
O�B
N�B
M�B
L�B
L�B
M�B
N�B
N�B
M�B
M�B
M�B
L�B
L�B
K�B
J�B
K�B
J�B
J�B
J�B
I�B
I�B
I�B
H�B
F�B
E�B
D�B
D�B
C�B
A�B
@�B
?}B
?}B
>wB
>wB
<jB
;dB
:^B
:^B
:^B
9XB
8RB
8RB
9XB
9XB
9XB
9XB
;dB
:^B
8RB
6FB
6FB
8RB
:^B
:^B
:^B
9XB
8RB
8RB
7LB
7LB
5?B
49B
2-B
2-B
1'B
1'B
0!B
/B
0!B
/B
/B
0!B
0!B
0!B
/B
.B
.B
-B
-B
,B
)�B
%�B
!�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
{B
�B
�B
uB
uB
oB
�B
uB
VB
\B
VB
oB
PB
PB
PB
JB
PB
PB
PB
PB
PB
VB
VB
VB
bB
VB
VB
VB
\B
\B
uB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
$�B
#�B
%�B
%�B
&�B
'�B
)�B
+B
)�B
,B	w�B	w�B	v�B	v�B	v�B	v�B	v�B	v�B	w�B	w�B	w�B	v�B	w�B	s�B	x�B	u�B	y�B	v�B	t�B	w�B	v�B	u�B	v�B	x�B	w�B	w�B	v�B	w�B	w�B	w�B	v�B	w�B	w�B	w�B	v�B	v�B	v�B	w�B	v�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	v�B	v�B	v�B	w�B	v�B	x�B	w�B	w�B	v�B	w�B	x�B	v�B	v�B	v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           B	v�B	w�B	w�B	v�B	w�B	w�B	w�B	v�B	w�B	w�B	w�B	w�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	v�B	u�B	joB	*�B	fUB	�B	�B	�B
�B
V�B
hdB
H�B
n�B
��B�B
�'B
�B
��B
��BH�B�Bt�Bw�Bm�B`8BR�BD�B(�B	.B �B
�B
�DB
w�B
A�B
 �B	�AB	��B	B	��B	|�B	[B	1"B	�B	cB	B	B��B��B��B��B��B��B�B�cB�B	sB	8PB	B�B	T�B	v�B	��B	��B	�4B	�B	�JB	�PB	�B	��B	��B
B
	<B
bB
�B
%�B
+
B
3;B
6OB
:gB
C�B
E�B
L�B
P�B
P�B
O�B
N�B
N�B
M�B
P�B
O�B
O�B
O�B
O�B
P�B
Q�B
P�B
O�B
O�B
P�B
P�B
RB
RB
SB
RB
O�B
P�B
P�B
RB
SB
P�B
O�B
X+B
Y3B
Z9B
Y4B
UB
TB
R
B
QB
RB
TB
UB
UB
SB
SB
TB
V(B
U"B
TB
SB
U$B
U$B
RB
QB
QB
QB
QB
P	B
OB
M�B
L�B
L�B
N B
OB
OB
NB
NB
NB
L�B
L�B
K�B
J�B
K�B
J�B
J�B
J�B
I�B
I�B
I�B
H�B
F�B
E�B
D�B
D�B
C�B
A�B
@�B
?�B
?�B
>�B
>�B
<�B
;�B
:�B
:�B
:�B
9�B
8�B
8�B
9�B
9�B
9�B
9�B
;�B
:�B
8�B
6�B
6�B
8�B
:�B
:�B
:�B
9�B
8�B
8�B
7�B
7�B
5�B
4�B
2zB
2zB
1uB
1uB
0pB
/kB
0qB
/lB
/lB
0sB
0sB
0tB
/oB
.hB
.iB
-cB
-dB
,^B
*SB
&<B
"%B
#.B
!"B
 B
B
B
�B
�B
�B
B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
*B
EB
2B
:B
CB
JB
?B
@B
AB
=B
EB
EB
GB
VB
WB
_B
ZB
gB
iB
pB
rB
zB
{B
}B
~B
B
�B
�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
$�B
&�B
&�B
'�B
(�B
*�B
+�B
*�B
,�B	w�B	w�B	v�B	v�B	v�B	v�B	v�B	v�B	w�B	w�B	w�B	v�B	w�B	s�B	x�B	u�B	y�B	v�B	t�B	w�B	v�B	u�B	v�B	x�B	w�B	w�B	v�B	w�B	w�B	w�B	v�B	w�B	w�B	w�B	v�B	v�B	v�B	w�B	v�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	v�B	v�B	v�B	w�B	v�B	x�B	w�B	w�B	v�B	w�B	x�B	v�B	v�B	v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202572021061413571720210614135717202107221611172021072216111720210722161117201807242202572021061413571720210614135717202107221611172021072216111720210722161117PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422025720180724220257  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025720180724220257QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025720180724220257QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216141720210722161417IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                