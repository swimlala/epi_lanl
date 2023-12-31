CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:12Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191712  20181005191712  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               iA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�ɥ�mP1   @�ɥ�8�P@4����F�dMXbM�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      iA   A   A   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B  B  B  B   B(  B0  B8ffB@��BG��BO��BX  B`ffBhffBp  Bw��B��B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  C   C  C  C  C�C
�C  C  C  C�C  C  C  C�C�C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C?�fCB  CD  CE�fCH  CI�fCK�fCM�fCP  CR  CS�fCU�fCX�CZ�C\  C]�fC`�Cb�Cd  Cf�Ch  Ci�fCl�Cn�Cp�Cr�Cs�fCu�fCw�fCz  C|�C~  C�  C��3C�  C��C�  C��C��C��C��C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C��C�  C��C��C��3C��3C�  C��3C��3C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C��C�  C�  C��3C��3C��3C�  C�  C��C��C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C��C��C��C��C�  C��3C��C��C��3C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C��C��C�  C�  C��C��C�  C��3C�  C�  C��fC�  C��C�  C�  C��C�  C�  C��C�  C��3C��3C�  C��C��C�  C��C��C��C��C�  C��C��D fD � D  D�fD  D� D��Dy�D  D� D  D� D��Dy�D��Dy�D  D� D	  D	� D
fD
� D  D� D��Dy�D��D� D  D� DfD� D  D�fD  Dy�D  D� D  D� D��Dy�D  D�fDfD� DfD�fDfD� DfD�fDfD�fDfD� D  D�fD  D�fDfD�fDfD�fD   D �fD!fD!�fD"fD"� D"��D#� D$fD$� D%  D%y�D%��D&� D'  D'� D(  D(� D(��D)� D*  D*�fD+fD+�fD,fD,� D,��D-y�D.  D.� D/fD/�fD0  D0�fD1  D1� D2  D2y�D3  D3�fD4  D4� D4��D5y�D6  D6� D7  D7� D7��D8� D8��D9y�D9��D:y�D:��D;� D<  D<y�D<��D=y�D>  D>�fD?  D?� D@fD@�fDAfDAy�DB  DB� DC  DC�fDDfDD��DEfDE� DE��DFs3DG  DG� DHfDH� DH��DIy�DI��DJy�DJ��DK�fDL�DL��DM  DM� DNfDN�fDO�DO� DO��DP� DQfDQ�fDRfDR�fDS  DSy�DT  DT� DT��DUy�DVfDV� DV��DW� DX  DX�fDYfDY� DZfDZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`  D`� DafDa��DbfDb�fDcfDc� Dd  Dd� DefDe�fDffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj��DkfDky�DlfDl� Dl��Dm� Dm��Dn� DofDo�fDp  Dpy�Dq  Dq�fDr  Dry�Dr��Dsy�Ds��Dty�Du  Du�fDv  Dvy�Dw  Dw�fDyt{D�0 D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@��A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A��HA��HB
=B	
=B
=B
=B!
=B)
=B1
=B9p�BA�
BH��BP��BY
=Bap�Bip�Bq
=Bx��B�Q�B��B��B�Q�B��B��B�Q�B��B��B��B��B��B��B��B��B��B��BąB�Q�B̅BЅBԅB؅B܅B��B�B�B�B��B�Q�B�Q�B��C B�CB�CB�CB�C\)C
\)CB�CB�CB�C\)CB�CB�CB�C\)C\)CB�C B�C"B�C$\)C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8\)C:B�C<B�C>B�C@(�CBB�CDB�CF(�CHB�CJ(�CL(�CN(�CPB�CRB�CT(�CV(�CX\)CZ\)C\B�C^(�C`\)Cb\)CdB�Cf\)ChB�Cj(�Cl\)Cn\)Cp\)Cr\)Ct(�Cv(�Cx(�CzB�C|\)C~B�C�!HC�{C�!HC�.C�!HC�.C�.C�.C�.C�!HC�!HC�!HC�{C�!HC�{C�!HC�!HC�!HC�!HC�:�C�!HC�.C�.C�{C�{C�!HC�{C�{C�!HC�!HC�{C�!HC�!HC�{C�{C�!HC�!HC�!HC�{C�!HC�!HC�.C�!HC�!HC�{C�{C�{C�!HC�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�.C�:�C�!HC�!HC�{C�!HC�.C�:�C�.C�.C�!HC�{C�.C�.C�{C�!HC�{C�{C�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�.C�.C�!HC�!HC�.C�.C�!HC�{C�!HC�!HC��C�!HC�.C�!HC�!HC�.C�!HC�!HC�.C�!HC�{C�{C�!HC�.C�.C�!HC�.C�.C�.C�.C�!HC�.C�.D 
D ��D�D�
D�D��D
>D�>D�D��D�D��D
>D�>D
>D�>D�D��D	�D	��D

D
��D�D��D
>D�>D
>D��D�D��D
D��D�D�
D�D�>D�D��D�D��D
>D�>D�D�
D
D��D
D�
D
D��D
D�
D
D�
D
D��D�D�
D�D�
D
D�
D
D�
D �D �
D!
D!�
D"
D"��D#
>D#��D$
D$��D%�D%�>D&
>D&��D'�D'��D(�D(��D)
>D)��D*�D*�
D+
D+�
D,
D,��D-
>D-�>D.�D.��D/
D/�
D0�D0�
D1�D1��D2�D2�>D3�D3�
D4�D4��D5
>D5�>D6�D6��D7�D7��D8
>D8��D9
>D9�>D:
>D:�>D;
>D;��D<�D<�>D=
>D=�>D>�D>�
D?�D?��D@
D@�
DA
DA�>DB�DB��DC�DC�
DD
DD�qDE
DE��DF
>DF��DG�DG��DH
DH��DI
>DI�>DJ
>DJ�>DK
>DK�
DLqDL�qDM�DM��DN
DN�
DOqDO��DP
>DP��DQ
DQ�
DR
DR�
DS�DS�>DT�DT��DU
>DU�>DV
DV��DW
>DW��DX�DX�
DY
DY��DZ
DZ��D[�D[��D\�D\��D]�D]��D^
D^��D_�D_��D`�D`��Da
Da�qDb
Db�
Dc
Dc��Dd�Dd��De
De�
Df
Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj�qDk
Dk�>Dl
Dl��Dm
>Dm��Dn
>Dn��Do
Do�
Dp�Dp�>Dq�Dq�
Dr�Dr�>Ds
>Ds�>Dt
>Dt�>Du�Du�
Dv�Dv�>Dw�Dw�
Dy�D�8RD�)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A݁A݁A݅A݇+A݇+A݉7A݉7A݇+A݋DAݍPAݍPAݏ\Aݏ\Aݏ\Aݏ\AݑhAݓuAݑhAݑhA݇+A�x�A�ffA�;dA�O�A�-A���A͋DA�S�Aɧ�A���A���A�/A�E�A�7LAA��
A�{A��mA�I�A�A�$�A�`BA��jA�hsA�&�A��hA�E�A�7LA�|�A�/A��DA���A���A�oA���A���A��\A�n�A�=qA�;dA��A��RA��TA��\A��A��HA��A�?}A��DA�z�A�?}A�-A� �A���A��A��RA�r�A��HA�
=A��+A���A�n�A�z�A��;A�(�A��
A�=qA��wA�
=A�^5A��DA�JA}hsA|��A{�
AyK�Aw�#Av�yAu�mAtAr�jAp�\Ao/Am��Al�DAjbNAe�^Ad�jAcC�Aa�FA_�-A[�AYƨAV��AU��AS/AR�yAR�RAQG�AO�PAN1'ALffAHQ�AG;dAF��AFv�AEp�AB~�A@�HA?`BA=��A;��A:��A:�A81'A4z�A3��A1ƨA0ffA.�`A.I�A-&�A+��A*E�A)7LA(��A(v�A'�mA&��A&Q�A&  A%��A$�`A$1'A#ƨA#�A"{A ��A bNA 9XA�TA��A��A"�A��A(�A�wA�A9XA^5A�A�A�A9XA��A��AZA�A�9A��A?}AS�A=qA7LA��A1'AƨA
^5A�A�A5?A�AZAƨA�A�A+Ar�AJA�wA�7A �jA �@�V@�I�@��T@���@�E�@�A�@�hs@�A�@�~�@��H@��@�u@�(�@�l�@�5?@���@�Ĝ@�I�@��;@�@�o@���@�@�K�@���@�n�@ݑh@���@��T@��@؃@��@�+@���@�ƨ@�"�@���@��/@��@ΰ!@�hs@�Z@˝�@�l�@�33@���@�ff@�?}@�Ĝ@�1'@��@�b@�1@��m@��
@Ǖ�@�+@���@�V@��@ź^@�X@�V@�Ĝ@�bN@� �@���@Õ�@�33@��H@§�@�v�@�=q@��@�@��@�1@���@�"�@��@���@���@�-@��T@���@��`@��;@�t�@�33@��H@��!@�{@�p�@�V@��@���@�bN@�+@���@���@��9@�Q�@��@�K�@��@��@��@��#@�/@��@�Z@��@�M�@���@��u@�ȴ@�ȴ@�~�@��@�O�@�7L@��D@��
@��@�33@�;d@�+@�@��@��9@��@���@���@��u@���@�5?@�x�@�X@�V@��`@�%@��@�%@��u@��j@���@�|�@�C�@���@�^5@��@�hs@��7@��@�E�@�-@���@���@���@��-@��@�p�@�`B@�%@��@�1'@�Q�@�z�@�z�@�j@�bN@�(�@�1@�ƨ@��@��R@�M�@�E�@�$�@��@��T@��#@��#@���@���@���@��-@��@�/@��@��u@��D@��@�r�@�j@�b@��m@�l�@���@��@�ȴ@�ȴ@���@�V@�{@�@���@��T@���@���@�`B@��@��9@��D@�Z@�Q�@�Q�@�I�@� �@�ƨ@��@��P@�|�@�S�@�
=@��y@���@��!@��!@���@�-@��#@�O�@��j@�bN@�(�@��@�  @��@��F@��@���@���@���@�dZ@��@�^5@�M�@��@���@�Q�@�ƨ@��R@��@���@���@��h@�O�@�7L@��@�%@��`@���@��u@��u@��@���@�I�@�dZ@���@��R@��R@���@���@�v�@��@���@�hs@���@� �@���@�t�@�K�@�+@�
=@��@��\@�n�@�^5@�=q@�{@�;@mrG@]�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A݁A݁A݅A݇+A݇+A݉7A݉7A݇+A݋DAݍPAݍPAݏ\Aݏ\Aݏ\Aݏ\AݑhAݓuAݑhAݑhA݇+A�x�A�ffA�;dA�O�A�-A���A͋DA�S�Aɧ�A���A���A�/A�E�A�7LAA��
A�{A��mA�I�A�A�$�A�`BA��jA�hsA�&�A��hA�E�A�7LA�|�A�/A��DA���A���A�oA���A���A��\A�n�A�=qA�;dA��A��RA��TA��\A��A��HA��A�?}A��DA�z�A�?}A�-A� �A���A��A��RA�r�A��HA�
=A��+A���A�n�A�z�A��;A�(�A��
A�=qA��wA�
=A�^5A��DA�JA}hsA|��A{�
AyK�Aw�#Av�yAu�mAtAr�jAp�\Ao/Am��Al�DAjbNAe�^Ad�jAcC�Aa�FA_�-A[�AYƨAV��AU��AS/AR�yAR�RAQG�AO�PAN1'ALffAHQ�AG;dAF��AFv�AEp�AB~�A@�HA?`BA=��A;��A:��A:�A81'A4z�A3��A1ƨA0ffA.�`A.I�A-&�A+��A*E�A)7LA(��A(v�A'�mA&��A&Q�A&  A%��A$�`A$1'A#ƨA#�A"{A ��A bNA 9XA�TA��A��A"�A��A(�A�wA�A9XA^5A�A�A�A9XA��A��AZA�A�9A��A?}AS�A=qA7LA��A1'AƨA
^5A�A�A5?A�AZAƨA�A�A+Ar�AJA�wA�7A �jA �@�V@�I�@��T@���@�E�@�A�@�hs@�A�@�~�@��H@��@�u@�(�@�l�@�5?@���@�Ĝ@�I�@��;@�@�o@���@�@�K�@���@�n�@ݑh@���@��T@��@؃@��@�+@���@�ƨ@�"�@���@��/@��@ΰ!@�hs@�Z@˝�@�l�@�33@���@�ff@�?}@�Ĝ@�1'@��@�b@�1@��m@��
@Ǖ�@�+@���@�V@��@ź^@�X@�V@�Ĝ@�bN@� �@���@Õ�@�33@��H@§�@�v�@�=q@��@�@��@�1@���@�"�@��@���@���@�-@��T@���@��`@��;@�t�@�33@��H@��!@�{@�p�@�V@��@���@�bN@�+@���@���@��9@�Q�@��@�K�@��@��@��@��#@�/@��@�Z@��@�M�@���@��u@�ȴ@�ȴ@�~�@��@�O�@�7L@��D@��
@��@�33@�;d@�+@�@��@��9@��@���@���@��u@���@�5?@�x�@�X@�V@��`@�%@��@�%@��u@��j@���@�|�@�C�@���@�^5@��@�hs@��7@��@�E�@�-@���@���@���@��-@��@�p�@�`B@�%@��@�1'@�Q�@�z�@�z�@�j@�bN@�(�@�1@�ƨ@��@��R@�M�@�E�@�$�@��@��T@��#@��#@���@���@���@��-@��@�/@��@��u@��D@��@�r�@�j@�b@��m@�l�@���@��@�ȴ@�ȴ@���@�V@�{@�@���@��T@���@���@�`B@��@��9@��D@�Z@�Q�@�Q�@�I�@� �@�ƨ@��@��P@�|�@�S�@�
=@��y@���@��!@��!@���@�-@��#@�O�@��j@�bN@�(�@��@�  @��@��F@��@���@���@���@�dZ@��@�^5@�M�@��@���@�Q�@�ƨ@��R@��@���@���@��h@�O�@�7L@��@�%@��`@���@��u@��u@��@���@�I�@�dZ@���@��R@��R@���@���@�v�@��@���@�hs@���@� �@���@�t�@�K�@�+@�
=@��@��\@�n�@�^5@�=q@�{@�;@mrG@]�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�B@�B@�B?}B8RB$�BDBoB�B+B1'B5?BD�BK�BT�BdZBhsBl�B�B�7B�VB��B��B��B�qB�#B�/B�TB�fB�sB�mB�fB�NB�NB�NB�
B��BŢBÖB�dB�-B�B��B��B�+B|�BZB?}B,B�BbBPBJBDB1B+BB��B�B�NBƨB��B�Bl�BB�B�B
�B
�wB
�oB
}�B
jB
`BB
YB
G�B
A�B
=qB
33B
+B
"�B
�B
bB
B	��B	�sB	�)B	��B	ÖB	��B	��B	�bB	�B	v�B	`BB	Q�B	B�B	:^B	.B	+B	'�B	�B	{B	JB	B�B�B�yB�mB�HB�B��BɺBÖB�qB�XB�?B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�VB�7B�B�B�B�B�B~�B�+B�7B�1B�B�B~�B{�Bz�Bz�Bx�Bx�B~�B�B�B�B�B�B�B|�Bs�B�B�B}�B{�B� B� B� B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�=B�VB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�'B�'B�-B�FB�RB�XB�jB�wB��BÖBŢBƨBǮBȴBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�#B�)B�/B�5B�5B�;B�BB�NB�mB�B�B�B��B��B��B��B��B��B	  B	%B	1B		7B	
=B	DB	VB	hB	uB	uB	{B	�B	�B	 �B	$�B	+B	.B	0!B	2-B	1'B	/B	0!B	9XB	;dB	:^B	:^B	;dB	9XB	8RB	6FB	5?B	9XB	9XB	6FB	B�B	G�B	D�B	B�B	A�B	@�B	B�B	B�B	D�B	L�B	VB	\)B	cTB	e`B	cTB	aHB	]/B	]/B	_;B	aHB	bNB	cTB	dZB	ffB	gmB	l�B	k�B	m�B	m�B	l�B	m�B	m�B	o�B	q�B	v�B	z�B	{�B	|�B	~�B	�B	�=B	�JB	�JB	�VB	�VB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�LB	�LB	�RB	�dB	�dB	�dB	�dB	�jB	�jB	�jB	�qB	�wB	��B	ĜB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�/B	�/B	�5B	�;B	�;B	�BB	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�fB	�`B	�fB	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
JB
$22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�B@�B@�B?}B8RB$�BDBoB�B+B1'B5?BD�BK�BT�BdZBhsBl�B�B�7B�VB��B��B��B�qB�#B�/B�TB�fB�sB�mB�fB�NB�NB�NB�
B��BŢBÖB�dB�-B�B��B��B�+B|�BZB?}B,B�BbBPBJBDB1B+BB��B�B�NBƨB��B�Bl�BB�B�B
�B
�wB
�oB
}�B
jB
`BB
YB
G�B
A�B
=qB
33B
+B
"�B
�B
bB
B	��B	�sB	�)B	��B	ÖB	��B	��B	�bB	�B	v�B	`BB	Q�B	B�B	:^B	.B	+B	'�B	�B	{B	JB	B�B�B�yB�mB�HB�B��BɺBÖB�qB�XB�?B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�VB�7B�B�B�B�B�B~�B�+B�7B�1B�B�B~�B{�Bz�Bz�Bx�Bx�B~�B�B�B�B�B�B�B|�Bs�B�B�B}�B{�B� B� B� B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�=B�VB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�'B�'B�-B�FB�RB�XB�jB�wB��BÖBŢBƨBǮBȴBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�#B�)B�/B�5B�5B�;B�BB�NB�mB�B�B�B��B��B��B��B��B��B	  B	%B	1B		7B	
=B	DB	VB	hB	uB	uB	{B	�B	�B	 �B	$�B	+B	.B	0!B	2-B	1'B	/B	0!B	9XB	;dB	:^B	:^B	;dB	9XB	8RB	6FB	5?B	9XB	9XB	6FB	B�B	G�B	D�B	B�B	A�B	@�B	B�B	B�B	D�B	L�B	VB	\)B	cTB	e`B	cTB	aHB	]/B	]/B	_;B	aHB	bNB	cTB	dZB	ffB	gmB	l�B	k�B	m�B	m�B	l�B	m�B	m�B	o�B	q�B	v�B	z�B	{�B	|�B	~�B	�B	�=B	�JB	�JB	�VB	�VB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�LB	�LB	�RB	�dB	�dB	�dB	�dB	�jB	�jB	�jB	�qB	�wB	��B	ĜB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�/B	�/B	�5B	�;B	�;B	�BB	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�fB	�`B	�fB	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
JB
$22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191712                              AO  ARCAADJP                                                                    20181005191712    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191712  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191712  QCF$                G�O�G�O�G�O�8000            