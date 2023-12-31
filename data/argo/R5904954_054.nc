CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:00Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191700  20181005191700  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               6A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׼��M1   @׼�I���@5VE�����c��\)1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      6A   A   A   @���@�  A   A   A@  A^ffA~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�33B�  B���B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33C   C  C  C  C  C
  C  C  C�fC  C�C  C  C  C  C  C   C"�C$�C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CM�fCP  CR�CT�CV�CX  CY�fC[�fC]�fC_�fCb�Cd  Cf  Ch�Cj  Ck�fCm�fCp�Cr  Ct  Cv  Cx  Cz�C|�C~�C��C�  C��3C�  C��C��C�  C��3C��C��C��3C��3C��fC�  C�  C��C��C��3C��C��3C�  C�  C�  C�  C��3C��fC��3C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C��C��3C�  C��C�  C��3C��C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C��3C��3C��3C��3C��3C�  C��C��fC�  C��C��C�  C�  C��C�  C��3C��3C��fC�  C��C��3C��C��C�  C�  C��C��C��C��C��C�  C��3C�  C�  C��3C��3C��3C��C��C��C��C��C��3C��fC��C�  C�  C�  D   D y�D  D�fDfD�fDfD� D  D� D  D�fD  Dy�D��D�fDfD� D	  D	� D
  D
� D  D� D  D�fDfD� DfD�fD  D� D  D� D  Dy�D��Dy�D  D� D  Dy�D��Dy�D  D�fDfD� D��Dy�D  D� D  Dy�D��Dy�DfD�fD  D� DfD� D  D�fD fD � D!  D!� D"  D"�fD"��D#y�D$  D$�fD%  D%� D&  D&�fD'fD'�fD(  D(� D)  D)� D*fD*�fD+fD+� D,  D,y�D,��D-y�D.fD.�fD/fD/�fD0  D0�fD1�D1�fD2fD2�fD3  D3� D4  D4� D5fD5� D6  D6� D7  D7�fD8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=�fD>fD>� D>��D?�fD@fD@� DA  DA� DB  DBy�DC  DC� DD  DD�fDE  DEy�DE��DFy�DF��DG� DHfDH�fDIfDI�fDI��DJy�DK  DK� DL  DLy�DM  DM� DN  DN� DN��DO� DPfDP�fDQfDQ�fDRfDR� DR�3DS� DTfDT�fDU  DU� DV  DV� DV��DWy�DX  DX� DY  DY� DY��DZ� D[  D[� D[��D\y�D\��D]� D^fD^�fD_  D_y�D`  D`� Da  Da� Da�3Dbs3Db�3Dc� DdfDd� De  De� De��Dfy�Dg  Dg�fDh  Dhy�Dh��Di� DjfDj�fDk  Dky�Dl  Dl�fDm  Dm� Dm��Dn� DofDo� Do��Dps3Dq  Dq�fDr  Dr� DsfDs�fDt  Dt� Du  Duy�Dv  Dvy�Dv��Dw� Dw�3Dy��D�N�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�A(�A$(�AD(�Ab�\A�G�A�{A�{A�{A�{A�{A�{A�{B
=B	
=B
=B
=B!
=B)
=B1
=B9
=BA
=BI
=BQ
=BY
=Ba
=Bi
=Bq
=By
=B�Q�B��B��B��B��RB��B��B��B��B��B��B�Q�B��B��RB��B�Q�B��B�Q�B�Q�B̅BЅBԅB؅B܅B��B�B�B�B�Q�B�B��B��RC B�CB�CB�CB�CB�C
B�CB�CB�C(�CB�C\)CB�CB�CB�CB�CB�C B�C"\)C$\)C&\)C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CF(�CHB�CJB�CLB�CN(�CPB�CR\)CT\)CV\)CXB�CZ(�C\(�C^(�C`(�Cb\)CdB�CfB�Ch\)CjB�Cl(�Cn(�Cp\)CrB�CtB�CvB�CxB�Cz\)C|\)C~\)C�.C�!HC�{C�!HC�.C�.C�!HC�{C�.C�.C�{C�{C��C�!HC�!HC�.C�.C�{C�.C�{C�!HC�!HC�!HC�!HC�{C��C�{C�!HC�!HC�{C�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�{C�!HC�!HC�!HC�!HC�!HC�!HC�.C�{C�!HC�.C�!HC�{C�.C�!HC�.C�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�{C�{C�!HC�{C�{C�{C�{C�{C�!HC�.C��C�!HC�.C�.C�!HC�!HC�.C�!HC�{C�{C��C�!HC�.C�{C�.C�.C�!HC�!HC�:�C�.C�.C�.C�:�C�!HC�{C�!HC�!HC�{C�{C�{C�.C�:�C�.C�.C�.C�{C��C�.C�!HC�!HC�!HD �D �>D�D�
D
D�
D
D��D�D��D�D�
D�D�>D
>D�
D
D��D	�D	��D
�D
��D�D��D�D�
D
D��D
D�
D�D��D�D��D�D�>D
>D�>D�D��D�D�>D
>D�>D�D�
D
D��D
>D�>D�D��D�D�>D
>D�>D
D�
D�D��D
D��D�D�
D 
D ��D!�D!��D"�D"�
D#
>D#�>D$�D$�
D%�D%��D&�D&�
D'
D'�
D(�D(��D)�D)��D*
D*�
D+
D+��D,�D,�>D-
>D-�>D.
D.�
D/
D/�
D0�D0�
D1qD1�
D2
D2�
D3�D3��D4�D4��D5
D5��D6�D6��D7�D7�
D8�D8��D9�D9�
D:�D:��D;�D;��D<�D<��D=�D=�
D>
D>��D?
>D?�
D@
D@��DA�DA��DB�DB�>DC�DC��DD�DD�
DE�DE�>DF
>DF�>DG
>DG��DH
DH�
DI
DI�
DJ
>DJ�>DK�DK��DL�DL�>DM�DM��DN�DN��DO
>DO��DP
DP�
DQ
DQ�
DR
DR��DS�DS��DT
DT�
DU�DU��DV�DV��DW
>DW�>DX�DX��DY�DY��DZ
>DZ��D[�D[��D\
>D\�>D]
>D]��D^
D^�
D_�D_�>D`�D`��Da�Da��Db�Db��Dc�Dc��Dd
Dd��De�De��Df
>Df�>Dg�Dg�
Dh�Dh�>Di
>Di��Dj
Dj�
Dk�Dk�>Dl�Dl�
Dm�Dm��Dn
>Dn��Do
Do��Dp
>Dp��Dq�Dq�
Dr�Dr��Ds
Ds�
Dt�Dt��Du�Du�>Dv�Dv�>Dw
>Dw��Dx�Dy��D�W
D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HAԬAԗ�A�O�A��AѴ9Aџ�AёhA�z�A�ffA�ZA�M�A�7LA�-A�"�A��AС�A���Aω7A�I�A��A��TAάA΅A�~�A�5?A�VA�A�$�Aˉ7AɼjA�1A�7LA�1A�  A�ƨA�/AŴ9A�bNA�ȴA��A�\)AA���A��/A��A�ffA��uA���A��A��uA�`BA���A�l�A�^5A��FA���A�%A���A���A�K�A�ƨA��jA���A��#A�ƨA�
=A�VA�(�A��mA�  A��9A�S�A�A��A�A���A��
A��7A�oA�t�A�%A�jA��wA�dZA��A���A�A�A�z�A��DA�5?A�ƨA���A�VA��DA�ȴA�(�A|�AzVAuoAp  Ak�PAh  Af  Ac�wAbȴAb��AbjAa��A^$�AZJAW
=AUp�AS�AR�+AQ`BANbNAL5?AH�AF{AC��AA�PAA/AA7LAA\)AA�hAAO�A?��A>�\A;�7A8��A7�;A5�mA3��A2A0n�A.��A+��A*�/A*��A*�A'�A&��A&v�A&A�A&A%��A%VA$^5A"��A!��A�A-AA��A�A-A��A��AG�A
=AȴA�+A1A|�AƨA��A��Ax�AXA;dA"�A��A�jA�+AI�A��A�A��A�yAȴAr�A�HA�PA|�A33A
I�A	l�A5?A�PAoAv�A��AM�AA��AdZAVA I�@���@�=q@��^@�Q�@��T@��9@�\)@��\@��T@�V@�l�@�-@@�=q@��@�S�@���@�ff@�O�@�K�@��@�@�l�@�r�@߮@��@�n�@�=q@�@�9X@���@�@��@���@���@؃@�1@�ƨ@ם�@ם�@ם�@׍P@ׅ@���@�?}@�1@ҟ�@�1'@���@�?}@���@��/@˶F@�ff@�@��T@ɲ-@��@�I�@Ǖ�@Ƨ�@ģ�@��@Ý�@�;d@�@���@���@��u@���@�^5@��7@�p�@���@���@�t�@�ȴ@�E�@��@���@�p�@���@��@�Q�@�A�@�1@�ƨ@�+@��R@�~�@�^5@�E�@���@��\@��-@�O�@�hs@���@���@��-@�@��^@�?}@��j@��@�I�@��m@�t�@�o@�@���@���@�n�@�$�@��@�p�@��@���@��@�`B@�X@��9@��u@�r�@��@��w@�t�@�+@��@��#@�@��^@�x�@�x�@��T@�{@���@�@�@�@�@��@�p�@��@�I�@�9X@�1@�t�@�
=@�o@��@�~�@�@��@�hs@�X@�O�@�7L@�%@��9@�9X@��;@�;d@��!@��!@�^5@��@�p�@��@��
@�l�@�"�@�o@��!@�$�@��@�O�@��@�Z@�Q�@�1'@��@��!@�@���@�/@��/@�r�@�1@�ƨ@���@���@���@��
@�  @�1@���@��@��+@��@��@��7@���@��@�  @��
@��
@��
@��
@��@�b@���@�;d@�33@���@���@��+@�E�@��#@�x�@�?}@�V@�z�@���@�|�@�C�@��\@�^5@�-@��T@��-@���@���@�x�@���@��@�Q�@� �@�  @�ƨ@��P@�l�@�C�@�+@�o@��@��y@��H@��@���@��\@�~�@�=q@��T@��-@���@��7@�hs@�/@���@���@�j@�bN@�bN@�bN@�A�@��m@���@��F@���@�dZ@��@��@���@�`B@��/@�9X@��m@�ƨ@�ƨ@��@��@��\@�^5@�5?@��@���@�@�`B@�O�@�/@��@���@�Ĝ@��9@���@���@x~(@f�8111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��HAԬAԗ�A�O�A��AѴ9Aџ�AёhA�z�A�ffA�ZA�M�A�7LA�-A�"�A��AС�A���Aω7A�I�A��A��TAάA΅A�~�A�5?A�VA�A�$�Aˉ7AɼjA�1A�7LA�1A�  A�ƨA�/AŴ9A�bNA�ȴA��A�\)AA���A��/A��A�ffA��uA���A��A��uA�`BA���A�l�A�^5A��FA���A�%A���A���A�K�A�ƨA��jA���A��#A�ƨA�
=A�VA�(�A��mA�  A��9A�S�A�A��A�A���A��
A��7A�oA�t�A�%A�jA��wA�dZA��A���A�A�A�z�A��DA�5?A�ƨA���A�VA��DA�ȴA�(�A|�AzVAuoAp  Ak�PAh  Af  Ac�wAbȴAb��AbjAa��A^$�AZJAW
=AUp�AS�AR�+AQ`BANbNAL5?AH�AF{AC��AA�PAA/AA7LAA\)AA�hAAO�A?��A>�\A;�7A8��A7�;A5�mA3��A2A0n�A.��A+��A*�/A*��A*�A'�A&��A&v�A&A�A&A%��A%VA$^5A"��A!��A�A-AA��A�A-A��A��AG�A
=AȴA�+A1A|�AƨA��A��Ax�AXA;dA"�A��A�jA�+AI�A��A�A��A�yAȴAr�A�HA�PA|�A33A
I�A	l�A5?A�PAoAv�A��AM�AA��AdZAVA I�@���@�=q@��^@�Q�@��T@��9@�\)@��\@��T@�V@�l�@�-@@�=q@��@�S�@���@�ff@�O�@�K�@��@�@�l�@�r�@߮@��@�n�@�=q@�@�9X@���@�@��@���@���@؃@�1@�ƨ@ם�@ם�@ם�@׍P@ׅ@���@�?}@�1@ҟ�@�1'@���@�?}@���@��/@˶F@�ff@�@��T@ɲ-@��@�I�@Ǖ�@Ƨ�@ģ�@��@Ý�@�;d@�@���@���@��u@���@�^5@��7@�p�@���@���@�t�@�ȴ@�E�@��@���@�p�@���@��@�Q�@�A�@�1@�ƨ@�+@��R@�~�@�^5@�E�@���@��\@��-@�O�@�hs@���@���@��-@�@��^@�?}@��j@��@�I�@��m@�t�@�o@�@���@���@�n�@�$�@��@�p�@��@���@��@�`B@�X@��9@��u@�r�@��@��w@�t�@�+@��@��#@�@��^@�x�@�x�@��T@�{@���@�@�@�@�@��@�p�@��@�I�@�9X@�1@�t�@�
=@�o@��@�~�@�@��@�hs@�X@�O�@�7L@�%@��9@�9X@��;@�;d@��!@��!@�^5@��@�p�@��@��
@�l�@�"�@�o@��!@�$�@��@�O�@��@�Z@�Q�@�1'@��@��!@�@���@�/@��/@�r�@�1@�ƨ@���@���@���@��
@�  @�1@���@��@��+@��@��@��7@���@��@�  @��
@��
@��
@��
@��@�b@���@�;d@�33@���@���@��+@�E�@��#@�x�@�?}@�V@�z�@���@�|�@�C�@��\@�^5@�-@��T@��-@���@���@�x�@���@��@�Q�@� �@�  @�ƨ@��P@�l�@�C�@�+@�o@��@��y@��H@��@���@��\@�~�@�=q@��T@��-@���@��7@�hs@�/@���@���@�j@�bN@�bN@�bN@�A�@��m@���@��F@���@�dZ@��@��@���@�`B@��/@�9X@��m@�ƨ@�ƨ@��@��@��\@�^5@�5?@��@���@�@�`B@�O�@�/@��@���@�Ĝ@��9@���@���@x~(@f�8111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B�LB�FB�?B�9B�9B�?B�9B�3B�3B�3B�!B�B��B��B�uB�bB�VB�JB�JB�JB�hB��B�uB�!B��B�ZB�B��B��B��BB�B$�B)�B49B@�BN�B_;Bm�Bw�B� B��B�^B�B��B
=B�B�B�B{BbBB�B�/BǮB�'B��B��B�\B� Bu�Bp�Bm�Bo�Bo�Bn�BbNB\)BT�B=qB!�BbB1B��B�B�sB�NB�B��B�qB�9B��B{�BjBQ�B5?B�BJB
�B
�B
�qB
�B
YB
@�B
�B	�B	�
B	�jB	�!B	��B	��B	��B	��B	�bB	|�B	cTB	XB	P�B	F�B	>wB	7LB	�B	hB��B�fB�
B��B��B��B��B�5B�ZB�BB�B��BĜB�wB�^B�FB�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�oB��B��B��B��B��B�{B��B��B��B��B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�9B�?B�FB�LB�RB�XB�dB�^B�RB�RB�XB�jB�}BŢBǮBȴB��B��B��B��B��B��B��B��B��B��B�B�)B�5B�NB�fB�sB�B�B�B�B�B�B��B��B��B��B	  B	%B	1B	VB	hB	uB	uB	uB	{B	�B	�B	�B	�B	�B	)�B	,B	-B	0!B	1'B	33B	5?B	6FB	8RB	:^B	?}B	D�B	F�B	I�B	M�B	Q�B	S�B	T�B	XB	YB	[#B	^5B	_;B	aHB	dZB	iyB	l�B	o�B	t�B	v�B	v�B	}�B	}�B	~�B	}�B	}�B	|�B	}�B	� B	�B	�B	�+B	�PB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�RB	�RB	�LB	�dB	�jB	�qB	�jB	�dB	�^B	�XB	�dB	�qB	�wB	�}B	ƨB	ǮB	ƨB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�B	�B	�
B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�5B	�;B	�BB	�BB	�NB	�`B	�fB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
1B
	7B
	7B
	7B
	7B

=B
DB
DB
JB
JB
JB
PB
PB
VB
VB
�B
+6222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B��B��B��B��B��B�LB�FB�?B�9B�9B�?B�9B�3B�3B�3B�!B�B��B��B�uB�bB�VB�JB�JB�JB�hB��B�uB�!B��B�ZB�B��B��B��BB�B$�B)�B49B@�BN�B_;Bm�Bw�B� B��B�^B�B��B
=B�B�B�B{BbBB�B�/BǮB�'B��B��B�\B� Bu�Bp�Bm�Bo�Bo�Bn�BbNB\)BT�B=qB!�BbB1B��B�B�sB�NB�B��B�qB�9B��B{�BjBQ�B5?B�BJB
�B
�B
�qB
�B
YB
@�B
�B	�B	�
B	�jB	�!B	��B	��B	��B	��B	�bB	|�B	cTB	XB	P�B	F�B	>wB	7LB	�B	hB��B�fB�
B��B��B��B��B�5B�ZB�BB�B��BĜB�wB�^B�FB�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�oB��B��B��B��B��B�{B��B��B��B��B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�9B�?B�FB�LB�RB�XB�dB�^B�RB�RB�XB�jB�}BŢBǮBȴB��B��B��B��B��B��B��B��B��B��B�B�)B�5B�NB�fB�sB�B�B�B�B�B�B��B��B��B��B	  B	%B	1B	VB	hB	uB	uB	uB	{B	�B	�B	�B	�B	�B	)�B	,B	-B	0!B	1'B	33B	5?B	6FB	8RB	:^B	?}B	D�B	F�B	I�B	M�B	Q�B	S�B	T�B	XB	YB	[#B	^5B	_;B	aHB	dZB	iyB	l�B	o�B	t�B	v�B	v�B	}�B	}�B	~�B	}�B	}�B	|�B	}�B	� B	�B	�B	�+B	�PB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�RB	�RB	�LB	�dB	�jB	�qB	�jB	�dB	�^B	�XB	�dB	�qB	�wB	�}B	ƨB	ǮB	ƨB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�B	�B	�
B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�5B	�;B	�BB	�BB	�NB	�`B	�fB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
1B
	7B
	7B
	7B
	7B

=B
DB
DB
JB
JB
JB
PB
PB
VB
VB
�B
+6222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191700                              AO  ARCAADJP                                                                    20181005191700    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191700  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005191700  QCF$                G�O�G�O�G�O�8000            