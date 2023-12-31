CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:59Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BP   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ih   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       RH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       [(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b@   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       d   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       k    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  r8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       t    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  {   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191659  20181005191659  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               .A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׺�N2�1   @׺�+�v�@5["��`B�c��\)1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      .A   A   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B���B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C �C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&�C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<�C>�C@  CB  CD  CF  CH  CJ  CL�CN  CP  CQ�fCS�fCV  CX�CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp�Cr�Ct�Cv�Cx  Cz  C{�fC}�fC�fC��fC�  C��C�  C�  C�  C��C��C�  C�  C��C��C��C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C��3C��fC��3C�  C��C��C�  C��3C�  C�  C�  C��C�  C�  C�  C��3C�  C��3C�  C��C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C��3C�  C��3C�  C��3C��C�  C��3C�  C��C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C��3C�  C��C��3C��3C��3C��fC��3C�  C��3C�  C��C��C�  C�  C�  C��C��C��C��C��C��3C��C�  C�  C��C�  C��3C��3C��3C��3C�  C�  C��3C��C��C�  C�  C��3C�  C�  C�  C��C��D fD � D ��Dy�D  D�fD  D�fDfD� D��D� D  D� D  D� D��Dy�D�3D	� D	��D
y�D  D�fD  D� D  D� D��Dy�D��Dy�D  Dy�D��Dy�D��D� D�D�fD��Dy�D��D� D  D�fDfD�fDfD� D  Dy�D��Dy�D  D� D  Dy�D  D� D  D� D  Dy�D   D � D!  D!�fD"  D"�fD.�fD/fD/� D/��D0y�D0��D1�fD2fD2�fD3  D3��D4fD4� D4��D5y�D6  D6� D7  D7�fD8  D8� D9fD9�fD9��D:� D;fD;� D<  D<s3D=  D=� D=��D>y�D>��D?y�D?��D@� D@��DAy�DB  DB�fDC  DC�fDD  DD� DD��DE� DFfDF�fDG  DGy�DG��DH�fDI  DIy�DI��DJ� DKfDK�fDL�DL�fDM  DM� DM��DN� DN��DO�fDP  DP�fDQfDQ� DRfDR�fDSfDS� DT  DT�fDU  DU�fDVfDV�fDWfDW�fDXfDX� DY  DYy�DY��DZ� D[  Df��Dgs3Dh  Dh�fDi  Di�fDjfDj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDqy�Dr  Dr� Ds  Dsy�Ds��Dt�fDufDu�fDvfDv� Dw  Dwy�Dw��DxFfDy��D�8�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @J=q@�Q�@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A��HA�{B
=B	
=B
=B
=B!
=B)
=B1p�B9
=BA
=BI
=BQ
=BY
=Ba
=Bi
=Bq
=By
=B��B��B��B��B��B��B��B��B��RB��B��B�Q�B��B��B��B��B�Q�B�Q�B�Q�B̅BЅBԅB؅B܅B��B�B�RB�B��B�B��B��C \)C\)CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�C\)C B�C"B�C$B�C&\)C(B�C*B�C,B�C.B�C0B�C2(�C4B�C6B�C8B�C:B�C<\)C>\)C@B�CBB�CDB�CFB�CHB�CJB�CL\)CNB�CPB�CR(�CT(�CVB�CX\)CZ\)C\B�C^B�C`B�CbB�CdB�CfB�ChB�Cj\)ClB�CnB�Cp\)Cr\)Ct\)Cv\)CxB�CzB�C|(�C~(�C�{C��C�!HC�.C�!HC�!HC�!HC�.C�.C�!HC�!HC�.C�.C�.C�.C�.C�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�{C�!HC�{C��C�{C�!HC�.C�:�C�!HC�{C�!HC�!HC�!HC�.C�!HC�!HC�!HC�{C�!HC�{C�!HC�.C�!HC�!HC�{C�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�{C�{C�{C�!HC�{C�!HC�{C�.C�!HC�{C�!HC�.C�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�{C�!HC�!HC�{C�!HC�.C�{C�{C�{C��C�{C�!HC�{C�!HC�.C�.C�!HC�!HC�!HC�.C�.C�.C�.C�.C�{C�.C�!HC�!HC�.C�!HC�{C�{C�{C�{C�!HC�!HC�{C�.C�.C�!HC�!HC�{C�!HC�!HC�!HC�.C�.D 
D ��D
>D�>D�D�
D�D�
D
D��D
>D��D�D��D�D��D
>D�>D	�D	��D

>D
�>D�D�
D�D��D�D��D
>D�>D
>D�>D�D�>D
>D�>D
>D��DqD�
D
>D�>D
>D��D�D�
D
D�
D
D��D�D�>D
>D�>D�D��D�D�>D�D��D�D��D�D�>D �D ��D!�D!�
D"�D"�
D.�
D/
D/��D0
>D0�>D1
>D1�
D2
D2�
D3�D3�qD4
D4��D5
>D5�>D6�D6��D7�D7�
D8�D8��D9
D9�
D:
>D:��D;
D;��D<�D<��D=�D=��D>
>D>�>D?
>D?�>D@
>D@��DA
>DA�>DB�DB�
DC�DC�
DD�DD��DE
>DE��DF
DF�
DG�DG�>DH
>DH�
DI�DI�>DJ
>DJ��DK
DK�
DLqDL�
DM�DM��DN
>DN��DO
>DO�
DP�DP�
DQ
DQ��DR
DR�
DS
DS��DT�DT�
DU�DU�
DV
DV�
DW
DW�
DX
DX��DY�DY�>DZ
>DZ��D[�Dg
>Dg��Dh�Dh�
Di�Di�
Dj
Dj��Dk
Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq
Dq�>Dr�Dr��Ds�Ds�>Dt
>Dt�
Du
Du�
Dv
Dv��Dw�Dw�>Dx
>DxW
Dy�>D�AHD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��
A��A���A�Q�Aײ-Aס�Aס�Aף�Aף�Aס�Aף�Aן�Aכ�Aי�Aכ�Aכ�Aם�Aם�Aם�Aי�Aי�AדuA�1A֍PA�AԬA��A�7LA�%A�|�AΗ�A�7LA�bNA���A�S�A˴9A�7LA�I�Aɺ^A�ƨA�{A��A�  A�I�A�^5A��A�C�A�Q�A�A���A��PA�G�A�"�A�A��A���A���A��A�A�~�A���A��A�-A��mA�p�A��hA��A�VA�jA��HA�"�A��A�I�A��jA�(�A��RA�\)A�r�A�E�A��7A�ĜA�{A���A���A�9XA�ffA�K�A�9XA�  A�-A�n�A��A��A��TA��hA�$�A��A���A�ȴA�7LA���A�n�A}A|E�Ax�HAw��Aw��AwC�AuVAs��As�Aq"�Al$�AiC�Ae
=A`(�A^~�A\�AY�PAWdZAV�AT�AR�+AQt�AP�AOt�AN�yANĜAM�AK�AJn�AG�mAD��AB�+A@A?�hA>�RA=��A<v�A;�A:�A:ffA:  A9p�A8��A7�wA5?}A3t�A2��A1�^A0�`A0�!A09XA/&�A.I�A,�\A+XA*�A*�\A*E�A)�;A)��A(�jA'��A't�A&ĜA%�hA#�
A#K�A"Q�A"1A!�A �A�
AI�AbA�A��AbNAO�A~�A��A�A��A��A�A�A��AI�A1A�!A(�AS�Ax�A�A�/AjA|�AQ�A	�hAx�An�A�At�A��AAoA��Ar�A�A$�AQ�A9XAo@��@��D@���@�ȴ@���@���@��P@�@�P@�O�@�l�@���@���@�9@��T@�P@�hs@�dZ@�o@��@�
=@��@��H@���@�~�@�M�@�-@��@ܣ�@�K�@�@�M�@��@���@�|�@�v�@�^5@��@թ�@Չ7@���@�C�@�$�@�  @��@θR@��@�7L@�%@̛�@˾w@˥�@�l�@�o@��T@ə�@���@ȋD@�b@ǶF@�dZ@�dZ@�\)@�o@��@��@Ý�@°!@���@��h@���@��w@��P@�l�@�;d@���@��!@�E�@���@��-@���@�`B@�/@���@��`@���@�Q�@�S�@��@�M�@���@�7L@�bN@�  @���@���@�t�@�S�@�@�ȴ@���@�5?@�p�@�7L@�&�@��`@���@�Ĝ@��9@��#@��^@���@�p�@��u@���@��w@�33@�ȴ@��@��@�bN@�Z@�I�@�(�@��@�|�@�o@���@�M�@�-@�J@���@��@��7@�/@���@��9@���@���@�Z@�z�@��@��@�r�@�r�@�r�@�bN@�I�@�(�@�1@��;@��
@���@�t�@�C�@���@��\@�n�@�V@��@��^@��@�hs@�`B@�X@�?}@��@�V@���@��@�1@���@���@��P@�t�@�S�@�+@��y@��@���@���@�ff@�E�@���@���@��7@��7@�x�@�G�@��@�9X@��@��P@�33@��@��H@���@�V@���@���@�`B@��`@��D@��@�o@��@���@��!@��\@�v�@�^5@�M�@�5?@��T@���@��7@��7@�`B@��@�Ĝ@��9@�z�@� �@��m@�\)@���@��+@�=q@�J@��@��^@�hs@���@��@�1@�iD@}S&@j��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��
A��A���A�Q�Aײ-Aס�Aס�Aף�Aף�Aס�Aף�Aן�Aכ�Aי�Aכ�Aכ�Aם�Aם�Aם�Aי�Aי�AדuA�1A֍PA�AԬA��A�7LA�%A�|�AΗ�A�7LA�bNA���A�S�A˴9A�7LA�I�Aɺ^A�ƨA�{A��A�  A�I�A�^5A��A�C�A�Q�A�A���A��PA�G�A�"�A�A��A���A���A��A�A�~�A���A��A�-A��mA�p�A��hA��A�VA�jA��HA�"�A��A�I�A��jA�(�A��RA�\)A�r�A�E�A��7A�ĜA�{A���A���A�9XA�ffA�K�A�9XA�  A�-A�n�A��A��A��TA��hA�$�A��A���A�ȴA�7LA���A�n�A}A|E�Ax�HAw��Aw��AwC�AuVAs��As�Aq"�Al$�AiC�Ae
=A`(�A^~�A\�AY�PAWdZAV�AT�AR�+AQt�AP�AOt�AN�yANĜAM�AK�AJn�AG�mAD��AB�+A@A?�hA>�RA=��A<v�A;�A:�A:ffA:  A9p�A8��A7�wA5?}A3t�A2��A1�^A0�`A0�!A09XA/&�A.I�A,�\A+XA*�A*�\A*E�A)�;A)��A(�jA'��A't�A&ĜA%�hA#�
A#K�A"Q�A"1A!�A �A�
AI�AbA�A��AbNAO�A~�A��A�A��A��A�A�A��AI�A1A�!A(�AS�Ax�A�A�/AjA|�AQ�A	�hAx�An�A�At�A��AAoA��Ar�A�A$�AQ�A9XAo@��@��D@���@�ȴ@���@���@��P@�@�P@�O�@�l�@���@���@�9@��T@�P@�hs@�dZ@�o@��@�
=@��@��H@���@�~�@�M�@�-@��@ܣ�@�K�@�@�M�@��@���@�|�@�v�@�^5@��@թ�@Չ7@���@�C�@�$�@�  @��@θR@��@�7L@�%@̛�@˾w@˥�@�l�@�o@��T@ə�@���@ȋD@�b@ǶF@�dZ@�dZ@�\)@�o@��@��@Ý�@°!@���@��h@���@��w@��P@�l�@�;d@���@��!@�E�@���@��-@���@�`B@�/@���@��`@���@�Q�@�S�@��@�M�@���@�7L@�bN@�  @���@���@�t�@�S�@�@�ȴ@���@�5?@�p�@�7L@�&�@��`@���@�Ĝ@��9@��#@��^@���@�p�@��u@���@��w@�33@�ȴ@��@��@�bN@�Z@�I�@�(�@��@�|�@�o@���@�M�@�-@�J@���@��@��7@�/@���@��9@���@���@�Z@�z�@��@��@�r�@�r�@�r�@�bN@�I�@�(�@�1@��;@��
@���@�t�@�C�@���@��\@�n�@�V@��@��^@��@�hs@�`B@�X@�?}@��@�V@���@��@�1@���@���@��P@�t�@�S�@�+@��y@��@���@���@�ff@�E�@���@���@��7@��7@�x�@�G�@��@�9X@��@��P@�33@��@��H@���@�V@���@���@�`B@��`@��D@��@�o@��@���@��!@��\@�v�@�^5@�M�@�5?@��T@���@��7@��7@�`B@��@�Ĝ@��9@�z�@� �@��m@�\)@���@��+@�=q@�J@��@��^@�hs@���@��@�1@�iD@}S&@j��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BbB\BbB\B\BbBoBoBoBuBuBoBuBuBoBoBuBuBuBuB{B�B�B{B&�B33B=qBXBx�B��B��B�B�jB��B��B�)B�BB
=BbB�BhB�B�BR�BffBk�Bw�B�1B�hB�{B��B��B��B��B��B��B��B��B�!B�!B�XB�FB��B��B��B��B�B��B�Bx�Bp�Be`BZBQ�BJ�B:^B33B,B�B�B\B�sB�B��BÖB�}B��Bz�Bo�Bv�BiyBVB
�RB
�hB
~�B
�B
�B
|�B
m�B
]/B
P�B
F�B
8RB
-B
"�B
\B

=B
1B
B	��B	�B	�ZB	��B	�XB	��B	�PB	r�B	hsB	]/B	K�B	@�B	:^B	0!B	%�B	�B	�B	�B	uB	hB	PB	B��B�B�`B�5B�B��B��B��B��BǮBŢBĜBB��B�wB�^B�3B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B�oB��B��B��B��B��B�'BǮB�B�#B��B��B�^B�B��B�B�LB�dB�LB�^B�}BĜB��B��B�
B�B�)B�
B��B��B��B��B��B��B��B��BɺBŢB�wB�9B�9B�'B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�3B�9B�3B�9B�9B�9B�XB�dB�dB�jB�qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#B�HB�NB�TB�`B�mB�yB�B�B�B��B��B��B��B	B	B	B	DB	JB	PB	VB	bB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	$�B	%�B	(�B	.B	0!B	1'B	2-B	33B	49B	6FB	7LB	9XB	<jB	A�B	C�B	C�B	E�B	F�B	F�B.B	u�B	v�B	v�B	w�B	z�B	}�B	~�B	�B	�B	�B	�%B	�DB	�JB	�PB	�bB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�?B	�?B	�FB	�LB	�RB	�RB	�RB	�RB	�RB	�XB	�XB	�XB	�^B	�wB	�}B	�}B	��B	��B	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B=qB	�sB	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
%B	��B
9B
%�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222  BbB\BbB\B\BbBoBoBoBuBuBoBuBuBoBoBuBuBuBuB{B�B�B{B&�B33B=qBXBx�B��B��B�B�jB��B��B�)B�BB
=BbB�BhB�B�BR�BffBk�Bw�B�1B�hB�{B��B��B��B��B��B��B��B��B�!B�!B�XB�FB��B��B��B��B�B��B�Bx�Bp�Be`BZBQ�BJ�B:^B33B,B�B�B\B�sB�B��BÖB�}B��Bz�Bo�Bv�BiyBVB
�RB
�hB
~�B
�B
�B
|�B
m�B
]/B
P�B
F�B
8RB
-B
"�B
\B

=B
1B
B	��B	�B	�ZB	��B	�XB	��B	�PB	r�B	hsB	]/B	K�B	@�B	:^B	0!B	%�B	�B	�B	�B	uB	hB	PB	B��B�B�`B�5B�B��B��B��B��BǮBŢBĜBB��B�wB�^B�3B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B�oB��B��B��B��B��B�'BǮB�B�#B��B��B�^B�B��B�B�LB�dB�LB�^B�}BĜB��B��B�
B�B�)B�
B��B��B��B��B��B��B��B��BɺBŢB�wB�9B�9B�'B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�3B�9B�3B�9B�9B�9B�XB�dB�dB�jB�qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#B�HB�NB�TB�`B�mB�yB�B�B�B��B��B��B��B	B	B	B	DB	JB	PB	VB	bB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	$�B	%�B	(�B	.B	0!B	1'B	2-B	33B	49B	6FB	7LB	9XB	<jB	A�B	C�B	C�B	E�B	F�B	F�B.B	u�B	v�B	v�B	w�B	z�B	}�B	~�B	�B	�B	�B	�%B	�DB	�JB	�PB	�bB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�?B	�?B	�FB	�LB	�RB	�RB	�RB	�RB	�RB	�XB	�XB	�XB	�^B	�wB	�}B	�}B	��B	��B	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B=qB	�sB	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
%B	��B
9B
%�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191659                              AO  ARCAADJP                                                                    20181005191659    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191659  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191659  QCF$                G�O�G�O�G�O�8000            