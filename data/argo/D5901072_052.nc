CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:53Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z           9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               4A   AO  20111130144028  20190522121829  1728_5048_052                   2C  D   APEX                            2142                            040306                          846 @Ԭ	�_��1   @Ԭ
�Y�@5�"��`B�c=p��
1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA�fDB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Dby�Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dy� D� D�6fD�y�D���D��3D�,�D�s3D���D��fD�)�D�s3Dǹ�D��3D�6fD�S3D��D���D�3D�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@l��@�  @�  A  A8  AX  Ax  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B&  B.  B6  B>  BF  BN  BV  B^  Bf  Bn  Bv  B~ffB�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C� C� C� C��C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CKffCMffCO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� D ` D � D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D	` D	� D
` D
� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D�fD` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D ` D � D!` D!� D"` D"� D#` D#� D$` D$� D%` D%� D&` D&� D'` D'� D(` D(� D)` D)� D*` D*� D+` D+� D,` D,� D-` D-� D.` D.� D/` D/� D0` D0� D1` D1� D2` D2� D3` D3� D4` D4� D5` D5� D6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=� D>` D>� D?` D?� D@` D@� DAffDA� DB` DB� DCY�DC� DD` DD� DE` DE� DF` DF� DG` DG� DH` DH� DI` DI� DJ` DJ� DK` DK� DL` DL� DM` DM� DN` DN� DO` DO� DP` DP� DQ` DQ� DR` DR� DS` DS� DT` DT� DU` DU� DV` DV� DW` DW� DX` DX� DY` DY� DZ` DZ� D[` D[� D\` D\� D]` D]� D^` D^� D_` D_� D`` D`� Da` Da� DbY�Db� Dc` Dc� Dd` Dd� De` De� Df` Df� Dg` Dg� Dh` Dh� Di` Di� Dj` Dj� Dk` Dk� Dl` Dl� Dm` Dm� Dn` Dn� Do` Do� Dp` Dp� Dq` Dq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du� Dv` Dy` D�  D�&fD�i�D���D��3D��D�c3D���D��fD��D�c3Dǩ�D��3D�&fD�C3D��D���D��3D�<�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA�VA�ZA�XA�ZA�VA�XA�XA�VA�XA�\)A�\)A�bNA�dZA�bNA�\)A�^5A�`BA�bNA�`BA�`BA�`BA�bNA�bNA�dZA�hsA�jA�p�A�z�AǁA�~�AǅAǋDA���A��yA�
=A���A�A�XA��A�A��yA�5?A�p�A��A�O�A��A��A�`BA��DA���A���A�bNA��TA�VA�`BA��^A��A��A�E�A�K�A�JA�?}A���A���A��TA�dZA��A�33A���A�VA�v�A�VA���A�-A�Q�A���A�ffA��
A�^5A��A�l�A��PA��A���A���A��A�A��A���A��A�ZA��
A��PA��A���A�A�~�A��;A��A���A��A��A�5?A�{A���A��hA}K�A{�TA{K�Az��Az�+Ay�AwO�AuXAtI�AsK�Aq��Aq"�Aop�Amt�Ag�Ab�A^ĜA[�AXz�AUK�AT^5AS�APZAN��AM\)ALZAJ�AH��AG�AF��AE�hADjAC7LAB$�AAS�A@=qA>�!A=A<E�A:��A8��A7�;A5�A4�/A3��A1�;A05?A/A/t�A/7LA.�`A.��A-�A-oA,��A+`BA*n�A)33A(�9A'|�A&�RA%��A%A$�uA$$�A#�FA#/A"n�A!��A!`BA!�A A�AƨA7LA�HAVA��A�A��A��A��AAƨA�A�7A��A  A�A�^Av�A��A��A�7A+AjA�TAl�A5?A%A �A&�A
JA	hsA~�A+A=qA�AJA��A�An�A��AdZA ��@�ƨ@���@�(�@�
=@��@�r�@�n�@�9X@�-@@�R@�{@�bN@�-@�u@�ƨ@�M�@�/@�j@�G�@�bN@�(�@�l�@݉7@܃@��@�  @ڰ!@�b@ָR@���@�o@��T@�O�@�j@θR@���@��;@��@�X@ț�@�I�@ǝ�@��@�\)@��@�^5@�G�@�I�@�o@�V@�K�@��@��H@�n�@��T@��/@��D@�-@��@�A�@���@��@�b@�l�@��R@�-@�@�%@��D@��@�ƨ@�33@���@�ff@�E�@�J@��^@�`B@���@��9@�A�@��w@�l�@���@���@��@��@��j@�O�@��@��`@��9@���@�j@��@��+@���@��@��9@�bN@��@�\)@�
=@��H@��+@��\@�33@�ȴ@��@�C�@�dZ@�l�@�dZ@�+@��+@��@��D@�j@�j@�1@�l�@�^5@���@��@��y@�v�@�$�@�$�@�E�@��\@��@���@��R@��@��@�r�@���@��@�ff@��@��y@��T@�`B@���@�+@��@���@�\)@��P@��P@�S�@�;d@��@�n�@�x�@�O�@�G�@�G�@�G�@�?}@�?}@�&�@��@�9X@�A�@���@���@�+@���@���@�~�@�M�@�-@��@��#@���@��^@��7@�G�@��@�%@��/@��9@�Z@� �@�1@���@�|�@�K�@�@���@�=q@��@���@���@�G�@���@�b@�o@��H@��R@��\@��R@��!@�~�@�V@�$�@��`@���@��;@��
@�S�@�@��@�\)@�t�@�|�@�\)@��@��y@���@�~�@�V@��@��@�?}@��@�Ĝ@���@�bN@� �@�  @��;@���@��w@���@�C�@�o@��H@��R@�n�@�M�@��@���@�G�@��/@��@��u@�z�@�bN@�1'@���@���@��
@��P@�\)@�C�@��@��@���@��R@�ff@���@���@�x�@�?}@��@���@��@�bN@�o@~��@u�-@l(�@e�-@_;d@X�u@Pr�@I��@B�H@<�/@8 �@1�@,j@%��@!&�@��@��@�@+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA�VA�ZA�XA�ZA�VA�XA�XA�VA�XA�\)A�\)A�bNA�dZA�bNA�\)A�^5A�`BA�bNA�`BA�`BA�`BA�bNA�bNA�dZA�hsA�jA�p�A�z�AǁA�~�AǅAǋDA���A��yA�
=A���A�A�XA��A�A��yA�5?A�p�A��A�O�A��A��A�`BA��DA���A���A�bNA��TA�VA�`BA��^A��A��A�E�A�K�A�JA�?}A���A���A��TA�dZA��A�33A���A�VA�v�A�VA���A�-A�Q�A���A�ffA��
A�^5A��A�l�A��PA��A���A���A��A�A��A���A��A�ZA��
A��PA��A���A�A�~�A��;A��A���A��A��A�5?A�{A���A��hA}K�A{�TA{K�Az��Az�+Ay�AwO�AuXAtI�AsK�Aq��Aq"�Aop�Amt�Ag�Ab�A^ĜA[�AXz�AUK�AT^5AS�APZAN��AM\)ALZAJ�AH��AG�AF��AE�hADjAC7LAB$�AAS�A@=qA>�!A=A<E�A:��A8��A7�;A5�A4�/A3��A1�;A05?A/A/t�A/7LA.�`A.��A-�A-oA,��A+`BA*n�A)33A(�9A'|�A&�RA%��A%A$�uA$$�A#�FA#/A"n�A!��A!`BA!�A A�AƨA7LA�HAVA��A�A��A��A��AAƨA�A�7A��A  A�A�^Av�A��A��A�7A+AjA�TAl�A5?A%A �A&�A
JA	hsA~�A+A=qA�AJA��A�An�A��AdZA ��@�ƨ@���@�(�@�
=@��@�r�@�n�@�9X@�-@@�R@�{@�bN@�-@�u@�ƨ@�M�@�/@�j@�G�@�bN@�(�@�l�@݉7@܃@��@�  @ڰ!@�b@ָR@���@�o@��T@�O�@�j@θR@���@��;@��@�X@ț�@�I�@ǝ�@��@�\)@��@�^5@�G�@�I�@�o@�V@�K�@��@��H@�n�@��T@��/@��D@�-@��@�A�@���@��@�b@�l�@��R@�-@�@�%@��D@��@�ƨ@�33@���@�ff@�E�@�J@��^@�`B@���@��9@�A�@��w@�l�@���@���@��@��@��j@�O�@��@��`@��9@���@�j@��@��+@���@��@��9@�bN@��@�\)@�
=@��H@��+@��\@�33@�ȴ@��@�C�@�dZ@�l�@�dZ@�+@��+@��@��D@�j@�j@�1@�l�@�^5@���@��@��y@�v�@�$�@�$�@�E�@��\@��@���@��R@��@��@�r�@���@��@�ff@��@��y@��T@�`B@���@�+@��@���@�\)@��P@��P@�S�@�;d@��@�n�@�x�@�O�@�G�@�G�@�G�@�?}@�?}@�&�@��@�9X@�A�@���@���@�+@���@���@�~�@�M�@�-@��@��#@���@��^@��7@�G�@��@�%@��/@��9@�Z@� �@�1@���@�|�@�K�@�@���@�=q@��@���@���@�G�@���@�b@�o@��H@��R@��\@��R@��!@�~�@�V@�$�@��`@���@��;@��
@�S�@�@��@�\)@�t�@�|�@�\)@��@��y@���@�~�@�V@��@��@�?}@��@�Ĝ@���@�bN@� �@�  @��;@���@��w@���@�C�@�o@��H@��R@�n�@�M�@��@���@�G�@��/@��@��u@�z�@�bN@�1'@���@���@��
@��P@�\)@�C�@��@��@���@��R@�ff@���@���@�x�@�?}@��@���@��@�bN@�o@~��@u�-@l(�@e�-@_;d@X�u@Pr�@I��@B�H@<�/@8 �@1�@,j@%��@!&�@��@��@�@+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBǮBǮBǮBǮBǮBƨBƨBƨBŢBŢBƨBƨBǮBǮBǮBŢBŢBƨBƨBƨBƨBƨBƨBƨBƨBƨBȴB��B��B�
B��B�B�/BB]/B�{B�3B�'B�oBz�B�B��B��B�=B�B�\B��B��B�B��B�=B��B��B��B��B�\B�7B�7B�uB��B�RB�^B�9B�B��B�BdZB^5B]/B]/BaHBcTBXBT�BW
BW
BN�BK�B>wB+B�B�B�`B�BɺB�'B��B�BgmBQ�B;dB.B&�BoB
�
B
�qB
�9B
�!B
��B
�bB
r�B
W
B
:^B
,B
)�B
&�B
&�B
�B
B
  B
B
	7B
B	�B	�B
  B
B	��B	��B	�sB	��B	�{B	m�B	M�B	6FB	(�B	B	  B�B�B�B�B�BB��B��B��B��BĜB�dB�9B�?B�9B�B�LB�9B�XB�qB�dB��B��B��B��B�hB�1B�B|�B{�B|�B{�B�B~�B�B�\Bw�Bt�Bt�Bx�Bu�Br�Bl�Bl�Bl�Bo�Bt�Bo�Bp�Bp�Br�Br�Bq�Bo�Bn�Bl�Bk�BjBl�Bk�BiyBgmBffBffBe`BbNBaHBbNB^5BZBYB[#B\)B_;B_;B^5B]/B^5B[#BW
BT�BS�BS�BQ�BQ�BO�BO�BR�BR�B_;BW
BXBR�BP�BP�B[#B[#BR�BN�BN�BO�BVB[#BZBO�BK�BI�BF�BD�BA�B@�BB�B@�B;dB7LB7LB5?B33B49B33B33B/B+B%�B#�B"�B"�B �B'�B"�B$�B#�B �B!�B �B�B �B#�B)�B.B,B)�B&�B%�B+B49B7LB<jB@�B@�BL�BJ�BQ�BQ�BT�BW
BW
B\)B^5BaHBgmBjBn�Bn�Bp�Bt�Bw�Bz�B{�B{�B|�B~�B�B�1B�7B�\B�{B�{B��B��B�3B�LB��B��B��B��B��B��B��B��B�B�B�B�/B�/B�;B�HB�NB�TB�`B�B��B��B	B	%B		7B	JB	\B	oB	uB	uB	oB	uB	�B	�B	�B	�B	�B	{B	�B	�B	�B	�B	 �B	 �B	'�B	-B	2-B	2-B	)�B	+B	-B	9XB	B�B	I�B	J�B	H�B	H�B	M�B	ZB	_;B	aHB	bNB	gmB	iyB	iyB	iyB	jB	m�B	p�B	p�B	q�B	p�B	p�B	p�B	q�B	u�B	u�B	v�B	z�B	{�B	|�B	}�B	� B	�B	�B	�+B	�7B	�DB	�JB	�PB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�B	�B	��B	�B	�B	�B	�3B	�-B	�3B	�3B	�B	�B	�B	�-B	�9B	�9B	�FB	�LB	�RB	�^B	�^B	�dB	�jB	�jB	�qB	�wB	�wB	��B	��B	B	ÖB	ĜB	ŢB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�5B	�5B	�;B	�;B	�;B	�BB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B
  B
JB
�B
�B
%�B
-B
2-B
;dB
A�B
G�B
K�B
Q�B
XB
[#B
`BB
jB
n�B
q�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BǮBǮBǮBǮBǮBƨBƨBƨBŢBŢBƨBƨBǮBǮBǮBŢBŢBƨBƨBƨBƨBƨBƨBƨBƨBƨBȴB��B��B�
B��B�B�/BB]/B��B�FB�LB��B� B�=B��B��B�oB�+B�uB��B��B�B�B�PB��B��B��B��B�oB�JB�JB�{B��B�^B�wB�RB�'B��B�JBffBaHB_;B`BBcTBgmB[#BW
BZBZBO�BM�BD�B1'B�B��B�sB�/B��B�?B��B�DBm�BW
B?}B1'B.B�B
�;B
ŢB
�LB
�9B
�B
��B
�B
bNB
?}B
-B
+B
(�B
.B
�B
B
B
B
DB
1B	��B	��B
B
%B	��B	��B	�B	�#B	��B	x�B	W
B	>wB	1'B	%B	B��B��B��B�B�mB�B��B��B��BȴB�}B�RB�RB�RB�?B�qB�LB�wBB�qB��B��B��B�B��B�7B�B}�B|�B}�B~�B�B�B�1B�oB{�Bv�Bx�Bz�Bx�Bu�Bn�Bn�Bn�Bq�Bw�Br�Bq�Bq�Bu�Bt�Bs�Bp�Bp�Bn�Bm�Bm�Bp�Bn�Bl�BhsBgmBgmBgmBffBdZBgmBbNB]/B\)B_;B^5BbNBaHB`BBaHBbNB^5BZBYBVBW
BW
BVBT�BR�BT�BT�BaHBYBYBT�BR�BS�B^5B]/BT�BQ�BQ�BS�BZB_;B\)BQ�BN�BN�BH�BF�BD�BB�BD�BE�B=qB8RB9XB9XB5?B5?B49B6FB49B.B(�B'�B$�B#�B"�B+B&�B&�B&�B"�B#�B!�B!�B!�B#�B+B0!B.B,B)�B)�B.B5?B8RB=qBA�BB�BM�BM�BS�BR�BW
BZBYB]/B_;BbNBhsBk�Bo�Bo�Bq�Bu�Bx�B{�B|�B|�B}�B~�B�B�7B�=B�bB��B��B��B��B�3B�FB��B��B��B��B��B��B�B�B�B�#B�#B�5B�5B�BB�NB�TB�ZB�`B�yB��B��B	B	%B		7B	JB	bB	uB	�B	{B	uB	uB	�B	�B	�B	!�B	�B	�B	�B	�B	�B	�B	 �B	 �B	'�B	.B	33B	5?B	)�B	+B	,B	8RB	A�B	J�B	L�B	I�B	G�B	K�B	ZB	_;B	bNB	bNB	gmB	jB	jB	jB	l�B	o�B	q�B	p�B	q�B	p�B	p�B	p�B	r�B	v�B	v�B	v�B	{�B	{�B	}�B	}�B	�B	�B	�%B	�1B	�=B	�JB	�JB	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�!B	�B	�B	�B	�B	�B	�9B	�3B	�9B	�?B	�'B	�B	�B	�3B	�?B	�9B	�FB	�LB	�RB	�dB	�dB	�jB	�qB	�qB	�wB	�}B	�}B	��B	B	ÖB	ĜB	ŢB	ŢB	ȴB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�)B	�5B	�;B	�BB	�;B	�BB	�HB	�TB	�NB	�ZB	�TB	�ZB	�ZB	�`B	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B
  B
JB
�B
�B
%�B
-B
2-B
;dB
A�B
G�B
K�B
Q�B
XB
\)B
`BB
jB
n�B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<u<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452112012011014521120120110145211  AO  ARGQ                                                                        20111130144028  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144028  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145211  IP                  G�O�G�O�G�O�                