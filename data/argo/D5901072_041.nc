CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:50Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               )A   AO  20111130143917  20190522121828  1728_5048_041                   2C  D   APEX                            2142                            040306                          846 @Ԑ�:g�1   @Ԑ���@@6S����c=�^5?}1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"�fD#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL�fDMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy��D��fD�0 D�i�D��3D�fD�9�D�33D�� D���D�#3D�y�Dǣ3D�� D�)�D�@ D�3D��fD���D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @s33@�  @�  A  A8  AX  Ax  A�  A�  A�33A�  A�  A�  A�  A�  B  B  BffB  B&  B.  B6  B>  BF  BN  BV  B^  Bf  Bn  Bv  B~ffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+��C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg��Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D ` D � D` D� D` D� D` D� D` D� D` D� D` D�fD` D� D` D� D	` D	� D
` D
� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� DffD� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D ` D � D!` D!�fD"ffD"�fD#` D#� D$` D$� D%` D%� D&` D&� D'` D'� D(` D(� D)` D)� D*` D*� D+` D+� D,` D,� D-` D-� D.` D.� D/` D/� D0` D0� D1` D1� D2` D2� D3` D3� D4` D4� D5` D5� D6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=� D>` D>� D?` D?� D@` D@� DA` DA� DB` DB� DC` DC� DD` DD�fDE` DE� DF` DF� DG` DG� DH` DH� DI` DI� DJ` DJ�fDK` DK� DLffDL�fDM` DM� DN` DN� DO` DO� DP` DP� DQ` DQ� DR` DR� DS` DS� DT` DT� DU` DU� DV` DV� DW` DW� DX` DX� DY` DY� DZ` DZ� D[` D[� D\` D\� D]` D]� D^` D^� D_` D_� D`` D`� Da` Da� Db` Db� DcffDc� Dd` Dd� De` De� Df` Df� Dg` Dg� Dh` Dh� Di` Di� Dj` Dj� Dk` Dk� Dl` Dl� Dm` Dm� Dn` Dn� Do` Do� Dp` Dp� DqffDq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du� Dv` Dv� Dw` Dyy�D��fD�  D�Y�D��3D��fD�)�D�#3D�� D��D�3D�i�DǓ3D�� D��D�0 D�3D��fD��D�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A҉7A�I�A��A���A��mA�ƨA��Aϴ9Aϙ�A�~�A�bNA�C�A�+A���A���A�Aκ^AΧ�A΍PA�~�A�n�A�VA�;dA�(�A��A�bA�%A��A��
Aͩ�A˅A�bNA�O�A��/A�  A�S�A�VA��!A�oA�JA���A�~�A���A�|�A���A��-A��7A�ffA���A��A���A�ȴA�A�A�jA��A���A��
A���A�VA��PA��
A�bNA��yA���A�z�A���A�G�A�;dA�E�A��RA�K�A���A��
A�I�A�oA��/A���A���A�?}A���A���A�+A���A�bNA���A�$�A��A�A���A�(�A�%A�+A�l�A�A�?}A�S�A��!A��A�C�A��RA�r�A�ĜA��hA�I�A�I�A�mA}�
A|1'Ay��Aw�
Avz�At�As�Arz�Aq��An�Ai��Ah{Ae&�Ab��A`z�A_+A]?}A[dZAZr�AW��AVAS��AP�jAN��AL�`AJ�AJz�AH�AFv�AE�ADbAB��AA
=A?�
A>�A<�HA<=qA;G�A9dZA6��A533A41'A3�-A2�A0�HA/`BA.9XA-%A,�+A,-A+�wA+�A)t�A&�9A%ƨA%`BA$��A#�A"ĜA!��A!�^A (�Ax�A�An�A��AM�Al�A�!A�-A?}AbNA��A\)A�!A9XA��A5?A��A"�A(�A��A��A�^A|�A�/AC�A�mA
�!A
M�A	��A��A|�A�uA�^A��A�A7LA^5A��A �yA bNA {@��@�&�@� �@��9@���@�\@�7@��@�9X@�-@�Q�@ꗍ@��#@�bN@��@���@�bN@�t�@�&�@޸R@�?}@܃@�C�@�M�@��@؋D@ם�@��@�ȴ@ՙ�@��/@��@�M�@�&�@У�@�9X@�1@���@ϕ�@ϕ�@�33@·+@��@͑h@��@���@�9X@���@�l�@ʰ!@�-@��@�p�@ȼj@��;@���@�A�@��y@���@���@���@�p�@�S�@��R@�hs@�  @���@�?}@��m@��!@���@��@���@�"�@�n�@��@�V@��u@�Q�@�9X@��@��@�K�@���@��@��@�p�@�?}@��D@�9X@��m@���@�dZ@��@�v�@�@���@�7L@���@��u@�r�@�I�@�1'@��@��
@���@���@���@�dZ@�33@�@��@���@�^5@�$�@���@�V@�Ĝ@���@�b@���@�J@���@�O�@���@���@��m@���@��m@�ƨ@��@��@��@�o@�K�@�1'@�I�@�A�@�1'@��
@�\)@�;d@��@��H@��!@�~�@�V@�5?@��@��@���@��T@�@���@�x�@�G�@�/@�&�@��@���@�(�@�A�@�bN@�r�@�r�@�I�@�1'@� �@� �@�b@���@��F@�
=@�n�@�^5@�E�@�{@��#@��-@���@��@�/@���@��j@�j@��F@�t�@�l�@�l�@�\)@�+@�
=@��H@��\@�V@��@��-@��@�G�@�&�@��@��@���@���@�Q�@�A�@��@�S�@�o@��y@���@�ȴ@�ȴ@���@�@���@��9@�Ĝ@��9@�r�@���@��@�S�@�@��H@���@�ȴ@�n�@��^@�p�@�x�@�?}@�O�@���@���@�dZ@�o@���@�V@�n�@���@�~�@�5?@��@�J@��@���@�`B@��@��@��u@��@��F@�l�@�;d@��R@�@�X@��@���@��u@�j@�I�@�1'@�ƨ@�S�@��y@���@�~�@�5?@�O�@�Ĝ@�I�@�b@�@��@��@��@�@�@��@��@;d@~ȴ@~v�@}`B@v�+@n��@eO�@^{@U�@Ol�@Ihs@B�@>V@7�@2n�@.��@)��@$��@��@r�@�j@M�@\)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A҉7A�I�A��A���A��mA�ƨA��Aϴ9Aϙ�A�~�A�bNA�C�A�+A���A���A�Aκ^AΧ�A΍PA�~�A�n�A�VA�;dA�(�A��A�bA�%A��A��
Aͩ�A˅A�bNA�O�A��/A�  A�S�A�VA��!A�oA�JA���A�~�A���A�|�A���A��-A��7A�ffA���A��A���A�ȴA�A�A�jA��A���A��
A���A�VA��PA��
A�bNA��yA���A�z�A���A�G�A�;dA�E�A��RA�K�A���A��
A�I�A�oA��/A���A���A�?}A���A���A�+A���A�bNA���A�$�A��A�A���A�(�A�%A�+A�l�A�A�?}A�S�A��!A��A�C�A��RA�r�A�ĜA��hA�I�A�I�A�mA}�
A|1'Ay��Aw�
Avz�At�As�Arz�Aq��An�Ai��Ah{Ae&�Ab��A`z�A_+A]?}A[dZAZr�AW��AVAS��AP�jAN��AL�`AJ�AJz�AH�AFv�AE�ADbAB��AA
=A?�
A>�A<�HA<=qA;G�A9dZA6��A533A41'A3�-A2�A0�HA/`BA.9XA-%A,�+A,-A+�wA+�A)t�A&�9A%ƨA%`BA$��A#�A"ĜA!��A!�^A (�Ax�A�An�A��AM�Al�A�!A�-A?}AbNA��A\)A�!A9XA��A5?A��A"�A(�A��A��A�^A|�A�/AC�A�mA
�!A
M�A	��A��A|�A�uA�^A��A�A7LA^5A��A �yA bNA {@��@�&�@� �@��9@���@�\@�7@��@�9X@�-@�Q�@ꗍ@��#@�bN@��@���@�bN@�t�@�&�@޸R@�?}@܃@�C�@�M�@��@؋D@ם�@��@�ȴ@ՙ�@��/@��@�M�@�&�@У�@�9X@�1@���@ϕ�@ϕ�@�33@·+@��@͑h@��@���@�9X@���@�l�@ʰ!@�-@��@�p�@ȼj@��;@���@�A�@��y@���@���@���@�p�@�S�@��R@�hs@�  @���@�?}@��m@��!@���@��@���@�"�@�n�@��@�V@��u@�Q�@�9X@��@��@�K�@���@��@��@�p�@�?}@��D@�9X@��m@���@�dZ@��@�v�@�@���@�7L@���@��u@�r�@�I�@�1'@��@��
@���@���@���@�dZ@�33@�@��@���@�^5@�$�@���@�V@�Ĝ@���@�b@���@�J@���@�O�@���@���@��m@���@��m@�ƨ@��@��@��@�o@�K�@�1'@�I�@�A�@�1'@��
@�\)@�;d@��@��H@��!@�~�@�V@�5?@��@��@���@��T@�@���@�x�@�G�@�/@�&�@��@���@�(�@�A�@�bN@�r�@�r�@�I�@�1'@� �@� �@�b@���@��F@�
=@�n�@�^5@�E�@�{@��#@��-@���@��@�/@���@��j@�j@��F@�t�@�l�@�l�@�\)@�+@�
=@��H@��\@�V@��@��-@��@�G�@�&�@��@��@���@���@�Q�@�A�@��@�S�@�o@��y@���@�ȴ@�ȴ@���@�@���@��9@�Ĝ@��9@�r�@���@��@�S�@�@��H@���@�ȴ@�n�@��^@�p�@�x�@�?}@�O�@���@���@�dZ@�o@���@�V@�n�@���@�~�@�5?@��@�J@��@���@�`B@��@��@��u@��@��F@�l�@�;d@��R@�@�X@��@���@��u@�j@�I�@�1'@�ƨ@�S�@��y@���@�~�@�5?@�O�@�Ĝ@�I�@�b@�@��@��@��@�@�@��@��@;d@~ȴ@~v�@}`B@v�+@n��@eO�@^{@U�@Ol�@Ihs@B�@>V@7�@2n�@.��@)��@$��@��@r�@�j@M�@\)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB>wB>wB>wB>wB<jB0!B&�B&�B&�B&�B&�B'�B'�B%�B"�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�BbB��B��B��B��B�B�B�)B�`B�B��B��B��B��B��B��B�B�B�B�B��B��B��B�B�`B�HB�;B�BB�fB�BB�NB�BB�TB�B�NB�TB�NB�yB�B�ZB�BB�B��B��B��B�dB�LB�3B��B��B��B��B�\B�7B� Bx�Bo�BT�BI�B2-BuB
��B
�B
�ZB
�)B
��B
�jB
�!B
��B
��B
|�B
bNB
ZB
@�B
(�B
(�B
+B	��B	�fB	��B	ƨB	�FB	�B	��B	�{B	�bB	u�B	R�B	K�B	J�B	49B	�B	PB	  B	B��B�fB�sB�#BB�jB�LB�!B�B��B��B��B��B��B��B�PB�7B�7B�B�B~�B�bB~�Bs�Bo�Bk�BiyBjBffBffBe`BffBjBbNBl�B|�BffBhsBe`BhsBcTB`BB_;BcTBbNBffBbNB^5Be`BbNBdZBcTBaHBbNBcTB_;BbNB_;BbNB`BB^5B]/B[#B]/B]/B\)B[#BZBYB[#BVBS�BT�BQ�BL�BL�BK�BH�BH�BH�BF�BC�BA�B@�BB�B@�BC�B>wB>wB@�B=qB6FB:^B33B1'B2-B;dB8RB9XB8RB7LB5?B9XB<jB:^B;dB5?B8RB6FB7LB7LB33B5?B6FB7LB6FB6FB6FB6FB7LB;dB=qB?}BB�BM�BR�BXBYB^5BcTBffBjBk�Bk�Bl�Bo�Bq�Bs�Bt�Bt�Bn�Bn�BhsBe`BffBbNBdZBk�B^5Be`BdZB\)BaHBbNBk�Bn�Bm�BjBp�Br�Bu�Bv�Bw�Bx�By�Bz�B}�B~�B�B�DB�\B�hB�uB��B��B��B��B��B��B��B��B�B�B�'B�9B�9B�FB�LB�RB�^B�}BĜBƨBȴB��B��B��B�B�)B�5B�TB�sB�yB�B�B��B��B��B��B��B��B	B	1B	JB	VB	VB	JB	PB	\B	oB	�B	!�B	"�B	"�B	'�B	.B	/B	1'B	33B	6FB	9XB	:^B	;dB	<jB	?}B	B�B	E�B	H�B	K�B	N�B	P�B	S�B	VB	ZB	]/B	^5B	^5B	_;B	aHB	hsB	l�B	m�B	o�B	o�B	p�B	q�B	q�B	q�B	r�B	s�B	t�B	u�B	x�B	z�B	{�B	}�B	� B	�B	�B	�B	�%B	�1B	�=B	�JB	�PB	�\B	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�FB	�RB	�RB	�jB	�^B	�dB	�qB	�qB	�wB	��B	��B	B	ÖB	ÖB	ŢB	ŢB	ȴB	ȴB	ƨB	ĜB	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�BB	�HB	�NB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
  B
VB
{B
�B
(�B
1'B
6FB
:^B
C�B
H�B
M�B
S�B
ZB
`BB
dZB
k�B
p�B
s�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B?}B?}B?}B?}BA�B49B'�B'�B'�B'�B'�B(�B(�B&�B"�B!�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B  B�B��B��B�/B�NB�fB�B�B��BBB%B  B��B��B��B�B��B��B��B��B��B�yB�`B�TB�`B�B�ZB�fB�TB�fB�B�`B�mB�fB�B��B�sB�TB�/B�
B��BB�qB�XB�XB�B��B��B��B�oB�DB�B{�Bt�BYBO�B9XB�BB
�B
�fB
�BB
��B
�}B
�9B
��B
��B
�B
gmB
aHB
H�B
0!B
2-B
PB	��B	�B	�B	��B	�dB	�B	��B	��B	��B	� B	VB	Q�B	O�B	8RB	"�B	hB	B	B	  B�B�B�NBǮBB�qB�-B�3B�B�B�B��B��B��B�uB�PB�JB�7B�=B�1B��B�Bu�Bt�Bo�Bn�Bn�BjBhsBffBgmBk�BgmBr�B� BgmBjBgmBk�Be`BaHBcTBe`BdZBhsBe`BbNBhsBdZBgmBe`BdZBdZBe`BaHBdZBcTBe`BbNBaHB`BB`BB_;B^5B]/B^5B_;B^5B_;BXBVBYBVBO�BO�BN�BK�BK�BK�BI�BF�BC�BB�BE�BC�BE�BC�BC�BC�B?}B7LB<jB6FB49B5?B<jB:^B<jB;dB8RB7LB=qB@�B=qB=qB7LB:^B7LB:^B9XB49B6FB8RB9XB8RB9XB8RB7LB8RB<jB>wB@�BB�BN�BT�BYBZB_;BdZBgmBk�Bl�Bm�Bm�Bp�Br�Bu�Bv�Bx�Br�Bq�Bk�BgmBffBe`BgmBl�B`BBgmBffB_;BcTBdZBm�Bp�Bo�Bl�Bq�Bs�Bw�Bw�Bx�By�Bz�B{�B~�B� B�%B�JB�bB�oB��B��B��B��B��B��B��B��B��B�B�B�-B�?B�?B�LB�RB�XB�dB�}BĜBǮBɺB��B��B��B�#B�)B�5B�TB�yB�B�B��B��B��B��B��B��B��B	B	1B	PB	\B	\B	PB	PB	\B	hB	�B	!�B	"�B	#�B	(�B	/B	0!B	2-B	49B	7LB	:^B	;dB	;dB	=qB	?}B	B�B	F�B	H�B	L�B	O�B	P�B	S�B	VB	[#B	^5B	^5B	^5B	_;B	aHB	iyB	l�B	m�B	o�B	o�B	q�B	r�B	r�B	r�B	r�B	t�B	u�B	v�B	y�B	z�B	|�B	~�B	�B	�B	�B	�+B	�+B	�1B	�=B	�JB	�VB	�bB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�FB	�RB	�XB	�qB	�dB	�jB	�wB	�wB	�wB	��B	B	ÖB	ĜB	ÖB	ƨB	ŢB	ɺB	��B	ǮB	ŢB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�B	�#B	�/B	�/B	�5B	�;B	�5B	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�fB	�fB	�sB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
B
\B
�B
�B
(�B
1'B
7LB
:^B
D�B
H�B
M�B
T�B
ZB
`BB
dZB
k�B
p�B
s�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452072012011014520720120110145207  AO  ARGQ                                                                        20111130143917  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143917  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145207  IP                  G�O�G�O�G�O�                