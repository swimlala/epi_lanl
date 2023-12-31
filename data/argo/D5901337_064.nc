CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:41Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               @A   AO  20111205113449  20190522121836  1901_5055_064                   2C  D   APEX                            2140                            040306                          846 @��K"��1   @��K�� @,U�$�/�c����o1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B'��B/��B7��B@  BH  BO��BX  B`  Bh  Bp  Bx  B��B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B�33B�  B�33B�33B�  B�33B�  B�  B�  B�  B�  B�  B���B�33B�33B�  B�  C   C�fC�fC�fC  C
�C  C�fC  C�C  C  C  C  C  C  C   C"  C$  C&  C(�C)�fC,  C.  C/�fC1�fC3�fC6  C8  C:  C<�C>  C@  CB�CD  CF  CH�CJ  CK�fCN  CP  CR  CT  CV  CX�CZ  C\  C^�C`  Ca�fCd  Cf  Cg�fCj  Cl  Cn  Co�fCr  Ct�Cv  Cx  Cy�fC|  C~�C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C��C��C��C�  C�  C��3C��3C��3C��3C�  C��C��C��C��C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C��3C��3C�  C�  C�  C��C��3C�  C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C�  C��C��C��C��C��C��C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C��C��C��C��C��C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C��3C�  C��D   D � D  Dy�D  D�fD  D� D  D� D  D�fDfD� D  D� DfD� D��D	� D
  D
� D  D�fD  D� DfD� D  D� D  Dy�D��Dy�D��Dy�D��D� D  D� D  D� D  D� DfD� D��Dy�D  D�fDfD� D��D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"fD"� D"��D#� D$  D$� D$��D%y�D&  D&y�D&��D'� D(fD(� D)  D)� D*  D*� D+  D+� D+��D,� D-  D-y�D.  D.� D/  D/� D0fD0� D0��D1y�D2  D2� D2��D3� D4fD4� D5  D5� D6  D6� D7  D7y�D8  D8� D9  D9�fD:  D:y�D:��D;� D<fD<� D<��D=� D>  D>� D?  D?y�D@  D@�fDA  DA� DBfDB� DC  DC� DDfDD�fDEfDE�fDF  DF�fDGfDG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DLfDL� DM  DMy�DN  DN� DO  DO� DO��DPy�DP��DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DW��DXy�DY  DY� DZ  DZ� DZ��D[� D\  D\� D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dg��Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDvy�DyffD�fD�6fD�y�D�ɚD��D�33D�ffD���D�fD�#3D�c3DǼ�D�3D��D�fD๚D�ٚD��D�Y�D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @`  @�  @�  A  A8  AX  Ax  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B��B%��B-��B5��B>  BF  BM��BV  B^  Bf  Bn  Bv  B}��B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B�33B�  B�33B�33B�  B�33B�  B�  B�  B�  B�  B�  B���B�33B�33B�  B�  B�  CffCffCffC� C	��C� CffC� C��C� C� C� C� C� C� C� C!� C#� C%� C'��C)ffC+� C-� C/ffC1ffC3ffC5� C7� C9� C;��C=� C?� CA��CC� CE� CG��CI� CKffCM� CO� CQ� CS� CU� CW��CY� C[� C]��C_� CaffCc� Ce� CgffCi� Ck� Cm� CoffCq� Cs��Cu� Cw� CyffC{� C}��C��C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C���C�� C��3C�� C�� C�� C�� C���C���C���C�� C�� C��3C��3C��3C��3C�� C���C���C���C���C���C�� C���C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C��3C�� C���C�� C�� C�� C�� C��3C��3C�� C���C�� C�� C��3C��3C�� C�� C�� C���Cų3C�� C�� C�� C�� Cʳ3C�� C���C�� C�� C���C�� C�� C�� C�� C���C���C���C���C���C���C���C�� Cܳ3Cݳ3C޳3C�� C�� C�� C�� C�� C�� C�� C���C���C���C���C���C���C���C���C���C�� C�� C�� C�3C�� C���C�� C�� C�� C��3C�� C�� C�� C��3C�� C���C�� D ` D � DY�D� DffD� D` D� D` D� DffD�fD` D� D` D�fD` DٚD	` D	� D
` D
� DffD� D` D�fD` D� D` D� DY�DٚDY�DٚDY�DٚD` D� D` D� D` D� D` D�fD` DٚDY�D� DffD�fD` DٚD` D�fD` D� D` D� D` D� D` D� D` D� D ` D � D!ffD!�fD"` D"ٚD#` D#� D$` D$ٚD%Y�D%� D&Y�D&ٚD'` D'�fD(` D(� D)` D)� D*` D*� D+` D+ٚD,` D,� D-Y�D-� D.` D.� D/` D/�fD0` D0ٚD1Y�D1� D2` D2ٚD3` D3�fD4` D4� D5` D5� D6` D6� D7Y�D7� D8` D8� D9ffD9� D:Y�D:ٚD;` D;�fD<` D<ٚD=` D=� D>` D>� D?Y�D?� D@ffD@� DA` DA�fDB` DB� DC` DC�fDDffDD�fDEffDE� DFffDF�fDG` DG� DH` DH� DI` DI� DJffDJ� DK` DK�fDL` DL� DMY�DM� DN` DN� DO` DOٚDPY�DPٚDQ` DQ� DRffDR� DS` DS� DT` DT� DU` DU� DV` DV� DWY�DWٚDXY�DX� DY` DY� DZ` DZٚD[` D[� D\` D\ٚD]` D]� D^` D^� D_` D_� D`` D`� Da` Da� Db` Db� Dc` Dc� Dd` Dd� DeffDe� Df` Df� Dg` DgٚDh` Dh�fDi` Di� Dj` Dj� Dk` Dk� DlY�Dl� Dm` Dm� Dn` Dn� Do` Do� Dp` Dp� Dq` Dq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du�fDvY�DyFfD��fD�&fD�i�D���D���D�#3D�VfD���D��fD�3D�S3DǬ�D��3D�	�D��fDਗ਼D�ɚD�	�D�I�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�33A�33A�+A�"�A�"�A�$�A�$�A�oA���AżjAŲ-Aś�Aŗ�Aŏ\AŇ+A�x�A�hsA�\)A�S�A�=qA�$�A��A��A��A�{A�bA�A��A��#AļjAĩ�Aě�AēuA�|�A�  A�bA�9XA�z�A���A�  A�A�hsA�$�A��#A�z�A�I�A��wA��A���A���A�v�A�oA�l�A�5?A��`A�/A��/A�I�A���A���A���A�z�A�bNA��A��A�M�A��wA�O�A�=qA�ZA�A�A�A�?}A�v�A�
=A�dZA�hsA�1'A��A���A���A�z�A��A�=qA�VA�A�33A��A���A�5?A�n�A�I�A�S�AyƨAv�At�Ap��Am?}AjI�Ai��AiXAh9XAf��AcoA`I�A_ƨA^�HA[��AZ{AW|�AU�AO��AK;dAI\)AC�#A=��A;XA9�
A6��A3�7A3A2�A1��A0I�A.E�A,��A,z�A+�A+&�A+"�A*��A*=qA)��A)�A);dA)
=A'ƨA&M�A$I�A#�wA$ �A$�9A$��A$�uA%��A%&�A$��A$r�A${A#��A#��A#��A#+A"�!A"��A"��A!�A!�hA!dZA!%A�A��A|�AO�A�/Ar�A1'A;dAr�A�A�A�;A�A/A��A�mA�-A��A��A��A�7Ax�AS�AĜA �A��AA�FA��AhsA+A�/A9XA�A`BA�A��A$�A�#A��A�A��AE�A��AdZA7LA33A��A�FAC�A�/AQ�A�TA��A?}A�jAt�A
�A
��A
�+A
  A	��A	��A	XAȴA�A��A5?A{A|�A��A�DAffAQ�A��Ax�A+AZA��A�A �RA 9XA �@���@��w@���@�-@��@�"�@��@���@�l�@��@��@�=q@��@���@�A�@�+@�\@���@�@�1'@��@��
@�"�@�~�@�@�@�X@���@�9@�bN@�Q�@��@��y@�~�@��@��@�bN@��y@��@�?}@�Z@�33@�n�@��@�O�@�r�@߮@��y@�ff@���@�`B@���@�1'@ۮ@��@�v�@�-@���@�1'@׶F@��@�E�@��@ա�@Չ7@�/@��@Դ9@�1'@Ӿw@��@�5?@щ7@��`@�9X@��
@ύP@�
=@θR@�{@ͺ^@�`B@̓u@�1'@���@ˍP@���@��@�/@�r�@� �@��;@�\)@�ȴ@ũ�@��/@���@Ĭ@���@ă@î@�=q@���@�/@���@��u@�b@���@��y@�=q@��@��-@��7@�x�@��@�Q�@���@�t�@��!@��@���@�X@�/@���@��j@�I�@�t�@�+@��@�ȴ@��\@�V@��#@���@�O�@��@�z�@� �@��F@���@�V@�@�X@���@��u@�r�@�A�@��F@�t�@�K�@��@���@���@�/@���@��u@�I�@��m@���@�\)@�K�@��@�~�@��@���@�x�@�/@���@���@�b@��w@���@�t�@��@�ȴ@���@�ff@�{@��@���@��h@�G�@��@�%@��/@��@�9X@���@��w@���@�
=@���@���@�M�@��^@��h@��@�`B@�G�@��@�I�@�  @��;@��w@�|�@�;d@�o@���@��@��R@���@��\@��\@�ff@�{@�`B@���@�1'@� �@��@�  @��m@�t�@�
=@���@���@�n�@��@���@�?}@�V@���@��/@��j@�(�@�|�@�l�@�S�@�33@�@���@�ff@�M�@�E�@�5?@�{@��@���@���@�9X@�`B@��H@�z�@xr�@q�@i�7@_\)@W|�@L(�@D��@:�H@5p�@,�D@&��@ �`@�!@�y@�#@/@	hs111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�33A�33A�+A�"�A�"�A�$�A�$�A�oA���AżjAŲ-Aś�Aŗ�Aŏ\AŇ+A�x�A�hsA�\)A�S�A�=qA�$�A��A��A��A�{A�bA�A��A��#AļjAĩ�Aě�AēuA�|�A�  A�bA�9XA�z�A���A�  A�A�hsA�$�A��#A�z�A�I�A��wA��A���A���A�v�A�oA�l�A�5?A��`A�/A��/A�I�A���A���A���A�z�A�bNA��A��A�M�A��wA�O�A�=qA�ZA�A�A�A�?}A�v�A�
=A�dZA�hsA�1'A��A���A���A�z�A��A�=qA�VA�A�33A��A���A�5?A�n�A�I�A�S�AyƨAv�At�Ap��Am?}AjI�Ai��AiXAh9XAf��AcoA`I�A_ƨA^�HA[��AZ{AW|�AU�AO��AK;dAI\)AC�#A=��A;XA9�
A6��A3�7A3A2�A1��A0I�A.E�A,��A,z�A+�A+&�A+"�A*��A*=qA)��A)�A);dA)
=A'ƨA&M�A$I�A#�wA$ �A$�9A$��A$�uA%��A%&�A$��A$r�A${A#��A#��A#��A#+A"�!A"��A"��A!�A!�hA!dZA!%A�A��A|�AO�A�/Ar�A1'A;dAr�A�A�A�;A�A/A��A�mA�-A��A��A��A�7Ax�AS�AĜA �A��AA�FA��AhsA+A�/A9XA�A`BA�A��A$�A�#A��A�A��AE�A��AdZA7LA33A��A�FAC�A�/AQ�A�TA��A?}A�jAt�A
�A
��A
�+A
  A	��A	��A	XAȴA�A��A5?A{A|�A��A�DAffAQ�A��Ax�A+AZA��A�A �RA 9XA �@���@��w@���@�-@��@�"�@��@���@�l�@��@��@�=q@��@���@�A�@�+@�\@���@�@�1'@��@��
@�"�@�~�@�@�@�X@���@�9@�bN@�Q�@��@��y@�~�@��@��@�bN@��y@��@�?}@�Z@�33@�n�@��@�O�@�r�@߮@��y@�ff@���@�`B@���@�1'@ۮ@��@�v�@�-@���@�1'@׶F@��@�E�@��@ա�@Չ7@�/@��@Դ9@�1'@Ӿw@��@�5?@щ7@��`@�9X@��
@ύP@�
=@θR@�{@ͺ^@�`B@̓u@�1'@���@ˍP@���@��@�/@�r�@� �@��;@�\)@�ȴ@ũ�@��/@���@Ĭ@���@ă@î@�=q@���@�/@���@��u@�b@���@��y@�=q@��@��-@��7@�x�@��@�Q�@���@�t�@��!@��@���@�X@�/@���@��j@�I�@�t�@�+@��@�ȴ@��\@�V@��#@���@�O�@��@�z�@� �@��F@���@�V@�@�X@���@��u@�r�@�A�@��F@�t�@�K�@��@���@���@�/@���@��u@�I�@��m@���@�\)@�K�@��@�~�@��@���@�x�@�/@���@���@�b@��w@���@�t�@��@�ȴ@���@�ff@�{@��@���@��h@�G�@��@�%@��/@��@�9X@���@��w@���@�
=@���@���@�M�@��^@��h@��@�`B@�G�@��@�I�@�  @��;@��w@�|�@�;d@�o@���@��@��R@���@��\@��\@�ff@�{@�`B@���@�1'@� �@��@�  @��m@�t�@�
=@���@���@�n�@��@���@�?}@�V@���@��/@��j@�(�@�|�@�l�@�S�@�33@�@���@�ff@�M�@�E�@�5?@�{@��@���@���@�9X@�`B@��H@�z�@xr�@q�@i�7@_\)@W|�@L(�@D��@:�H@5p�@,�D@&��@ �`@�!@�y@�#@/@	hs111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�5B	�5B	�;B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�ZB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B
  B
B
  B
B
B
H�B
�LB
��BD�Bs�B� B��B�}BBBɺB��B��B��B��B�B�+B�bB�\B�+B{�Bx�Bz�B��B��Bw�B�7B��B��B��B�B�FB�dBBƨB�jB�wB��BŢB�}BÖBȴBĜB�wB�FB�RBŢB�;B�)B��B�B�%BC�B�B
�fB
�FB
w�B
N�B

=B	�)B	B	�B	��B	�B	q�B	m�B	iyB	bNB	T�B	@�B	33B	,B	#�B	{B	1B��B�mB�BǮB�^B�B�3B�'B�B�B�!B�B��B��B��B��B�B�B�'B�LB��B��B��B�B��B	,B	33B	9XB	5?B	49B	C�B	aHB	z�B	�%B	�B	ŢB	��B	��B	��B	��B	��B	��B	�B	�/B	�/B	�`B	�yB	�B	�B	��B	��B	��B
B
+B
	7B
JB
DB

=B
PB
uB
�B
�B
�B
�B
 �B
$�B
+B
,B
,B
+B
,B
,B
+B
+B
-B
0!B
2-B
2-B
33B
2-B
2-B
1'B
0!B
1'B
1'B
0!B
0!B
.B
.B
.B
.B
/B
0!B
0!B
2-B
1'B
1'B
1'B
1'B
1'B
0!B
-B
+B
-B
-B
+B
(�B
'�B
(�B
(�B
(�B
'�B
&�B
$�B
"�B
!�B
!�B
 �B
 �B
 �B
 �B
!�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
VB

=B
	7B
+B
+B
	7B
%B
B
B
B
B
B
+B
+B
%B
+B
B
B
B
B
B
B
B
B
B
B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
  B
  B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
%B
%B
%B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
JB
JB
JB
JB
DB
JB
PB
VB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
\B
bB
hB
oB
oB
hB
oB
hB
oB
oB
oB
oB
oB
uB
uB
{B
{B
{B
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
$�B
)�B
33B
9XB
=qB
E�B
I�B
O�B
T�B
[#B
_;B
ffB
iyB
n�B
t�B
v�B
{�B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�5B	�5B	�;B	�5B	�5B	�5B	�;B	�BB	�;B	�;B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�TB	�`B	�yB	�B	�B	�B	�B	�B	��B	��B	��B
  B
B
B
B
B

=B
Q�B
�}BBL�By�B�B��BBŢBǮB��B�fB�;B�#BƨB�bB��B�B��B�{B�%B�%B�B�?B��B�B�PB��B��B�B�3B�qBB��B��BBȴB�
B��BƨB��B�B��B��B��B�dB��B�B�sB�fB��B��B^5B5?BPB
�ZB
��B
�1B
33B	�B	�
B	ĜB	�B	�oB	v�B	r�B	t�B	p�B	l�B	O�B	9XB	7LB	9XB	#�B	�B	uB	PB�B�/B�TB��BĜB��BÖB�}B�9B�-B�3B�-B�B�!B�3B�9B�FB�RBĜB��B�
B�B��B	1'B	=qB	E�B	@�B	6FB	@�B	_;B	z�B	�B	��B	ɺB	��B	��B	��B	��B	��B	�
B	�5B	�BB	�5B	�mB	�B	�B	�B	��B	��B	��B
1B
	7B
PB
bB
\B
hB
oB
�B
�B
�B
 �B
"�B
$�B
)�B
,B
-B
,B
,B
-B
-B
.B
0!B
1'B
2-B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
33B
33B
33B
33B
0!B
1'B
2-B
33B
33B
33B
5?B
33B
33B
6FB
7LB
49B
33B
1'B
.B
/B
1'B
0!B
1'B
+B
+B
,B
,B
(�B
)�B
'�B
'�B
(�B
(�B
$�B
"�B
$�B
$�B
#�B
!�B
"�B
#�B
"�B
!�B
#�B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
VB
PB
1B
1B
JB
	7B
+B
+B
B
1B
B
DB
	7B
+B
	7B
1B
B
B
B
B
B
B
B
B
%B
B
B
B	��B
  B
B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
B	��B
B
  B
B
  B
  B
  B	��B
B
B
B
B
B
B
B
B
B
B
%B
B
B
%B
%B
B
+B
+B
+B
+B
%B
+B
1B
	7B
	7B
1B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
JB
DB
JB
JB
DB
PB
PB
PB
JB
PB
VB
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
\B
\B
bB
bB
\B
oB
uB
oB
oB
hB
uB
hB
uB
uB
uB
uB
{B
{B
uB
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
$�B
)�B
49B
:^B
>wB
E�B
J�B
P�B
VB
[#B
`BB
gmB
jB
n�B
t�B
v�B
{�B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<��
<ě�<�`B<u<���<���<T��<T��<#�
<T��<#�
<e`B<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<D��<#�
<#�
<#�
<49X<#�
<#�
<49X<T��<�o<e`B<49X<#�
<#�
<#�
<49X<�1<�`B=\)<���<�`B=��=8Q�=#�
=e`B=#�
<�1<��
<ě�<�9X<u<#�
<#�
<#�
<e`B<�9X<u<#�
<#�
<�1<e`B<���<���=�P<�h<�1=#�
=t�<�o<u<�9X<�t�<#�
<#�
<#�
<T��<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250302012011312503020120113125030  AO  ARGQ                                                                        20111205113449  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113449  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125030  IP                  G�O�G�O�G�O�                