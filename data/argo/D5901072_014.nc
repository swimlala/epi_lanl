CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:43Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143622  20190522121828  1728_5048_014                   2C  D   APEX                            2142                            040306                          846 @�M�=�1   @�M����@4��x����cy\(�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @&ff@y��@�  A   A   A@  A`  A���A���A�  A�  A�  A�  A�  A�  A�33B��B  B��B��B(  B0  B8ffB@  BG��BO��BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B���B�33B�33B�33B�  B�  B�  B�  B�  B�  B�33B�  B���B�  C �C  C�fC�fC�fC
  C  C  C�fC  C�C�C�C  C  C  C�fC!�fC#�fC&  C(�C*�C,  C.  C0�C2�C4  C5�fC8  C:  C<  C>  C@�CB�CD�CF�CH  CI�fCK�fCN  CP  CR  CT  CV  CX�CZ�C\�C^  C`  Cb  Cc�fCe�fCg�fCj  Cl  Cn  Cp  Cr  Ct  Cu�fCw�fCz  C|  C}�fC�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C��3C��3C��3C�  C�  C��3C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C��3C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��C�  C��3C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C��C�  C�  C��C�  C��3C�  C��C�  C�  C��3C�  C��C�  D   D � D  D�fDfD� D  D� D  D� D��D� D  Dy�D  D�fDfD� D	fD	� D
  D
�fD  D� D  Dy�D  D� D  D�fDfDy�D  D�fD  D� D  Dy�D  D� D  D�fDfD� D��Dy�D��Dy�D��Dy�D  D�fD  Dy�D��Dy�D��D� D  D� D  D� D  D� D   D � D!  D!� D!��D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D'��D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/� D0  D0� D0��D1� D2  D2y�D3  D3�fD4fD4� D4��D5� D6  D6y�D6��D7y�D7��D8� D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DLfDL� DL��DM� DNfDN�fDO  DO� DP  DPy�DP��DQ� DR  DR� DR��DS� DT  DT� DU  DUy�DU��DVy�DV��DW� DX  DX� DY  DY� DZ  DZ�fD[fD[� D\  D\� D\��D]� D^fD^� D_  D_� D`  D`�fDafDa� DbfDb�fDc  Dcy�Dc��Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy��D�3D�)�D�c3D���D�3D�&fD�y�D�ɚD���D�fD���D�� D��3D�#3D�I�D๚D��3D�#3D�S311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@`  @�33@�33A��A9��AY��A{33A���A���A���A���A���A���A���A�  B  BffB  B  B&ffB.ffB6��B>ffBF  BN  BVffB^ffBfffBn��BvffB~ffB�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�  B�  B�ffB�ffB�ffB�33B�33B�33B�33B�33B�33B�ffB�33B�  B�33B�ffC��C� C� C� C	��C��C��C� C��C�3C�3C�3C��C��C��C� C!� C#� C%��C'�3C)�3C+��C-��C/�3C1�3C3��C5� C7��C9��C;��C=��C?�3CA�3CC�3CE�3CG��CI� CK� CM��CO��CQ��CS��CU��CW�3CY�3C[�3C]��C_��Ca��Cc� Ce� Cg� Ci��Ck��Cm��Co��Cq��Cs��Cu� Cw� Cy��C{��C}� C��C�ٚC���C���C���C���C���C�� C���C���C���C���C���C���C�ٚC�ٚC���C���C���C���C���C���C���C�� C���C�ٚC�ٚC���C���C���C���C�� C�� C�� C���C���C�� C���C���C���C�� C���C���C�� C���C�ٚC���C�� C�� C���C���C�ٚC���C�� C���C���C���C���C���C�ٚC���C���C���C���C���C�ٚC���C�� C���C���C�ٚC���C�� C���C�ٚC���C�� C���C���C�� C���C���C���C���C���C�ٚC���C���C���C���C���C���C�ٚC���C���C���C���C���C���C�ٚC���C���C���C�� C���C���C���C�� C���C���C���C�ٚC���C���C�ٚC���C���C�ٚC���C�� C���C�ٚC���C���C�� C���C�ٚC���C���D ffD �fDl�D��DffD�fDffD�fDffD� DffD�fD` D�fDl�D��DffD��D	ffD	�fD
l�D
�fDffD�fD` D�fDffD�fDl�D��D` D�fDl�D�fDffD�fD` D�fDffD�fDl�D��DffD� D` D� D` D� D` D�fDl�D�fD` D� D` D� DffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!� D"ffD"�fD#ffD#��D$ffD$�fD%ffD%�fD&ffD&�fD'ffD'� D(ffD(��D)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-��D.ffD.�fD/ffD/�fD0ffD0� D1ffD1�fD2` D2�fD3l�D3��D4ffD4� D5ffD5�fD6` D6� D7` D7� D8ffD8�fD9ffD9�fD:ffD:�fD;` D;�fD<ffD<�fD=l�D=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC��DDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJl�DJ�fDKffDK��DLffDL� DMffDM��DNl�DN�fDOffDO�fDP` DP� DQffDQ�fDRffDR� DSffDS�fDTffDT�fDU` DU� DV` DV� DWffDW�fDXffDX�fDYffDY�fDZl�DZ��D[ffD[�fD\ffD\� D]ffD]��D^ffD^�fD_ffD_�fD`l�D`��DaffDa��Dbl�Db�fDc` Dc� Dd` Dd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwffDy� D��fD��D�VfD�� D��fD��D�l�D���D�� D�	�D�|�Dǳ3D��fD�fD�<�D��D��fD�fD�Ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aȉ7Aȉ7Aȉ7Aȉ7Aȉ7AȋDAȋDAȋDAȋDAȋDAȋDAȋDAȋDAȉ7Aȉ7Aȇ+A�l�AǕ�A��A��A�A�K�A�^5A�A�AÍPA�t�A�ĜA���A�t�A���A��^A�bNA�1A��RA��A�E�A��7A�A��9A�Q�A��HA���A���A��A�A��/A�ȴA��uA�
=A��-A�5?A��A���A�C�A��`A�~�A��A���A�oA���A���A�$�A�A��TA��A�1'A��RA���A���A�bA��A�1A��A�&�A��A�33A�G�A�M�A�O�A���A��A��^A�(�A��mA���A�33A�G�A��9A�1'A�/A��9A���A��A��!A�1A� �A���A�jA���A�`BA��uA�1'A���A�;dA��A��HA�bA�r�A��+A��FA��HA�^5A�5?A���A�"�A��A��FAK�A}33A{�#Azv�Ax�/AwC�Au��At-As"�Ap��AodZAn�uAmt�Al �AjAg+Ae�AbZA`�/A`n�A_�^A^r�A]VA\I�AZ�uAX�AX  AV�jAR��AO�AN5?AMC�AL�yAL��AL-AK�AK�^AK\)AJr�AHI�AF�jAF-ADr�AAt�A>�A>5?A=�hA<��A<�DA;A9oA6��A5�A4��A3�FA29XA1�A0��A.jA-�A+��A+O�A*��A)��A(��A'\)A&�`A&�A&{A$�`A$Q�A#�7A!�;A��At�A�AZAȴA1A��A�A�A�DAO�A�TAjAZAM�AjAK�A�uA=qAA�A�
Al�A
9XA�`AZA��A�A��A�A��A�wA33An�AXA��A�/AVAt�A?}A ��A 1'@�%@�@��@���@��
@�?}@�{@���@�t�@�G�@�~�@���@�@���@�+@�$�@�@�@�/@�A�@�t�@��@◍@���@��`@߅@�=q@�p�@��@�Z@�K�@ڸR@�M�@�{@�@ف@�X@��@؛�@�j@�(�@׮@���@�x�@���@ԋD@�9X@��;@�S�@��@Ѳ-@ϥ�@Ώ\@Ͳ-@�bN@��@�33@�ff@�%@�1@��
@��
@��
@Ǿw@�|�@�\)@�\)@�S�@�+@Ɵ�@�$�@���@�7L@Ĭ@��
@�"�@¸R@\@�-@��-@�j@�ff@�p�@��@��D@�b@��w@�o@���@�n�@�$�@���@�`B@�x�@��@��@�-@��@�
=@���@�=q@��T@��7@��h@���@��@��^@�@�@���@���@��@��^@��@�~�@�J@��@�ƨ@��R@�-@�-@���@��-@�G�@���@���@���@�Ĝ@��9@��j@��u@��@�z�@�r�@�1'@��;@���@�K�@��P@��w@�A�@��D@�  @��P@�S�@��@��\@���@��@�ƨ@�l�@��@���@��@��7@���@��@�9X@��@�1@�(�@� �@��@�o@���@���@�V@���@���@�V@��j@���@��D@�I�@�  @��F@�|�@�K�@��@���@�ȴ@��!@���@�n�@�-@���@��7@���@��@���@�33@���@��H@��R@�n�@�E�@��@���@�G�@��/@�bN@��;@���@�\)@�o@�ȴ@���@�^5@�$�@�{@�{@�@��T@���@��^@���@�hs@��@��@��/@���@��9@�r�@�(�@��@���@���@�K�@�;d@�"�@���@���@���@�^5@�=q@�J@��T@��^@��@�X@�X@�X@�G�@�7L@�V@��@��@��`@��@�(�@�b@�b@��@�b@��w@��P@�;d@���@�ff@���@��h@�X@��@�b@��@���@�-@r=q@a��@W�@R=q@L��@D��@<Z@0b@)�@!��@@�\@1'@@��@?}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aȉ7Aȉ7Aȉ7Aȉ7Aȉ7AȋDAȋDAȋDAȋDAȋDAȋDAȋDAȋDAȉ7Aȉ7Aȇ+A�l�AǕ�A��A��A�A�K�A�^5A�A�AÍPA�t�A�ĜA���A�t�A���A��^A�bNA�1A��RA��A�E�A��7A�A��9A�Q�A��HA���A���A��A�A��/A�ȴA��uA�
=A��-A�5?A��A���A�C�A��`A�~�A��A���A�oA���A���A�$�A�A��TA��A�1'A��RA���A���A�bA��A�1A��A�&�A��A�33A�G�A�M�A�O�A���A��A��^A�(�A��mA���A�33A�G�A��9A�1'A�/A��9A���A��A��!A�1A� �A���A�jA���A�`BA��uA�1'A���A�;dA��A��HA�bA�r�A��+A��FA��HA�^5A�5?A���A�"�A��A��FAK�A}33A{�#Azv�Ax�/AwC�Au��At-As"�Ap��AodZAn�uAmt�Al �AjAg+Ae�AbZA`�/A`n�A_�^A^r�A]VA\I�AZ�uAX�AX  AV�jAR��AO�AN5?AMC�AL�yAL��AL-AK�AK�^AK\)AJr�AHI�AF�jAF-ADr�AAt�A>�A>5?A=�hA<��A<�DA;A9oA6��A5�A4��A3�FA29XA1�A0��A.jA-�A+��A+O�A*��A)��A(��A'\)A&�`A&�A&{A$�`A$Q�A#�7A!�;A��At�A�AZAȴA1A��A�A�A�DAO�A�TAjAZAM�AjAK�A�uA=qAA�A�
Al�A
9XA�`AZA��A�A��A�A��A�wA33An�AXA��A�/AVAt�A?}A ��A 1'@�%@�@��@���@��
@�?}@�{@���@�t�@�G�@�~�@���@�@���@�+@�$�@�@�@�/@�A�@�t�@��@◍@���@��`@߅@�=q@�p�@��@�Z@�K�@ڸR@�M�@�{@�@ف@�X@��@؛�@�j@�(�@׮@���@�x�@���@ԋD@�9X@��;@�S�@��@Ѳ-@ϥ�@Ώ\@Ͳ-@�bN@��@�33@�ff@�%@�1@��
@��
@��
@Ǿw@�|�@�\)@�\)@�S�@�+@Ɵ�@�$�@���@�7L@Ĭ@��
@�"�@¸R@\@�-@��-@�j@�ff@�p�@��@��D@�b@��w@�o@���@�n�@�$�@���@�`B@�x�@��@��@�-@��@�
=@���@�=q@��T@��7@��h@���@��@��^@�@�@���@���@��@��^@��@�~�@�J@��@�ƨ@��R@�-@�-@���@��-@�G�@���@���@���@�Ĝ@��9@��j@��u@��@�z�@�r�@�1'@��;@���@�K�@��P@��w@�A�@��D@�  @��P@�S�@��@��\@���@��@�ƨ@�l�@��@���@��@��7@���@��@�9X@��@�1@�(�@� �@��@�o@���@���@�V@���@���@�V@��j@���@��D@�I�@�  @��F@�|�@�K�@��@���@�ȴ@��!@���@�n�@�-@���@��7@���@��@���@�33@���@��H@��R@�n�@�E�@��@���@�G�@��/@�bN@��;@���@�\)@�o@�ȴ@���@�^5@�$�@�{@�{@�@��T@���@��^@���@�hs@��@��@��/@���@��9@�r�@�(�@��@���@���@�K�@�;d@�"�@���@���@���@�^5@�=q@�J@��T@��^@��@�X@�X@�X@�G�@�7L@�V@��@��@��`@��@�(�@�b@�b@��@�b@��w@��P@�;d@���@�ff@���@��h@�X@��@�b@��@���@�-@r=q@a��@W�@R=q@L��@D��@<Z@0b@)�@!��@@�\@1'@@��@?}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�TB�TB�TB�TB�TB�TB�TB�ZB�ZB�ZB�TB�ZB�ZB�ZB�`B�B/B�-B�qB�qB�wB��BB�dB�3B��B��B��B��B��B�B�wB��BƨB��B�B�)B�#B�NB�NB�BB�5B�NB�NB�TB�TB�`B�B�B�B�sB�fB�fB�yB�B�yB�mB�NB�B�B�NB�mB�fB�`B�ZB�TB�BB�)B�)B��BĜB�LB�B��B��B�JB{�Bn�B^5BQ�BG�BA�B8RB33B-B%�BuBB��B�yB�BŢB�?B�B��B�1B]/BB�B49B.B,B&�B�BuBB
�B
��B
�B
��B
�hB
�B
z�B
u�B
l�B
R�B
?}B
/B
�B
\B
%B	��B	�B	�NB	�B	��B	ƨB	�XB	�XB	�FB	�B	��B	�PB	t�B	ffB	VB	I�B	E�B	?}B	9XB	/B	'�B	�B	JB	%B��B�BB��BŢBÖBȴB��B��B��B��B��B��BɺB�wB�XB��B��B��B��B��B��B��B��B�hB�=B�JB�oB��B�uB�\B�7B�DB�JB�JB�VB�bB�oB�{B��B��B��B��B��B��B��B��B�uB�{B�uB�oB�{B�uB��B��B��B��B��B��B�oB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�RB�wB��B��BƨB��B��B��B�
B�B�#B�)B�/B�BB�NB�TB�fB�B�B��B��B��B��B��B��B	%B	hB	�B	�B	&�B	)�B	-B	1'B	8RB	=qB	=qB	=qB	=qB	<jB	<jB	<jB	<jB	=qB	A�B	D�B	E�B	E�B	D�B	C�B	B�B	@�B	A�B	A�B	@�B	B�B	D�B	C�B	C�B	C�B	D�B	E�B	F�B	H�B	J�B	M�B	N�B	O�B	R�B	W
B	ZB	`BB	cTB	dZB	bNB	aHB	aHB	bNB	e`B	jB	l�B	n�B	p�B	q�B	r�B	r�B	s�B	u�B	y�B	}�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�1B	�DB	�JB	�JB	�JB	�DB	�=B	�=B	�7B	�7B	�7B	�7B	�=B	�JB	�VB	�uB	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�!B	�B	�B	�B	�B	�'B	�3B	�?B	�?B	�?B	�FB	�LB	�RB	�XB	�^B	�dB	�dB	�jB	�jB	�qB	�qB	�qB	�wB	�}B	�}B	B	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�ZB	�ZB	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
+B
\B
�B
.B
5?B
;dB
>wB
F�B
M�B
W
B
]/B
^5B
cTB
e`B
iyB
m�B
q�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�TB�TB�TB�TB�TB�TB�TB�ZB�ZB�ZB�TB�ZB�ZB�ZB�fB�B7LB�FB�}B��BŢB��B��BȴB��BÖB�B��B��B�B�dBÖBǮB��B�)B�5B�NB�BB�mB�sB�fB�B�B�mB�`B�`B�yB�B�B�B�B�B�B�B�B�B�B�yB�HB�ZB�yB�yB�sB�yB�B�yB�`B�B�B�;B��BĜB�FB��B��B��B�7Bz�BgmBYBM�BH�B<jB9XB5?B2-B�BVB1B��B�NB��B�qB�XB�!B��Bo�BL�B:^B9XB2-B.B%�B�B�B	7B
�`B
�jB
��B
��B
�PB
�B
�B
�B
e`B
O�B
>wB
,B
�B
hB
	7B	��B	�B	�NB	�B	�B	��B	��B	��B	�dB	�!B	��B	�B	w�B	_;B	M�B	L�B	H�B	C�B	7LB	5?B	&�B	uB	uB	oB�B�)B��BƨB��B��B�B�B�B�/B�;B��BƨBǮB�jB�B��B�B�B�B�B��B��B�oB�uB��B��B��B��B��B�{B�uB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�!B�-B�?B�9B�-B�B�B�3B�?B�!B�'B�FB�3B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�RB�jB�wB��BĜB��B��B��B�B�
B�B�)B�)B�;B�BB�NB�fB�fB�B��B��B��B��B��B��B	B	PB	�B	�B	�B	&�B	-B	-B	6FB	;dB	>wB	=qB	=qB	>wB	=qB	<jB	<jB	=qB	=qB	A�B	D�B	E�B	E�B	D�B	F�B	B�B	B�B	A�B	A�B	@�B	B�B	J�B	F�B	E�B	E�B	F�B	G�B	H�B	I�B	L�B	N�B	P�B	O�B	R�B	W
B	YB	`BB	jB	gmB	dZB	cTB	bNB	cTB	e`B	jB	l�B	n�B	p�B	q�B	s�B	r�B	s�B	t�B	x�B	}�B	�B	�B	�=B	�%B	�B	�B	�+B	�%B	�7B	�JB	�JB	�PB	�JB	�DB	�=B	�=B	�7B	�7B	�7B	�7B	�=B	�JB	�\B	�oB	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�3B	�-B	�!B	�!B	�!B	�!B	�3B	�?B	�FB	�FB	�FB	�FB	�RB	�XB	�XB	�dB	�dB	�jB	�jB	�jB	�wB	�wB	�wB	�wB	�}B	��B	ĜB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�#B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�/B	�;B	�;B	�;B	�BB	�HB	�HB	�NB	�TB	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
+B
\B
�B
.B
5?B
;dB
>wB
F�B
N�B
W
B
]/B
_;B
cTB
ffB
iyB
m�B
q�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<T��<���<���<T��<T��<#�
<49X<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<���<e`B<e`B<T��<#�
<#�
<#�
<T��<T��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<�o<���<49X<#�
<#�
<D��<�t�<���<�t�<#�
<#�
<#�
<#�
<#�
<#�
<49X<�o<ě�<��
<T��<49X<#�
<#�
<#�
<e`B<��
<�t�<�o<u<e`B<#�
<49X<D��<D��<49X<#�
<#�
<u<#�
<#�
<#�
<D��<u<���<e`B<�o<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<#�
<T��<ě�<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<e`B<�t�<T��<#�
<#�
<#�
<#�
<D��<e`B<T��<#�
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
<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<49X<D��<�o<e`B<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451572012011014515720120110145157  AO  ARGQ                                                                        20111130143622  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143622  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145157  IP                  G�O�G�O�G�O�                