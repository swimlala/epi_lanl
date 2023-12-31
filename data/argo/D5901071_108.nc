CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:21Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               lA   AO  20111130141622  20190522121826  1727_5046_108                   2C  D   APEX                            2143                            040306                          846 @ԡI�|`1   @ԡKW?�@6Z�1'�c-V1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  @���A!��A@  A^ffA~ffA�  A�  A�  A�  A�  A�33A�  B ffB  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bo��Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B���C   C  C  C�fC  C
�C  C  C�C  C  C  C�fC  C  C�fC   C!�fC#�fC&  C(  C*  C,  C-�fC0  C2  C4  C6  C7�fC:  C<  C>  C@�CB  CD  CF  CG�fCJ  CL  CN  CO�fCR  CT  CU�fCW�fCZ  C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl  Cn  Co�fCr  Ct�Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C��3C��3C��C��C��C�  C��3C��3C��3C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��3C�  C��C�  C�  C�  C�  C�  C��C��C�  C��3C��3C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C��C��C�  C�  C��3C�  C�  C��3C��3D y�D  D� D  D� D  D�fD  Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	� D
fD
� D  D�fD  D� DfD� D��D� D  Dy�D  D� D��Dy�D��Dy�D  D� D  D� D  Dy�D��Dy�D��D� DfD�fD  D� D  D� D  Dy�D��Dy�D��D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'y�D(  D(�fD)  D)� D)��D*� D+fD+� D,  D,y�D-  D-�fD.  D.� D/  D/y�D0  D0� D1  D1� D1��D2� D3  D3�fD4  D4� D5  D5� D6fD6� D6��D7� D8  D8y�D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA�fDB  DBy�DC  DC� DC��DD� DE  DE� DF  DF�fDG  DGy�DH  DH� DI  DIy�DJ  DJ� DJ��DK� DL  DL� DM  DMy�DN  DN� DO  DO�fDP  DPy�DQ  DQ� DR  DRy�DS  DS�fDT  DT� DU  DU� DV  DV� DW  DWy�DW��DX� DY  DY� DZfDZ� D[  D[� D\  D\� D]  D]� D]��D^� D_fD_� D`  D`� Da  Da� DbfDb�fDc  Dc� Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� Dy�fD�33D�c3D���D�� D��D�p D��fD��fD�&fD�VfD��fD�� D�3D�S3Dڰ D��fD� D�Y�D�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�33@�33A   A#33AA��A`  A�  A���A���A���A���A���A�  A���B ��BffBffBffB ffB(ffB0ffB8ffB@ffBH��BPffBXffB`ffBhffBp  BxffB�33B�ffB�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�ffB�33B�  C �C�C�C  C�C
33C�C�C33C�C�C�C  C�C�C  C �C"  C$  C&�C(�C*�C,�C.  C0�C2�C4�C6�C8  C:�C<�C>�C@33CB�CD�CF�CH  CJ�CL�CN�CP  CR�CT�CV  CX  CZ�C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl�Cn�Cp  Cr�Ct33Cv�Cx  Cz�C|�C~�C��C��C��C��C�  C�  C��C��C��C��C�  C�  C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C�  C��C��C��C�  C�  C�  C�  C�  C��C��C�  C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C�  C�  C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C�  D   D � DfD�fDfD�fDfD��DfD� D  D� D  D� D  D� D  D� D	  D	�fD
�D
�fDfD��DfD�fD�D�fD  D�fDfD� DfD�fD  D� D  D� DfD�fDfD�fDfD� D  D� D  D�fD�D��DfD�fDfD�fDfD� D  D� D  D�fDfD�fDfD�fD fD �fD!fD!� D"fD"�fD#fD#�fD$fD$�fD%fD%� D&fD&�fD'fD'� D(fD(��D)fD)�fD*  D*�fD+�D+�fD,fD,� D-fD-��D.fD.�fD/fD/� D0fD0�fD1fD1�fD2  D2�fD3fD3��D4fD4�fD5fD5�fD6�D6�fD7  D7�fD8fD8� D9fD9��D:fD:�fD;fD;�fD<fD<�fD=fD=�fD>  D>�fD?fD?�fD@fD@�fDAfDA��DBfDB� DCfDC�fDD  DD�fDEfDE�fDFfDF��DGfDG� DHfDH�fDIfDI� DJfDJ�fDK  DK�fDLfDL�fDMfDM� DNfDN�fDOfDO��DPfDP� DQfDQ�fDRfDR� DSfDS��DTfDT�fDUfDU�fDVfDV�fDWfDW� DX  DX�fDYfDY�fDZ�DZ�fD[fD[�fD\fD\�fD]fD]�fD^  D^�fD_�D_�fD`fD`�fDafDa�fDb�Db��DcfDc�fDdfDd�fDefDe�fDf�Df�fDgfDg�fDhfDh�fDifDi�fDj�Dj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDt�Dt�fDy��D�6fD�ffD���D��3D�  D�s3D���D�ɚD�)�D�Y�D���D��3D�fD�VfDڳ3D��D�3D�\�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�?}A���Aϙ�A�7LA�&�A��A�oA�JA�A���A���A��A��A��TA���A���Aκ^AΩ�A�~�A�1A͟�A�\)A�(�A��A̅A�"�A�z�A�jA�p�AƓuA�ȴA�K�A×�A�A���A���A���A���A��\A���A�r�A���A���A���A��A�O�A�VA�9XA�^5A��;A�S�A���A��A�ĜA���A��A�$�A���A�oA��A��A�A�A�n�A��;A�/A��hA�Q�A�bA���A��yA��jA���A�r�A�5?A���A��^A�K�A��A�;dA�\)A�&�A�S�A���A�`BA���A���A�XA�A���A�hsA�S�A�ĜA�bNA�/A�VA� �A���A��PA��HA���A�"�A�K�A���A��A�5?A���A�K�A��A�O�A�p�A�A�x�A��A~�+A{��A{�Az�RAx{AvA�As�wAp^5Ak�wAjffAh�/Ag�Ag33Ad��Ab��Aa�A`ĜA_�TA_�A_VA^1A]A\�A\��A\  AYO�AU�FAQ��AP�AO�AN  AL��AL  AJjAHVAF  AD�uACoA?�7A<��A;�wA8�`A77LA6��A6M�A5�A3A2v�A1l�A17LA0�DA/�A/�A.��A-�PA,Q�A+t�A+�A+33A)x�A(��A(^5A'C�A%hsA$E�A#�A"bNA!G�A�A�#A�A33A��AAĜA�^A
=A"�A��A`BA�AO�A/AA�jAbNAbAffA��AĜA�A�+AM�A�A�A+A��AJA
n�A	�FA�A�-AbAO�A%AȴAv�AbA�@�t�@���@�X@�/@��@��@���@�t�@�|�@��^@�r�@�w@�@�dZ@�o@�5?@��@� �@ꗍ@��@�Q�@�n�@�@�@�p�@�`B@�O�@���@�9@�bN@��@�t�@�n�@�G�@�Ĝ@ڰ!@��/@��H@��@�X@�r�@�|�@�5?@���@��@�1@��T@�/@˥�@�-@�`B@��/@��@�;d@�ƨ@��@��@��/@��`@��/@�1@�J@���@���@�j@��@��@�/@��m@���@�hs@�b@�
=@�5?@�@���@���@��@�X@�%@�bN@�K�@��R@��#@�?}@��/@���@�E�@�@��-@�O�@�hs@�x�@�7L@���@��@�A�@�I�@�A�@�A�@��@���@��@�%@��D@�1'@�+@�^5@��/@�Q�@�  @�|�@�~�@�Z@�X@�X@�|�@�bN@�ȴ@���@���@���@��@�I�@�@���@�G�@�G�@�&�@�&�@��@��/@��@���@���@���@�^5@�=q@��T@��@��@��7@�`B@��@���@���@��j@���@�j@�I�@�b@��@��@��m@�ƨ@��@�t�@�l�@�K�@�+@��@��+@�-@�J@���@��@��T@��^@��h@�/@�Ĝ@��@��@��;@��P@�o@���@�V@�5?@�@�@�@��^@���@��7@�`B@�&�@���@��@��u@�j@�(�@��F@�@�v�@�{@���@��7@�G�@�7L@�G�@�G�@�`B@�hs@�`B@�hs@�`B@�X@�X@�O�@�G�@���@��j@��j@��j@��9@���@���@���@���@�z�@� �@��F@��@�"�@�@��y@���@�v�@���@�?}@� �@���@���@�S�@��@��R@��\@�M�@�p�@�A�@��
@�ƨ@�@�-@�J@��@��D@��
@�o@��!@���@���@��@��@��y@��@��H@��R@���@�5?@��#@��^@��^@���@���@�@{"�@nV@g;d@\9X@W�P@O�;@EV@;��@49X@.�R@'�@"-@$�@�@�y@X@$�@�9@?}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�?}A���Aϙ�A�7LA�&�A��A�oA�JA�A���A���A��A��A��TA���A���Aκ^AΩ�A�~�A�1A͟�A�\)A�(�A��A̅A�"�A�z�A�jA�p�AƓuA�ȴA�K�A×�A�A���A���A���A���A��\A���A�r�A���A���A���A��A�O�A�VA�9XA�^5A��;A�S�A���A��A�ĜA���A��A�$�A���A�oA��A��A�A�A�n�A��;A�/A��hA�Q�A�bA���A��yA��jA���A�r�A�5?A���A��^A�K�A��A�;dA�\)A�&�A�S�A���A�`BA���A���A�XA�A���A�hsA�S�A�ĜA�bNA�/A�VA� �A���A��PA��HA���A�"�A�K�A���A��A�5?A���A�K�A��A�O�A�p�A�A�x�A��A~�+A{��A{�Az�RAx{AvA�As�wAp^5Ak�wAjffAh�/Ag�Ag33Ad��Ab��Aa�A`ĜA_�TA_�A_VA^1A]A\�A\��A\  AYO�AU�FAQ��AP�AO�AN  AL��AL  AJjAHVAF  AD�uACoA?�7A<��A;�wA8�`A77LA6��A6M�A5�A3A2v�A1l�A17LA0�DA/�A/�A.��A-�PA,Q�A+t�A+�A+33A)x�A(��A(^5A'C�A%hsA$E�A#�A"bNA!G�A�A�#A�A33A��AAĜA�^A
=A"�A��A`BA�AO�A/AA�jAbNAbAffA��AĜA�A�+AM�A�A�A+A��AJA
n�A	�FA�A�-AbAO�A%AȴAv�AbA�@�t�@���@�X@�/@��@��@���@�t�@�|�@��^@�r�@�w@�@�dZ@�o@�5?@��@� �@ꗍ@��@�Q�@�n�@�@�@�p�@�`B@�O�@���@�9@�bN@��@�t�@�n�@�G�@�Ĝ@ڰ!@��/@��H@��@�X@�r�@�|�@�5?@���@��@�1@��T@�/@˥�@�-@�`B@��/@��@�;d@�ƨ@��@��@��/@��`@��/@�1@�J@���@���@�j@��@��@�/@��m@���@�hs@�b@�
=@�5?@�@���@���@��@�X@�%@�bN@�K�@��R@��#@�?}@��/@���@�E�@�@��-@�O�@�hs@�x�@�7L@���@��@�A�@�I�@�A�@�A�@��@���@��@�%@��D@�1'@�+@�^5@��/@�Q�@�  @�|�@�~�@�Z@�X@�X@�|�@�bN@�ȴ@���@���@���@��@�I�@�@���@�G�@�G�@�&�@�&�@��@��/@��@���@���@���@�^5@�=q@��T@��@��@��7@�`B@��@���@���@��j@���@�j@�I�@�b@��@��@��m@�ƨ@��@�t�@�l�@�K�@�+@��@��+@�-@�J@���@��@��T@��^@��h@�/@�Ĝ@��@��@��;@��P@�o@���@�V@�5?@�@�@�@��^@���@��7@�`B@�&�@���@��@��u@�j@�(�@��F@�@�v�@�{@���@��7@�G�@�7L@�G�@�G�@�`B@�hs@�`B@�hs@�`B@�X@�X@�O�@�G�@���@��j@��j@��j@��9@���@���@���@���@�z�@� �@��F@��@�"�@�@��y@���@�v�@���@�?}@� �@���@���@�S�@��@��R@��\@�M�@�p�@�A�@��
@�ƨ@�@�-@�J@��@��D@��
@�o@��!@���@���@��@��@��y@��@��H@��R@���@�5?@��#@��^@��^@���@���@�@{"�@nV@g;d@\9X@W�P@O�;@EV@;��@49X@.�R@'�@"-@$�@�@�y@X@$�@�9@?}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�FB�3B�-B�'B�'B�'B�'B�'B�'B�'B�'B�'B�!B�'B�!B�!B�B�B�B�B�B�B�B�'B�'B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�B��B��B�B�B��B��B�B:^B1'BL�B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�VB�=B�+B�By�Bw�Bu�Br�Bo�Bl�BhsBbNBZBS�BI�BD�B:^B0!B-B$�B�BB��B�fB��B�LB��Bt�BT�B33B.B&�B�BoB
��B
�;B
��B
�wB
��B
�7B
p�B
cTB
VB
B�B
33B
&�B
uB
%B	��B	�mB	�/B	�
B	��B	�'B	��B	�B	m�B	l�B	hsB	dZB	cTB	\)B	[#B	S�B	O�B	O�B	L�B	H�B	H�B	L�B	Q�B	R�B	M�B	YB	R�B	F�B	@�B	;dB	8RB	33B	.B	&�B	�B	hB	
=B��B��B�yB�TB�B��B��BƨB�qB�!B��B�B�B�B��B��B��B��B��B��B�uB��B��B��B��B��B�uB�bB�=B�B|�By�Bv�Bv�Bs�Bo�BiyBe`BaHB`BB`BB^5B]/B^5B]/B]/B\)B[#BZBW
BXBW
BW
BVBT�BS�BR�BP�BO�BL�BI�BH�BD�BA�B@�BA�BA�B@�B>wB<jB;dB8RB8RB:^B:^B;dB<jB?}BB�B@�B@�B>wB>wB>wB=qB<jB:^B8RB6FB5?B49B49B:^BD�BF�BI�BJ�BJ�BJ�BM�BM�BL�BK�BI�BF�BB�B=qB=qBB�BF�BP�BVBYBYBZBYBXBZB^5B_;BhsBl�Bl�Bl�Bk�BjBt�By�Bz�Bz�By�Bx�Bz�Bz�By�Bx�Bw�Bv�Bt�Bx�Bz�Bz�B|�B|�B~�B�B�B�DB��B��B��B��B��B��B��B�B�B�B�B�RB�^BB��B��B��B�B�B�#B�HB�`B�mB�yB�B�B�B��B�B�B�B�B��B��B��B��B	B	bB	�B	�B	%�B	0!B	,B	(�B	+B	-B	1'B	6FB	H�B	I�B	J�B	M�B	M�B	O�B	P�B	O�B	P�B	XB	YB	XB	]/B	dZB	ffB	ffB	ffB	gmB	gmB	iyB	jB	iyB	jB	jB	k�B	k�B	n�B	p�B	q�B	q�B	s�B	v�B	w�B	w�B	y�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�7B	�DB	�\B	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�9B	�RB	�^B	�dB	�dB	�qB	�qB	�qB	�wB	�wB	�wB	�}B	�wB	�}B	��B	B	B	B	B	B	B	B	B	B	ÖB	ŢB	ŢB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B
+B
PB
�B
#�B
/B
7LB
>wB
F�B
K�B
R�B
XB
]/B
`BB
e`B
l�B
o�B
u�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�RB�FB�9B�'B�'B�'B�'B�'B�'B�'B�'B�'B�!B�-B�'B�!B�!B�B�B�B�B�B�!B�FB�RB�jB��B�XB�FB�dB�B�B�B��B�B�B��B�B�-B�!B�'B�FB�LB�RB�RB��B��BK�B7LBT�B��B��B�B�B�B�B�B�B�B�B�!B�3B�?B�B��B�hB�VB�PB�7B{�By�Bx�Bu�Br�Bp�Bn�BjBaHB]/BN�BM�BB�B6FB6FB33B'�BJB%B��B�;B��B��B�\BffB7LB33B-B+B#�BDB
�B
�#B
�B
�^B
��B
{�B
o�B
dZB
N�B
?}B
7LB
�B
bB
	7B	�B	�`B	�yB	��B	ŢB	�-B	��B	w�B	v�B	o�B	m�B	r�B	iyB	bNB	YB	T�B	R�B	P�B	N�B	N�B	N�B	W
B	\)B	aHB	o�B	ffB	N�B	H�B	E�B	@�B	9XB	:^B	5?B	,B	�B	�B	{B	B��B�B�TB�B��B��BȴB�RB�B�B�9B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�hB�PB�DB�By�By�Bx�Bv�Br�Bm�BiyBl�BffBhsBgmB`BB^5B_;B_;B_;B_;BcTB`BBXBXBXBW
BVBVBVBS�BT�BT�BQ�BR�BP�BJ�BF�BC�BB�BB�BA�BC�BA�B=qB<jB;dB<jB=qBB�BJ�BM�BF�BB�B@�B?}B>wB?}B?}BB�B@�B:^B8RB:^B?}BF�BH�BI�BJ�BK�BK�BN�BN�BN�BN�BN�BM�BB�BD�BD�BH�BI�BR�BVB]/BYBZBYBXBZB^5B_;BhsBl�Bl�Bp�Bk�Bu�By�By�B|�Bz�By�B}�Bz�B}�Bz�Bx�By�Bz�Bt�Bx�Bz�Bz�B�B�B�B�B�7B�DB��B��B��B��B��B��B�B�B�B�!B�'B�RB�^BB��B��B��B�
B�B�)B�HB�`B�mB�yB�B�B��B��B��B��B��B�B��B��B	  B��B��B	bB	{B	oB	%�B	5?B	,B	)�B	+B	-B	2-B	6FB	I�B	K�B	J�B	M�B	M�B	P�B	Q�B	Q�B	P�B	YB	[#B	XB	^5B	e`B	ffB	ffB	ffB	hsB	gmB	jB	jB	iyB	k�B	jB	l�B	l�B	n�B	p�B	q�B	r�B	t�B	v�B	w�B	w�B	z�B	|�B	~�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�=B	�PB	�bB	�bB	�bB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�9B	�RB	�^B	�dB	�dB	�qB	�qB	�qB	�wB	�wB	�wB	�}B	�wB	��B	��B	B	B	B	B	B	B	B	B	B	ĜB	ŢB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B
+B
PB
�B
#�B
/B
7LB
>wB
E�B
K�B
R�B
XB
]/B
`BB
e`B
l�B
o�B
u�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<�t�<#�
<#�
<u<#�
<#�
<#�
<#�
<T��<#�
<#�
<49X<#�
<#�
<#�
<T��<D��<#�
<e`B<�=�P<�C�<#�
<#�
<#�
<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�t�<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<�o<#�
<u<�C�<�C�<��
<��
<���<�C�<#�
<#�
<#�
<D��<�C�<�C�<T��<e`B<�j<��
<�o<49X<T��<e`B<D��<D��<�o<#�
<#�
<u<#�
<#�
<�t�<�o<��
<�j<���<#�
<#�
<#�
<#�
<u<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<�9X<���<#�
<#�
<#�
<#�
<#�
<D��<e`B<e`B<49X<e`B<�1<u<49X<�o<#�
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
<D��<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
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
<e`B<u<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447112012010314471220120103144712  AO  ARGQ                                                                        20111130141622  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141622  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144712  IP                  G�O�G�O�G�O�                