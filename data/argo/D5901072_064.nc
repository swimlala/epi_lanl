CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:56Z UW 3.1 conversion   
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               @A   AO  20111130144145  20190522121829  1728_5048_064                   2C  D   APEX                            2142                            040306                          846 @��դ�	1   @���-��
@5���$��b�KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�33A�  A���A�  A�  B ffB  B��B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B���B���B�  B�33B�  B�  B���B�  B�33B�33B�33B�33B�  B���B�  B�  B�33B�  B���B�  B�  B�  B�  B���B�  B�33B�  B�  C   C  C�C  C  C
  C  C�fC  C�C  C  C�C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C9�fC<  C>  C?�fCB  CD  CE�fCH  CJ  CL  CN  CP  CR  CS�fCU�fCX  CZ�C[�fC]�fC`  Cb�Cd�Cf�Ch  Ci�fCk�fCm�fCp  Cr�Ct  Cu�fCx  Cz�C|  C}�fC��C�  C�  C�  C��3C��C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��3C�  C��C��C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C��C��C�  C��C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fD ��D� D  D� D  Dy�D  D� D��Dy�DfD�fD  D� D��D� D	  D	y�D
  D
� D  D� D  D� D  D� D  D� D��Dy�D  Dy�DfD�fDfD� D  D� D  D� D  D� D��D� D  D�fDfD�fDfD�fDfD� D��Dy�D  D� D  D� D  Dy�D��D� D fD �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(fD(� D)  D)�fD*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6�fD7  D7y�D8  D8� D9  D9y�D:  D:�fD;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DC��DDy�DD��DE� DFfDF�fDGfDG� DHfDH� DI  DI� DJfDJ�fDKfDK� DK��DLy�DM  DM� DN  DN� DN��DO� DP  DP� DP��DQ� DRfDR� DS  DS� DT  DTy�DU  DU� DU��DVy�DW  DWy�DX  DX�fDY  DYy�DZ  DZ� D[  D[� D[��D\� D]fD]� D^  D^�fD_  D_� D`fD`�fDa  Da� Db  Db�fDc  Dc� Dc��Dd� De  De� Df  Dfy�Df��Dgy�Dg��Dh� DifDi� Dj  Dj�fDkfDk� Dl  Dl� DmfDm�fDn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds�fDt  Dt� Du  Du� Dv  Dy9�D� D�@ D�l�D��3D�� D�)�D�s3D��3D��fD�� D�s3DǠ D�� D�,�DچfD��D��fD�  D�<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @l��@�33@�33A��A9��AY��Ay��A���A���A�  A���A͙�A���A���A���BffB  BffBffB&ffB.ffB6  B>ffBFffBNffBVffB^ffBfffBn  BvffB~ffB�33B�33B�33B�  B�  B�33B�ffB�33B�33B�  B�33B�ffB�ffB�ffB�ffB�33B�  B�33B�33B�ffB�33B�  B�33B�33B�33B�33B�  B�33B�ffB�33B�33B�33C��C�3C��C��C	��C��C� C��C�3C��C��C�3C��C��C��C��C!�3C#��C%��C'��C)��C+��C-��C/��C1��C3�3C5��C7��C9� C;��C=��C?� CA��CC��CE� CG��CI��CK��CM��CO��CQ��CS� CU� CW��CY�3C[� C]� C_��Ca�3Cc�3Ce�3Cg��Ci� Ck� Cm� Co��Cq�3Cs��Cu� Cw��Cy�3C{��C}� C�3C���C���C���C�� C�ٚC���C���C���C���C�ٚC���C���C���C�ٚC���C���C�ٚC���C�� C���C�ٚC���C���C���C���C���C�� C�� C�� C���C���C���C���C�� C���C�ٚC�ٚC���C���C�ٚC���C���C���C�� C���C���C���C�ٚC���C�� C���C�ٚC���C�� C���C���C���C���C���C���C���C�ٚC���C�ٚC�ٚC���C�ٚC�� C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C�ٚC�ٚC���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C�� C���C���C���C���C���C�ٚC���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D l�D � DffD�fDffD�fD` D�fDffD� D` D��Dl�D�fDffD� DffD�fD	` D	�fD
ffD
�fDffD�fDffD�fDffD�fDffD� D` D�fD` D��Dl�D��DffD�fDffD�fDffD�fDffD� DffD�fDl�D��Dl�D��Dl�D��DffD� D` D�fDffD�fDffD�fD` D� DffD��D l�D �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%l�D%�fD&ffD&�fD'ffD'��D(ffD(�fD)l�D)�fD*ffD*�fD+ffD+�fD,` D,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0��D1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6l�D6�fD7` D7�fD8ffD8�fD9` D9�fD:l�D:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB��DCffDC� DD` DD� DEffDE��DFl�DF��DGffDG��DHffDH�fDIffDI��DJl�DJ��DKffDK� DL` DL�fDMffDM�fDNffDN� DOffDO�fDPffDP� DQffDQ��DRffDR�fDSffDS�fDT` DT�fDUffDU� DV` DV�fDW` DW�fDXl�DX�fDY` DY�fDZffDZ�fD[ffD[� D\ffD\��D]ffD]�fD^l�D^�fD_ffD_��D`l�D`�fDaffDa�fDbl�Db�fDcffDc� DdffDd�fDeffDe�fDf` Df� Dg` Dg� DhffDh��DiffDi�fDjl�Dj��DkffDk�fDlffDl��Dml�Dm�fDnffDn�fDoffDo�fDpl�Dp�fDqffDq�fDrffDr�fDsl�Ds�fDtffDt�fDuffDu�fDy  D�3D�33D�` D��fD��3D��D�ffD��fD��D��3D�ffDǓ3D��3D�  D�y�D� D�ٚD�3D�0 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�n�A�r�A�p�A�r�A�t�A�t�A�r�A�r�A�r�A�r�A�r�A�t�A�p�A�n�A�p�A�p�A�dZA�ZA�G�A�+A��A��A�JA�%A�A�  A���A��A��;A�ĜA���A�A�A�(�A��A�oA��A�&�A�+A�VA���A��uA�  A��7A�$�A���A���A�A�A��\A�^5A�?}A�O�A�r�A�l�A�O�A�7LA�1A��mA��`A��#A���A���A��PA�/A���A���A���A�jA�E�A�"�A��A��A�^5A��A��A�-A�Q�A�jA��`A�?}A�1A���A���A��uA�\)A�M�A��DA��HA�Q�A���A�~�A�{A���A���A�5?A���A��#A��mA��A�p�A��RA��A��hA�{A�t�A�G�A���A�z�A��
A�33A�ƨA�33A���A��-A�O�A���A���A�;dA��uA�~�A��jA��!A�jA�A}�Az-Av~�Aq/Al�HAh�Ae�TAd�Ab{A`$�A]x�AZ�`AY|�AX��AW�PAT9XAO��AM��AK��AHv�AD��AD-AC�TAC"�AA�#A?��A>��A=G�A<Q�A< �A<A;A;�A;��A;C�A:�HA:ffA8��A6��A6  A4ĜA2��A25?A1�TA1��A0�A0�A.�HA-�-A,�A+K�A)%A'hsA&�yA&�\A&M�A%�TA%?}A#�7A"��A"�+A"I�A"=qA"�A!A!\)A!%A 1'A�A&�A��A5?A33AA�A&�A�A{A$�A5?A�TA\)A�FA�A�uA�^A�jAC�A�AVAp�A	��A	A=qA�A�A+A�mA��A��A �j@�t�@��`@��\@��T@��@�dZ@���@�@�^5@��
@�O�@���@� �@��y@�-@�7@��`@�9X@�C�@�V@�O�@�D@��;@�S�@�~�@�h@���@�bN@��m@އ+@��
@�@���@׾w@���@�@�j@�l�@��@�-@Ѳ-@У�@�dZ@�~�@�p�@�t�@�hs@ǶF@�ff@�bN@�
=@�$�@���@��h@�?}@���@�  @��R@��-@�G�@�&�@���@�z�@��@��y@�v�@��#@��h@�`B@�?}@��@�z�@�9X@�ƨ@�o@���@�V@�r�@�V@���@��@�x�@��^@�ff@�~�@�~�@�~�@���@�I�@�;d@�+@��H@�n�@���@��j@��P@�^5@��^@��@��@�S�@�"�@�ȴ@���@�~�@��\@�~�@�E�@��@�X@�bN@�I�@�n�@�^5@�^5@�V@�5?@�J@���@�&�@�z�@���@��@�K�@�@��@���@���@���@�^5@�-@�@��@�@��h@��-@��^@���@�hs@��@���@���@�Z@�1'@�bN@�j@�r�@�j@�Z@�(�@�b@�1@��
@��F@�;d@���@�v�@�{@���@��@�J@���@�7L@�/@�?}@�O�@�7L@���@��@���@��P@�dZ@�C�@�
=@���@��R@��\@�ff@�M�@�M�@�E�@�E�@�=q@�-@�J@��T@���@�x�@�hs@�X@�O�@�&�@��@��9@�z�@�bN@�Q�@�9X@�b@��
@���@�|�@�dZ@�;d@�"�@��@��!@�~�@�^5@�E�@�@��^@��7@�?}@��j@�A�@��@�ƨ@���@�t�@�C�@��@�@��@��!@���@�v�@�E�@���@�7L@��@���@���@���@��D@�z�@�j@�bN@�A�@�(�@�b@���@��P@�K�@�33@�o@�ȴ@�~�@�5?@���@���@��^@���@��@�G�@��`@��u@�bN@�9X@��@��@�l�@�33@��@��y@��+@�-@���@y��@s"�@i%@bJ@Z-@S33@Kƨ@C��@=O�@65?@0�u@*�@&�+@!�#@9X@��@S�@�h@
�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�p�A�n�A�r�A�p�A�r�A�t�A�t�A�r�A�r�A�r�A�r�A�r�A�t�A�p�A�n�A�p�A�p�A�dZA�ZA�G�A�+A��A��A�JA�%A�A�  A���A��A��;A�ĜA���A�A�A�(�A��A�oA��A�&�A�+A�VA���A��uA�  A��7A�$�A���A���A�A�A��\A�^5A�?}A�O�A�r�A�l�A�O�A�7LA�1A��mA��`A��#A���A���A��PA�/A���A���A���A�jA�E�A�"�A��A��A�^5A��A��A�-A�Q�A�jA��`A�?}A�1A���A���A��uA�\)A�M�A��DA��HA�Q�A���A�~�A�{A���A���A�5?A���A��#A��mA��A�p�A��RA��A��hA�{A�t�A�G�A���A�z�A��
A�33A�ƨA�33A���A��-A�O�A���A���A�;dA��uA�~�A��jA��!A�jA�A}�Az-Av~�Aq/Al�HAh�Ae�TAd�Ab{A`$�A]x�AZ�`AY|�AX��AW�PAT9XAO��AM��AK��AHv�AD��AD-AC�TAC"�AA�#A?��A>��A=G�A<Q�A< �A<A;A;�A;��A;C�A:�HA:ffA8��A6��A6  A4ĜA2��A25?A1�TA1��A0�A0�A.�HA-�-A,�A+K�A)%A'hsA&�yA&�\A&M�A%�TA%?}A#�7A"��A"�+A"I�A"=qA"�A!A!\)A!%A 1'A�A&�A��A5?A33AA�A&�A�A{A$�A5?A�TA\)A�FA�A�uA�^A�jAC�A�AVAp�A	��A	A=qA�A�A+A�mA��A��A �j@�t�@��`@��\@��T@��@�dZ@���@�@�^5@��
@�O�@���@� �@��y@�-@�7@��`@�9X@�C�@�V@�O�@�D@��;@�S�@�~�@�h@���@�bN@��m@އ+@��
@�@���@׾w@���@�@�j@�l�@��@�-@Ѳ-@У�@�dZ@�~�@�p�@�t�@�hs@ǶF@�ff@�bN@�
=@�$�@���@��h@�?}@���@�  @��R@��-@�G�@�&�@���@�z�@��@��y@�v�@��#@��h@�`B@�?}@��@�z�@�9X@�ƨ@�o@���@�V@�r�@�V@���@��@�x�@��^@�ff@�~�@�~�@�~�@���@�I�@�;d@�+@��H@�n�@���@��j@��P@�^5@��^@��@��@�S�@�"�@�ȴ@���@�~�@��\@�~�@�E�@��@�X@�bN@�I�@�n�@�^5@�^5@�V@�5?@�J@���@�&�@�z�@���@��@�K�@�@��@���@���@���@�^5@�-@�@��@�@��h@��-@��^@���@�hs@��@���@���@�Z@�1'@�bN@�j@�r�@�j@�Z@�(�@�b@�1@��
@��F@�;d@���@�v�@�{@���@��@�J@���@�7L@�/@�?}@�O�@�7L@���@��@���@��P@�dZ@�C�@�
=@���@��R@��\@�ff@�M�@�M�@�E�@�E�@�=q@�-@�J@��T@���@�x�@�hs@�X@�O�@�&�@��@��9@�z�@�bN@�Q�@�9X@�b@��
@���@�|�@�dZ@�;d@�"�@��@��!@�~�@�^5@�E�@�@��^@��7@�?}@��j@�A�@��@�ƨ@���@�t�@�C�@��@�@��@��!@���@�v�@�E�@���@�7L@��@���@���@���@��D@�z�@�j@�bN@�A�@�(�@�b@���@��P@�K�@�33@�o@�ȴ@�~�@�5?@���@���@��^@���@��@�G�@��`@��u@�bN@�9X@��@��@�l�@�33@��@��y@��+@�-@���@y��@s"�@i%@bJ@Z-@S33@Kƨ@C��@=O�@65?@0�u@*�@&�+@!�#@9X@��@S�@�h@
�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB.B.B.B.B.B.B.B.B.B.B.B.B.B.B.B.B.B-B-B,B,B,B,B-B.B.B.B.B.B/B1'B5?B@�BA�BE�BN�B[#Bn�B�B�\B��B��B�#B�BB�B/BD�B6FB6FBG�BbNB�B�VB�{B��B��B��B��B�B�LB�jB�qB�jB�}B��BĜBǮBȴBǮBƨBɺB��B��BƨB�?B��B�oB�%By�Be`BS�BF�B9XB0!B/B1'B.B,B&�B!�B�BPB��B+B	7B��B�B�B�B�yB�TB�5B�B��BB�'B��B�{B�DB�Bu�BZB-B
=B
�B
�HB
��B
ĜB
�B
�\B
l�B
:^B
�B
B	�TB	ĜB	��B	�7B	k�B	[#B	S�B	H�B	8RB	$�B	�B	oB	PB	B�B�;B��BɺB�XB�B�B��B��B��B�\B�\B�bB�uB��B��B�B�XBBĜBB��B�LB��B��B�hB�JB�uB��B��B��B�\B�JB�JB�PB�+B~�By�Bw�Bu�Bt�Br�Bu�B�B�B�B� B� B~�B}�Bz�Bx�B{�B� B�B�B~�B|�B|�B� B}�B}�B�B�B�B� B|�B� B~�By�Bt�BhsBcTBbNBZBK�BI�BD�BA�B@�B?}B>wB=qB;dB;dB8RB7LB7LB6FB5?B33B0!B0!B0!B1'B2-B1'B1'B0!B0!B0!B0!B/B/B.B/B/B/B.B.B.B/B.B-B,B+B,B-B/B-B0!B2-B49B5?B7LB5?B6FB8RB<jB;dB>wB?}B@�B@�BB�BD�BG�BH�BH�BI�BJ�BN�BR�BW
BXBXBYBZB_;BcTBdZBgmBjBl�Bn�Bp�Br�Br�Bu�Bw�Bx�Bw�B{�Bu�Br�Bw�B}�B�+B�PB�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�9B�9B�LB�^BÖB��B�B�B�)B�/B�5B�/B�5B�NB�fB�B�B�B��B��B��B	B	%B	DB	VB	oB	{B	�B	 �B	(�B	-B	/B	1'B	49B	8RB	=qB	B�B	F�B	H�B	K�B	N�B	Q�B	R�B	T�B	W
B	ZB	]/B	_;B	aHB	aHB	cTB	e`B	hsB	n�B	u�B	y�B	y�B	z�B	}�B	� B	�B	�=B	�1B	�7B	�DB	�PB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�LB	�RB	�^B	�jB	�qB	�wB	�wB	��B	��B	��B	B	ÖB	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�/B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�HB	�NB	�NB	�TB	�TB	�`B	�`B	�`B	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
1B
hB
�B
"�B
(�B
1'B
6FB
>wB
E�B
K�B
Q�B
XB
\)B
aHB
ffB
jB
o�B
u�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B.B.B.B.B.B.B.B.B.B.B.B.B.B.B.B.B/B.B.B-B-B,B-B-B.B.B.B/B/B0!B33B8RBA�BB�BE�BN�B[#Bn�B�B�hB��BǮB�BB��BB�B5?BJ�B8RB7LBF�BaHB�B�\B��B��B��B��B��B�B�XB�}B��B�}B��BĜBǮBɺB��B��B��B��B��B�
B��B��B�B��B�bB�7Bt�BdZBVBH�B?}BB�B9XB5?B33B,B(�B!�B�BB\B�BB��B��B��B�B�yB�fB�NB�5B��B�}B��B��B�oB�VB�=Bu�BG�B�BB
�B
�5B
�B
ȴB
�!B
�oB
VB
(�B
�B	��B	�fB	��B	��B	}�B	ffB	dZB	YB	I�B	5?B	 �B	�B	�B	�B	PB�B�fB�;B��B�?B�B�'B�'B��B��B��B��B��B��B��B�B�XBƨBȴBɺB��BÖB�B��B��B�\B��B��B��B��B��B��B��B��B��B�7B}�Bz�Bx�By�By�B� B�1B�%B�B�B�B�B�B� B� B�%B�PB�bB�JB�%B�B�B�B� B}�B�B�+B�1B�=B�B�B�%B�B~�Bq�Bl�Bn�BffBQ�BP�BL�BI�BH�BI�BG�BE�BB�BD�BB�B>wB;dB;dB;dB:^B8RB7LB9XB8RB49B5?B5?B33B33B33B33B33B33B2-B2-B2-B2-B2-B2-B1'B1'B2-B49B6FB2-B,B-B33B2-B5?B6FB49B8RB:^B5?B6FB8RB<jBC�BF�B?}BF�BG�BB�BG�BI�BI�BJ�BL�BN�BS�BW
BYBYBXBYB]/B_;BcTBdZBgmBk�Bm�Bp�Br�Bt�Bt�Bx�Bw�B|�B�B{�Bu�Br�Bw�B}�B�+B�PB�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�?B�FB�^B�qBÖB�
B�B�B�/B�5B�;B�;B�HB�NB�sB�B�B�B��B��B��B	B	%B	JB	\B	uB	�B	�B	 �B	(�B	.B	0!B	2-B	49B	9XB	=qB	C�B	F�B	H�B	K�B	N�B	Q�B	S�B	VB	XB	[#B	^5B	aHB	cTB	bNB	dZB	ffB	hsB	n�B	v�B	{�B	y�B	z�B	}�B	�B	�B	�=B	�1B	�=B	�DB	�PB	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�-B	�-B	�9B	�?B	�FB	�RB	�RB	�dB	�qB	�qB	�wB	�}B	��B	��B	��B	ÖB	ÖB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�
B	�B	�B	�B	�)B	�5B	�;B	�;B	�BB	�HB	�BB	�HB	�HB	�HB	�NB	�TB	�TB	�ZB	�fB	�`B	�`B	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
	7B
hB
�B
"�B
)�B
1'B
7LB
>wB
E�B
L�B
Q�B
XB
\)B
aHB
ffB
k�B
o�B
u�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<u<u<�o<u<u<u<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<D��<�C�<�C�<e`B<#�
<#�
<#�
<49X<��
<���<���<�C�<T��<#�
<49X<�C�<���=o=�P<�/<�t�<�/<���=+<�h<ě�<�C�<49X<�o<u<�C�<�o<#�
<#�
<T��<���<���<�o<�C�<�1<���<#�
<#�
<#�
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
<#�
<49X<D��<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452152012011014521520120110145215  AO  ARGQ                                                                        20111130144145  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144145  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145215  IP                  G�O�G�O�G�O�                