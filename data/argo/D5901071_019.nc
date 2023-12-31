CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:56Z UW 3.1 conversion   
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
_FillValue                 �  Kt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135607  20190522121825  1727_5046_019                   2C  D   APEX                            2143                            040306                          846 @�.@&�7�1   @�.@����@7��O�;d�c�7KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   AffA@  Aa��A���A���A�  A�  A���A���A�  A�  A�33B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B_��Bg��Bo��Bw��B��B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�33B�33B�  B�  B�33B�  B���B�  B�33B�  B�  B�  B���B�  C   C�fC  C  C�fC
  C  C�fC  C�C  C�fC  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C;�fC=�fC@  CB�CD  CF  CH�CJ  CL  CN  CP  CQ�fCS�fCV  CX  CZ  C\  C^�C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cw�fCz  C|�C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C��3C�  C��C�  C�  C��3C��3C�  C��C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��3C��3C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C��C��C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C��3C��C��C��3C��3C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3D fD � D ��D� DfD� D��D� DfD� D  D� DfD� D  D� D  D�fD	  D	� D
fD
� D  D� D��D� D  D� DfD�fD  D� D��D� D  Dy�D  D�fD  D� D  D�fD  Dy�D  D� D  Dy�D  D� D  D� D��D� D  D� D  D� D  Dy�D��D� D  D� D   D �fD!  D!� D"  D"� D"��D#� D$  D$� D%  D%� D%��D&� D'fD'� D'��D(� D)fD)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0�fD1  D1y�D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6y�D7  D7� D8fD8� D8��D9� D:fD:� D;  D;�fD<  D<� D=fD=� D>  D>�fD?  D?� D@fD@� DA  DA� DB  DB� DCfDC�fDDfDD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DNy�DO  DO�fDP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^� D_  D_� D`  D`� Da  Da�fDb  Dby�Dc  Dc� Dd  Ddy�De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy� D�#3D�l�D�� D���D�)�D�ffD��3D�� D�,�D�I�D��fD�� D�#3D�` Dډ�D๚D�&fD�S3D�3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @l��@�ff@�ffA��A;33A\��A|��A�ffA���A���A�ffA�ffAݙ�A홚A���B��B��B��B��B&ffB.��B6��B>��BF��BN��BV��B^ffBfffBnffBvffB~ffB�ffB�ffB���B�ffB�33B�33B�ffB�ffB�ffB�ffB�ffB�ffB���B�ffB�ffB�33B�ffBǙ�B˙�B�ffB�ffBי�B�ffB�33B�ffB癚B�ffB�ffB�ffB�33B�ffB�ffC��C�3C�3C��C	�3C�3C��C�3C��C�3C��C�3C�3C��C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/��C1�3C3�3C5�3C7�3C9�3C;��C=��C?�3CA��CC�3CE�3CG��CI�3CK�3CM�3CO�3CQ��CS��CU�3CW�3CY�3C[�3C]��C_��Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co��Cq�3Cs�3Cu�3Cw��Cy�3C{��C}�3C�3C�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC��fC�ٚC���C�ٚC��fC�ٚC�ٚC���C���C�ٚC��fC�ٚC���C�ٚC�ٚC���C�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC���C���C�ٚC�ٚC��fC��fC��fC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC��fC��fC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC��fC�ٚC���C�ٚC�ٚC��fC��fC�ٚC�ٚC�ٚC�ٚC���C�ٚC��fC�ٚC���C�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC���C�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC���C��fC��fC���C���C��fC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C���C��fD l�D �fDl�D�3Dl�D�fDl�D�3Dl�D��Dl�D�3Dl�D��Dl�D��Ds3D��D	l�D	�3D
l�D
��Dl�D�fDl�D��Dl�D�3Ds3D��Dl�D�fDl�D��DffD��Ds3D��Dl�D��Ds3D��DffD��Dl�D��DffD��Dl�D��Dl�D�fDl�D��Dl�D��Dl�D��DffD�fDl�D��Dl�D��D s3D ��D!l�D!��D"l�D"�fD#l�D#��D$l�D$��D%l�D%�fD&l�D&�3D'l�D'�fD(l�D(�3D)l�D)��D*s3D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.�3D/l�D/��D0s3D0��D1ffD1��D2l�D2��D3l�D3��D4s3D4��D5l�D5��D6ffD6��D7l�D7�3D8l�D8�fD9l�D9�3D:l�D:��D;s3D;��D<l�D<�3D=l�D=��D>s3D>��D?l�D?�3D@l�D@��DAl�DA��DBl�DB�3DCs3DC�3DDs3DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ�3DKl�DK��DLl�DL��DMl�DM��DNffDN��DOs3DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU�fDVl�DV�3DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]�fD^l�D^��D_l�D_��D`l�D`��Das3Da��DbffDb��Dcl�Dc��DdffDd��Des3De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��DlffDl�fDml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Dy��D��D�c3D��fD��3D�  D�\�D���D��fD�#3D�@ D���D��fD��D�VfDڀ D� D��D�I�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A�v�A�A�A�1'A�&�A��A��A�bA�1A�  A���A��A��A��A��yA��`A��;A��#A���A���AΥ�AͮA�~�A�;dA�;dAăA²-A�?}A��\A��A�7LA� �A��A�O�A��^A��A�z�A��jA�O�A�`BA�|�A���A�(�A�v�A��wA�+A��A��A���A�E�A�33A��A�ĜA�-A��-A���A��PA�ZA�x�A�~�A�{A��+A���A�t�A��TA��jA�S�A�ȴA�t�A�E�A�A�7LA���A�O�A��A�33A�$�A�
=A�M�A��
A�A�A�1'A��DA��#A�p�A�z�A���A�?}A���A���A�`BA�
=A��hA�|�A��TA���A�AXA}��A|M�AyAw�Av-AuArE�Ap��An��Am��AmAlbAj��Ai�FAhZAg;dAfAd�Ac�mAb��Ab-A`��A_�mA^�HA^n�A\JA[�A[`BA[+AZ�\AY�#AYO�AX-AV�\ATbAQ�;AQ�AQ?}AQ�AP��APz�AP�AOAM��AL��AK33AIO�AH�DAG�PAF�uAD�/ACp�ABĜAB�AAx�A@=qA>^5A=�FA=\)A<VA;�TA;x�A:��A:M�A8��A7p�A6jA5O�A4�\A3�TA3VA2~�A1�hA0v�A/��A/�A.z�A-�A,��A,�A+�7A*��A*{A)�FA(-A'l�A&bNA%S�A$�yA$VA#�A#O�A!�A!��A ��A   A%A��A��A�A��A�A(�A��A�7A+AffA�-A�AM�AAQ�A�yAp�AA��A�TAbA�A�\AM�AJAXA	�A��A��A/A��A(�A�
AG�A��AhsA�HA�RA�!An�Ap�@�;d@���@���@��h@���@�$�@��h@��@��+@���@�@�/@�1'@�\)@�&�@�(�@�v�@��@�33@��@㝲@�~�@�J@�I�@�@ܣ�@��@ۍP@۾w@�S�@�o@�^5@ى7@؋D@��@�V@�33@��H@��@�&�@ԓu@��m@ӥ�@��H@Ѻ^@�Q�@�dZ@�;d@�v�@���@�G�@̃@�ƨ@ʧ�@�hs@�&�@Ȭ@���@�@�S�@��`@�ƨ@��
@���@��@���@��@�;d@�C�@��@��@�$�@�r�@��@���@�33@�$�@��/@���@��@�x�@��u@��@�(�@�b@�%@��@���@�hs@��`@���@�+@�V@��@���@�7L@���@�|�@�
=@���@�M�@��@���@��`@��D@�bN@�A�@�  @�S�@�
=@�@���@��@��y@�~�@��#@��u@�bN@�(�@��F@�t�@��y@���@�ff@�V@�E�@�{@��@��@���@�p�@��@��@���@���@��@��9@��/@�9X@�ƨ@�t�@�"�@��H@���@�ff@�=q@�@��@��h@�X@��j@�(�@��@� �@�1@��@���@�\)@�;d@���@�M�@�M�@��+@�33@��@�o@�ƨ@�  @�Q�@���@�9X@�9X@�(�@��F@��u@�1'@�b@��@�  @��
@��P@�|�@�l�@���@�V@��@��7@��`@���@��@��P@���@��w@���@�r�@�?}@��@��@�bN@�Z@�r�@�Q�@�1'@�A�@�9X@�1'@� �@��@�1@��m@���@�t�@�K�@�
=@��y@��!@��@��T@��^@���@��h@��@�p�@�?}@���@�z�@� �@���@���@��P@�
=@��@�v�@��T@��^@�V@���@�I�@�(�@� �@�1@��m@��w@��w@��F@���@�+@��@��@xĜ@qG�@f��@`��@X�@Q�^@I��@DZ@?
=@9G�@5�@0Q�@(��@"�\@��@��@�!@E�@
�\@K�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A���A�v�A�A�A�1'A�&�A��A��A�bA�1A�  A���A��A��A��A��yA��`A��;A��#A���A���AΥ�AͮA�~�A�;dA�;dAăA²-A�?}A��\A��A�7LA� �A��A�O�A��^A��A�z�A��jA�O�A�`BA�|�A���A�(�A�v�A��wA�+A��A��A���A�E�A�33A��A�ĜA�-A��-A���A��PA�ZA�x�A�~�A�{A��+A���A�t�A��TA��jA�S�A�ȴA�t�A�E�A�A�7LA���A�O�A��A�33A�$�A�
=A�M�A��
A�A�A�1'A��DA��#A�p�A�z�A���A�?}A���A���A�`BA�
=A��hA�|�A��TA���A�AXA}��A|M�AyAw�Av-AuArE�Ap��An��Am��AmAlbAj��Ai�FAhZAg;dAfAd�Ac�mAb��Ab-A`��A_�mA^�HA^n�A\JA[�A[`BA[+AZ�\AY�#AYO�AX-AV�\ATbAQ�;AQ�AQ?}AQ�AP��APz�AP�AOAM��AL��AK33AIO�AH�DAG�PAF�uAD�/ACp�ABĜAB�AAx�A@=qA>^5A=�FA=\)A<VA;�TA;x�A:��A:M�A8��A7p�A6jA5O�A4�\A3�TA3VA2~�A1�hA0v�A/��A/�A.z�A-�A,��A,�A+�7A*��A*{A)�FA(-A'l�A&bNA%S�A$�yA$VA#�A#O�A!�A!��A ��A   A%A��A��A�A��A�A(�A��A�7A+AffA�-A�AM�AAQ�A�yAp�AA��A�TAbA�A�\AM�AJAXA	�A��A��A/A��A(�A�
AG�A��AhsA�HA�RA�!An�Ap�@�;d@���@���@��h@���@�$�@��h@��@��+@���@�@�/@�1'@�\)@�&�@�(�@�v�@��@�33@��@㝲@�~�@�J@�I�@�@ܣ�@��@ۍP@۾w@�S�@�o@�^5@ى7@؋D@��@�V@�33@��H@��@�&�@ԓu@��m@ӥ�@��H@Ѻ^@�Q�@�dZ@�;d@�v�@���@�G�@̃@�ƨ@ʧ�@�hs@�&�@Ȭ@���@�@�S�@��`@�ƨ@��
@���@��@���@��@�;d@�C�@��@��@�$�@�r�@��@���@�33@�$�@��/@���@��@�x�@��u@��@�(�@�b@�%@��@���@�hs@��`@���@�+@�V@��@���@�7L@���@�|�@�
=@���@�M�@��@���@��`@��D@�bN@�A�@�  @�S�@�
=@�@���@��@��y@�~�@��#@��u@�bN@�(�@��F@�t�@��y@���@�ff@�V@�E�@�{@��@��@���@�p�@��@��@���@���@��@��9@��/@�9X@�ƨ@�t�@�"�@��H@���@�ff@�=q@�@��@��h@�X@��j@�(�@��@� �@�1@��@���@�\)@�;d@���@�M�@�M�@��+@�33@��@�o@�ƨ@�  @�Q�@���@�9X@�9X@�(�@��F@��u@�1'@�b@��@�  @��
@��P@�|�@�l�@���@�V@��@��7@��`@���@��@��P@���@��w@���@�r�@�?}@��@��@�bN@�Z@�r�@�Q�@�1'@�A�@�9X@�1'@� �@��@�1@��m@���@�t�@�K�@�
=@��y@��!@��@��T@��^@���@��h@��@�p�@�?}@���@�z�@� �@���@���@��P@�
=@��@�v�@��T@��^@�V@���@�I�@�(�@� �@�1@��m@��w@��w@��F@���@�+@��@��@xĜ@qG�@f��@`��@X�@Q�^@I��@DZ@?
=@9G�@5�@0Q�@(��@"�\@��@��@�!@E�@
�\@K�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�wB�wB�}B�}B��B�}B�}B�}B�}B�}B�wB�wB�wB�wB�wB�wB�wB�wB�qB�jB�^B�RBÖBɺB��B��B�mB��B�#B�sB�sB�`B�NB�BB�5B�5B�5B�#B�
B��B��B��BǮBB�wB�^B�FB�3B�B�B��B��B��B�VB�%B}�Bu�BjB\)BH�B=qB8RB1'B+B"�B�B	7BB��B��B��B�B�TB�B��B��BB�9B��B��B��B�VB� Bm�BXBJ�B@�B1'B"�B\B
��B
�sB
�
B
ŢB
�jB
�!B
��B
�{B
�PB
�1B
~�B
r�B
VB
I�B
F�B
9XB
!�B
oB

=B
1B
VB
DB
%B
B	��B	�B	�B	�NB	�#B	��B	��B	��B	ƨB	��B	�jB	�LB	�'B	�B	�B	��B	��B	��B	�bB	�B	s�B	bNB	aHB	`BB	^5B	\)B	\)B	hsB	cTB	\)B	ZB	R�B	I�B	D�B	=qB	7LB	1'B	+B	'�B	%�B	 �B	�B	�B	�B	�B	oB	bB	VB	DB	%B	B��B��B�B�B�B�yB�fB�TB�;B�#B�B��B��B��B��BǮBĜB��B�qB�dB�LB�3B�B�B�B�B��B��B��B��B��B��B��B��B�uB�\B�\B�PB�JB�DB�1B�B�B� B|�Bx�Bt�Bp�Bm�Bk�BiyBffBcTBbNB`BB_;B\)BYBW
BW
BT�BS�BR�BQ�BP�BO�BL�BM�BK�BJ�BI�BF�BC�BE�BF�BE�BC�BB�BD�BC�BA�BB�BD�BC�BC�BC�BC�BE�BD�BF�BE�BA�B=qB:^B8RB5?B33B1'B2-B<jBG�BO�BO�BR�BYB[#B_;BcTBhsBq�Bv�B{�B}�B|�B� B�B�B� B�B�B�B� B~�B}�B}�B|�B|�B~�B~�B}�B|�Bz�Bv�Bt�Bx�B� B�B�B�B�B�B�+B�=B�VB�hB�hB�PB�JB�bB�\B�VB�JB�DB�+B�+B�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�'B�'B�!B�!B�'B�?B�}B��BBƨBɺB��B��B��B��B��B��B�B�B�
B�#B�)B�TB�fB�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	+B	DB	VB	uB	�B	�B	�B	�B	�B	�B	�B	"�B	'�B	+B	-B	0!B	49B	6FB	:^B	=qB	<jB	=qB	?}B	@�B	H�B	J�B	R�B	T�B	VB	ZB	]/B	]/B	]/B	_;B	aHB	bNB	e`B	iyB	l�B	n�B	o�B	o�B	t�B	w�B	|�B	�B	�1B	�1B	�1B	�7B	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�FB	�XB	�jB	�wB	�}B	��B	��B	B	ĜB	ŢB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	�B	��B
B
PB
�B
#�B
+B
49B
:^B
A�B
H�B
L�B
Q�B
[#B
_;B
dZB
ffB
jB
o�B
s�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B�}B�}B�}B�}B�}B�wB�wB�wB�wB�wB�wB�wB�wB�wB�qB�qBB��B�NB�yB�B��B�;B�B��B�B�B�sB�ZB�yB�B�B�TB�5B�5B�5B�;B��B��BƨB��B�jB�XB�9B�!B��B�B��B��B�VB�=B�%B|�Bo�BS�BC�B?}B:^B6FB49B"�B\B
=BB��B��BB�B�NB�B�B��B��B�'B��B��B��B��B~�BaHBR�BM�B?}B49B �B1B
��B
�mB
��B
��B
ÖB
�B
��B
�{B
�{B
�=B
�B
bNB
P�B
P�B
I�B
-B
�B
oB
PB
�B
uB
\B
JB
B	��B	�B	�yB	�NB	�)B	�B	��B	��B	ǮB	ɺB	�^B	�3B	�-B	�'B	�B	��B	��B	��B	�uB	~�B	e`B	cTB	bNB	`BB	^5B	]/B	r�B	o�B	ffB	dZB	_;B	P�B	L�B	F�B	C�B	:^B	0!B	.B	-B	+B	&�B	�B	�B	�B	�B	{B	uB	oB	bB	DB	B��B��B��B�B�B�B�B�`B�BB�/B�B�B��B��B��BɺBƨBǮBB�wB�^B�-B�-B�!B�'B�!B��B��B��B��B��B��B��B��B��B�oB�bB�\B�\B�VB�=B�1B�%B�B�B~�By�Bq�Bp�Bp�Bq�BiyBffBcTBcTBcTBdZB^5B\)B[#BYBW
BVBVBT�BVBQ�BM�BL�BM�BO�BN�BJ�BG�BG�BH�BI�BH�BH�BI�BK�BI�BJ�BG�BH�BJ�BJ�BJ�BF�BM�BA�BB�B:^B<jB5?B;dB1'B6FB;dBG�BO�BQ�BVB]/B_;B_;Be`BgmBq�Bz�B~�B�B|�B�B�B�%B� B�B�B�B� B�B�B}�B�B|�B�B�B�B|�B�Bv�Bw�By�B�B�B�B�B�B�B�+B�PB�bB��B��B�hB�oB�bB�\B�oB�\B�bB�7B�+B�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�!B�'B�-B�'B�!B�9B�?B��B��BĜBǮB��B��B��B��B��B��B�B�B�B�B�#B�/B�TB�fB�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	+B	DB	VB	uB	�B	�B	�B	�B	�B	�B	�B	!�B	&�B	,B	,B	.B	49B	5?B	9XB	?}B	<jB	>wB	@�B	@�B	I�B	J�B	S�B	T�B	W
B	[#B	]/B	^5B	_;B	aHB	bNB	cTB	gmB	jB	l�B	o�B	o�B	o�B	t�B	u�B	{�B	�B	�7B	�7B	�1B	�7B	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�LB	�^B	�qB	�}B	��B	B	��B	B	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	�B	��B
B
PB
�B
$�B
,B
49B
:^B
A�B
H�B
L�B
Q�B
[#B
_;B
dZB
ffB
k�B
o�B
s�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<ě�<�/<���<�t�<u<�C�<u<#�
<#�
<#�
<#�
<49X<���<�1<#�
<#�
<49X<T��<��
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<e`B<#�
<#�
<D��<�o<�t�<���<49X<#�
<#�
<#�
<49X<�C�<T��<#�
<#�
<#�
<#�
<#�
<�1<e`B<#�
<#�
<D��<e`B<T��<#�
<#�
<e`B<�C�<�1<�C�<#�
<#�
<T��<e`B<�C�<�C�<u<���<�o<#�
<u<���<u<#�
<#�
<D��<49X<�t�<D��<#�
<#�
<�o<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<�o<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<D��<#�
<#�
<#�
<D��<#�
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
<D��<#�
<#�
<#�
<#�
<#�
<49X<#�
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
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446402012010314464020120103144640  AO  ARGQ                                                                        20111130135607  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135607  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144640  IP                  G�O�G�O�G�O�                