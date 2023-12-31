CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:02Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               'A   AO  20111130135936  20190522121825  1727_5046_039                   2C  D   APEX                            2143                            040306                          846 @�H�؟�1   @�H�8��@7-�hr�!�c�bM��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A���A���A�  A���A�  A�  B ffB  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`ffBhffBpffBxffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�33B�33B�  B���B���C   C  C  C  C  C
�C  C  C  C  C�fC  C  C�fC  C�C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C7�fC:  C<  C=�fC@  CB�CD  CF  CG�fCI�fCL  CN  CP�CR  CS�fCV  CX  CY�fC\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cu�fCx  Cz  C|  C~  C��C��C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C��3C��3C�  C��3C��3C�  C�  C��3C�  C�  C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C��C�  C�  C�  D   D �fDfD� D  D� DfD� D  D� D  D� D  D� D  D�fD  D� D	fD	� D
  D
�fD  D� D  Dy�DfD�fD��Dy�D��Dy�DfD�fDfD� D  D� D  D� D  D� D  D� DfD� D��D� D  D� D��D� DfD� D  D� D��D� DfD� D  D� D  D� D   D y�D!  D!�fD"  D"� D#  D#y�D#��D$y�D%  D%�fD&  D&y�D&��D'�fD(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D4��D5� D6  D6� D7  D7y�D8  D8�fD9fD9� D:  D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DB� DC  DC� DD  DDy�DD��DE� DF  DF� DG  DG�fDH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ� DZ��D[y�D\  D\y�D]  D]� D^  D^y�D_  D_�fD`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dy��D�<�D�y�D���D���D�&fD�` D�� D�� D��D�Y�D��fD��fD�fD� Dڙ�D�� D�3D�C3D�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @���@���AffA>ffA^ffA~ffA�33A�  A�  A�33A�  A�33A�33B   B��B��B��B��B(  B/��B7��B?��BG��BO��BW��B`  Bh  Bp  Bx  B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B�  B�  B�  B���B���B���B���C�fC�fC�fC�fC
  C�fC�fC�fC�fC��C�fC�fC��C�fC  C�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC0  C1�fC3�fC5�fC7��C9�fC;�fC=��C?�fCB  CC�fCE�fCG��CI��CK�fCM�fCP  CQ�fCS��CU�fCW�fCY��C\  C]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo��Cq�fCs�fCu��Cw�fCy�fC{�fC}�fC�  C�  C��3C��3C��3C�  C��3C��3C��3C�  C�  C��3C��3C��3C�  C��3C��fC��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��fC��fC��3C��3C��3C��fC��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C�  C��3C��3C�  C��3C��fC��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C�  C��3C��3C�  C��3C��3C��3C��fC��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��fC��fC��3C��3C�  C��3C��fC��3C��3C��3C��3C��fC��fC��fC��3C��fC��fC��3C��3C��fC��3C��3C��fC��3C��3C�  C��3C��fC��3C��3C��3C��3C��fC�  C��3C��3C��3C��3D � D  Dy�D��Dy�D  Dy�D��Dy�D��Dy�D��Dy�D��D� D��Dy�D	  D	y�D	��D
� D
��Dy�D��Ds3D  D� D�3Ds3D�3Ds3D  D� D  Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  Dy�D�3Dy�D��Dy�D�3Dy�D  Dy�D��Dy�D�3Dy�D  Dy�D��Dy�D��Dy�D��D s3D ��D!� D!��D"y�D"��D#s3D#�3D$s3D$��D%� D%��D&s3D&�3D'� D'��D(y�D(��D)y�D)��D*y�D+  D+y�D+��D,y�D,��D-y�D-�3D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4�3D5y�D5��D6y�D6��D7s3D7��D8� D9  D9y�D9��D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBy�DB��DCy�DC��DDs3DD�3DEy�DE��DFy�DF��DG� DG��DHs3DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ�3DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DZ  DZy�DZ�3D[s3D[��D\s3D\��D]y�D]��D^s3D^��D_� D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��De� Df  Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dps3Dp��Dqy�Dq��Dry�Dr��Dsy�Dy�fD�9�D�vfD���D�ɚD�#3D�\�D���D���D�fD�VfD��3D��3D�3D��DږfD���D� D�@ D� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A·+A΋DA΋DAΑhAΓuAΙ�AΛ�AΡ�AΝ�AΛ�AΟ�AΟ�AΣ�AΣ�AΡ�AΡ�AΟ�AΣ�AΧ�AΣ�AΧ�AΩ�Aΰ!Aΰ!Aΰ!AΥ�AΥ�AΥ�AΣ�AΝ�AΝ�AΝ�AΝ�AΙ�AΑhA΍PA�z�A�  A���A�~�A���A�$�A��HA��A�S�A��A��`A�x�A��!A��A��A��+A���A���A��7A���A�A�  A��yA��+A�ĜA�VA��#A���A�?}A��A��yA�G�A�(�A��A�O�A��A��A�t�A�p�A�K�A��!A��FA�1A��DA���A���A�XA�1'A�C�A��A�$�A���A��A�A�5?A���A�1A��
A���A���A���A�p�A�A�^5A�ƨA���A�A�+A�p�A�z�A~�DA|jAy�mAwhsAvbAu�AtZAr�DAq��Ap�`AoƨAml�AkdZAjZAi��Ah�/Ag��Af�\Ae�mAe?}Ad1Ab�A`{A];dA[33AY33AV�DAU�AT�`AT9XARĜAR�AQ��AQ�#AQ��APȴAOK�AN�yAN��AM�7AL�\AK?}AI�^AH��AHA�AG�AF��AFZAE��AEXAD��AD�ADbNADACdZAB��AB��AA�A?`BA<�HA:~�A7O�A5S�A4�HA4�jA4��A4�\A2ȴA0�A0{A/�A/�#A.�A.  A,�A+33A*��A*$�A);dA(Q�A'�
A&ȴA%�^A$ȴA#�
A"��A"jA"1A!�A!&�A ZA�AȴA�jA�A�A�wA��A  A`BA��A=qA
=Av�AA7LA��A��A�TA�A-AƨA��A9XAr�A�
A
�yA
  A	A�9AZA��A��A;dA��Ax�A
=A�AbAdZA��AI�Ax�A ��A (�@�o@��#@���@��P@�hs@�  @�;d@��!@�{@�V@�I�@�|�@�@���@�t�@�7@�D@�I�@�  @�ff@��`@��m@��y@�@�^5@�1'@�V@�9X@�\)@���@�@�dZ@�~�@љ�@���@�5?@͉7@�Z@�dZ@�ff@�j@ǅ@�ff@ũ�@�G�@�Z@�o@�Ĝ@��@�dZ@���@��@��;@�ƨ@�"�@��7@�  @�@�=q@��@�dZ@�v�@�E�@�@��@��^@�hs@���@��m@���@�X@�  @���@�%@�Ĝ@���@�bN@�^5@�x�@��@��@��9@�A�@��F@��#@��/@���@��D@�j@�;d@���@�ȴ@��\@�`B@��@��@�I�@� �@�b@�b@�1@��w@��@�dZ@�+@��y@���@��@�5?@���@�=q@�{@�v�@�=q@���@���@�O�@���@�9X@�b@��
@�K�@��@���@��T@��`@�bN@��@��@��D@�bN@�1@�ƨ@���@�ƨ@��@��!@��@�7L@���@���@���@���@��/@��D@��F@�+@���@��@�=q@�E�@��
@���@��D@��
@��w@�"�@�=q@�t�@�l�@�K�@�l�@�C�@�l�@�33@�;d@�;d@��@�^5@�$�@���@���@�X@��/@��@�I�@�(�@��@��;@���@��P@�33@��y@��!@���@��+@�M�@��@�{@�J@�@���@��-@���@���@��@�O�@�/@���@��@�A�@��@���@���@��@�t�@�K�@�+@�o@���@��+@�5?@��@�@���@�G�@�&�@���@��j@��@�bN@�9X@��@�  @���@���@���@�33@��H@�ȴ@���@�v�@�v�@�^5@�5?@���@��#@���@�hs@�G�@���@��`@�j@� �@�b@}O�@t�@k�F@b��@[��@U��@Lz�@Ep�@>�@9��@3��@/
=@)��@#S�@�-@�9@��@M�@?}@	��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A·+A΋DA΋DAΑhAΓuAΙ�AΛ�AΡ�AΝ�AΛ�AΟ�AΟ�AΣ�AΣ�AΡ�AΡ�AΟ�AΣ�AΧ�AΣ�AΧ�AΩ�Aΰ!Aΰ!Aΰ!AΥ�AΥ�AΥ�AΣ�AΝ�AΝ�AΝ�AΝ�AΙ�AΑhA΍PA�z�A�  A���A�~�A���A�$�A��HA��A�S�A��A��`A�x�A��!A��A��A��+A���A���A��7A���A�A�  A��yA��+A�ĜA�VA��#A���A�?}A��A��yA�G�A�(�A��A�O�A��A��A�t�A�p�A�K�A��!A��FA�1A��DA���A���A�XA�1'A�C�A��A�$�A���A��A�A�5?A���A�1A��
A���A���A���A�p�A�A�^5A�ƨA���A�A�+A�p�A�z�A~�DA|jAy�mAwhsAvbAu�AtZAr�DAq��Ap�`AoƨAml�AkdZAjZAi��Ah�/Ag��Af�\Ae�mAe?}Ad1Ab�A`{A];dA[33AY33AV�DAU�AT�`AT9XARĜAR�AQ��AQ�#AQ��APȴAOK�AN�yAN��AM�7AL�\AK?}AI�^AH��AHA�AG�AF��AFZAE��AEXAD��AD�ADbNADACdZAB��AB��AA�A?`BA<�HA:~�A7O�A5S�A4�HA4�jA4��A4�\A2ȴA0�A0{A/�A/�#A.�A.  A,�A+33A*��A*$�A);dA(Q�A'�
A&ȴA%�^A$ȴA#�
A"��A"jA"1A!�A!&�A ZA�AȴA�jA�A�A�wA��A  A`BA��A=qA
=Av�AA7LA��A��A�TA�A-AƨA��A9XAr�A�
A
�yA
  A	A�9AZA��A��A;dA��Ax�A
=A�AbAdZA��AI�Ax�A ��A (�@�o@��#@���@��P@�hs@�  @�;d@��!@�{@�V@�I�@�|�@�@���@�t�@�7@�D@�I�@�  @�ff@��`@��m@��y@�@�^5@�1'@�V@�9X@�\)@���@�@�dZ@�~�@љ�@���@�5?@͉7@�Z@�dZ@�ff@�j@ǅ@�ff@ũ�@�G�@�Z@�o@�Ĝ@��@�dZ@���@��@��;@�ƨ@�"�@��7@�  @�@�=q@��@�dZ@�v�@�E�@�@��@��^@�hs@���@��m@���@�X@�  @���@�%@�Ĝ@���@�bN@�^5@�x�@��@��@��9@�A�@��F@��#@��/@���@��D@�j@�;d@���@�ȴ@��\@�`B@��@��@�I�@� �@�b@�b@�1@��w@��@�dZ@�+@��y@���@��@�5?@���@�=q@�{@�v�@�=q@���@���@�O�@���@�9X@�b@��
@�K�@��@���@��T@��`@�bN@��@��@��D@�bN@�1@�ƨ@���@�ƨ@��@��!@��@�7L@���@���@���@���@��/@��D@��F@�+@���@��@�=q@�E�@��
@���@��D@��
@��w@�"�@�=q@�t�@�l�@�K�@�l�@�C�@�l�@�33@�;d@�;d@��@�^5@�$�@���@���@�X@��/@��@�I�@�(�@��@��;@���@��P@�33@��y@��!@���@��+@�M�@��@�{@�J@�@���@��-@���@���@��@�O�@�/@���@��@�A�@��@���@���@��@�t�@�K�@�+@�o@���@��+@�5?@��@�@���@�G�@�&�@���@��j@��@�bN@�9X@��@�  @���@���@���@�33@��H@�ȴ@���@�v�@�v�@�^5@�5?@���@��#@���@�hs@�G�@���@��`@�j@� �@�b@}O�@t�@k�F@b��@[��@U��@Lz�@Ep�@>�@9��@3��@/
=@)��@#S�@�-@�9@��@M�@?}@	��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�ZB�TB�ZB�TB�ZB�ZB�ZB�fB�fB�fB�fB�mB�mB�mB�fB�`B�ZB�TB�NB�;B�/B�B�B��B��B��B�B�B�NB�BĜB�RB�B��B��B��B�bB�7B}�Bo�Be`B\)BP�BJ�BE�B?}B49B&�B{BB�B�TB�;B�#B�B��B��B��BƨB�qB�B��B�hB�7B~�BhsB\)BK�B>wB-B#�BuB
��B
�`B
�/B
�B
�B
��B
ɺB
ÖB
�wB
�FB
�B
��B
��B
�JB
� B
r�B
dZB
W
B
H�B
;dB
2-B
,B
$�B
�B
uB
VB
%B	��B	�B	�sB	�NB	�/B	�B	��B	��B	��B	��B	ƨB	�dB	�B	��B	�{B	�+B	� B	|�B	z�B	z�B	v�B	t�B	s�B	p�B	l�B	gmB	dZB	ffB	e`B	^5B	T�B	I�B	B�B	?}B	=qB	;dB	9XB	7LB	7LB	5?B	5?B	49B	33B	1'B	.B	(�B	 �B	�B	hB	B��B�B�B�B�B�sB�BB�#B�B�B��B��B��BƨBB��B�qB�XB�FB�3B�!B�B��B��B��B��B��B��B��B�uB�bB�JB�+B�B�B�B�B� B}�B|�Bz�Bx�Bw�Bu�Bt�Br�Bp�Bn�Bl�BjBgmBe`BaHBaHB^5B\)BZBZBXBW
BVBT�BR�BP�BO�BN�BL�BJ�BI�BG�BE�BD�BB�BA�B?}B>wB=qB;dB;dB:^B9XB8RB7LB6FB5?B49B2-B1'B0!B0!B0!B.B,B+B)�B(�B%�B%�B#�B"�B$�B#�B �B!�B �B&�B&�B%�B&�B(�B(�B+B,B,B-B-B.B-B,B,B-B2-B49B33B49B7LB9XB8RB8RB=qBA�BC�BD�BF�BM�BP�BP�BQ�BP�BP�BP�BP�BR�BT�BXB\)BffBgmBhsBgmBffBn�Bq�Br�Br�Br�Bs�Bt�B~�B�B�B�+B�+B�JB�PB�PB�VB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�jBĜBŢB��B��B��B��B�B�B�5B�5B�BB�ZB�mB�yB�B�B�B��B��B	1B	DB	JB	PB	PB	VB	bB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	$�B	)�B	1'B	<jB	B�B	E�B	G�B	I�B	K�B	O�B	VB	W
B	XB	[#B	]/B	cTB	e`B	jB	k�B	m�B	o�B	p�B	s�B	t�B	v�B	{�B	~�B	�B	�B	�B	�B	�%B	�+B	�=B	�PB	�VB	�VB	�\B	�bB	�oB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�9B	�?B	�FB	�LB	�XB	�XB	�^B	�wB	�}B	�}B	��B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�)B	�/B	�5B	�5B	�fB	��B
	7B
�B
!�B
,B
49B
:^B
A�B
E�B
L�B
P�B
YB
]/B
_;B
bNB
ffB
jB
o�B
s�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�ZB�TB�ZB�TB�ZB�ZB�ZB�fB�fB�fB�fB�mB�mB�mB�fB�`B�`B�`B�yB�B��BBPBJB{BDB%BB��B�B�HB��B�^B�B��B��B��B��B�hB}�Bv�Bn�BXBN�BL�BP�BE�B@�B)�B�BB�fB�ZB�;B�
B�
B�B�B��B��B�wB��B��B�uB�VBu�Bk�BZBO�B8RB33B)�B	7B
�B
�;B
�#B
�B
�#B
��B
��B
ǮB
�}B
�^B
�B
��B
��B
�PB
�B
t�B
gmB
W
B
C�B
7LB
5?B
/B
!�B
�B
�B
{B
%B	��B	�B	�yB	�`B	�;B	�B	�B	�#B	�#B	�B	��B	�jB	�B	��B	�PB	�%B	�B	�B	~�B	w�B	v�B	v�B	w�B	u�B	jB	hsB	n�B	m�B	gmB	_;B	O�B	G�B	D�B	B�B	?}B	=qB	:^B	:^B	7LB	8RB	7LB	7LB	5?B	33B	5?B	2-B	&�B	$�B	�B	%B��B�B�B�B��B�B�;B�B�B�)B�B�B��BƨBŢBĜB�}B�dB�dB�LB�3B�B��B��B��B��B��B��B��B��B��B�JB�DB�JB�7B�+B�B�B�B�B|�B{�Bz�Bv�Bv�Bv�Bu�Bq�Bn�Bn�Bl�Bl�BffBffBcTB`BB]/B[#B[#BYBYBXBYBR�BQ�BR�BO�BN�BK�BK�BJ�BF�BF�BD�BC�BB�BB�B@�B=qB;dB;dB;dB9XB9XB9XB7LB7LB0!B0!B2-B1'B2-B+B/B.B/B/B-B,B(�B(�B �B+B,B&�B,B,B&�B-B.B/B,B,B-B-B.B-B,B,B-B2-B6FB7LB:^B;dB;dB<jB>wBB�BE�BC�BI�BF�BP�BQ�BQ�BQ�BQ�BP�BS�BT�BXBT�B]/B\)BiyBhsBiyBjBl�Bq�Bq�Bs�Br�Bt�Bv�Bz�B~�B�B�B�1B�+B�PB�PB�PB�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�qBĜBĜB��B��B��B�
B�B�)B�;B�5B�NB�fB�sB�B�B�B�B��B	B		7B	DB	JB	PB	PB	VB	bB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	(�B	1'B	<jB	C�B	G�B	H�B	K�B	K�B	L�B	VB	W
B	XB	[#B	]/B	dZB	e`B	jB	l�B	n�B	p�B	q�B	t�B	u�B	w�B	|�B	� B	�B	�B	�B	�B	�+B	�1B	�DB	�VB	�VB	�\B	�bB	�hB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�?B	�FB	�LB	�RB	�^B	�^B	�dB	�wB	�}B	�}B	B	B	ĜB	ĜB	ŢB	ƨB	ǮB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�fB	��B
	7B
�B
!�B
,B
49B
:^B
A�B
E�B
L�B
Q�B
YB
]/B
_;B
bNB
ffB
jB
o�B
s�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B=o<ě�<���<�o<���<���<���<���<���<�1<�`B<�9X<T��<#�
<#�
<#�
<#�
<u<���<e`B<�C�<�t�<#�
<#�
<#�
<�C�<�C�<���<�1<�j<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<��
<�t�<49X<#�
<#�
<u<T��<u<e`B<�C�<49X<u<�9X<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<49X<D��<T��<�o<�o<u<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<���<���<u<e`B<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�C�<�C�<�t�<�t�<49X<#�
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
<49X<#�
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
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446472012010314464720120103144647  AO  ARGQ                                                                        20111130135936  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135936  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144647  IP                  G�O�G�O�G�O�                