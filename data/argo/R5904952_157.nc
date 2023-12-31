CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:40Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190540  20181005190540  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�੷���1   @��F)��@0�I�^5?�c�bM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @@  @�33@�  A��A   A@  A^ffA~ffA���A�  A�  A�  A�  A�  A�  B ffBffB  B��B��B(  B0ffB8  B@  BH  BO��BX  B_��Bh  Bp  Bw��B�  B�33B�  B�  B�33B�33B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�33B�  B���C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C��C�  C�  C�  C��3C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C��C�  C��3C��3C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D   D y�D ��D� D  Dy�D  D� D  D� D  D� D  D� DfD� D  D�fD	  D	� D
  D
� D  D� DfD� D  D� D  D� DfD� D  D� D��Dy�D  D� DfD�fDfD�fDfD�fDfD� D��Dy�D��D� D  Dy�D��D� D  D� D  D�fD  D� D  D� D  Dy�D   D � D ��D!� D"  D"�fD#  D#� D$fD$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/�fD0  D0y�D1  D1� D2  D2� D3fD3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9� D:  D:y�D:��D;� D<fD<�fD=fD=�fD>  D>� D?  D?� D?��D@y�DA  DA� DA��DBy�DB��DC� DD  DD� DEfDE� DF  DF�fDGfDG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DLy�DM  DM� DN  DN� DN��DO� DPfDP� DQ  DQy�DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZfDZ� DZ��D[y�D\  D\� D]fD]� D^  D^� D_fD_� D`  D`� DafDa�fDb  Dby�Db��Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� DjfDj�fDk  Dk� Dl  Dl� Dm  Dm� DnfDn� Dn��Doy�Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw� Dy��D�;3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @S33@���@ə�AfgA$��AD��Ac33A���A�33A�ffA�ffA�ffA�ffA�ffA�ffB��B	��B33B��B ��B)33B1��B933BA33BI33BP��BY33B`��Bi33Bq33Bx��B���B���B���B���B���B���B���B���B���B���B���B���B���B�fgB���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B���B虚B왚B�B���B���B�fgC L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�CL�CfgCL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPfgCRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC��C��C�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�33C�33C�&fC��C��C�33C�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fD 3D ��D�D�3D3D��D3D�3D3D�3D3D�3D3D�3D�D�3D3D��D	3D	�3D
3D
�3D3D�3D�D�3D3D�3D3D�3D�D�3D3D�3D�D��D3D�3D�D��D�D��D�D��D�D�3D�D��D�D�3D3D��D�D�3D3D�3D3D��D3D�3D3D�3D3D��D 3D �3D!�D!�3D"3D"��D#3D#�3D$�D$�3D%3D%�3D&3D&�3D'�D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.��D/3D/��D03D0��D13D1�3D23D2�3D3�D3��D43D4�3D53D5�3D63D6�3D73D7�3D83D8��D93D9�3D:3D:��D;�D;�3D<�D<��D=�D=��D>3D>�3D?3D?�3D@�D@��DA3DA�3DB�DB��DC�DC�3DD3DD�3DE�DE�3DF3DF��DG�DG�3DH3DH��DI3DI�3DJ3DJ�3DK3DK�3DL3DL��DM3DM�3DN3DN�3DO�DO�3DP�DP�3DQ3DQ��DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW��DX3DX�3DY3DY�3DZ�DZ�3D[�D[��D\3D\�3D]�D]�3D^3D^�3D_�D_�3D`3D`�3Da�Da��Db3Db��Dc�Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh��Di3Di�3Dj�Dj��Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn�Dn�3Do�Do��Dp3Dp��Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dw�3Dy�D�D�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1A�1A�
=A�JA�
=A�1A�
=A�1A�  A��A��HA���A�ȴA϶FAϑhA�t�A�;dA�ĜA�VA�+A���A�AͶFA�ƨA��
A��TA��#A���Aʹ9Aͩ�A͡�A͙�Aͥ�AͰ!AͰ!AͮAͩ�A͟�A͟�A͡�A͟�A͟�A͟�A͟�A͓uA͑hA͗�AͲ-AͲ-Aͩ�A͟�AͰ!A�ĜA�ƨAͰ!A͝�AͅA���A̴9A̍PA��/A˃A�M�A�%A�O�AżjAď\A�"�A�^5A�bNA�t�A��RA�-A�
=A���A�K�A���A��A��
A���A��A���A��A�JA��TA�jA�r�A���A�+A�1A���A���A�+A�?}A��A��HA�p�A�A���A���A�1A�33A�(�A�A�&�A�hsA�XA�r�A�jA��yA�A|��Aw�mAs�^Aq�An��Ak�#Ag�Ae33Ab��A_�7AZ�yAX��AX=qAX{AVM�AR�9AP�AL��AJn�AJ�AI
=AGx�AEK�ACVAAK�A@��A@JA>��A<��A;��A:��A97LA6�\A4�A45?A2�9A17LA/`BA.�A-\)A,A�A*��A)�wA)+A(v�A'��A'oA&Q�A%�A%O�A$n�A#��A"�`A"M�A��AbNA��A�A7LAVA{A�A`BA5?A�A%A��Az�AO�A�TA7LA��AVAQ�AA
E�A	`BAffA��AA�9AbNA/A�A��A��AK�A r�@�v�@���@�n�@���@�  @��@��@�@�hs@��D@�9X@�O�@�Z@�\)@�Z@�ȴ@�=q@�7L@�D@㕁@�E�@�^@��`@���@���@��#@ܼj@�1@ڰ!@�x�@�%@ش9@�A�@��@�v�@��@��#@�V@� �@ӥ�@��@�ȴ@��@�G�@���@Л�@Ϯ@�v�@�-@�=q@�$�@�?}@���@���@ʟ�@��@ɑh@���@�Ĝ@�I�@��@ǶF@���@�-@ũ�@��@��@�V@�O�@�`B@ě�@��
@Å@�K�@�C�@��@���@�{@�5?@�$�@�`B@��@�Q�@��@��F@�\)@���@�M�@���@�Z@��w@��F@��P@���@��@��h@��@�%@�9X@���@�
=@��@�J@�p�@�O�@�G�@���@��@�o@�@��#@�@�X@�&�@��9@�A�@�b@��;@��@�@��@��y@�-@��^@���@�1'@��;@��
@��m@��@���@��m@���@��F@��@��@��H@�ff@�-@�{@�p�@�V@�Ĝ@�r�@�b@��F@�K�@���@��\@�5?@�@�@�hs@�%@��u@�bN@�(�@��
@��F@�K�@���@��#@�`B@���@��9@��@���@��@�(�@��;@�  @��@�;d@���@�o@�@���@���@��@��y@���@�$�@��T@��#@��@��@�&�@��`@��@��@� �@��@���@�V@�@��T@�p�@�G�@�/@��/@��@�ƨ@���@�l�@��@�@��H@���@�V@�5?@��#@��h@�p�@�G�@��`@��u@�b@��@�+@���@��@�ȴ@���@�n�@���@�O�@��@��@��@�I�@�A�@�(�@���@��;@��F@���@�l�@�
=@��R@�M�@�@���@���@��7@�hs@��`@���@�z�@�r�@�I�@�b@��F@�|�@�\)@�C�@�;d@�33@�"�@�"�@��y@���@�^5@�E�@�-@�{@��@��T@���@��h@�7L@��`@���@�1'@���@��F@��@�dZ@�+@�
=@�@���@��R@��\@�E�@�$�@��@�J@��#@�`B@���@��u@�I�@�(�@�(�@�A�@�9X@� �@��>@tj@b��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1A�1A�
=A�JA�
=A�1A�
=A�1A�  A��A��HA���A�ȴA϶FAϑhA�t�A�;dA�ĜA�VA�+A���A�AͶFA�ƨA��
A��TA��#A���Aʹ9Aͩ�A͡�A͙�Aͥ�AͰ!AͰ!AͮAͩ�A͟�A͟�A͡�A͟�A͟�A͟�A͟�A͓uA͑hA͗�AͲ-AͲ-Aͩ�A͟�AͰ!A�ĜA�ƨAͰ!A͝�AͅA���A̴9A̍PA��/A˃A�M�A�%A�O�AżjAď\A�"�A�^5A�bNA�t�A��RA�-A�
=A���A�K�A���A��A��
A���A��A���A��A�JA��TA�jA�r�A���A�+A�1A���A���A�+A�?}A��A��HA�p�A�A���A���A�1A�33A�(�A�A�&�A�hsA�XA�r�A�jA��yA�A|��Aw�mAs�^Aq�An��Ak�#Ag�Ae33Ab��A_�7AZ�yAX��AX=qAX{AVM�AR�9AP�AL��AJn�AJ�AI
=AGx�AEK�ACVAAK�A@��A@JA>��A<��A;��A:��A97LA6�\A4�A45?A2�9A17LA/`BA.�A-\)A,A�A*��A)�wA)+A(v�A'��A'oA&Q�A%�A%O�A$n�A#��A"�`A"M�A��AbNA��A�A7LAVA{A�A`BA5?A�A%A��Az�AO�A�TA7LA��AVAQ�AA
E�A	`BAffA��AA�9AbNA/A�A��A��AK�A r�@�v�@���@�n�@���@�  @��@��@�@�hs@��D@�9X@�O�@�Z@�\)@�Z@�ȴ@�=q@�7L@�D@㕁@�E�@�^@��`@���@���@��#@ܼj@�1@ڰ!@�x�@�%@ش9@�A�@��@�v�@��@��#@�V@� �@ӥ�@��@�ȴ@��@�G�@���@Л�@Ϯ@�v�@�-@�=q@�$�@�?}@���@���@ʟ�@��@ɑh@���@�Ĝ@�I�@��@ǶF@���@�-@ũ�@��@��@�V@�O�@�`B@ě�@��
@Å@�K�@�C�@��@���@�{@�5?@�$�@�`B@��@�Q�@��@��F@�\)@���@�M�@���@�Z@��w@��F@��P@���@��@��h@��@�%@�9X@���@�
=@��@�J@�p�@�O�@�G�@���@��@�o@�@��#@�@�X@�&�@��9@�A�@�b@��;@��@�@��@��y@�-@��^@���@�1'@��;@��
@��m@��@���@��m@���@��F@��@��@��H@�ff@�-@�{@�p�@�V@�Ĝ@�r�@�b@��F@�K�@���@��\@�5?@�@�@�hs@�%@��u@�bN@�(�@��
@��F@�K�@���@��#@�`B@���@��9@��@���@��@�(�@��;@�  @��@�;d@���@�o@�@���@���@��@��y@���@�$�@��T@��#@��@��@�&�@��`@��@��@� �@��@���@�V@�@��T@�p�@�G�@�/@��/@��@�ƨ@���@�l�@��@�@��H@���@�V@�5?@��#@��h@�p�@�G�@��`@��u@�b@��@�+@���@��@�ȴ@���@�n�@���@�O�@��@��@��@�I�@�A�@�(�@���@��;@��F@���@�l�@�
=@��R@�M�@�@���@���@��7@�hs@��`@���@�z�@�r�@�I�@�b@��F@�|�@�\)@�C�@�;d@�33@�"�@�"�@��y@���@�^5@�E�@�-@�{@��@��T@���@��h@�7L@��`@���@�1'@���@��F@��@�dZ@�+@�
=@�@���@��R@��\@�E�@�$�@��@�J@��#@�`B@���@��u@�I�@�(�@�(�@�A�@�9X@� �@��>@tj@b��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BdZBdZBdZBdZBdZBdZBe`BdZBe`BgmBhsBjBjBl�Bn�Bo�Bn�Bm�BgmBaHBZBVBZBcTBl�B|�B�7B�PB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�-B�3B�'B�B�B��B��B�'B�^B��B�}B��B�BB�NB�B �B8RBP�BbNBffBhsBn�B~�B�B�+B�Bw�Bm�BgmB\)BXBVBI�B-B�BuBB��B�TB�B�dB��B�+Bu�B\)BA�B-B�BJB
��B
�`B
�}B
��B
�PB
r�B
XB
7LB
hB	�TB	�?B	��B	�B	�=B	|�B	n�B	\)B	M�B	A�B	49B	 �B	�B	hB	VB	B�B�BB�?B��B�B��BĜB�?B��B��B��B�B��B��B�B�'B�-B�-B�3B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�'B�'B�B�B�3B�'B�FB�XB�qBÖBĜBɺBȴBŢBȴB��B��B��B�B�#B�)B�TB�TB�fB�sB�mB�mB�fB�sB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	DB	VB	\B	bB	hB	�B	�B	�B	�B	�B	%�B	)�B	,B	0!B	33B	5?B	6FB	9XB	8RB	6FB	7LB	A�B	C�B	C�B	C�B	D�B	E�B	F�B	G�B	J�B	K�B	N�B	P�B	R�B	W
B	W
B	W
B	W
B	XB	W
B	W
B	^5B	_;B	`BB	`BB	bNB	ffB	iyB	k�B	o�B	p�B	q�B	v�B	x�B	x�B	x�B	|�B	~�B	�B	�B	�B	�B	�B	� B	~�B	�B	�+B	�7B	�=B	�=B	�=B	�JB	�\B	�oB	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�FB	�XB	�^B	�^B	�^B	�dB	�qB	�wB	��B	��B	��B	��B	��B	B	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�;B	�NB	�TB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
	7B
	7B
	7B
	7B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
JB
PB
PB
VB
\B
\B
\B
\B
bB
hB
hB
hB
hB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
dB
)�B
6+2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 BdZBdZBdZBdZBdZBdZBe`BdZBe`BgmBhsBjBjBl�Bn�Bo�Bn�Bm�BgmBaHBZBVBZBcTBl�B|�B�7B�PB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�-B�3B�'B�B�B��B��B�'B�^B��B�}B��B�BB�NB�B �B8RBP�BbNBffBhsBn�B~�B�B�+B�Bw�Bm�BgmB\)BXBVBI�B-B�BuBB��B�TB�B�dB��B�+Bu�B\)BA�B-B�BJB
��B
�`B
�}B
��B
�PB
r�B
XB
7LB
hB	�TB	�?B	��B	�B	�=B	|�B	n�B	\)B	M�B	A�B	49B	 �B	�B	hB	VB	B�B�BB�?B��B�B��BĜB�?B��B��B��B�B��B��B�B�'B�-B�-B�3B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�'B�'B�B�B�3B�'B�FB�XB�qBÖBĜBɺBȴBŢBȴB��B��B��B�B�#B�)B�TB�TB�fB�sB�mB�mB�fB�sB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	DB	VB	\B	bB	hB	�B	�B	�B	�B	�B	%�B	)�B	,B	0!B	33B	5?B	6FB	9XB	8RB	6FB	7LB	A�B	C�B	C�B	C�B	D�B	E�B	F�B	G�B	J�B	K�B	N�B	P�B	R�B	W
B	W
B	W
B	W
B	XB	W
B	W
B	^5B	_;B	`BB	`BB	bNB	ffB	iyB	k�B	o�B	p�B	q�B	v�B	x�B	x�B	x�B	|�B	~�B	�B	�B	�B	�B	�B	� B	~�B	�B	�+B	�7B	�=B	�=B	�=B	�JB	�\B	�oB	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�FB	�XB	�^B	�^B	�^B	�dB	�qB	�wB	��B	��B	��B	��B	��B	B	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�;B	�NB	�TB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
	7B
	7B
	7B
	7B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
JB
PB
PB
VB
\B
\B
\B
\B
bB
hB
hB
hB
hB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
dB
)�B
6+2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190540                              AO  ARCAADJP                                                                    20181005190540    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190540  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190540  QCF$                G�O�G�O�G�O�8000            