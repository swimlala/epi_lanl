CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:58Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190558  20181005190558  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��it��1   @��j�o�@0�I�^5�c��l�C�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @@  @�  @�33A   A   A@  A`  A~ffA�  A�  A�  A���A���A�  A�  B   B  B  B  B ffB(  B0  B8  B@ffBH  BP  BX  B`ffBh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C�C
�C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*�C,  C-�fC0  C2  C4  C6  C7�fC9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C��C��C�  C�  C��3C�  C�  C��C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  D   D � D  D� D��D� DfD� D  Dy�D  D� D��Dy�D  D� D  D� D	  D	� D
  D
� DfD�fDfD� D  D� D  D� D  D� D��D� DfD�fD  D� D  D� D  Dy�D  D� D��D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D   D � D!  D!� D"  D"� D"��D#y�D$  D$� D%fD%� D&  D&y�D'  D'� D(  D(y�D)  D)� D*  D*� D+  D+� D,  D,y�D,��D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9y�D:  D:� D;  D;� D<  D<� D<��D=� D>  D>� D>��D?� D@  D@y�DA  DA� DA��DB� DC  DC�fDD  DD� DE  DE� DF  DF� DF��DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU�fDV  DV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`fD`�fDa  Da� Da��Db� DcfDc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dg��Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dty�Dt��Du� Dv  Dv� Dw  Dwy�DwٚDy��D�B�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @S33@���@���A��A$��AD��Ad��A���A�ffA�ffA�ffA�33A�33A�ffA�ffB33B	33B33B33B!��B)33B133B933BA��BI33BQ33BY33Ba��Bi33Bq33Bx��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�B���Bؙ�Bܙ�B���B䙚B虚B왚B�B���B�fgB���C L�CL�CL�CL�CfgC
fgCL�CL�CL�CL�CL�CL�CL�CL�CL�C33C L�C"L�C$L�C&L�C(L�C*fgC,L�C.33C0L�C2L�C4L�C6L�C833C:33C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CV33CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC��C�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC��C�33C�33C�&fC�&fC��C�&fC�&fC�33C�33C�33C�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�33C�&fC�&fC�&fC�&fC�&fD 3D �3D3D�3D�D�3D�D�3D3D��D3D�3D�D��D3D�3D3D�3D	3D	�3D
3D
�3D�D��D�D�3D3D�3D3D�3D3D�3D�D�3D�D��D3D�3D3D�3D3D��D3D�3D�D�3D�D��D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D�D�3D 3D �3D!3D!�3D"3D"�3D#�D#��D$3D$�3D%�D%�3D&3D&��D'3D'�3D(3D(��D)3D)�3D*3D*�3D+3D+�3D,3D,��D-�D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2��D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D9�D9��D:3D:�3D;3D;�3D<3D<�3D=�D=�3D>3D>�3D?�D?�3D@3D@��DA3DA�3DB�DB�3DC3DC��DD3DD�3DE3DE�3DF3DF�3DG�DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO�DO��DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT��DU3DU��DV3DV�3DW3DW�3DX�DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_��D`�D`��Da3Da�3Db�Db�3Dc�Dc�3Dd3Dd�3De�De�3Df3Df�3Dg3Dg�3Dh�Dh�3Di�Di�3Dj3Dj�3Dk3Dk�3Dl3Dl��Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt�Dt��Du�Du�3Dv3Dv�3Dw3Dw��Dw��Dy��D�L{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A�A�A�%A�%A�%A�%A�1A�%A�1A�1A�
=A�
=A�JA�VA�oA�{A��A�VA��TA�ƨAȶFAț�AȃA�v�A�hsA�?}A��A�VA���Aǧ�A�v�A�1AƉ7A�ffA�n�A�O�A�1A��A��A��AöFAÍPA�"�A��
A7A�jA��A���A�`BA�(�A��A�O�A�(�A��uA�Q�A�  A�/A��A��A���A���A�^5A�ƨA��A���A�ƨA���A��A�dZA��TA�1'A��jA��
A�%A���A�=qA�bA��jA�O�A���A�S�A�VA�oA��RA���A�hsA��A�S�A��+A�oA��A��A��A��
A��^A�I�A�9XA��A�M�A�-A��;A�K�A�ȴA�ZA�I�A{AuXAs�TAs\)Ap9XAl1Ai�#AgS�Ad��A`�jA^JA\�A[�A[�AU��AP�ANr�AL9XAH��AF��AE�ADQ�AA��A>��A:�RA9VA6ȴA3%A0��A/��A/�A.M�A-�A+t�A*��A*VA'��A%��A$r�A#�A"�A!
=A 9XAAhsA��A��AM�A��AdZA��AI�AA$�A�Ax�AoAS�A��A+A�TA��Ax�A
�\AjAbNA�#A\)A��AjA1'A{A  �@�A VA r�A b@���@��@��!@�G�@�9X@��@��w@�C�@�{@��-@�7L@��;@�@���@@��@��m@�C�@�=q@�r�@���@�E�@��T@�O�@��`@�@�C�@�-@��@���@އ+@��#@�O�@�b@�o@�=q@��@؋D@�9X@���@��@�@ԃ@Ӯ@�l�@�|�@�dZ@�M�@щ7@ЋD@��m@�|�@Η�@�x�@��@���@�9X@���@��@� �@� �@ˍP@�ȴ@ɲ-@�?}@�&�@�V@���@ȴ9@���@�5?@��#@�x�@�hs@�/@���@��m@�ff@�o@�ȴ@�G�@��@�z�@��D@�ƨ@�l�@�C�@�ȴ@��!@��@���@�{@�X@��@���@��w@�"�@���@�M�@�$�@�{@�J@���@��@�7L@���@��/@���@��`@�7L@���@��@���@�ƨ@��@�S�@�l�@�l�@�dZ@��!@�ff@��^@�&�@���@��D@�Z@�  @�|�@��H@�$�@�{@���@�p�@��u@�b@�+@���@�o@��y@���@�$�@�7L@��`@��@��@��P@���@�+@���@�|�@���@�t�@�;d@��@�@���@�^5@�=q@���@��@��/@��9@�bN@� �@��
@�ƨ@���@�
=@��!@���@�E�@���@��@��h@�G�@��@��@�Z@��m@�ƨ@�+@���@��\@�K�@���@�K�@�K�@�+@���@�~�@���@�@��\@��-@��@���@�z�@��D@���@�j@��;@�l�@�
=@���@�ff@�-@�$�@�@���@��^@���@�7L@�V@���@�Z@� �@��
@��@�S�@��@�@��R@�v�@��@�X@��@�Z@�b@��w@�K�@��R@�ff@�^5@�=q@��h@�hs@���@��@��@�j@�Q�@�ƨ@�@��R@�M�@��T@���@��@�Ĝ@�j@�j@�I�@��;@�K�@�C�@���@�v�@�^5@�J@���@��h@�X@���@� �@�  @�ƨ@��w@��F@���@�S�@�|�@�K�@�o@���@���@�^5@�5?@��T@��7@��@�Ĝ@�r�@�(�@�ƨ@���@�\)@�dZ@�C�@�
=@���@���@�n�@�V@�{@��#@���@��h@�x�@�X@�G�@��@���@��`@���@���@�Ĝ@��@��u@�I�@�1@�=@tA�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A�A�A�%A�%A�%A�%A�1A�%A�1A�1A�
=A�
=A�JA�VA�oA�{A��A�VA��TA�ƨAȶFAț�AȃA�v�A�hsA�?}A��A�VA���Aǧ�A�v�A�1AƉ7A�ffA�n�A�O�A�1A��A��A��AöFAÍPA�"�A��
A7A�jA��A���A�`BA�(�A��A�O�A�(�A��uA�Q�A�  A�/A��A��A���A���A�^5A�ƨA��A���A�ƨA���A��A�dZA��TA�1'A��jA��
A�%A���A�=qA�bA��jA�O�A���A�S�A�VA�oA��RA���A�hsA��A�S�A��+A�oA��A��A��A��
A��^A�I�A�9XA��A�M�A�-A��;A�K�A�ȴA�ZA�I�A{AuXAs�TAs\)Ap9XAl1Ai�#AgS�Ad��A`�jA^JA\�A[�A[�AU��AP�ANr�AL9XAH��AF��AE�ADQ�AA��A>��A:�RA9VA6ȴA3%A0��A/��A/�A.M�A-�A+t�A*��A*VA'��A%��A$r�A#�A"�A!
=A 9XAAhsA��A��AM�A��AdZA��AI�AA$�A�Ax�AoAS�A��A+A�TA��Ax�A
�\AjAbNA�#A\)A��AjA1'A{A  �@�A VA r�A b@���@��@��!@�G�@�9X@��@��w@�C�@�{@��-@�7L@��;@�@���@@��@��m@�C�@�=q@�r�@���@�E�@��T@�O�@��`@�@�C�@�-@��@���@އ+@��#@�O�@�b@�o@�=q@��@؋D@�9X@���@��@�@ԃ@Ӯ@�l�@�|�@�dZ@�M�@щ7@ЋD@��m@�|�@Η�@�x�@��@���@�9X@���@��@� �@� �@ˍP@�ȴ@ɲ-@�?}@�&�@�V@���@ȴ9@���@�5?@��#@�x�@�hs@�/@���@��m@�ff@�o@�ȴ@�G�@��@�z�@��D@�ƨ@�l�@�C�@�ȴ@��!@��@���@�{@�X@��@���@��w@�"�@���@�M�@�$�@�{@�J@���@��@�7L@���@��/@���@��`@�7L@���@��@���@�ƨ@��@�S�@�l�@�l�@�dZ@��!@�ff@��^@�&�@���@��D@�Z@�  @�|�@��H@�$�@�{@���@�p�@��u@�b@�+@���@�o@��y@���@�$�@�7L@��`@��@��@��P@���@�+@���@�|�@���@�t�@�;d@��@�@���@�^5@�=q@���@��@��/@��9@�bN@� �@��
@�ƨ@���@�
=@��!@���@�E�@���@��@��h@�G�@��@��@�Z@��m@�ƨ@�+@���@��\@�K�@���@�K�@�K�@�+@���@�~�@���@�@��\@��-@��@���@�z�@��D@���@�j@��;@�l�@�
=@���@�ff@�-@�$�@�@���@��^@���@�7L@�V@���@�Z@� �@��
@��@�S�@��@�@��R@�v�@��@�X@��@�Z@�b@��w@�K�@��R@�ff@�^5@�=q@��h@�hs@���@��@��@�j@�Q�@�ƨ@�@��R@�M�@��T@���@��@�Ĝ@�j@�j@�I�@��;@�K�@�C�@���@�v�@�^5@�J@���@��h@�X@���@� �@�  @�ƨ@��w@��F@���@�S�@�|�@�K�@�o@���@���@�^5@�5?@��T@��7@��@�Ĝ@�r�@�(�@�ƨ@���@�\)@�dZ@�C�@�
=@���@���@�n�@�V@�{@��#@���@��h@�x�@�X@�G�@��@���@��`@���@���@�Ĝ@��@��u@�I�@�1@�=@tA�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B]/B\)B]/B]/B]/B]/B]/B^5B^5B^5B_;B^5B_;B_;B_;B_;B_;B`BBbNBffBiyBt�B��B�wB�B��B	  B	%B	DB	�B	)�B	6FB	Q�B	dZB	y�B	��B	��B
�B
5?B
hsB
s�B
�DB
��B
�B
ÖB
��B
�`B
�BB+B�B>wBO�BZBu�B�B�PB��B�3B�qB�
BB!�B'�B-B1'B6FBB�BI�BJ�B@�B<jB/B)�B'�B'�B&�B'�B%�B#�B"�B!�B�BB��B�B�mB�B��BŢB�}B�'B�oBn�BXB49B!�BB
��B
�B
�5B
��B
��B
�PB
y�B
]/B
;dB
!�B	��B	��B	��B	�=B	�B	o�B	VB	G�B	9XB	'�B	oB	B��B��B�B�B�wB�FB�B��B��B��B��B��B�uB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�'B�'B�'B�!B�B�'B�^B�^B�^BĜBɺB��B��B��B��BŢB�9B�wBBB��B��BɺB��B�?B�'BÖB�B�
B��B��B��B��B��B��B��BȴBƨBĜBÖBBƨBȴBɺBǮBƨBɺB��B��B�B�/B�5B�;B�HB�HB�NB�NB�NB�NB�TB�TB�ZB�yB�B�B�B�B�B��B��B��B��B	B	B	%B		7B		7B		7B	+B	DB	JB	PB	hB	{B	�B	�B	�B	$�B	'�B	)�B	/B	49B	:^B	<jB	<jB	=qB	=qB	=qB	>wB	>wB	=qB	>wB	>wB	?}B	>wB	=qB	:^B	@�B	B�B	>wB	@�B	K�B	M�B	N�B	P�B	P�B	P�B	Q�B	VB	YB	]/B	^5B	^5B	_;B	dZB	gmB	l�B	n�B	o�B	o�B	o�B	o�B	o�B	s�B	u�B	v�B	w�B	{�B	�B	�B	�B	�B	�B	�B	�B	�%B	�=B	�oB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�FB	�LB	�RB	�qB	��B	��B	B	ÖB	ĜB	ŢB	ǮB	ǮB	ǮB	ȴB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�TB	�mB	�mB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
	7B
	7B

=B
DB

=B

=B
JB
JB
PB
PB
PB
PB
\B
bB
bB
bB
bB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
"�B
"�B
"�B
"�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
%�B
$ZB
6+222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B]/B\)B]/B]/B]/B]/B]/B^5B^5B^5B_;B^5B_;B_;B_;B_;B_;B`BBbNBffBiyBt�B��B�wB�B��B	  B	%B	DB	�B	)�B	6FB	Q�B	dZB	y�B	��B	��B
�B
5?B
hsB
s�B
�DB
��B
�B
ÖB
��B
�`B
�BB+B�B>wBO�BZBu�B�B�PB��B�3B�qB�
BB!�B'�B-B1'B6FBB�BI�BJ�B@�B<jB/B)�B'�B'�B&�B'�B%�B#�B"�B!�B�BB��B�B�mB�B��BŢB�}B�'B�oBn�BXB49B!�BB
��B
�B
�5B
��B
��B
�PB
y�B
]/B
;dB
!�B	��B	��B	��B	�=B	�B	o�B	VB	G�B	9XB	'�B	oB	B��B��B�B�B�wB�FB�B��B��B��B��B��B�uB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�'B�'B�'B�!B�B�'B�^B�^B�^BĜBɺB��B��B��B��BŢB�9B�wBBB��B��BɺB��B�?B�'BÖB�B�
B��B��B��B��B��B��B��BȴBƨBĜBÖBBƨBȴBɺBǮBƨBɺB��B��B�B�/B�5B�;B�HB�HB�NB�NB�NB�NB�TB�TB�ZB�yB�B�B�B�B�B��B��B��B��B	B	B	%B		7B		7B		7B	+B	DB	JB	PB	hB	{B	�B	�B	�B	$�B	'�B	)�B	/B	49B	:^B	<jB	<jB	=qB	=qB	=qB	>wB	>wB	=qB	>wB	>wB	?}B	>wB	=qB	:^B	@�B	B�B	>wB	@�B	K�B	M�B	N�B	P�B	P�B	P�B	Q�B	VB	YB	]/B	^5B	^5B	_;B	dZB	gmB	l�B	n�B	o�B	o�B	o�B	o�B	o�B	s�B	u�B	v�B	w�B	{�B	�B	�B	�B	�B	�B	�B	�B	�%B	�=B	�oB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�FB	�LB	�RB	�qB	��B	��B	B	ÖB	ĜB	ŢB	ǮB	ǮB	ǮB	ȴB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�TB	�mB	�mB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
	7B
	7B

=B
DB

=B

=B
JB
JB
PB
PB
PB
PB
\B
bB
bB
bB
bB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
"�B
"�B
"�B
"�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
%�B
$ZB
6+222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190558                              AO  ARCAADJP                                                                    20181005190558    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190558  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190558  QCF$                G�O�G�O�G�O�8000            