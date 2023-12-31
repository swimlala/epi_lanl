CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:42Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181024140842  20181024140842  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��d�Q��1   @��eW:۬@5=/��w�d�hr�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C��3C�  C��C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C��D fD �fD  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D��D� D  D� D��Dy�D��Dy�D  D� D  D� D  D� D��D� D��D� D  Dy�D  D� D��D� D  D� D  D� D  D�fD  Dy�D  D� D��Dy�D   D � D ��D!y�D!��D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D+��D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5�fD6  D6� D7  D7� D8  D8� D9  D9� D9��D:y�D;  D;� D<  D<� D=  D=� D>  D>� D?  D?�fD@  D@� D@��DA� DB  DBy�DB��DC� DD  DD� DE  DE� DFfDF� DF��DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DPy�DQ  DQ� DR  DR� DSfDS� DT  DT� DT��DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZy�DZ��D[� D\  D\� D\��D]y�D]��D^y�D_  D_� D`  D`� Da  Da� Db  Db� DcfDc�fDdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Diy�Dj  Dj� Dk  Dk� Dl  Dl�fDmfDm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr�fDsfDs�fDtfDt�fDu  Du� Dv  Dv� Dv��Dw� Dx  DxFfDy��D�)�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�34@ə�A��A$��AD��Ad��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB��B	33B33B33B!33B)33B133B933BA33BI33BQ��BY33Ba33Bi33Bq33By33B���B���B���B���B�fgB�fgB���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*33C,L�C.fgC0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\fgC^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|fgC~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�33C�&fC�&fC��C��C�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC��C�&fC��C�&fC�33C�33C�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�33C�&fC�&fC�&fC�&fC�33D �D ��D3D�3D3D�3D3D�3D3D�3D�D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D�D�3D3D�3D�D��D�D��D3D�3D3D�3D3D�3D�D�3D�D�3D3D��D3D�3D�D�3D3D�3D3D�3D3D��D3D��D3D�3D�D��D 3D �3D!�D!��D"�D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,�D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D5�D5��D63D6�3D73D7�3D83D8�3D93D9�3D:�D:��D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?��D@3D@�3DA�DA�3DB3DB��DC�DC�3DD3DD�3DE3DE�3DF�DF�3DG�DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP�DP��DQ3DQ�3DR3DR�3DS�DS�3DT3DT�3DU�DU�3DV3DV��DW3DW�3DX3DX�3DY3DY�3DZ3DZ��D[�D[�3D\3D\�3D]�D]��D^�D^��D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc�Dc��Dd�Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh��Di3Di��Dj3Dj�3Dk3Dk�3Dl3Dl��Dm�Dm��Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr�Dr��Ds�Ds��Dt�Dt��Du3Du�3Dv3Dv�3Dw�Dw�3Dx3DxY�Dy��D�3�D��41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AΗ�AΗ�AΩ�AθRA���Aκ^A���A���A��yA�A�A�hsA�?}Aͥ�A�jA�;dA�  A�{A�$�A�9XA�p�A���A�\)A�I�A�&�A���A�x�A�33A���A�|�A�$�A�C�A�ƨA�p�A�O�A�7LAɥ�A�r�A�p�A���A���A7A���A���A���A�`BA��A�  A�p�A�dZA�+A�K�A�ĜA���A���A�JA���A��-A�
=A�p�A�A�;dA��A��A��wA��A��hA�$�A��uA��A�
=A�t�A�`BA��yA�bNA�JA�n�A�33A��A��FA��A���A�ĜA��!A���A�VA�~�A��A�S�A���A���A��DA��RA�\)A��A�A��A�A���A��HA�x�A�jA��jA�&�A��^A���A�VA�I�A}�A{�FAyoAx1AuC�AlI�AidZAh�Ag�mAg"�AfZAc��Aa"�A_�hA^�/A^�9A^�A\��AZ$�AY�#AX�uATffAO��AM�^AM33ALI�AJ��AIC�AH$�AFȴAE�mAD�`AD�9ADQ�AC�AC�7ABbNA@�HA@JA>��A>1'A<�+A<{A:��A7��A7VA6r�A6bA5�-A4��A2��A0A-�mA-dZA,�jA+�FA*9XA)dZA($�A%�A#�A#�A"�A!�#A!K�A ȴA�^Az�A��A��A�uAoAM�At�A{A��A�RAz�A  A7LA/A�/A(�A�HA�DAv�A5?A$�A�
A�A��A
  A	
=AA��A�A��A+A�A�jA�uAA�AdZA�uAoA ��A ��A 1@�/@�  @��P@�|�@�l�@�t�@�t�@�"�@�v�@�z�@�\)@��`@��D@�1'@���@�|�@�+@�-@�@�S�@�=q@��@�K�@�P@���@�Q�@�P@��m@���@ް!@۶F@ڏ\@�=q@���@��@�Z@֏\@��y@�dZ@��y@�ff@�J@�V@��
@�C�@҇+@��@�|�@�~�@�@�hs@�Ĝ@̣�@�r�@�I�@˝�@ʇ+@��T@�&�@Ȭ@Ų-@��h@�9X@�ƨ@�o@���@�@��@�b@�
=@���@��@���@�
=@�hs@���@���@�n�@��@���@���@���@�1@��;@�dZ@�5?@���@�&�@�V@��/@�I�@��m@�I�@��9@�+@��@��D@�O�@��@���@��y@��F@���@�t�@�\)@�K�@�dZ@�S�@�
=@��@��F@�r�@�b@�K�@�o@�@��@���@�@�@�{@��@��#@��-@�X@��@��@��@�Z@�b@���@�+@�
=@��@��R@�~�@�-@���@�G�@���@��@�I�@�1@��;@�ƨ@��@���@���@���@���@��R@�v�@�E�@���@���@��h@�hs@��@��@�Z@�Q�@�A�@��@���@�l�@�C�@���@���@�V@��@��h@�p�@�X@�?}@���@��`@��9@��D@��u@�Q�@�(�@��m@���@��w@��@��@��P@�
=@�@�ȴ@�M�@�-@�J@�@�{@��@�x�@��h@��@�`B@�O�@�?}@��@��`@��u@��@�z�@�r�@�j@�bN@�Z@�Q�@��@��
@�C�@��@��R@���@��\@���@�~�@��@���@�p�@�&�@��/@���@�(�@��w@���@��@�;d@�o@��!@�n�@�M�@�-@�-@��@��#@���@�p�@�/@���@��j@�bN@�9X@�1@��w@���@�t�@�33@�
=@��H@���@��+@�^5@�M�@�{@��-@�hs@�?}@��@���@��@�A�@���@��@��F@�dZ@�K�@�+@��@�n�@�E�@��T@���@�X@�/@���@���@���@vC�@hh�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AΗ�AΗ�AΩ�AθRA���Aκ^A���A���A��yA�A�A�hsA�?}Aͥ�A�jA�;dA�  A�{A�$�A�9XA�p�A���A�\)A�I�A�&�A���A�x�A�33A���A�|�A�$�A�C�A�ƨA�p�A�O�A�7LAɥ�A�r�A�p�A���A���A7A���A���A���A�`BA��A�  A�p�A�dZA�+A�K�A�ĜA���A���A�JA���A��-A�
=A�p�A�A�;dA��A��A��wA��A��hA�$�A��uA��A�
=A�t�A�`BA��yA�bNA�JA�n�A�33A��A��FA��A���A�ĜA��!A���A�VA�~�A��A�S�A���A���A��DA��RA�\)A��A�A��A�A���A��HA�x�A�jA��jA�&�A��^A���A�VA�I�A}�A{�FAyoAx1AuC�AlI�AidZAh�Ag�mAg"�AfZAc��Aa"�A_�hA^�/A^�9A^�A\��AZ$�AY�#AX�uATffAO��AM�^AM33ALI�AJ��AIC�AH$�AFȴAE�mAD�`AD�9ADQ�AC�AC�7ABbNA@�HA@JA>��A>1'A<�+A<{A:��A7��A7VA6r�A6bA5�-A4��A2��A0A-�mA-dZA,�jA+�FA*9XA)dZA($�A%�A#�A#�A"�A!�#A!K�A ȴA�^Az�A��A��A�uAoAM�At�A{A��A�RAz�A  A7LA/A�/A(�A�HA�DAv�A5?A$�A�
A�A��A
  A	
=AA��A�A��A+A�A�jA�uAA�AdZA�uAoA ��A ��A 1@�/@�  @��P@�|�@�l�@�t�@�t�@�"�@�v�@�z�@�\)@��`@��D@�1'@���@�|�@�+@�-@�@�S�@�=q@��@�K�@�P@���@�Q�@�P@��m@���@ް!@۶F@ڏ\@�=q@���@��@�Z@֏\@��y@�dZ@��y@�ff@�J@�V@��
@�C�@҇+@��@�|�@�~�@�@�hs@�Ĝ@̣�@�r�@�I�@˝�@ʇ+@��T@�&�@Ȭ@Ų-@��h@�9X@�ƨ@�o@���@�@��@�b@�
=@���@��@���@�
=@�hs@���@���@�n�@��@���@���@���@�1@��;@�dZ@�5?@���@�&�@�V@��/@�I�@��m@�I�@��9@�+@��@��D@�O�@��@���@��y@��F@���@�t�@�\)@�K�@�dZ@�S�@�
=@��@��F@�r�@�b@�K�@�o@�@��@���@�@�@�{@��@��#@��-@�X@��@��@��@�Z@�b@���@�+@�
=@��@��R@�~�@�-@���@�G�@���@��@�I�@�1@��;@�ƨ@��@���@���@���@���@��R@�v�@�E�@���@���@��h@�hs@��@��@�Z@�Q�@�A�@��@���@�l�@�C�@���@���@�V@��@��h@�p�@�X@�?}@���@��`@��9@��D@��u@�Q�@�(�@��m@���@��w@��@��@��P@�
=@�@�ȴ@�M�@�-@�J@�@�{@��@�x�@��h@��@�`B@�O�@�?}@��@��`@��u@��@�z�@�r�@�j@�bN@�Z@�Q�@��@��
@�C�@��@��R@���@��\@���@�~�@��@���@�p�@�&�@��/@���@�(�@��w@���@��@�;d@�o@��!@�n�@�M�@�-@�-@��@��#@���@�p�@�/@���@��j@�bN@�9X@�1@��w@���@�t�@�33@�
=@��H@���@��+@�^5@�M�@�{@��-@�hs@�?}@��@���@��@�A�@���@��@��F@�dZ@�K�@�+@��@�n�@�E�@��T@���@�X@�/@���@���@���@vC�@hh�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B!�B-BA�BL�BP�B#�B#�B'�B.B9XB>wBC�BO�BdZB�BȴB�)B�/B�BB�B��B	7B�B�BoB{B�B �B1'B5?B5?B5?B8RB?}BC�BG�BG�BH�BH�BL�BL�BR�BT�BR�BP�BL�BK�BR�BaHBq�B�PB�1B�B� B|�Bs�B`BBXBR�BP�BM�BJ�BD�BA�B?}B<jB7LB5?B<jBD�B[#B|�Bu�Br�Be`BR�B=qB33B!�B�B��B�HBȴB�RB��B{�BbNBC�BPB
��B
�`B
�B
��B
��B
��B
��B
�hB
�B
k�B
YB
2-B
'�B
\B	��B	�;B	��B	�7B	�B	|�B	u�B	m�B	]/B	K�B	D�B	@�B	>wB	;dB	6FB	,B	(�B	�B	+B�B�B��B��B	hB	PB	+B	B	B��B��B��B��B��B��B��B��B�B�B�B�B�`B�)B�B�B��B��B��B��B�FB�'B�B�B��B��B��B��B��B��B��B��B�uB�oB�oB�uB�bB�VB�=B�+B�B�B�B�+B�7B�1B�1B�+B�B� B�B�B�B�B�B�B�B� B{�Bs�BjBjBe`BaHB\)BW
BT�BT�BS�BS�BR�BP�BO�BO�BN�BM�BM�BO�BS�BYBZB[#BaHBt�B~�B�JB�%B{�Bz�By�By�Bx�By�B{�B|�B{�B� B�B�B}�B�B~�B� B� B�%B�DB�B{�By�By�Bx�Bw�By�B�B�hB��B��B��B��B��B��B��B��B��B�B�B�!B�FB�LB�LB�LB�FB�?B�LB�FB�?B�-B��B��B�bB�%B�1B�\B�bB�uB�{B�oB�uB�uB�uB�{B��B��B��B��B��B��B��B��B�B�B�!B�dBȴB��B��B�
B�5B�sB�B�B	JB	{B	�B	�B	%�B	-B	2-B	;dB	=qB	=qB	?}B	A�B	H�B	M�B	R�B	VB	ZB	_;B	e`B	e`B	e`B	dZB	dZB	cTB	aHB	cTB	gmB	iyB	jB	k�B	m�B	o�B	q�B	q�B	r�B	t�B	v�B	w�B	x�B	x�B	y�B	{�B	}�B	�B	�B	�B	�+B	�1B	�7B	�=B	�=B	�bB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�-B	�3B	�9B	�?B	�FB	�RB	�XB	�jB	�qB	�wB	�wB	�}B	�}B	�}B	��B	B	B	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�;B	�HB	�NB	�NB	�TB	�NB	�NB	�`B	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
+B
+B
	7B
DB
DB
JB
JB
JB
JB
PB
PB
VB
VB
\B
\B
bB
bB
hB
oB
oB
{B
@B
�B
-�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B!�B-BA�BL�BP�B#�B#�B'�B.B9XB>wBC�BO�BdZB�BȴB�)B�/B�BB�B��B	7B�B�BoB{B�B �B1'B5?B5?B5?B8RB?}BC�BG�BG�BH�BH�BL�BL�BR�BT�BR�BP�BL�BK�BR�BaHBq�B�PB�1B�B� B|�Bs�B`BBXBR�BP�BM�BJ�BD�BA�B?}B<jB7LB5?B<jBD�B[#B|�Bu�Br�Be`BR�B=qB33B!�B�B��B�HBȴB�RB��B{�BbNBC�BPB
��B
�`B
�B
��B
��B
��B
��B
�hB
�B
k�B
YB
2-B
'�B
\B	��B	�;B	��B	�7B	�B	|�B	u�B	m�B	]/B	K�B	D�B	@�B	>wB	;dB	6FB	,B	(�B	�B	+B�B�B��B��B	hB	PB	+B	B	B��B��B��B��B��B��B��B��B�B�B�B�B�`B�)B�B�B��B��B��B��B�FB�'B�B�B��B��B��B��B��B��B��B��B�uB�oB�oB�uB�bB�VB�=B�+B�B�B�B�+B�7B�1B�1B�+B�B� B�B�B�B�B�B�B�B� B{�Bs�BjBjBe`BaHB\)BW
BT�BT�BS�BS�BR�BP�BO�BO�BN�BM�BM�BO�BS�BYBZB[#BaHBt�B~�B�JB�%B{�Bz�By�By�Bx�By�B{�B|�B{�B� B�B�B}�B�B~�B� B� B�%B�DB�B{�By�By�Bx�Bw�By�B�B�hB��B��B��B��B��B��B��B��B��B�B�B�!B�FB�LB�LB�LB�FB�?B�LB�FB�?B�-B��B��B�bB�%B�1B�\B�bB�uB�{B�oB�uB�uB�uB�{B��B��B��B��B��B��B��B��B�B�B�!B�dBȴB��B��B�
B�5B�sB�B�B	JB	{B	�B	�B	%�B	-B	2-B	;dB	=qB	=qB	?}B	A�B	H�B	M�B	R�B	VB	ZB	_;B	e`B	e`B	e`B	dZB	dZB	cTB	aHB	cTB	gmB	iyB	jB	k�B	m�B	o�B	q�B	q�B	r�B	t�B	v�B	w�B	x�B	x�B	y�B	{�B	}�B	�B	�B	�B	�+B	�1B	�7B	�=B	�=B	�bB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�-B	�3B	�9B	�?B	�FB	�RB	�XB	�jB	�qB	�wB	�wB	�}B	�}B	�}B	��B	B	B	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�;B	�HB	�NB	�NB	�TB	�NB	�NB	�`B	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
+B
+B
	7B
DB
DB
JB
JB
JB
JB
PB
PB
VB
VB
\B
\B
bB
bB
hB
oB
oB
{B
@B
�B
-�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140842                              AO  ARCAADJP                                                                    20181024140842    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140842  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140842  QCF$                G�O�G�O�G�O�0               