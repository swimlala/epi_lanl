CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:50Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190550  20181005190550  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i]LO�1   @��i��^�@1�ȴ9X�c�bM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   AA��A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C��3C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D�fD  D� D��Dy�D  D� D��D� D  D� D  D� D  D� D	  D	� D
fD
�fD  D� D  D�fDfD�fDfD� DfD� D��D� D  Dy�D  D� D��D� D  D� D  D�fD  Dy�D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D�fD   D � D ��D!� D"fD"�fD#  D#� D$  D$� D$��D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D+��D,y�D,��D-� D.  D.y�D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4y�D5  D5y�D5��D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDCfDC�fDDfDD�fDEfDE�fDF  DFy�DF��DGy�DH  DH� DH��DIy�DI��DJy�DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DVfDV� DW  DW� DX  DXy�DY  DY� DZ  DZ� D[  D[� D\  D\�fD]  D]� D^fD^� D_  D_� D`  D`�fDa  Da� Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dg��Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds� DtfDt� Du  Du�fDv  Dv� Dw  Dw� Dx  DxL�Dy��D�3�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @ə�A��A$��AFfgAd��A�ffA�ffA���A�ffA�ffA�ffA�ffA�ffB33B	33B33B33B!33B)33B0��B933BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:33C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�33C�33C�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�33C��C�&fC�33C�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fD 3D �3D3D��D3D�3D�D��D3D�3D�D�3D3D�3D3D�3D3D�3D	3D	�3D
�D
��D3D�3D3D��D�D��D�D�3D�D�3D�D�3D3D��D3D�3D�D�3D3D�3D3D��D3D��D3D�3D3D�3D3D�3D3D�3D3D�3D�D�3D3D�3D3D�3D3D��D 3D �3D!�D!�3D"�D"��D#3D#�3D$3D$�3D%�D%��D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+�D+�3D,�D,��D-�D-�3D.3D.��D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D4�D4��D53D5��D6�D6��D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=�D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB��DC�DC��DD�DD��DE�DE��DF3DF��DG�DG��DH3DH�3DI�DI��DJ�DJ��DK3DK�3DL3DL�3DM3DM�3DN�DN�3DO3DO�3DP�DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV�DV�3DW3DW�3DX3DX��DY3DY�3DZ3DZ�3D[3D[�3D\3D\��D]3D]�3D^�D^�3D_3D_�3D`3D`��Da3Da�3Db3Db�3Dc�Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg��Dh�Dh�3Di3Di�3Dj3Dj��Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp��Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt�Dt�3Du3Du��Dv3Dv�3Dw3Dw�3Dx3Dx` Dy��D�=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aơ�Aƣ�AƧ�AƧ�AƩ�AƩ�AƧ�Aơ�A�z�A�33A�bA��A��;A�p�A�K�A�33A�  A��A���A���A���A�Aħ�Aě�Aě�Ağ�AēuAāA�v�AāAď\A���A�~�A�^5A�=qA�&�A��A���A���AìAÏ\AÍPA���A�A�C�A���A�A��!A��A��/A�p�A�5?A��A�|�A�bNA��-A��A��A���A�dZA�
=A��7A�ȴA�jA�t�A�oA��A��A� �A�S�A���A�9XA�5?A�/A���A� �A���A���A��DA�A���A�E�A��yA�XA�\)A�Q�A�ƨA��7A�v�A�ȴA��/A���A���A��#A�A�A���A���A�+A�p�A���A�+A���A��A��7A�(�A�/A��RA��A�-A�  A���A��9A�~�A|��Az�Aw�Ar��Ai|�AcK�Aa�A`�A_l�A\��AZĜAY�AW&�AT��ARE�AQ&�ANĜAIx�AG�AF�jAFQ�AE��ACt�A?��A> �A<�jA8I�A7�hA5��A3t�A1��A0bNA/;dA.^5A-�A-��A+��A)��A'�A&�A&(�A#ƨA!\)AG�A9XAȴA�A�9A�AbA�wAbA�A~�A&�A�!A�
AO�A�A�RAjA�A&�A��A��A�hA�FA
��A	%A��A&�AjA�\A��A��A��AƨA��A�7A �u@���@�x�@��@���@�E�@���@�+@��@�9X@�33@�+@��T@�hs@��@�(�@@�@��`@��@�r�@���@�l�@�@���@�-@��@�I�@��@�@��`@�w@�33@�ȴ@�n�@�-@��@��@�Ĝ@�1@��
@�@�E�@�@��@�9X@�l�@��@ٲ-@�Ĝ@�z�@��m@��@֗�@���@Դ9@� �@��;@�dZ@�33@�
=@�@Гu@��@��@�=q@͡�@��@���@̛�@��;@�
=@�-@���@ɡ�@�X@ȋD@� �@�b@�t�@���@��y@�C�@�dZ@�\)@�dZ@ǍP@ǝ�@ǅ@�C�@���@�V@�J@�n�@Ɵ�@Ə\@��@š�@Ł@���@�j@�(�@��;@þw@Õ�@�t�@��@�$�@�{@���@��@��@��T@��#@���@��-@��@��j@��u@�(�@�|�@�@�~�@�^5@�{@��#@���@�&�@��/@���@��D@���@�j@��@��!@��T@�?}@���@���@��@�\)@�33@�@�O�@�I�@�"�@�X@���@��@�t�@�;d@�"�@��@���@�~�@��@��7@�7L@��j@��@��P@�t�@�@���@��!@��\@��#@���@�p�@���@�1'@��;@��P@�+@���@���@�$�@��@��@��#@��@���@���@�bN@�b@��m@���@��;@�t�@�;d@�+@�@��!@���@��\@�5?@���@�x�@�X@�O�@�?}@��@�V@���@���@��/@��9@��@�9X@�ƨ@�l�@�
=@�n�@�=q@�@��^@�`B@��j@��D@�Z@�|�@��+@�=q@�-@��T@��7@���@��@�(�@�b@�b@�b@��@�1@��w@���@��P@�l�@�dZ@�K�@�+@�
=@��H@���@�V@��T@���@�X@�/@��9@�1'@��@�  @���@�S�@�K�@�33@��+@���@��T@���@�O�@�/@�Ĝ@��D@�Z@�(�@�ƨ@���@�t�@�S�@�C�@�;d@�+@�"�@�@���@���@�@�`B@�V@���@��@��@��@��`@���@��9@��@�r�@�(�@�1@���@�t�@�"�@��R@��\@�=q@�{@�J@���@�X@��`@��j@���@�($111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aơ�Aƣ�AƧ�AƧ�AƩ�AƩ�AƧ�Aơ�A�z�A�33A�bA��A��;A�p�A�K�A�33A�  A��A���A���A���A�Aħ�Aě�Aě�Ağ�AēuAāA�v�AāAď\A���A�~�A�^5A�=qA�&�A��A���A���AìAÏ\AÍPA���A�A�C�A���A�A��!A��A��/A�p�A�5?A��A�|�A�bNA��-A��A��A���A�dZA�
=A��7A�ȴA�jA�t�A�oA��A��A� �A�S�A���A�9XA�5?A�/A���A� �A���A���A��DA�A���A�E�A��yA�XA�\)A�Q�A�ƨA��7A�v�A�ȴA��/A���A���A��#A�A�A���A���A�+A�p�A���A�+A���A��A��7A�(�A�/A��RA��A�-A�  A���A��9A�~�A|��Az�Aw�Ar��Ai|�AcK�Aa�A`�A_l�A\��AZĜAY�AW&�AT��ARE�AQ&�ANĜAIx�AG�AF�jAFQ�AE��ACt�A?��A> �A<�jA8I�A7�hA5��A3t�A1��A0bNA/;dA.^5A-�A-��A+��A)��A'�A&�A&(�A#ƨA!\)AG�A9XAȴA�A�9A�AbA�wAbA�A~�A&�A�!A�
AO�A�A�RAjA�A&�A��A��A�hA�FA
��A	%A��A&�AjA�\A��A��A��AƨA��A�7A �u@���@�x�@��@���@�E�@���@�+@��@�9X@�33@�+@��T@�hs@��@�(�@@�@��`@��@�r�@���@�l�@�@���@�-@��@�I�@��@�@��`@�w@�33@�ȴ@�n�@�-@��@��@�Ĝ@�1@��
@�@�E�@�@��@�9X@�l�@��@ٲ-@�Ĝ@�z�@��m@��@֗�@���@Դ9@� �@��;@�dZ@�33@�
=@�@Гu@��@��@�=q@͡�@��@���@̛�@��;@�
=@�-@���@ɡ�@�X@ȋD@� �@�b@�t�@���@��y@�C�@�dZ@�\)@�dZ@ǍP@ǝ�@ǅ@�C�@���@�V@�J@�n�@Ɵ�@Ə\@��@š�@Ł@���@�j@�(�@��;@þw@Õ�@�t�@��@�$�@�{@���@��@��@��T@��#@���@��-@��@��j@��u@�(�@�|�@�@�~�@�^5@�{@��#@���@�&�@��/@���@��D@���@�j@��@��!@��T@�?}@���@���@��@�\)@�33@�@�O�@�I�@�"�@�X@���@��@�t�@�;d@�"�@��@���@�~�@��@��7@�7L@��j@��@��P@�t�@�@���@��!@��\@��#@���@�p�@���@�1'@��;@��P@�+@���@���@�$�@��@��@��#@��@���@���@�bN@�b@��m@���@��;@�t�@�;d@�+@�@��!@���@��\@�5?@���@�x�@�X@�O�@�?}@��@�V@���@���@��/@��9@��@�9X@�ƨ@�l�@�
=@�n�@�=q@�@��^@�`B@��j@��D@�Z@�|�@��+@�=q@�-@��T@��7@���@��@�(�@�b@�b@�b@��@�1@��w@���@��P@�l�@�dZ@�K�@�+@�
=@��H@���@�V@��T@���@�X@�/@��9@�1'@��@�  @���@�S�@�K�@�33@��+@���@��T@���@�O�@�/@�Ĝ@��D@�Z@�(�@�ƨ@���@�t�@�S�@�C�@�;d@�+@�"�@�@���@���@�@�`B@�V@���@��@��@��@��`@���@��9@��@�r�@�(�@�1@���@�t�@�"�@��R@��\@�=q@�{@�J@���@�X@��`@��j@���@�($111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�jB	�B	�FB	�9B	�!B	�'B	�qB	��B
$�B
ǮB
�#B
��B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B'�B'�B&�B0!B:^BW
B`BBaHBgmBiyBiyBiyBn�Bz�B�{B��B�B�?B�dBĜB��B�sB�B�B��BBPB�B!�B �B&�B'�B(�B'�B'�B(�B0!B:^B=qB?}BC�BF�BN�BR�BQ�BN�BE�BB�BA�B>wB:^B5?B,B"�B�B%B�B�B�'B��B�FBÖBŢB�-B��B�\Bu�B`BBA�BT�BYBP�BG�B=qB49B-B�B+B
�B
�B
��B
�9B
��B
�uB
hsB
8RB
"�B	��B	�ZB	ǮB	��B	D�B	�B	�B	�B	�B	�B	{B	bB	%B��B�B�`B�5B��BɺB��B��B��BȴBÖB�wB�XB�-B�B�B��B��B��B��B��B��B��B�-B�XB��B�qB�wB�jB�RB�!B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�LB�XB�wB��B��B�qB�XB��BĜBƨBƨBƨBŢBŢBǮBǮBĜBB��BÖBŢBɺB��B��B��B��B��B��B��B��B�B�B�B�B�#B�BB�sB�B�B�B�sB�yB�sB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	%B	%B	1B	JB	PB	VB	hB	oB	oB	oB	oB	hB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	$�B	$�B	%�B	'�B	(�B	)�B	0!B	1'B	33B	:^B	@�B	D�B	H�B	N�B	Q�B	Q�B	R�B	R�B	S�B	XB	_;B	dZB	iyB	jB	jB	k�B	m�B	n�B	m�B	n�B	n�B	p�B	s�B	v�B	u�B	u�B	v�B	v�B	w�B	w�B	w�B	w�B	x�B	x�B	x�B	x�B	x�B	z�B	{�B	|�B	}�B	}�B	}�B	�B	�1B	�=B	�=B	�DB	�\B	�hB	�\B	�PB	�VB	�PB	�PB	�PB	�VB	�hB	�{B	��B	�uB	�uB	�hB	�bB	�\B	�hB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�?B	�FB	�^B	�qB	�wB	�}B	�}B	B	ĜB	ƨB	ȴB	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�)B	�BB	�HB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
1B
1B
1B
1B
+B
+B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B

=B
DB
JB
JB
PB
PB
PB
VB
jB
"h222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B	�jB	�B	�FB	�9B	�!B	�'B	�qB	��B
$�B
ǮB
�#B
��B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B'�B'�B&�B0!B:^BW
B`BBaHBgmBiyBiyBiyBn�Bz�B�{B��B�B�?B�dBĜB��B�sB�B�B��BBPB�B!�B �B&�B'�B(�B'�B'�B(�B0!B:^B=qB?}BC�BF�BN�BR�BQ�BN�BE�BB�BA�B>wB:^B5?B,B"�B�B%B�B�B�'B��B�FBÖBŢB�-B��B�\Bu�B`BBA�BT�BYBP�BG�B=qB49B-B�B+B
�B
�B
��B
�9B
��B
�uB
hsB
8RB
"�B	��B	�ZB	ǮB	��B	D�B	�B	�B	�B	�B	�B	{B	bB	%B��B�B�`B�5B��BɺB��B��B��BȴBÖB�wB�XB�-B�B�B��B��B��B��B��B��B��B�-B�XB��B�qB�wB�jB�RB�!B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�LB�XB�wB��B��B�qB�XB��BĜBƨBƨBƨBŢBŢBǮBǮBĜBB��BÖBŢBɺB��B��B��B��B��B��B��B��B�B�B�B�B�#B�BB�sB�B�B�B�sB�yB�sB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	%B	%B	1B	JB	PB	VB	hB	oB	oB	oB	oB	hB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	$�B	$�B	%�B	'�B	(�B	)�B	0!B	1'B	33B	:^B	@�B	D�B	H�B	N�B	Q�B	Q�B	R�B	R�B	S�B	XB	_;B	dZB	iyB	jB	jB	k�B	m�B	n�B	m�B	n�B	n�B	p�B	s�B	v�B	u�B	u�B	v�B	v�B	w�B	w�B	w�B	w�B	x�B	x�B	x�B	x�B	x�B	z�B	{�B	|�B	}�B	}�B	}�B	�B	�1B	�=B	�=B	�DB	�\B	�hB	�\B	�PB	�VB	�PB	�PB	�PB	�VB	�hB	�{B	��B	�uB	�uB	�hB	�bB	�\B	�hB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�?B	�FB	�^B	�qB	�wB	�}B	�}B	B	ĜB	ƨB	ȴB	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�)B	�BB	�HB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
1B
1B
1B
1B
+B
+B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B

=B
DB
JB
JB
PB
PB
PB
VB
jB
"h222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190550                              AO  ARCAADJP                                                                    20181005190550    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190550  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190550  QCF$                G�O�G�O�G�O�8000            