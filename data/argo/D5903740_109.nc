CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-17T09:15:38Z AOML 3.0 creation; 2016-06-01T00:08:23Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150317091538  20160531170823  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               mA   AO  4055_7112_109                   2C  D   APEX                            5374                            041511                          846 @�BW�1   @�B��d	@:.��+�d3n��P1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    mA   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  Dy�D  D� D  D� D  D� D  Dy�D��Dy�D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy� D�3D�P D�|�D���D��D�C3D�vfD���D��D�I�D�vfD��3D�	�D�0 Dڐ D��fD��fD�9�D�l�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @S33@���@ə�A��A$��AD��Ad��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B	33B33B33B!33B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D��D3D��D3D�3D3D�3D3D�3D3D��D�D��D�D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)�D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D1�D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�fDy�3D��D�Y�D��gD��gD�gD�L�D�� D��gD�gD�S4D�� D���D�4D�9�Dڙ�D�� D�  D�C4D�vgD��4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�JA�A��DA�K�A�A�A�33A�$�A�bA�
=A�
=A�  A���A��A���A��^A��DA�|�A�hsA�O�A���A�
=A���A� �A���A�A��!A��!A��jA���A��TA�jA��PA�dZA�ZA�;dA���A��7A���A��#A�n�A�33A��+A��A�ĜA�33A��;A��RA�dZA�A�A�|�A�Q�A���A�5?A�33A�A�^5A��A��A�v�A��!A��;A�A|bAz�Ay��Av�yAv=qAu�
At��At=qAs|�Ar�HAr��Arr�ArAq��AqXAp��ApZAnz�Am��Am
=Al�`Al�Al�Al  Ak�AkXAj�Aj�+Ah��Ag�7Af��AfE�Ad�9Ac&�AbVAa`BA_hsA]�^A[��AY/AX5?AW�;AWO�AVȴAVVAV5?AV  AT�+ASG�ARE�AP��ANȴAL�RAK��AI+AH��AH�DAHz�AHbNAHI�AG&�AFA�AE��AE��ADĜADQ�AC�7AA�
A@v�A>��A>1'A=�#A=��A<�/A;\)A:M�A9��A9p�A8ȴA8JA7A6��A6�9A6E�A533A3�;A3�7A2�`A2��A2��A2�A133A09XA/��A/+A.��A.v�A-�FA-��A-��A-�A-t�A-l�A,��A++A*bA)�A(��A&��A%VA$^5A#�A#�PA"��A"M�A"$�A!�TA!�-A!��A!��A!x�A!�A ��A -A��A�!A1'AA�A�jA��A�FA�AA�AdZA��A�`A�/A�A�RA1'A��A��AXA%A�A��AM�AG�A�A33A��A�A&�A
ZA	AȴA�A|�A��AffA�#A��AM�AJA��AXAĜA�;A Ĝ@��@�dZ@���@�1@���@�J@�j@�  @���@�?}@�j@�t�@�^5@�x�@��@��@�P@��y@�@��@�+@�@�\)@�&�@�
=@�t�@���@�@�X@��H@ԃ@�S�@�^5@�  @͡�@���@�;d@��@ȃ@�(�@ǍP@���@�hs@ă@�o@��@�A�@�|�@�ff@���@�X@��w@��@��@�x�@�`B@��@���@�bN@� �@��@�ƨ@�|�@��h@�|�@�K�@�@���@�v�@�`B@���@��9@�Z@��m@�33@��R@�=q@��@��@�j@�9X@��
@�|�@�"�@���@��^@��h@�X@���@��@�o@��@� �@�|�@�\)@�K�@�+@��y@�ȴ@�~�@�J@�p�@��u@��@�^5@�@�G�@���@�Q�@��@�\)@���@�E�@�{@�X@��`@� �@���@�
=@�J@���@�X@��`@���@���@�M�@�E�@�E�@�-@�$�@�@�`B@���@�9X@�|�@�@��@��@���@���@���@��\@�v�@�M�@�E�@�E�@�5?@�{@��@�Z@��;@�ƨ@���@�|�@�S�@�"�@��R@�5?@��@�@��^@��^@�`B@��@��`@���@���@�Ĝ@�bN@�Q�@�  @��@�\)@�K�@�K�@�C�@�33@�"�@���@�M�@��^@��h@���@�Z@�  @�ƨ@�|�@�+@�ff@�-@�$�@��@���@�Ĝ@��D@�j@�(�@�1@�@K�@~��@~��@~ff@}@}`B@|�@|�@|z�@|I�@{�
@{��@{�@{dZ@{C�@{"�@z��@z=q@y�^@yhs@y&�@y%@y%@x��@x�`@x��@xb@w��@w�w@w�@w�P@w+@v�+@vff@vE�@v@u��@u�@tz�@s��@sC�@s33@r��@r-@r�@rJ@q�@q�#@q�^@p��@p��@pĜ@pr�@pb@o�;@o�@o�P@lI�@e?}@`�@ZJ@Q�^@H�`@C�
@>�@9&�@4z�@,9X@'
=@"M�@�j@G�@��@bN@t�@
^5@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�JA�A��DA�K�A�A�A�33A�$�A�bA�
=A�
=A�  A���A��A���A��^A��DA�|�A�hsA�O�A���A�
=A���A� �A���A�A��!A��!A��jA���A��TA�jA��PA�dZA�ZA�;dA���A��7A���A��#A�n�A�33A��+A��A�ĜA�33A��;A��RA�dZA�A�A�|�A�Q�A���A�5?A�33A�A�^5A��A��A�v�A��!A��;A�A|bAz�Ay��Av�yAv=qAu�
At��At=qAs|�Ar�HAr��Arr�ArAq��AqXAp��ApZAnz�Am��Am
=Al�`Al�Al�Al  Ak�AkXAj�Aj�+Ah��Ag�7Af��AfE�Ad�9Ac&�AbVAa`BA_hsA]�^A[��AY/AX5?AW�;AWO�AVȴAVVAV5?AV  AT�+ASG�ARE�AP��ANȴAL�RAK��AI+AH��AH�DAHz�AHbNAHI�AG&�AFA�AE��AE��ADĜADQ�AC�7AA�
A@v�A>��A>1'A=�#A=��A<�/A;\)A:M�A9��A9p�A8ȴA8JA7A6��A6�9A6E�A533A3�;A3�7A2�`A2��A2��A2�A133A09XA/��A/+A.��A.v�A-�FA-��A-��A-�A-t�A-l�A,��A++A*bA)�A(��A&��A%VA$^5A#�A#�PA"��A"M�A"$�A!�TA!�-A!��A!��A!x�A!�A ��A -A��A�!A1'AA�A�jA��A�FA�AA�AdZA��A�`A�/A�A�RA1'A��A��AXA%A�A��AM�AG�A�A33A��A�A&�A
ZA	AȴA�A|�A��AffA�#A��AM�AJA��AXAĜA�;A Ĝ@��@�dZ@���@�1@���@�J@�j@�  @���@�?}@�j@�t�@�^5@�x�@��@��@�P@��y@�@��@�+@�@�\)@�&�@�
=@�t�@���@�@�X@��H@ԃ@�S�@�^5@�  @͡�@���@�;d@��@ȃ@�(�@ǍP@���@�hs@ă@�o@��@�A�@�|�@�ff@���@�X@��w@��@��@�x�@�`B@��@���@�bN@� �@��@�ƨ@�|�@��h@�|�@�K�@�@���@�v�@�`B@���@��9@�Z@��m@�33@��R@�=q@��@��@�j@�9X@��
@�|�@�"�@���@��^@��h@�X@���@��@�o@��@� �@�|�@�\)@�K�@�+@��y@�ȴ@�~�@�J@�p�@��u@��@�^5@�@�G�@���@�Q�@��@�\)@���@�E�@�{@�X@��`@� �@���@�
=@�J@���@�X@��`@���@���@�M�@�E�@�E�@�-@�$�@�@�`B@���@�9X@�|�@�@��@��@���@���@���@��\@�v�@�M�@�E�@�E�@�5?@�{@��@�Z@��;@�ƨ@���@�|�@�S�@�"�@��R@�5?@��@�@��^@��^@�`B@��@��`@���@���@�Ĝ@�bN@�Q�@�  @��@�\)@�K�@�K�@�C�@�33@�"�@���@�M�@��^@��h@���@�Z@�  @�ƨ@�|�@�+@�ff@�-@�$�@��@���@�Ĝ@��D@�j@�(�@�1@�@K�@~��@~��@~ff@}@}`B@|�@|�@|z�@|I�@{�
@{��@{�@{dZ@{C�@{"�@z��@z=q@y�^@yhs@y&�@y%@y%@x��@x�`@x��@xb@w��@w�w@w�@w�P@w+@v�+@vff@vE�@v@u��@u�@tz�@s��@sC�@s33@r��@r-@r�@rJ@q�@q�#@q�^@p��@p��@pĜ@pr�@pb@o�;@o�@o�P@lI�@e?}@`�@ZJ@Q�^@H�`@C�
@>�@9&�@4z�@,9X@'
=@"M�@�j@G�@��@bN@t�@
^5@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBA�BB�BA�B@�BA�B@�B?}B>wB=qB=qB<jB<jB<jB;dB9XB6FB33B33B1'B/B �BhB��B�5B��B�wB�'B�jB��BȴB�XB��B�B��B��BȴB�FB��B�B)�B�BɺB��Bx�Be`BXB?}B-B%�B%�B33B5?BoB�BBBBBBB
�B
�jB
y�B
_;B
_;B
ZB
D�B
=qB
:^B
49B
/B
(�B
$�B
!�B
 �B
�B
�B
{B
bB
	7B	��B	��B	��B	��B	�B	�B	�B	�B	�sB	�ZB	�BB	��B	ƨB	�jB	�FB	��B	�{B	�VB	�+B	z�B	p�B	dZB	XB	S�B	Q�B	N�B	K�B	J�B	I�B	G�B	@�B	:^B	5?B	.B	(�B	"�B	�B	�B	�B	�B	�B	�B	�B	�B	uB	uB	hB	bB	PB	
=B	%B	B��B��B��B��B�B�B�fB�TB�BB�/B�B��B��B��B��B��B��BɺBŢBĜBB��B�qB�dB�jB�jB�dB�XB�dB�dB�jB�qB�wB�wB�^B�9B�!B�B��B��B��B��B��B��B��B��B�{B�{B�uB�oB�oB�hB�bB�oB�\B�DB�+B�B�%B�B�Bz�Bu�Br�Bn�BjBe`BcTBcTBbNBaHB_;B^5B]/B[#BYBVBQ�BO�BL�BI�BG�BE�BC�BA�B?}B=qB;dB9XB8RB7LB5?B33B1'B0!B/B.B,B)�B'�B%�B$�B#�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BhBoBhBbB\B\B\B\BVBhB�B�B�B�B�B�B�B�B�B�B�B!�B"�B#�B&�B(�B(�B,B-B-B-B-B-B,B,B,B,B,B+B0!B9XB:^B;dB;dB<jB>wB?}B?}B>wB?}BA�BB�BB�BC�BD�BD�BE�BF�BG�BG�BM�BN�BN�BN�BO�BS�BXBbNBffBiyBjBjBjBk�Bk�Bk�Bm�Bo�Br�Bx�B|�B}�B�B�B�B�1B�7B�DB�PB�VB�oB�{B��B��B��B��B��B��B��B�B�3B�?B�FB�FB�^B�jB�}B��BBŢBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�5B�NB�TB�ZB�`B�sB�B�B�B��B��B��B��B��B	B	B	B	B	B	1B		7B	JB	\B	oB	uB	uB	uB	{B	{B	�B	�B	#�B	%�B	+B	1'B	49B	6FB	8RB	:^B	A�B	E�B	E�B	J�B	M�B	O�B	Q�B	S�B	W
B	XB	YB	\)B	^5B	_;B	aHB	dZB	ffB	iyB	k�B	l�B	l�B	o�B	p�B	q�B	r�B	r�B	s�B	u�B	w�B	z�B	|�B	}�B	~�B	~�B	� B	� B	�B	�%B	�+B	�1B	�1B	�7B	�=B	�PB	�PB	�VB	�\B	�\B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	��B	�B	�fB	�B
B
{B
�B
$�B
.B
49B
@�B
G�B
M�B
VB
[#B
`BB
ffB
k�B
m�B
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BArBBuBAoB@fBAsB@mB?eB>`B=WB=UB<QB<OB<OB;JB9@B6,B3B3B1B.�B �BKB��B�B��B�^B�B�PB�gBȖB�=B��B��B��BλBȘB�.B��B� B)�B��BɝB�lBx�BeEBW�B?`B,�B%�B%�B3B5"BUBeBB�B �B�BB�B
�tB
�PB
y�B
_#B
_$B
ZB
D�B
=WB
:CB
4B
/B
(�B
$�B
!�B
 �B
�B
~B
cB
KB
	B	��B	��B	��B	��B	�B	�B	�B	�{B	�^B	�DB	�-B	��B	ƒB	�TB	�/B	��B	�jB	�BB	�B	z�B	p�B	dHB	W�B	S�B	Q�B	N�B	K�B	J�B	I�B	G�B	@qB	:LB	5-B	.B	(�B	"�B	�B	�B	�B	{B	}B	|B	qB	oB	dB	fB	XB	QB	AB	
,B	B	�B��B��B��B��B�B�uB�TB�GB�4B�B�
B��B��B��B��B˷BʲBɬBŐBďBB�sB�bB�VB�ZB�[B�VB�GB�TB�UB�YB�dB�iB�iB�PB�,B�B�B��B��B��B��B��B��B�B�uB�oB�mB�gB�cB�cB�\B�TB�`B�NB�9B�B�B�B�B��Bz�Bu�Br�Bn�BjqBeSBcGBcIBb@Ba<B_/B^)B]"B[BYBU�BQ�BO�BL�BI�BG�BE�BC�BAB?sB=MB;YB9NB8GB7@B53B3*B1B/�B/B.B+�B)�B'�B%�B$�B#�B!�B!�B �B�B�B�B�B�BzBvBuBvB�B}B�B�ByB�B�B�B|BpBCBIBAB<BRB6BPBQB.BBB_B�B�B�B�B�B�BwB�B�B�B!�B"�B#�B&�B(�B(�B+�B-B,�B,�B,�B-B+�B+�B+�B+�B+�B*�B0B9JB:LB;UB;WB<]B>hB?nB?pB>gB?mBAxBB~BB�BC�BD�BD�BE�BF�BG�BG�BM�BN�BN�BN�BO�BS�BW�Bb>BfVBigBjnBjnBjlBkuBkuBkuBm�Bo�Br�Bx�B|�B}�B��B��B�
B� B�$B�2B�=B�BB�\B�fB�zB��B��B��B��B��B��B��B�B�)B�2B�3B�HB�TB�fB�lB�zBŋBɣB̶B̷B̷BͼBͽBͽB��B��B��B��B��B��B��B��B�B�6B�>B�AB�GB�ZB�iB�wB�B��B��B��B��B��B	 �B	�B	�B	B	�B	B		B	1B	AB	TB	\B	\B	[B	cB	cB	sB	�B	#�B	%�B	*�B	1
B	4B	6(B	87B	:AB	AmB	E�B	E�B	J�B	M�B	O�B	Q�B	S�B	V�B	W�B	X�B	\B	^B	_B	a*B	d>B	fHB	i\B	kiB	lmB	lnB	oB	p�B	q�B	r�B	r�B	s�B	u�B	w�B	z�B	|�B	}�B	~�B	~�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�2B	�2B	�6B	�?B	�<B	�CB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�jB	��B	�EB	�B
�B
ZB
�B
$�B
-�B
4B
@`B
G�B
M�B
U�B
[B
`B
fCB
k_B
mmB
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.3 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708232016053117082320160531170823  AO  ARCAADJP                                                                    20150317091538    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150317091538  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150317091538  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170823  IP                  G�O�G�O�G�O�                