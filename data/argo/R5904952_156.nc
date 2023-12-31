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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190540  20181005190540  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��iSo�.1   @��j[,@0��^5?}�cΰ ě�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�ff@�  @���AffA@  A`  A�  A�  A���A���A�  A�  A�  A�  B   B  B  B  B   B(  B/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C��3C�  C��C�  C��3C��3C��3C��3C��3C��3C�  C��C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��3C�  C��D fD �fD  Dy�D  D� D  D� D  D� D  D� D  D� D  D�fD  Dy�D	  D	� D
  D
� DfD� D��Dy�D  D� D  D�fD  D� D  D�fD  D� DfD� D  Dy�D��D� D  D� D��Dy�D��D� D  D� D  D� DfD� D��D� D  D� DfD�fD  D� DfD� D   D � D!  D!� D"fD"� D#  D#� D$fD$� D$��D%y�D&  D&� D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7fD7�fD8  D8� D8��D9� D:  D:� D;fD;� D;��D<� D=  D=�fD>  D>� D?fD?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DL��DMy�DN  DN�fDOfDO�fDP  DP� DP��DQ� DRfDR�fDSfDS� DT  DT� DUfDU�fDVfDV� DV��DWy�DX  DX� DY  DY� DZfDZ� DZ��D[y�D\  D\�fD]fD]� D^  D^�fD_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� Do��Dp� DqfDq�fDrfDr�fDs  Ds� Ds��Dty�Dt��Du� Dv  Dv� Dv��Dwl�DyZ�D�L�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @ə�A33A#33AD��Ad��A�ffA�ffA�33A�33A�ffA�ffA�ffA�ffB33B	33B33B33B!33B)33B0��B8��BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B�fgB���B���B���B���B���B���B���B���B���B���Bę�B�fgB̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<fgC>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CffgChL�CjL�ClfgCnL�CpL�CrL�CtL�CvL�CxL�CzL�C|fgC~fgC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�33C�&fC��C��C�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�33C�&fC��C�&fC�33C�&fC��C��C��C��C��C��C�&fC�33C�&fC�&fC�&fC�&fC�&fC��C�&fC�33C�&fC�&fC��C�&fC�33D �D ��D3D��D3D�3D3D�3D3D�3D3D�3D3D�3D3D��D3D��D	3D	�3D
3D
�3D�D�3D�D��D3D�3D3D��D3D�3D3D��D3D�3D�D�3D3D��D�D�3D3D�3D�D��D�D�3D3D�3D3D�3D�D�3D�D�3D3D�3D�D��D3D�3D�D�3D 3D �3D!3D!�3D"�D"�3D#3D#�3D$�D$�3D%�D%��D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*�D*�3D+3D+�3D,�D,�3D-3D-�3D.3D.�3D/3D/�3D03D0��D13D1�3D23D2��D33D3�3D43D4�3D53D5�3D63D6�3D7�D7��D83D8�3D9�D9�3D:3D:�3D;�D;�3D<�D<�3D=3D=��D>3D>�3D?�D?�3D@3D@�3DA3DA�3DB�DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ��DK3DK�3DL3DL�3DM�DM��DN3DN��DO�DO��DP3DP�3DQ�DQ�3DR�DR��DS�DS�3DT3DT�3DU�DU��DV�DV�3DW�DW��DX3DX�3DY3DY�3DZ�DZ�3D[�D[��D\3D\��D]�D]�3D^3D^��D_3D_�3D`3D`��Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj�Dj�3Dk3Dk�3Dl3Dl��Dm3Dm�3Dn3Dn�3Do3Do�3Dp�Dp�3Dq�Dq��Dr�Dr��Ds3Ds�3Dt�Dt��Du�Du�3Dv3Dv�3Dw�Dw� DynD�VgD��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A͟�A͡�A͡�A͡�Aͧ�Aͥ�Aͧ�Aͧ�Aͩ�Aͧ�Aͩ�AͬAͩ�Aͧ�Aͥ�A͛�Aͺ^A���A���AͼjA͙�A�v�A�1'A�
=A���A��A��mA��TA��`A��yA��#A���A���A���A���A���A̾wA̶FA̰!A̡�Ạ�Ḁ�A̧�A̝�A̕�Ȁ\A̍PẢ7Ả7Ȧ+A̅Ȧ+A̅A̅Ȧ+A̅A�|�A�x�A�x�A̶FA��A���A���A��A��FA�ĜA�5?A�1'A���A�~�A���A���A�hsA�+A���A�ffA�5?A��+A� �A���A�x�A���A���A�x�A"�A{��Ay�7Av �As��Ar��AqK�AoƨAm�7AkAi�Ag��Ag�Af��Ad1A_+A[`BAX�yAW�AW�AT��AS�PAQl�AN�AJ�/AE�ACoA@=qA?�A=�;A<�DA9�#A8bA6�jA5��A4M�A3&�A2��A2n�A1�^A0�!A/�7A.��A.bA,(�A*��A)A(��A'�7A&9XA%VA$�DA#"�A!��A ZA|�A�HA�jA�\AI�A�TA`BA��AȴA�uAx�A-A
=An�A7LAA��A�wAM�A��At�A��A��AhsA$�A�A�wA
�RA	��A	"�AA�A
=AM�AdZAv�AC�A �AhsA�
AA��AA M�@�;d@�
=@��@��;@���@��@�+@�w@�~�@��@�@�Z@�1@�S�@�@��;@�33@�-@�X@睲@�@�/@�Ĝ@�1@��@�-@�9@�I�@��
@���@�X@۝�@�o@ڧ�@���@�hs@��@�Z@�I�@��
@�C�@��@ָR@�^5@�@Դ9@�;d@��@�?}@Ь@� �@ϕ�@�C�@���@�~�@�=q@�@��T@͙�@�%@�j@��m@�"�@�5?@�?}@ȋD@�1@Ɨ�@�J@���@�@�x�@�V@��@ě�@�;d@�K�@�l�@ÍP@Å@��H@�v�@���@�&�@�%@���@��@�Z@�\)@��@�5?@�J@��h@��@�z�@�I�@��w@�@��R@���@�+@���@�n�@�J@�@��^@���@��-@�r�@�Q�@�(�@��@��F@�@�o@��@�@�&�@�Ĝ@�z�@�bN@� �@���@���@�t�@�S�@�ȴ@��@�hs@���@�Q�@�9X@� �@��
@�  @��;@��P@�K�@��@�~�@��T@�p�@�V@�bN@�Z@�9X@� �@��@��@��;@���@�dZ@��@�M�@��-@��@��u@�bN@�I�@�1'@��@��P@�S�@�33@��@���@�v�@�=q@��#@���@�G�@���@��@��u@�Z@�1@��@��@�l�@�K�@�"�@�+@�"�@��y@�V@��@���@��@�?}@��/@��@��@��9@��@�33@��y@���@��!@��\@�M�@��@��@�V@���@���@�j@��@��@�\)@���@��R@�M�@�-@�@���@��@���@�z�@�b@���@�K�@��@��R@��+@�E�@�J@��#@���@�V@��9@� �@���@�t�@�;d@��@��R@�~�@�V@���@��^@�hs@��/@�z�@���@���@�dZ@�
=@��H@�ff@�5?@��@�@��@�p�@���@���@�r�@�9X@�  @��
@��@�C�@�C�@�C�@�C�@���@���@�~�@�-@���@�@��-@�`B@��@��u@�1@���@�|�@�|�@��@��@��P@�;d@�@���@�+@�\)@��y@��!@�ff@s@O@b�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A͟�A͡�A͡�A͡�Aͧ�Aͥ�Aͧ�Aͧ�Aͩ�Aͧ�Aͩ�AͬAͩ�Aͧ�Aͥ�A͛�Aͺ^A���A���AͼjA͙�A�v�A�1'A�
=A���A��A��mA��TA��`A��yA��#A���A���A���A���A���A̾wA̶FA̰!A̡�Ạ�Ḁ�A̧�A̝�A̕�Ȁ\A̍PẢ7Ả7Ȧ+A̅Ȧ+A̅A̅Ȧ+A̅A�|�A�x�A�x�A̶FA��A���A���A��A��FA�ĜA�5?A�1'A���A�~�A���A���A�hsA�+A���A�ffA�5?A��+A� �A���A�x�A���A���A�x�A"�A{��Ay�7Av �As��Ar��AqK�AoƨAm�7AkAi�Ag��Ag�Af��Ad1A_+A[`BAX�yAW�AW�AT��AS�PAQl�AN�AJ�/AE�ACoA@=qA?�A=�;A<�DA9�#A8bA6�jA5��A4M�A3&�A2��A2n�A1�^A0�!A/�7A.��A.bA,(�A*��A)A(��A'�7A&9XA%VA$�DA#"�A!��A ZA|�A�HA�jA�\AI�A�TA`BA��AȴA�uAx�A-A
=An�A7LAA��A�wAM�A��At�A��A��AhsA$�A�A�wA
�RA	��A	"�AA�A
=AM�AdZAv�AC�A �AhsA�
AA��AA M�@�;d@�
=@��@��;@���@��@�+@�w@�~�@��@�@�Z@�1@�S�@�@��;@�33@�-@�X@睲@�@�/@�Ĝ@�1@��@�-@�9@�I�@��
@���@�X@۝�@�o@ڧ�@���@�hs@��@�Z@�I�@��
@�C�@��@ָR@�^5@�@Դ9@�;d@��@�?}@Ь@� �@ϕ�@�C�@���@�~�@�=q@�@��T@͙�@�%@�j@��m@�"�@�5?@�?}@ȋD@�1@Ɨ�@�J@���@�@�x�@�V@��@ě�@�;d@�K�@�l�@ÍP@Å@��H@�v�@���@�&�@�%@���@��@�Z@�\)@��@�5?@�J@��h@��@�z�@�I�@��w@�@��R@���@�+@���@�n�@�J@�@��^@���@��-@�r�@�Q�@�(�@��@��F@�@�o@��@�@�&�@�Ĝ@�z�@�bN@� �@���@���@�t�@�S�@�ȴ@��@�hs@���@�Q�@�9X@� �@��
@�  @��;@��P@�K�@��@�~�@��T@�p�@�V@�bN@�Z@�9X@� �@��@��@��;@���@�dZ@��@�M�@��-@��@��u@�bN@�I�@�1'@��@��P@�S�@�33@��@���@�v�@�=q@��#@���@�G�@���@��@��u@�Z@�1@��@��@�l�@�K�@�"�@�+@�"�@��y@�V@��@���@��@�?}@��/@��@��@��9@��@�33@��y@���@��!@��\@�M�@��@��@�V@���@���@�j@��@��@�\)@���@��R@�M�@�-@�@���@��@���@�z�@�b@���@�K�@��@��R@��+@�E�@�J@��#@���@�V@��9@� �@���@�t�@�;d@��@��R@�~�@�V@���@��^@�hs@��/@�z�@���@���@�dZ@�
=@��H@�ff@�5?@��@�@��@�p�@���@���@�r�@�9X@�  @��
@��@�C�@�C�@�C�@�C�@���@���@�~�@�-@���@�@��-@�`B@��@��u@�1@���@�|�@�|�@��@��@��P@�;d@�@���@�+@�\)@��y@��!@�ff@s@O@b�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B@�B?}B?}B?}BA�B@�BA�B@�BA�B@�BA�B@�BC�BF�BJ�BW
B�B�\B�oB��B��B�uB�=B�B�B�B�B�B�B�%B�B�B�B�B�B�B�B� B~�B|�B}�B~�B~�B|�B{�Bz�By�By�By�Bx�Bx�Bx�Bx�Bx�By�Bx�Bw�Bw�Bx�B��B_;BT�BI�B>wB49B �BB��B�HB�qB��B�%Bl�B9XB%B
�5B
ǮB
�{B
ffB
T�B
=qB
-B
!�B	��B	�#B	��B	�9B	��B	��B	�{B	�DB	�B	{�B	p�B	ffB	\)B	VB	P�B	B�B	49B	$�B	�B	oB	JB	B��B�B�ZB��B�dB�9B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�?B�?B�?B�3B�dBǮB��B��B��B��B��B�B�
B�5B�;B�;B�#B�B�B�TB�NB�TB�ZB�mB�sB�fB�sB�mB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	B	%B	+B	1B	DB	JB	PB	\B	hB	{B	uB	�B	�B	�B	�B	�B	#�B	(�B	+B	.B	/B	1'B	33B	5?B	6FB	8RB	:^B	<jB	>wB	>wB	@�B	E�B	F�B	G�B	G�B	H�B	K�B	M�B	N�B	T�B	XB	XB	YB	[#B	_;B	`BB	cTB	e`B	e`B	e`B	ffB	gmB	gmB	gmB	k�B	o�B	p�B	q�B	s�B	t�B	u�B	w�B	x�B	y�B	y�B	z�B	{�B	|�B	~�B	� B	�B	�B	�%B	�+B	�+B	�+B	�%B	�%B	�DB	�PB	�JB	�VB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�?B	�RB	�^B	�^B	�XB	�^B	�^B	�dB	�jB	�jB	�qB	�qB	�qB	�wB	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	B	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�5B	�BB	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�`B	�yB	�B	�B	�yB	�mB	�mB	�mB	�mB	�fB	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
1B
1B
1B
1B
1B
	7B

=B

=B
DB
DB
DB
JB
JB
PB
PB
PB
PB
VB
\B
\B
bB
bB
hB
hB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!|B
*�B
5�222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B@�B?}B?}B?}BA�B@�BA�B@�BA�B@�BA�B@�BC�BF�BJ�BW
B�B�\B�oB��B��B�uB�=B�B�B�B�B�B�B�%B�B�B�B�B�B�B�B� B~�B|�B}�B~�B~�B|�B{�Bz�By�By�By�Bx�Bx�Bx�Bx�Bx�By�Bx�Bw�Bw�Bx�B��B_;BT�BI�B>wB49B �BB��B�HB�qB��B�%Bl�B9XB%B
�5B
ǮB
�{B
ffB
T�B
=qB
-B
!�B	��B	�#B	��B	�9B	��B	��B	�{B	�DB	�B	{�B	p�B	ffB	\)B	VB	P�B	B�B	49B	$�B	�B	oB	JB	B��B�B�ZB��B�dB�9B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�?B�?B�?B�3B�dBǮB��B��B��B��B��B�B�
B�5B�;B�;B�#B�B�B�TB�NB�TB�ZB�mB�sB�fB�sB�mB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	B	%B	+B	1B	DB	JB	PB	\B	hB	{B	uB	�B	�B	�B	�B	�B	#�B	(�B	+B	.B	/B	1'B	33B	5?B	6FB	8RB	:^B	<jB	>wB	>wB	@�B	E�B	F�B	G�B	G�B	H�B	K�B	M�B	N�B	T�B	XB	XB	YB	[#B	_;B	`BB	cTB	e`B	e`B	e`B	ffB	gmB	gmB	gmB	k�B	o�B	p�B	q�B	s�B	t�B	u�B	w�B	x�B	y�B	y�B	z�B	{�B	|�B	~�B	� B	�B	�B	�%B	�+B	�+B	�+B	�%B	�%B	�DB	�PB	�JB	�VB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�?B	�RB	�^B	�^B	�XB	�^B	�^B	�dB	�jB	�jB	�qB	�qB	�qB	�wB	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	B	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�5B	�BB	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�`B	�yB	�B	�B	�yB	�mB	�mB	�mB	�mB	�fB	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
1B
1B
1B
1B
1B
	7B

=B

=B
DB
DB
DB
JB
JB
PB
PB
PB
PB
VB
\B
\B
bB
bB
hB
hB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!|B
*�B
5�222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190540                              AO  ARCAADJP                                                                    20181005190540    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190540  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190540  QCF$                G�O�G�O�G�O�8000            