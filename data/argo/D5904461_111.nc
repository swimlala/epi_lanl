CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-09T19:17:25Z AOML 3.0 creation; 2016-08-07T21:36:45Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160409191725  20160807143645  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               oA   AO  5286_8897_111                   2C  D   APEX                            6531                            072314                          846 @ףA��.1   @ףb��@3i��l�D�cW��$�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    oA   B   B   @333@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp��Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C�fC	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�fDy� D��D�P D���D��fD�� D�9�D�p D�� D��D�I�D�� D�ٚD�  D�@ Dڜ�D�� D�  D�#3D�vfD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @333@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp��Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C�fC	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�fDy� D��D�P D���D��fD�� D�9�D�p D�� D��D�I�D�� D�ٚD�  D�@ Dڜ�D�� D�  D�#3D�vfD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A���A��A��A��A��A��A���A���A�  A���A���A��A���A���A��#A��A̾wA̾wA�l�A��yA˧�AˑhA˅A�ffA�`BA�ffA�ffA�\)A���A���Aȴ9A�x�A�?}A��TAǩ�AǁA�ZA�9XA���A�{A���A�n�A�{A��mAĶFAăA���Aß�AÁA�7LA�A��A��A�ȴA�A��A�K�A��A�/A��mA��A��mA��\A�"�A�dZA��A���A�E�A�hsA���A�|�A��TA���A�r�A�XA�S�A�-A��-A���A�bNA�JA�1'A�`BA�33A��FA���A�$�A���A���A��A��A���A��A�ȴA�l�A�
=A��#A��A�t�A��A�;dA�;dA���A�E�A��A�{A��/A�+A��jA�%A�ZA���A�C�A���A�C�A�G�A���A��`A��mA��HA�$�A}%AyƨAx(�Aw�At{Al�9Af�A]ƨA\�+A\ffA\ZA[
=AX�\AW&�ASVAM+AKS�AE�AC?}AB��A@�/A>ȴA=`BA;�;A;x�A;/A:ȴA:$�A8$�A5�-A333A2r�A2E�A0�A/�PA.E�A,r�A*v�A(��A(n�A'�A'�A&��A%XA#�
A#x�A"��A"JA �`A;dA��A�+A�A�-A��A�AK�AȴA��AffA7LAA�A?}A��A-A�A�^A��A1'A�A;dAVA�A�AƨA�hA+AQ�AG�A	7LA��An�A��A�A �A�yA��Al�AE�A��A�hA �R@��\@��^@��9@��+@�X@��@��@��
@�@�~�@�{@��@�V@�  @���@���@�9@�ȴ@�-@���@�z�@�I�@���@��y@�@��`@�w@�K�@���@�~�@�^5@ᙚ@���@��;@ާ�@��@�t�@�J@���@ׅ@�v�@�J@��@��m@҇+@��@�hs@ϥ�@��@Ώ\@�E�@�Z@˝�@�o@�J@�7L@���@��@�{@�@� �@��#@��#@�?}@�%@��`@�Ĝ@��9@�Ĝ@�1@��D@��@�p�@���@�/@���@��/@���@��9@��@��@��@�t�@��y@�J@�@�X@���@�r�@�Z@�A�@��@�33@��@���@�^5@�I�@� �@��@��w@���@���@���@�t�@�\)@�@��R@��@�G�@��9@�r�@��@�+@�@��`@�Z@��m@��w@�
=@�V@��h@�%@��R@�C�@��F@�;d@�V@�&�@�j@��P@�K�@���@�~�@��@�x�@�O�@�V@��@�j@���@���@�+@��@���@���@�ff@���@�z�@��u@�p�@�x�@��@��`@���@�bN@�;d@�@�@��H@��+@�ff@�=q@��@��@��@��^@��@�Ĝ@��u@�  @�  @��@���@�v�@�M�@�n�@���@�ff@�@���@�hs@��@��u@�/@�&�@��@�Ĝ@��u@�bN@� �@�C�@�v�@���@��`@��@��u@��u@��u@�V@��7@�hs@���@�/@��@��-@��@��-@��@���@�%@���@���@��@��;@���@�33@�J@�X@�Ĝ@�Q�@��@�K�@�@��@���@�M�@��@���@�?}@�&�@�%@���@���@��9@�bN@�I�@�9X@�b@��@��@�;d@���@���@��\@��+@�n�@�5?@�J@��@��#@��-@���@��7@�x�@�p�@�hs@�hs@�O�@�?}@�&�@�%@��/@��9@���@���@��@�(�@�ƨ@���@��P@�+@���@�V@���@|�@p1'@i%@c"�@]�h@U/@Nȴ@G
=@@b@97L@0�`@*��@$��@ 1'@7L@��@ƨ@��@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��A��A���A��A��A��A��A��A���A���A�  A���A���A��A���A���A��#A��A̾wA̾wA�l�A��yA˧�AˑhA˅A�ffA�`BA�ffA�ffA�\)A���A���Aȴ9A�x�A�?}A��TAǩ�AǁA�ZA�9XA���A�{A���A�n�A�{A��mAĶFAăA���Aß�AÁA�7LA�A��A��A�ȴA�A��A�K�A��A�/A��mA��A��mA��\A�"�A�dZA��A���A�E�A�hsA���A�|�A��TA���A�r�A�XA�S�A�-A��-A���A�bNA�JA�1'A�`BA�33A��FA���A�$�A���A���A��A��A���A��A�ȴA�l�A�
=A��#A��A�t�A��A�;dA�;dA���A�E�A��A�{A��/A�+A��jA�%A�ZA���A�C�A���A�C�A�G�A���A��`A��mA��HA�$�A}%AyƨAx(�Aw�At{Al�9Af�A]ƨA\�+A\ffA\ZA[
=AX�\AW&�ASVAM+AKS�AE�AC?}AB��A@�/A>ȴA=`BA;�;A;x�A;/A:ȴA:$�A8$�A5�-A333A2r�A2E�A0�A/�PA.E�A,r�A*v�A(��A(n�A'�A'�A&��A%XA#�
A#x�A"��A"JA �`A;dA��A�+A�A�-A��A�AK�AȴA��AffA7LAA�A?}A��A-A�A�^A��A1'A�A;dAVA�A�AƨA�hA+AQ�AG�A	7LA��An�A��A�A �A�yA��Al�AE�A��A�hA �R@��\@��^@��9@��+@�X@��@��@��
@�@�~�@�{@��@�V@�  @���@���@�9@�ȴ@�-@���@�z�@�I�@���@��y@�@��`@�w@�K�@���@�~�@�^5@ᙚ@���@��;@ާ�@��@�t�@�J@���@ׅ@�v�@�J@��@��m@҇+@��@�hs@ϥ�@��@Ώ\@�E�@�Z@˝�@�o@�J@�7L@���@��@�{@�@� �@��#@��#@�?}@�%@��`@�Ĝ@��9@�Ĝ@�1@��D@��@�p�@���@�/@���@��/@���@��9@��@��@��@�t�@��y@�J@�@�X@���@�r�@�Z@�A�@��@�33@��@���@�^5@�I�@� �@��@��w@���@���@���@�t�@�\)@�@��R@��@�G�@��9@�r�@��@�+@�@��`@�Z@��m@��w@�
=@�V@��h@�%@��R@�C�@��F@�;d@�V@�&�@�j@��P@�K�@���@�~�@��@�x�@�O�@�V@��@�j@���@���@�+@��@���@���@�ff@���@�z�@��u@�p�@�x�@��@��`@���@�bN@�;d@�@�@��H@��+@�ff@�=q@��@��@��@��^@��@�Ĝ@��u@�  @�  @��@���@�v�@�M�@�n�@���@�ff@�@���@�hs@��@��u@�/@�&�@��@�Ĝ@��u@�bN@� �@�C�@�v�@���@��`@��@��u@��u@��u@�V@��7@�hs@���@�/@��@��-@��@��-@��@���@�%@���@���@��@��;@���@�33@�J@�X@�Ĝ@�Q�@��@�K�@�@��@���@�M�@��@���@�?}@�&�@�%@���@���@��9@�bN@�I�@�9X@�b@��@��@�;d@���@���@��\@��+@�n�@�5?@�J@��@��#@��-@���@��7@�x�@�p�@�hs@�hs@�O�@�?}@�&�@�%@��/@��9@���@���@��@�(�@�ƨ@���@��P@�+@���G�O�@���@|�@p1'@i%@c"�@]�h@U/@Nȴ@G
=@@b@97L@0�`@*��@$��@ 1'@7L@��@ƨ@��@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	hB	hB	hB	oB	oB	oB	oB	oB	oB	oB	oB	oB	hB	hB	hB	hB	\B	\B	PB	PB		7B	B	  B	  B��B��B		7B	hB	�B	1'B	�9B	�TB
(�B
B�B
T�B
q�B
�B
�JB
��B
��B
��B
��B
��B
�B
�dB
��B
�NB
��B�B33B;dBT�BdZBn�Bu�Bv�B�JB��B��B��B��B��B�
B�B��B��B%BbB%�B�BBBBBB
=B1B+BB��B�B�B�B�B�`B�sB�ZB��BĜB��B��B�qB��Bx�B^5BW
BQ�BM�BI�B=qB'�B\B
�B
�sB
�NB
�B
�B
�
B
��B
��B
ĜB
�B
��B
�VB
�7B
�B
x�B
jB
^5B
R�B
6FB
&�B
PB	�B	�B	��B	ŢB	�B	p�B	49B	�B	�B	�B	�B	\B	B��B�ZBB�XB�!B�9B�?B��BɺBȴBǮBƨBŢBŢBÖB�wB�RB�-B�B�B�LB�?B�B��B�{B�\B�\B�bB�bB�\B�7B�B�B�B�B�B�1B�7B�7B�1B�+B�+B�1B�7B�7B�=B�hB�oB�uB�uB�uB�oB�hB�bB�PB�DB�JB�hB�hB�PB�bB��B��B��B��B��B��B�B�!B�B�B��B��B��B��B��B��B��B��B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�RB�jB�wB�}B�}B��BÖBŢBǮBɺBƨBĜBB��B�qB�dB�XB�}BɺB��B��B�HB�NB�`B��B��B��B��B	B	
=B	\B	hB	�B	�B	�B	�B	 �B	#�B	%�B	'�B	.B	2-B	7LB	?}B	A�B	D�B	H�B	I�B	I�B	J�B	M�B	O�B	Q�B	S�B	T�B	YB	]/B	]/B	]/B	]/B	^5B	^5B	_;B	`BB	cTB	e`B	iyB	m�B	o�B	p�B	q�B	s�B	q�B	o�B	p�B	v�B	z�B	�B	�1B	�\B	�bB	�DB	�bB	��B	��B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�9B	�9B	�3B	�?B	�RB	�XB	�XB	�jB	�jB	�qB	�qB	�wB	�wB	�wB	�wB	�qB	�jB	�}B	��B	��B	��B	�}B	��B	B	ƨB	ȴB	ǮB	ƨB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�BB	�HB	�TB	�NB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�fB	�fB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B	��B
DB
�B
!�B
%�B
)�B
1'B
8RB
=qB
D�B
J�B
P�B
W
B
]/B
bNB
hsB
k�B
n�B
p�B
t�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	{B	|B	zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	{B	|B	zB	|B	oB	mB	bB	dB		KB	B	 B	 B�B�B		IB	{B	�B	16B	�CB	�^B
(�B
B�B
UB
q�B
�B
�OB
��B
��B
��B
��B
��B
�B
�gB
��B
�PB
��B�B34B;cBT�BdYBn�Bu�Bv�B�NB��B��B��B��B��B�
B�B��B��B#BcB%�B�BBBBBB
;B/B+BB��B�B�B�B�B�`B�sB�XB��BĝB��B��B�qB��Bx�B^5BWBQ�BM�BI�B=sB'�B`B
�B
�uB
�OB
�"B
�B
�B
��B
��B
ĠB
�!B
��B
�]B
�<B
�B
x�B
j�B
^:B
R�B
6LB
&�B
XB	�B	�B	��B	ũB	�B	p�B	4HB	�B	�B	�B	�B	nB	+B��B�kB¤B�kB�5B�NB�SB��B��B��B��BƻBŵBŶBêB��B�gB�AB�2B�)B�_B�SB�.B��B��B�sB�sB�wB�yB�sB�MB�4B�3B�/B�)B�.B�IB�MB�NB�FB�AB�CB�GB�OB�KB�SB�~B��B��B��B��B��B�}B�vB�fB�XB�`B�B�|B�hB�wB��B��B��B��B��B�B�/B�4B�.B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�B�B�B�bB�zB��B��B��B��BçBųBǽB��BƸBĮB B��B��B�wB�hB��B��B��B��B�XB�_B�mB��B��B��B��B	B	
MB	jB	wB	�B	�B	�B	�B	 �B	#�B	%�B	'�B	.#B	2:B	7XB	?�B	A�B	D�B	H�B	I�B	I�B	J�B	M�B	O�B	Q�B	TB	UB	Y"B	]8B	];B	]:B	]:B	^@B	^AB	_GB	`NB	c_B	emB	i�B	m�B	o�B	p�B	q�B	s�B	q�B	o�B	p�B	v�B	z�B	�#B	�;B	�eB	�mB	�MB	�mB	��B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�5B	�AB	�BB	�<B	�GB	�ZB	�_B	�`B	�sB	�sB	�zB	�xB	��B	�B	��B	��B	�{B	�sB	��B	��B	��B	��B	��B	��B	B	ƱB	ȻB	ǶB	ƮB	ũB	ƯB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�0B	�JB	�PB	�]B	�UB	�gB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�lB	�iB	�fB	�mB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	� B
JB
�B
!�B
%�B
*B
1-B
8YB
=xB
D�B
J�B
P�B
WB
]4B
bRB
huB
k�B
n�B
p�B
t�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436452016080714364520160807143645  AO  ARCAADJP                                                                    20160409191725    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160409191725  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160409191725  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143645  IP                  G�O�G�O�G�O�                