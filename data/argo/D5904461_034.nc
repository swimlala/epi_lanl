CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-08T19:17:25Z AOML 3.0 creation; 2016-08-07T21:36:33Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150308191725  20160807143633  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               "A   AO  5286_8897_034                   2C  D   APEX                            6531                            072314                          846 @�=Ԙ?�1   @�=����@2��9Xb�c�XbM�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    "A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Dfy�Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�3D�	�D�Y�D���D���D���D�)�D���D���D�3D�6fD�� D�� D�fD�0 D�ffD��3D�fD�P D� D�ɚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bx\)B��B���B���B���B���B���B���B���B�.B�ǮB�ǮB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qCC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C��C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9x�D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Dfx�Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt��Dy��D�	HD�YHD��HD��HD��{D�)HD��{D��{D��D�6D��D�ϮD�D�/�D�fD���D�D�O�D��D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AΣ�AΥ�AΣ�AΥ�AΥ�AΧ�AΥ�AΣ�AΗ�A�x�A�r�A�l�A�I�A���A��TA���A�ȴAͮA͕�A͗�A͛�A͕�A͏\A͋DA͋DAͣ�A͇+A�l�A�VA�z�A�p�A͇+A���A̬A̛�A��;Aʩ�A���Aɝ�AȁAƇ+A�7LA�dZA���A��TA�hsA�5?A�l�A�ȴA��uA��A���A�z�A�ƨA��A��A�dZA���A��A�G�A���A��7A�G�A�S�A�=qA��A�^5A�jA��jA�n�A�ƨA�G�A��wA��A��A�JA�1'A��A��
A�bNA���A���A��7A���A�dZA�XA���A��A���A��/A�-A�7LA��9A�I�A�9XA�1'A�t�A��A�A���A��yA�z�A�ȴA�`BA���A���A��#A�S�A}�Ay�FAtv�Am�PAj��Ag��Ad��Aa��A^�A[�
AU�;AP�AN�RAJbNAF��AE%AC%AB=qAA�
AAO�A>�\A;�A9�PA7|�A6{A5+A4E�A3�hA2�`A2=qA0ĜA.z�A-33A+G�A*�DA*  A)�A'�
A'K�A&��A%�A%x�A$�jA#ƨA"��A �A 9XA��A�yA��A?}AĜA��A�
A��A1'AƨA�A�wA��A�!A��AdZA{A�\AS�A
�yA
�A��A�AO�Az�A�TA
=AVAl�AC�A�A33A�`A��A\)A�wA�PA �A I�@���@��@�E�@�A�@��@��h@�Ĝ@��@�z�@���@�l�@��@�V@�|�@���@�r�@�t�@ꟾ@�{@�G�@�j@�b@�@�@�\)@�;d@��@�p�@��@�!@��@���@�%@ߝ�@�M�@�-@ݑh@�7L@�%@��`@ܼj@ܬ@�A�@�@ڇ+@�-@�@ؼj@��m@�|�@�;d@ְ!@�J@ա�@��@�r�@���@�C�@��H@ӍP@�V@Ѳ-@�`B@�7L@д9@�bN@��;@϶F@�\)@�;d@�@�$�@��@�z�@̬@̛�@�Q�@��;@�l�@�o@ʸR@�{@�@�hs@�V@��`@���@ȃ@�Q�@�1@�dZ@�
=@�o@��y@�p�@Ĭ@�9X@î@��y@��@§�@�ff@�p�@��/@���@��D@�(�@���@�;d@�@�;d@���@�(�@��F@�ff@�x�@��u@���@�%@���@���@��@��@���@��@�V@�V@��/@�bN@���@��!@�5?@��#@�?}@��/@�Z@�  @��@�ȴ@���@��!@��@��7@�/@��@��@��/@��@�I�@�A�@�(�@�b@��w@��H@���@��@�x�@�7L@��@�1@���@��@�ƨ@�o@��y@�E�@���@�@��-@��@�hs@��D@�I�@�A�@� �@���@��F@���@�@�ff@�@��-@��h@�`B@�7L@��@�Ĝ@���@�(�@��@��F@�;d@��\@�-@�J@��@�%@���@�Q�@�(�@�ƨ@�l�@���@��!@�n�@�{@���@�p�@�7L@��@�%@���@��j@��D@�Z@�1@��P@�S�@�"�@��@�v�@�$�@��T@�hs@�&�@��@�b@���@���@�t�@�\)@�S�@��!@�E�@��#@��-@��7@�O�@���@�Ĝ@�z�@�I�@�9X@�(�@�b@�  @���@��
@�;d@��@���@���@�~�@�ff@�=q@�{@��T@��^@���@�`B@���@�Ĝ@��9@��@���@��u@��u@��D@�j@�9X@�b@��P@�;d@���@�ff@��#@�hs@��@���@��9@��@�Q�@�9X@��@�  @�(�@�9X@�9X@�A�@�I�@�I�@�1@��m@��F@��@��@���@�@v��@m`B@d1@]�@T��@Lj@G;d@@�@:�@3��@.�+@)�@$1@K�@�!@�h@&�@ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AΣ�AΥ�AΣ�AΥ�AΥ�AΧ�AΥ�AΣ�AΗ�A�x�A�r�A�l�A�I�A���A��TA���A�ȴAͮA͕�A͗�A͛�A͕�A͏\A͋DA͋DAͣ�A͇+A�l�A�VA�z�A�p�A͇+A���A̬A̛�A��;Aʩ�A���Aɝ�AȁAƇ+A�7LA�dZA���A��TA�hsA�5?A�l�A�ȴA��uA��A���A�z�A�ƨA��A��A�dZA���A��A�G�A���A��7A�G�A�S�A�=qA��A�^5A�jA��jA�n�A�ƨA�G�A��wA��A��A�JA�1'A��A��
A�bNA���A���A��7A���A�dZA�XA���A��A���A��/A�-A�7LA��9A�I�A�9XA�1'A�t�A��A�A���A��yA�z�A�ȴA�`BA���A���A��#A�S�A}�Ay�FAtv�Am�PAj��Ag��Ad��Aa��A^�A[�
AU�;AP�AN�RAJbNAF��AE%AC%AB=qAA�
AAO�A>�\A;�A9�PA7|�A6{A5+A4E�A3�hA2�`A2=qA0ĜA.z�A-33A+G�A*�DA*  A)�A'�
A'K�A&��A%�A%x�A$�jA#ƨA"��A �A 9XA��A�yA��A?}AĜA��A�
A��A1'AƨA�A�wA��A�!A��AdZA{A�\AS�A
�yA
�A��A�AO�Az�A�TA
=AVAl�AC�A�A33A�`A��A\)A�wA�PA �A I�@���@��@�E�@�A�@��@��h@�Ĝ@��@�z�@���@�l�@��@�V@�|�@���@�r�@�t�@ꟾ@�{@�G�@�j@�b@�@�@�\)@�;d@��@�p�@��@�!@��@���@�%@ߝ�@�M�@�-@ݑh@�7L@�%@��`@ܼj@ܬ@�A�@�@ڇ+@�-@�@ؼj@��m@�|�@�;d@ְ!@�J@ա�@��@�r�@���@�C�@��H@ӍP@�V@Ѳ-@�`B@�7L@д9@�bN@��;@϶F@�\)@�;d@�@�$�@��@�z�@̬@̛�@�Q�@��;@�l�@�o@ʸR@�{@�@�hs@�V@��`@���@ȃ@�Q�@�1@�dZ@�
=@�o@��y@�p�@Ĭ@�9X@î@��y@��@§�@�ff@�p�@��/@���@��D@�(�@���@�;d@�@�;d@���@�(�@��F@�ff@�x�@��u@���@�%@���@���@��@��@���@��@�V@�V@��/@�bN@���@��!@�5?@��#@�?}@��/@�Z@�  @��@�ȴ@���@��!@��@��7@�/@��@��@��/@��@�I�@�A�@�(�@�b@��w@��H@���@��@�x�@�7L@��@�1@���@��@�ƨ@�o@��y@�E�@���@�@��-@��@�hs@��D@�I�@�A�@� �@���@��F@���@�@�ff@�@��-@��h@�`B@�7L@��@�Ĝ@���@�(�@��@��F@�;d@��\@�-@�J@��@�%@���@�Q�@�(�@�ƨ@�l�@���@��!@�n�@�{@���@�p�@�7L@��@�%@���@��j@��D@�Z@�1@��P@�S�@�"�@��@�v�@�$�@��T@�hs@�&�@��@�b@���@���@�t�@�\)@�S�@��!@�E�@��#@��-@��7@�O�@���@�Ĝ@�z�@�I�@�9X@�(�@�b@�  @���@��
@�;d@��@���@���@�~�@�ff@�=q@�{@��T@��^@���@�`B@���@�Ĝ@��9@��@���@��u@��u@��D@�j@�9X@�b@��P@�;d@���@�ff@��#@�hs@��@���@��9@��@�Q�@�9X@��@�  @�(�@�9X@�9X@�A�@�I�@�I�@�1@��m@��FG�O�@��@���@�@v��@m`B@d1@]�@T��@Lj@G;d@@�@:�@3��@.�+@)�@$1@K�@�!@�h@&�@ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	ZB	ZB	ZB	ZB	ZB	ZB	[#B	^5B	`BB	gmB	iyB	l�B	z�B	�oB	��B	��B	��B	�XB	ŢB	ǮB	ǮB	ǮB	ǮB	ƨB	ɺB	��B
2-B
F�B
XB
k�B
s�B
��B
�9B
B
ɺB
�BB%�B8RB@�BXBcTBx�B�%B��B�FB�?B�?B�LB�?B�'B�B�B�B�BȴB��B�!B�wBÖB�wB�dB��B��B�B!�B�B�BhBVB��B�B�ZB��B�dB��B�JBw�BQ�B8RB0!B�B
��B
�B
�TB
�HB
�BVB1'B>wBL�Bk�B^5B;dB0!B�B{B�B�BPBB
�NB
B
��B
v�B
ZB
M�B
7LB
 �B	��B	�
B	��B	�hB	}�B	hsB	R�B	A�B	49B		7B�B�B�5B��B��B��BǮBĜB�}B�XB�?B�9B�FB�FB�FB�FB�FB�?B�3B�-B�-B�-B�3B�3B�3B�9B�FB�LB�LB�^B�jB�qB�}B�}BÖBBǮBɺB��B��B��B��B��B��B��B��B��BɺBBBĜBÖB�wB�dB�^B�XB�^B�^B�jB�qB�XB�XB�jB�qB�}BƨB��B�B�B��B�
B�/B�/B�/B�)B�B�B�
B�B�)B�;B�HB�HB�HB�NB�TB�`B�mB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B	B	B	DB	\B	bB	hB	hB	hB	oB	�B	�B	�B	�B	 �B	"�B	%�B	&�B	)�B	,B	+B	+B	)�B	(�B	(�B	-B	7LB	<jB	?}B	A�B	B�B	D�B	E�B	G�B	H�B	I�B	I�B	J�B	L�B	L�B	M�B	S�B	T�B	W
B	XB	YB	ZB	ZB	\)B	]/B	^5B	aHB	aHB	aHB	bNB	e`B	ffB	ffB	hsB	k�B	l�B	q�B	q�B	q�B	r�B	s�B	s�B	u�B	x�B	}�B	�B	�B	�%B	�=B	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�9B	�FB	�RB	�RB	�RB	�RB	�RB	�XB	�XB	�RB	�LB	�FB	�FB	�FB	�LB	�qB	�wB	�qB	�^B	�XB	�XB	�XB	�^B	�dB	�jB	�jB	�qB	�wB	�}B	�}B	�}B	�wB	�wB	�wB	��B	ÖB	ÖB	ÖB	ĜB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�/B	�5B	�;B	�5B	�;B	�;B	�BB	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B
B
B
B
B
B
B
%B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B
1B
VB
�B
�B
$�B
,B
2-B
7LB
>wB
C�B
H�B
N�B
R�B
XB
\)B
aHB
e`B
hsB
m�B
q�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	Z/B	Z.B	Z/B	Z/B	Z/B	Z1B	[1B	^GB	`SB	g{B	i�B	l�B	z�B	�|B	��B	��B	�B	�dB	ŰB	ǸB	ǺB	ǸB	ǺB	ƴB	��B	��B
29B
F�B
XB
k�B
s�B
��B
�AB
B
ɽB
�B#B%�B8TB@�BXBcWBx�B�%B��B�GB�@B�?B�OB�AB�)B�	B�B�	B�BȶB��B�#B�wB×B�xB�eB��B��B�B!�B�B�BhBVB��B�B�ZB��B�bB��B�JBw�BQ�B8VB0B�B
��B
�B
�XB
�JB
�BYB1)B>xBL�Bk�B^6B;hB0$B�BB�B�BUBB
�RB
B
��B
v�B
Z%B
M�B
7UB
 �B	�B	�B	��B	�qB	~B	h�B	SB	A�B	4KB		HB��B�B�IB�B��B��B��BİB��B�mB�TB�NB�]B�[B�\B�[B�[B�VB�HB�BB�CB�DB�GB�HB�FB�MB�[B�_B�`B�sB�}B��B��B��BêB¤B��B��B��B��B��B��B��B��B� B��B��B��B¥B¤BĲBëB��B�xB�tB�kB�sB�sB�|B��B�lB�kB��B��B��BƺB�B�B�)B�B�B�AB�CB�@B�<B�/B�"B�B�$B�<B�NB�YB�\B�[B�_B�iB�pB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B	B	.B	VB	kB	sB	yB	xB	xB	~B	�B	�B	�B	�B	 �B	"�B	%�B	&�B	*B	,B	+B	+B	*B	)B	)B	-B	7[B	<xB	?�B	A�B	B�B	D�B	E�B	G�B	H�B	I�B	I�B	J�B	L�B	L�B	M�B	TB	UB	WB	XB	Y(B	Z*B	Z)B	\7B	]<B	^AB	aTB	aVB	aTB	b\B	emB	fsB	fqB	h�B	k�B	l�B	q�B	q�B	q�B	r�B	s�B	s�B	u�B	x�B	~B	� B	�&B	�3B	�LB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�&B	�9B	�CB	�PB	�_B	�^B	�]B	�[B	�[B	�aB	�cB	�]B	�VB	�OB	�RB	�NB	�VB	�yB	��B	�zB	�jB	�bB	�bB	�cB	�hB	�oB	�sB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	àB	ÝB	ßB	ģB	ūB	ŪB	ȽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�,B	�0B	�9B	�7B	�<B	�:B	�>B	�FB	�>B	�AB	�CB	�KB	�RB	�]B	�\B	�^B	�cB	�`B	�aB	�jB	�oB	�uB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B
 B	�B	�B	�B	�B
B
B
B
B
B
B
-B
8B
	>B
	@B
	@B
	@B
	?B
	=B
	>G�O�B
:B
^B
�B
�B
$�B
,B
26B
7QB
>{B
C�B
H�B
N�B
R�B
XB
\-B
aMB
ehB
hzB
m�B
q�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.01 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436332016080714363320160807143633  AO  ARCAADJP                                                                    20150308191725    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150308191725  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150308191725  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143633  IP                  G�O�G�O�G�O�                