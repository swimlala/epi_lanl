CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:41Z AOML 3.0 creation; 2016-05-31T19:14:35Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230541  20160531121435  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               BA   AO  4051_7090_066                   2C  D   APEX                            5368                            041511                          846 @�ڈx���1   @�ډ@47
=p���e-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    BA   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy� D�fD�0 D�ffD�ٚD�fD�6fD��fD���D��fD�P D�33D��fD�  D�P Dڙ�D��D��3D�P D� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @8��@\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D&�D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dtr�Dy�\D�D�/�D�fD��HD�D�6D��D��{D��D�O�D�2�D��D���D�O�DڙHD�{D���D�O�D�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�;dA�=qA�;dA�;dA�;dA�;dA�=qA�;dA�5?A�33A�&�A�VA�A���A��A��A��A��A��A��A��A��mA��/A���Aɩ�Aə�A�l�A�ZA�Q�A�I�A�1'A� �A���AȺ^Aȇ+A�E�AǁA�l�A�hsA�hsA�ffA�bNA�^5A�?}A��Aƛ�A�C�A��`A� �Aė�A�bNAç�A�XA�C�A�1A�ȴA��#A�(�A�A��/A�x�A� �A�  A�-A�hsA�E�A���A��A���A��-A�JA�oA�  A���A�A�
=A��A��A���A�ZA�A�A�;dA�;dA�=qA�5?A��A���A�(�A� �A�5?A��A���A���A�x�A��HA�&�A��9A��A��hA��A�1'A�A��PA�?}A��wA���A� �A���A� �A���A���A�&�A��
A�\)A��`A�bNA���A�Az1'Av��AvA�Aul�AsC�Aqp�AqXAqoAn��Ai&�Ag�TAd�Ab9XAa%A_�wA]�AZn�AY�AY�TAY�#AY�wAYp�AVĜATr�AR�HAR��AR^5AR-ARJAQ��AQ+AP�!AO
=AKdZAI��AHQ�AF�ADM�ADI�AC�mAC�AC?}AB�!ABA@��A>1'A=O�A<JA9t�A8�A8 �A6��A6  A5��A5��A5p�A4�A2�yA0�+A.��A-�A,�A*bA)
=A'�TA'7LA&�A%+A$-A#|�A!�A!XA!oA �A��A��A{A�A�A�A�`A9XAAbA��A�A�A �A�FAK�A��A�DAĜA�A�A�mA
1A��A-A�#A��AG�A%At�AI�A�AjA ��@��m@�S�@��@�M�@� �@���@�b@��;@�z�@�1'@�(�@��m@�M�@@�1@땁@�K�@�+@��@��@��#@��/@��@�hs@�=q@���@�1'@߮@�;d@��
@��@ڗ�@�V@��T@�j@ְ!@�M�@�%@ӍP@�33@�^5@�?}@�;d@�  @�^5@��`@Ǖ�@���@�~�@�-@ź^@�Ĝ@�r�@��@���@�dZ@�\)@�S�@�33@���@�E�@�@���@�V@��D@��@���@��F@�t�@�l�@�K�@��y@��!@�V@�7L@�Z@���@���@�ff@�M�@�=q@�=q@�=q@�5?@�-@�-@�J@�@���@���@�(�@��@�o@���@���@�@�9X@���@���@�l�@�"�@���@���@�~�@�n�@�=q@�-@��T@�x�@��@�
=@�=q@��T@�p�@���@�1'@���@�K�@�o@��#@�`B@��/@��@��@��@���@��@�K�@�+@��@���@��@��R@���@�v�@�-@��@�{@�5?@�E�@�=q@��@�X@�V@��@��/@���@��@�j@�j@�Q�@��@���@�l�@���@�^5@�-@�`B@�V@��@�r�@��;@�dZ@���@��\@�~�@�v�@�=q@�$�@�@��h@�`B@�/@�%@��9@�j@�A�@��@�K�@�33@�+@�
=@��@��R@���@�n�@�^5@�M�@�=q@��@�J@�J@��T@���@�hs@�X@�O�@�O�@�G�@�?}@�?}@�G�@�G�@�7L@��/@��j@��w@���@��+@��+@�n�@�ff@�^5@�V@�5?@��@�@��@��@��#@���@��-@���@��h@�hs@�hs@�`B@�G�@���@��9@�1'@�+@�n�@�=q@��@���@��T@�@��h@�G�@�/@���@��`@��9@��@���@�bN@�(�@���@���@�+@���@�5?@��@���@���@��h@�x�@�`B@�7L@��@���@��9@��F@��F@|Z@r=q@j��@b��@YG�@S@L�/@Ep�@<I�@6{@2-@+t�@'K�@"��@�@�#@\)@33@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�;dA�=qA�;dA�;dA�;dA�;dA�=qA�;dA�5?A�33A�&�A�VA�A���A��A��A��A��A��A��A��A��mA��/A���Aɩ�Aə�A�l�A�ZA�Q�A�I�A�1'A� �A���AȺ^Aȇ+A�E�AǁA�l�A�hsA�hsA�ffA�bNA�^5A�?}A��Aƛ�A�C�A��`A� �Aė�A�bNAç�A�XA�C�A�1A�ȴA��#A�(�A�A��/A�x�A� �A�  A�-A�hsA�E�A���A��A���A��-A�JA�oA�  A���A�A�
=A��A��A���A�ZA�A�A�;dA�;dA�=qA�5?A��A���A�(�A� �A�5?A��A���A���A�x�A��HA�&�A��9A��A��hA��A�1'A�A��PA�?}A��wA���A� �A���A� �A���A���A�&�A��
A�\)A��`A�bNA���A�Az1'Av��AvA�Aul�AsC�Aqp�AqXAqoAn��Ai&�Ag�TAd�Ab9XAa%A_�wA]�AZn�AY�AY�TAY�#AY�wAYp�AVĜATr�AR�HAR��AR^5AR-ARJAQ��AQ+AP�!AO
=AKdZAI��AHQ�AF�ADM�ADI�AC�mAC�AC?}AB�!ABA@��A>1'A=O�A<JA9t�A8�A8 �A6��A6  A5��A5��A5p�A4�A2�yA0�+A.��A-�A,�A*bA)
=A'�TA'7LA&�A%+A$-A#|�A!�A!XA!oA �A��A��A{A�A�A�A�`A9XAAbA��A�A�A �A�FAK�A��A�DAĜA�A�A�mA
1A��A-A�#A��AG�A%At�AI�A�AjA ��@��m@�S�@��@�M�@� �@���@�b@��;@�z�@�1'@�(�@��m@�M�@@�1@땁@�K�@�+@��@��@��#@��/@��@�hs@�=q@���@�1'@߮@�;d@��
@��@ڗ�@�V@��T@�j@ְ!@�M�@�%@ӍP@�33@�^5@�?}@�;d@�  @�^5@��`@Ǖ�@���@�~�@�-@ź^@�Ĝ@�r�@��@���@�dZ@�\)@�S�@�33@���@�E�@�@���@�V@��D@��@���@��F@�t�@�l�@�K�@��y@��!@�V@�7L@�Z@���@���@�ff@�M�@�=q@�=q@�=q@�5?@�-@�-@�J@�@���@���@�(�@��@�o@���@���@�@�9X@���@���@�l�@�"�@���@���@�~�@�n�@�=q@�-@��T@�x�@��@�
=@�=q@��T@�p�@���@�1'@���@�K�@�o@��#@�`B@��/@��@��@��@���@��@�K�@�+@��@���@��@��R@���@�v�@�-@��@�{@�5?@�E�@�=q@��@�X@�V@��@��/@���@��@�j@�j@�Q�@��@���@�l�@���@�^5@�-@�`B@�V@��@�r�@��;@�dZ@���@��\@�~�@�v�@�=q@�$�@�@��h@�`B@�/@�%@��9@�j@�A�@��@�K�@�33@�+@�
=@��@��R@���@�n�@�^5@�M�@�=q@��@�J@�J@��T@���@�hs@�X@�O�@�O�@�G�@�?}@�?}@�G�@�G�@�7L@��/@��j@��w@���@��+@��+@�n�@�ff@�^5@�V@�5?@��@�@��@��@��#@���@��-@���@��h@�hs@�hs@�`B@�G�@���@��9@�1'@�+@�n�@�=q@��@���@��T@�@��h@�G�@�/@���@��`@��9@��@���@�bN@�(�@���@���@�+@���@�5?@��@���@���@��h@�x�@�`B@�7L@��@���@��9@��F@��F@|Z@r=q@j��@b��@YG�@S@L�/@Ep�@<I�@6{@2-@+t�@'K�@"��@�@�#@\)@33@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�'B�'B�'B�3B�?B�FB�XB��BǮB��B��B��B�#B�/B�5B�;B�NB�NB�ZB�B�B�B��BBBBBBBB+BBBB��B��B�B�B�sB�mB�HB�)B��BɺBƨBÖB�qB�XB�FB�B��B� Bu�B_;BG�B%�B\B��B�B�)B��BȴB�3B��B��B��B��B��B��B��B��B��B�%Bq�BbNBW
BE�B(�B	7B�B�sB��B�LB��B�bBo�B\)BI�BA�B9XB#�BuB	7B
�B
�)B
ŢB
�9B
��B
�VB
�+B
�B
w�B
n�B
Q�B
)�B
uB
\B
1B	��B	�B	�B	�sB	�B	�XB	�'B	��B	�uB	�DB	�B	s�B	iyB	gmB	gmB	ffB	e`B	bNB	ZB	N�B	H�B	G�B	F�B	E�B	D�B	B�B	?}B	;dB	49B	)�B	#�B	�B	�B	oB	hB	hB	bB	VB	PB	
=B	B��B��B�B�B�B�mB�ZB�NB�NB�BB�;B�)B�B��B��BǮBB�jB�RB�?B�-B�!B�B�B��B��B��B��B��B��B��B��B�bB�PB�DB�7B�+B�B�B�B~�B{�By�Bw�Bw�Bu�Bs�Bt�Bs�Bw�Bt�Bo�Bk�BiyBhsBgmBffBcTBaHB_;B^5B]/B\)B\)BZB[#BbNBhsBhsBl�Bn�Bs�Bu�Bz�B� B|�By�By�B~�B~�B}�B|�B{�B{�Bz�B}�B�B�B}�B{�B{�B|�Bw�Bu�Bu�Bu�Bx�B�B� B� B� B�B�B�B�B~�B}�B|�B}�B�B�DB�JB�PB�\B�{B��B��B�FB�LB�LB�LB�RB�^B�wB��BBƨB��B��B��B��B��B��B��B�B�B�#B�NB�fB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	B	B	DB	PB	PB	VB	\B	hB	oB	oB	oB	uB	uB	{B	�B	�B	 �B	$�B	%�B	'�B	+B	.B	0!B	2-B	33B	:^B	=qB	@�B	A�B	B�B	D�B	F�B	I�B	L�B	L�B	M�B	M�B	N�B	O�B	Q�B	S�B	W
B	ZB	^5B	bNB	dZB	iyB	o�B	v�B	{�B	}�B	~�B	� B	�B	�B	�B	�B	�+B	�+B	�1B	�JB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�9B	�LB	�XB	�^B	�wB	��B	B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�TB	�NB	�NB	�TB	�ZB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
JB
�B
�B
$�B
,B
2-B
9XB
=qB
B�B
J�B
N�B
S�B
ZB
^5B
bNB
dZB
iyB
l�B
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B�B�$B�.B�.B�.B�.B�:B�HB�NB�_B��BǵB��B��B��B�.B�7B�CB�HB�VB�XB�eB�B�B�B�BBBBBBB#B3B,B)B!B��B��B�B�B�yB�yB�PB�-B�B��BƱBÝB�vB�`B�NB�B��B�Bu�B_CBG�B%�B_B��B�B�*B��BȸB�3B��B��B��B��B��B��B��B��B��B�)Bq�BbRBWBE�B(�B	8B�B�uB��B�OB��B�dBo�B\)BI�BA�B9[B#�BxB	:B
�B
�/B
ŢB
�>B
��B
�\B
�/B
�B
w�B
n�B
Q�B
*B
}B
fB
<B	��B	�B	�B	�~B	�B	�dB	�4B	��B	��B	�QB	�B	s�B	i�B	gzB	g{B	fsB	eoB	b\B	Z+B	N�B	H�B	G�B	F�B	E�B	D�B	B�B	?�B	;vB	4HB	*B	#�B	�B	�B	~B	yB	yB	wB	hB	bB	
OB	+B��B��B��B�B�B�B�mB�aB�aB�UB�PB�=B�B��B��B��B¤B��B�fB�UB�CB�7B�,B�B�B��B��B��B��B��B��B��B�xB�fB�\B�NB�BB�6B�)B�"BB{�By�Bw�Bw�Bu�Bs�Bt�Bs�Bw�Bt�Bo�Bk�Bi�Bh�Bg�Bf~BcnBaaB_TB^MB]FB\AB\BBZ6B[;BbgBh�Bh�Bl�Bn�Bs�Bu�Bz�B�B}By�By�BBB~B}B| B{�Bz�B~B�/B�"B~B{�B{�B}Bw�Bu�Bu�Bu�Bx�B�B�B�B�B�B�B�B�BB~	B}B~B�4B�YB�`B�dB�rB��B��B��B�WB�^B�^B�aB�gB�rB��B��B£BƹB��B��B��B��B�	B�B�B�$B�(B�4B�aB�yB�B��B��B��B��B��B��B��B��B��B��B��B��B��B�	B�	B	"B	'B	)B	0B	QB	bB	`B	fB	mB	xB	}B	~B	�B	�B	�B	�B	�B	�B	 �B	$�B	%�B	'�B	+B	.#B	0,B	2;B	3@B	:lB	=�B	@�B	A�B	B�B	D�B	F�B	I�B	L�B	L�B	M�B	M�B	N�B	O�B	Q�B	TB	WB	Z+B	^BB	bZB	deB	i�B	o�B	v�B	{�B	~B	B	�B	�B	�#B	�&B	�)B	�7B	�9B	�=B	�TB	�oB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�B	�B	�B	�.B	�8B	�<B	�CB	�XB	�bB	�iB	��B	��B	B	B	ßB	ĦB	ūB	ŭB	ưB	ǹB	ǹB	ȼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�9B	�9B	�7B	�?B	�AB	�@B	�=B	�BB	�DB	�JB	�KB	�HB	�LB	�QB	�RB	�QB	�UB	�\B	�TB	�VB	�]B	�cB	�bB	�nB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
!B
VB
�B
�B
$�B
,B
23B
9\B
=yB
B�B
J�B
N�B
S�B
Z!B
^=B
bUB
d_B
i{B
l�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.01 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214352016053112143520160531121435  AO  ARCAADJP                                                                    20140721230541    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230541  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230541  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121435  IP                  G�O�G�O�G�O�                