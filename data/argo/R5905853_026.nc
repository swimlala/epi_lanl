CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:27:16Z creation;2022-06-04T17:27:16Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604172716  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�嗟ۗS1   @���X�@.�(�\�c.V�u1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BI33BO��BX  B^��BjffBo��Bw33B�  B�  B�  B�  B���B�33B�  B�  B�33B���B���B�  B�  B�  B�  B�  B���B�33BǙ�B�  B�  B�  B���Bۙ�B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C�C�fC�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D��3D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @p�@��@��A ��A ��A@��A`��A\)A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBIp�BO�BX=qB_
>Bj��Bo�Bwp�B��B��B��B��B��RB�Q�B��B��B�Q�B��B��B��B��B��B��B��B��RB�Q�BǸRB��B��B��B��B۸RB��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C\C(�C(�C��C��C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CC��CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX(�CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C�{C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+
=D+�=D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW�=DX
=DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�ED��D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D��D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D�D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D�~�D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D���D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D���D���D��D�ED���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��&A��pA�ɆA�ƨA���A�� A��sA���A��`A���A��A��fA��.A� 4A�  A�A�SA��A��A�!�A�%�A�+�A�6�A�;dA�(�A��A�ӏA�B�A٭CA��A�_A��#A�AɀiA�$tA���A�SA��A���A��=A�XA�_�A�W?A���A���A��PA�*�A���A� 4A�w2A�f�A�I�A�X�A��dA�WsA���A��A�s�A�
�A���AƨA|v`Az�Ar�NAn��Am�Ak��Ai($Ae�Ab�[A`ĜA^2aAZ��AV�ARxAPT�AM|AIݘAG�@AE��ACkQAA6zA?0�A=��A=�A=<6A<��A:��A9ѷA89�A5�\A5_pA4~�A3��A2S�A/_A,�A,�A+��A+6A*�mA),�A(��A'��A'a|A'CA&�DA%��A$�}A$�XA$a|A#�6A#�rA#kQA#\�A#+A!QA !�A(A�hAG�A��A�#A��A#�A	A�5A��A�PA�,A�A?}A��A�SA��AqA��A�AjA��A8A��AN�A=qA%�A�A�A�zAb�A)�A��A��A��A%Av�A0�A��AH�AoA��A��AzxA]dA=qA�A�A-wAx�A��Aa�A2�A
��A
i�A	�BA	W?A�HA�1Ah
AqA��AZA�A^5A�vArGA��AH�A	A�3A�~A<�A�A�A}�A�WAMA+A ��@��6@�Vm@�=q@���@�ff@��D@�^�@��u@�)�@��m@��N@�x�@��@��@���@���@��$@��v@���@�\@�{@�1�@�@�X@�0�@�&�@�C@��@��U@�	@@��.@�@�z@��@�e,@��@�d�@�@��@镁@��@� �@��@�X�@�W�@��o@��8@�@�Z�@��H@�+k@��@�/@��@�b@�f�@���@���@�G@߈f@�/�@ލ�@�7@ݥ�@ܶ�@�v`@�=@��@ڞ@�M@�g8@�h
@�E�@���@ٵt@� i@�^5@�#:@��@���@�^�@ֿ�@֚�@�O@Ձ@�RT@Լj@�r�@�-�@ӶF@��@Ҍ�@�'R@��d@�S�@���@Е�@�g8@�_@��@�y�@�y>@�x@�
=@̔F@�~(@�G@� i@ʆY@�^5@��)@ɀ4@��@ȵ�@�`�@��@�\)@�*0@��X@�6�@���@�8@��@İ�@�M@�
=@®}@�c @�]d@�V�@�R�@�=q@��:@��@�oi@��@�o@�_@�A @� \@�Ov@�J@���@�n/@�2a@��	@��B@��_@�e�@� �@��*@�#�@�($@�p�@�*0@�͟@���@�7�@��t@�zx@�/@��E@��b@�z�@��T@��@��.@�~@��D@��9@���@�e,@�&@���@�n�@�?�@�{@���@�~�@�A�@�)_@��@���@�n/@�?}@�%@�@��"@��x@�I�@��A@��[@�x�@�F@�P�@�;d@�,�@��@�~�@�_@�@���@�x�@��@��/@���@�i�@���@��@�-w@���@�w�@�bN@�l�@���@��@�8@��@�_�@���@��@��@��@���@�{J@�E9@���@��@���@�g�@�=@� i@��@�p;@��@��@��@��}@�qv@�8@��@���@��@�_�@�6@��&@��d@��{@��@�xl@�(�@�1@��@���@���@�T�@�@��@�=q@���@���@�c@��@���@�h
@�6�@��@��@�a�@�@@��@�a|@��@��V@��"@���@���@�Y�@��8@�͟@�� @���@���@�\�@�;d@��@�ѷ@���@�]d@�@��d@���@�}�@�RT@��@��@���@���@�N�@�!�@��@���@��"@� \@�&�@��!@�tT@�h�@�6�@��@��h@�|@�\�@��@��,@��@��1@�Q�@�-@�	�@��d@���@�IR@��@�`�@�G@��t@�N<@��E@���@�p;@�Ft@�6@��]@���@�ݘ@���@��P@�C�@��@��D@���@��F@�K^@�zx@�H�@�1�@�%@���@��$@���@�tT@�Ov@�4@��@��9@��}@���@��N@��#@��@���@��@��@@�iD@�B�@��@��P@��f@��p@��O@���@�	@~��@~_�@}u�@|�|@|�4@|�@|e�@|9X@{��@{@O@z��@z�!@z\�@z_@ye,@x�@wݘ@w+@v��@v�@uhs@t�O@tbN@s�Q@s�:@s>�@r��@r)�@q�T@q�'@qL�@qV@p��@p�I@oخ@o33@n�c@n�'@nGE@m�d@m`B@l�z@lFt@k��@k@O@j��@i�@ix�@hz�@h@g�a@gqv@gA�@f��@e��@e \@d|�@c(@b�@b��@b{�@bkQ@b3�@b_@aϫ@a��@ac@aJ�@`�@_��@_iD@_;d@^�B@^O@]A @\�Y@\�@[b�@[4�@[�@Z�s@ZOv@Z	@Yc�@X�j@X�@X��@Xg8@X7@Wݘ@W��@W~�@Wo@V��@V3�@V	@US&@T��@T�u@Tg8@T7@S�m@SF�@Rxl@RO@R
�@Q��@P�@Pg8@P@O��@Oj�@O�@N�@N�X@N��@Np;@N#:@M�M@M8�@M+@Le�@L'R@L�@K�+@K�w@K\)@KC@K�@J�c@J�,@Js�@J�@I�C@Ik�@Iq@H��@G˒@F��@F�,@F�}@Fu%@FTa@E��@DPH@C��@C�
@C33@B��@BB[@A��@A�@Aϫ@A�@A��@A+�@A�@@�j@@�4@@z�@@M@?�@?��@?&@>��@>s�@>c @>.�@=�=@=@<Z@<2�@<�@<  @;ƨ@;E9@:�@:�R@:�b@:xl@:C�@:�@9�3@9��@94@8PH@7��@7a@7$t@6�@6�X@6��@6h
@6J@5�T@5@5��@5e,@4�@4Ft@3��@3$t@3@2��@2��@2��@2!�@1`B@1�@0�@0�u@04n@/�W@/�0@/�:@/P�@/!-@/�@.�8@.C�@-�T@-j@,��@,��@,?�@+�&@+�a@+J#@+@*��@*҉@*H�@)�>@)��@)@)�X@)rG@)<6@)�@(��@(��@(�p@(��@(��@(��@(Xy@('R@'�]@'��@'�a@'��@'��@'j�@'RT@'H�@';d@'�@&�H@&�R@&�L@&��@&}V@&C�@%�.@%��@%�'@%s�@% \@%�@%�@$�e@$@#�@#�@#ݘ@#�K@#�q@#j�@#C@#(@#�@"��@"ȴ@"��@"($@!��@!}�@!B�@!%@ �$@ �@ q@ -�@��@o@��@�F@}V@s�@\�@�@��@�C@�S@Vm@�@�e@%�@� @��@;d@��@��@Ta@$�@�H@c@\�@B�@4@0�@	l@�p@��@�@`�@2�@�@�;@��@�$@K�@o@��@�'@{�@\�@@�9@��@�7@}�@Vm@!�@��@ѷ@��@��@K^@,=@	�@�@ƨ@��@��@�@j�@$t@��@҉@�X@��@Q@5?@�@��@�Z@��@�9@�C@x�@\�@A @+@�K@�E@�[@Ĝ@�I@z�@Q�@>B@7�@M@	�@�@��@�Q@�@�$@t�@U�@8@�@��@��@kQ@GE@	@�o@�@�^@�7@rG@^�@�@��@�Y@h�@bN@Xy@9X@��@�g@�:@e�@4�@
��@
�R@
��@
Ta@
GE@
#:@	�Z@	ԕ@	��@	�~@	hs@	7L@	�@	;@��@��@ی11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��&A��pA�ɆA�ƨA���A�� A��sA���A��`A���A��A��fA��.A� 4A�  A�A�SA��A��A�!�A�%�A�+�A�6�A�;dA�(�A��A�ӏA�B�A٭CA��A�_A��#A�AɀiA�$tA���A�SA��A���A��=A�XA�_�A�W?A���A���A��PA�*�A���A� 4A�w2A�f�A�I�A�X�A��dA�WsA���A��A�s�A�
�A���AƨA|v`Az�Ar�NAn��Am�Ak��Ai($Ae�Ab�[A`ĜA^2aAZ��AV�ARxAPT�AM|AIݘAG�@AE��ACkQAA6zA?0�A=��A=�A=<6A<��A:��A9ѷA89�A5�\A5_pA4~�A3��A2S�A/_A,�A,�A+��A+6A*�mA),�A(��A'��A'a|A'CA&�DA%��A$�}A$�XA$a|A#�6A#�rA#kQA#\�A#+A!QA !�A(A�hAG�A��A�#A��A#�A	A�5A��A�PA�,A�A?}A��A�SA��AqA��A�AjA��A8A��AN�A=qA%�A�A�A�zAb�A)�A��A��A��A%Av�A0�A��AH�AoA��A��AzxA]dA=qA�A�A-wAx�A��Aa�A2�A
��A
i�A	�BA	W?A�HA�1Ah
AqA��AZA�A^5A�vArGA��AH�A	A�3A�~A<�A�A�A}�A�WAMA+A ��@��6@�Vm@�=q@���@�ff@��D@�^�@��u@�)�@��m@��N@�x�@��@��@���@���@��$@��v@���@�\@�{@�1�@�@�X@�0�@�&�@�C@��@��U@�	@@��.@�@�z@��@�e,@��@�d�@�@��@镁@��@� �@��@�X�@�W�@��o@��8@�@�Z�@��H@�+k@��@�/@��@�b@�f�@���@���@�G@߈f@�/�@ލ�@�7@ݥ�@ܶ�@�v`@�=@��@ڞ@�M@�g8@�h
@�E�@���@ٵt@� i@�^5@�#:@��@���@�^�@ֿ�@֚�@�O@Ձ@�RT@Լj@�r�@�-�@ӶF@��@Ҍ�@�'R@��d@�S�@���@Е�@�g8@�_@��@�y�@�y>@�x@�
=@̔F@�~(@�G@� i@ʆY@�^5@��)@ɀ4@��@ȵ�@�`�@��@�\)@�*0@��X@�6�@���@�8@��@İ�@�M@�
=@®}@�c @�]d@�V�@�R�@�=q@��:@��@�oi@��@�o@�_@�A @� \@�Ov@�J@���@�n/@�2a@��	@��B@��_@�e�@� �@��*@�#�@�($@�p�@�*0@�͟@���@�7�@��t@�zx@�/@��E@��b@�z�@��T@��@��.@�~@��D@��9@���@�e,@�&@���@�n�@�?�@�{@���@�~�@�A�@�)_@��@���@�n/@�?}@�%@�@��"@��x@�I�@��A@��[@�x�@�F@�P�@�;d@�,�@��@�~�@�_@�@���@�x�@��@��/@���@�i�@���@��@�-w@���@�w�@�bN@�l�@���@��@�8@��@�_�@���@��@��@��@���@�{J@�E9@���@��@���@�g�@�=@� i@��@�p;@��@��@��@��}@�qv@�8@��@���@��@�_�@�6@��&@��d@��{@��@�xl@�(�@�1@��@���@���@�T�@�@��@�=q@���@���@�c@��@���@�h
@�6�@��@��@�a�@�@@��@�a|@��@��V@��"@���@���@�Y�@��8@�͟@�� @���@���@�\�@�;d@��@�ѷ@���@�]d@�@��d@���@�}�@�RT@��@��@���@���@�N�@�!�@��@���@��"@� \@�&�@��!@�tT@�h�@�6�@��@��h@�|@�\�@��@��,@��@��1@�Q�@�-@�	�@��d@���@�IR@��@�`�@�G@��t@�N<@��E@���@�p;@�Ft@�6@��]@���@�ݘ@���@��P@�C�@��@��D@���@��F@�K^@�zx@�H�@�1�@�%@���@��$@���@�tT@�Ov@�4@��@��9@��}@���@��N@��#@��@���@��@��@@�iD@�B�@��@��P@��f@��p@��O@���@�	@~��@~_�@}u�@|�|@|�4@|�@|e�@|9X@{��@{@O@z��@z�!@z\�@z_@ye,@x�@wݘ@w+@v��@v�@uhs@t�O@tbN@s�Q@s�:@s>�@r��@r)�@q�T@q�'@qL�@qV@p��@p�I@oخ@o33@n�c@n�'@nGE@m�d@m`B@l�z@lFt@k��@k@O@j��@i�@ix�@hz�@h@g�a@gqv@gA�@f��@e��@e \@d|�@c(@b�@b��@b{�@bkQ@b3�@b_@aϫ@a��@ac@aJ�@`�@_��@_iD@_;d@^�B@^O@]A @\�Y@\�@[b�@[4�@[�@Z�s@ZOv@Z	@Yc�@X�j@X�@X��@Xg8@X7@Wݘ@W��@W~�@Wo@V��@V3�@V	@US&@T��@T�u@Tg8@T7@S�m@SF�@Rxl@RO@R
�@Q��@P�@Pg8@P@O��@Oj�@O�@N�@N�X@N��@Np;@N#:@M�M@M8�@M+@Le�@L'R@L�@K�+@K�w@K\)@KC@K�@J�c@J�,@Js�@J�@I�C@Ik�@Iq@H��@G˒@F��@F�,@F�}@Fu%@FTa@E��@DPH@C��@C�
@C33@B��@BB[@A��@A�@Aϫ@A�@A��@A+�@A�@@�j@@�4@@z�@@M@?�@?��@?&@>��@>s�@>c @>.�@=�=@=@<Z@<2�@<�@<  @;ƨ@;E9@:�@:�R@:�b@:xl@:C�@:�@9�3@9��@94@8PH@7��@7a@7$t@6�@6�X@6��@6h
@6J@5�T@5@5��@5e,@4�@4Ft@3��@3$t@3@2��@2��@2��@2!�@1`B@1�@0�@0�u@04n@/�W@/�0@/�:@/P�@/!-@/�@.�8@.C�@-�T@-j@,��@,��@,?�@+�&@+�a@+J#@+@*��@*҉@*H�@)�>@)��@)@)�X@)rG@)<6@)�@(��@(��@(�p@(��@(��@(��@(Xy@('R@'�]@'��@'�a@'��@'��@'j�@'RT@'H�@';d@'�@&�H@&�R@&�L@&��@&}V@&C�@%�.@%��@%�'@%s�@% \@%�@%�@$�e@$@#�@#�@#ݘ@#�K@#�q@#j�@#C@#(@#�@"��@"ȴ@"��@"($@!��@!}�@!B�@!%@ �$@ �@ q@ -�@��@o@��@�F@}V@s�@\�@�@��@�C@�S@Vm@�@�e@%�@� @��@;d@��@��@Ta@$�@�H@c@\�@B�@4@0�@	l@�p@��@�@`�@2�@�@�;@��@�$@K�@o@��@�'@{�@\�@@�9@��@�7@}�@Vm@!�@��@ѷ@��@��@K^@,=@	�@�@ƨ@��@��@�@j�@$t@��@҉@�X@��@Q@5?@�@��@�Z@��@�9@�C@x�@\�@A @+@�K@�E@�[@Ĝ@�I@z�@Q�@>B@7�@M@	�@�@��@�Q@�@�$@t�@U�@8@�@��@��@kQ@GE@	@�o@�@�^@�7@rG@^�@�@��@�Y@h�@bN@Xy@9X@��@�g@�:@e�@4�@
��@
�R@
��@
Ta@
GE@
#:@	�Z@	ԕ@	��@	�~@	hs@	7L@	�@	;@��@��@ی11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�0B��BɆBɆB�lB˒B��B�DB�jB�jB�PB�B��B��B�HBЗB� B�BݘB�BB��B�BB.cBQ Bj�B{�B	��B	��B	�oB	z^B	~�B	��B	��B	�B	�B
(�B
L�B
5�B
c�B
[�B
aB
�B	�HB	��B
KB

=B	��B	�,B	߾B	��B	�_B	B	�	B	��B	�/B	��B	�hB	�7B	��B	�PB	�=B	��B	m�B	IB	C�B	<�B	6FB	0!B	'B	�B	�B	�B�XB�B��B�BżBðB��B�B��BĶB�%BƨB�_B��B�3B��B��B�B�	B�{BԕB�BچB�BܬB�BB� B��B��B�B��B	EB	PB	+kB	%zB	#�B	1AB	;�B	D�B	T�B	^B	^�B	`�B	Y�B	N�B	H1B	_B	kB	u�B	�KB	�tB	�9B	�vB	��B	��B	�B	��B	��B	��B	�GB	�XB	��B	�SB	�)B	�B	ĶB	��B	��B	ּB	׍B	׍B	��B	��B	��B	خB	��B	�
B	רB	��B	��B	��B	ΊB	ϑB	��B	�NB	�B	�B	�/B	�B	�vB	�|B	� B	�B	�TB	��B	�dB	�IB	�5B	�\B	��B	�NB	�B	��B	��B	�B	�5B	��B	�OB	�CB	ۦB	�kB	ٚB	�yB	��B	׍B	רB	��B	��B	��B	רB	�$B	׍B	ּB	�B	׍B	�sB	ּB	ּB	֡B	��B	֡B	�YB	��B	��B	��B	��B	�B	�CB	ݘB	ބB	�B	�'B	ߤB	ߤB	�!B	��B	�OB	�dB	��B	��B	�xB	�CB	��B	�B	�B	��B	�]B	�CB	�CB	�)B	��B	�]B	ܒB	ݲB	�;B	�B	�bB	�mB	�kB	�)B	��B	�B	�IB	��B	�B	��B	��B	�B	�B	��B	�B	��B	�0B	�B	�KB	�B	�B	�B	�B	�B	�B	��B	�`B	�B	��B	�FB	�B	��B	�LB	�B	��B	��B	�qB	�"B	�"B	�"B	�"B	�B	�B	�KB	��B	�B	�B	�0B	��B	��B	�B	��B	�mB	��B	�B	�B	�LB	��B	�zB	�`B	��B	�B	��B	�B	�0B	�0B	�B	�QB	�B	�B	�eB	�B	�6B	�"B	�B	�B	�*B	�B	��B	�=B	�WB	�B	��B	�)B	��B	��B	� B	��B	�B	��B	�UB	��B	�'B	�UB	�B	��B	�B	�hB	��B	��B	�?B	�B	�B	��B	�?B	��B	��B	�2B	�2B	��B	��B	��B	��B	��B	��B	�JB	��B	�B	��B	��B	�B	�PB	�6B	�B	��B	�xB	��B	��B	�PB	�PB	��B	��B	�"B	��B	��B	��B	�"B	�B	�<B	�B	�"B	��B	�jB	�"B	�<B	��B	��B	��B
 4B
 �B	�cB	��B	�(B	�wB
oB
[B
�B
�B
aB
�B
�B
9B
9B
�B
B
�B
�B
�B
fB
	7B
	RB

	B

=B
<B
�B
�B
�B
<B
PB

rB
)B
DB
DB
)B

�B

�B

�B
DB
^B
0B
JB
�B
jB
�B
�B
�B
�B
VB
(B
�B
B
.B
�B
B
�B
:B
:B
oB
�B
[B
&B
&B
B
�B
&B
[B
�B
�B
@B
�B
�B
[B
uB
,B
�B
�B
2B
2B
�B
�B
�B
SB
�B
YB
sB
YB
?B
�B
B
�B
�B
�B
�B
#B
=B
�B
xB
jB
�B
 �B
!|B
!�B
!�B
"4B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#B
#�B
"�B
$&B
#�B
#�B
$&B
$tB
%FB
&�B
(sB
)yB
)�B
)�B
)�B
)�B
)�B
*eB
*�B
*B
*eB
+kB
*�B
)yB
(�B
(�B
(�B
)DB
)yB
)yB
)�B
*�B
,B
,B
,WB
,WB
+�B
,qB
,WB
,B
,�B
-�B
./B
-�B
.B
.cB
.IB
.IB
.IB
./B
.IB
.cB
.}B
.�B
.�B
/ B
/�B
0UB
0�B
0�B
1�B
2-B
2�B
2�B
2�B
2|B
2�B
2�B
2�B
2�B
2�B
2�B
3hB
3�B
3�B
4B
4B
49B
4B
4�B
4�B
5B
5B
5?B
5�B
6�B
6�B
7LB
7B
7B
72B
7�B
7�B
7�B
8lB
8RB
8�B
9XB
9�B
9�B
9�B
:*B
:^B
:DB
:^B
;0B
;B
;�B
;dB
;�B
<B
<6B
<�B
<�B
=qB
=VB
=�B
=�B
=�B
>(B
>B
>B
>B
>BB
>�B
?B
?B
?cB
?�B
?�B
?�B
?�B
?�B
@ B
?�B
@B
@4B
@B
@ B
@�B
AB
A;B
AUB
A�B
BAB
B�B
CaB
C�B
D3B
DMB
DMB
DMB
D�B
DgB
ESB
F?B
F�B
GB
G+B
HfB
H�B
H�B
HfB
HB
G�B
G�B
G�B
H�B
HfB
H�B
HfB
H�B
H�B
IRB
I7B
IB
H�B
IB
I�B
J	B
J�B
J�B
KxB
K�B
L0B
LJB
L0B
L�B
MB
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O(B
O�B
O�B
O�B
O�B
O�B
PHB
P�B
RB
RoB
R�B
S@B
S�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
W$B
W
B
W�B
W�B
W�B
XB
XB
X+B
XB
XB
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
YKB
Y�B
ZB
ZB
ZB
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
\B
\xB
\�B
\�B
\�B
]B
]/B
]IB
]dB
]dB
]�B
]�B
^5B
^OB
^jB
^�B
^�B
^�B
^�B
_B
_B
_;B
_VB
_�B
_�B
`'B
_�B
`�B
a�B
a�B
a�B
bB
b�B
cB
c B
cnB
c�B
c�B
dB
d@B
dtB
d�B
d�B
dtB
eB
e,B
e,B
e,B
e�B
fB
fLB
fLB
f�B
gB
gB
gB
g�B
g�B
h>B
hXB
h�B
h�B
iB
i*B
i*B
iDB
iDB
i_B
i_B
i�B
i�B
i�B
i�B
jB
jB
j0B
j0B
jB
jB
j�B
j�B
kB
k6B
kB
kB
k6B
kQB
k�B
lB
l"B
l"B
l=B
lWB
l=B
l"B
lqB
lqB
lWB
lWB
lWB
lqB
lqB
l�B
l�B
l�B
m�B
m�B
nB
n/B
n�B
n�B
nIB
nIB
n/B
ncB
n}B
n}B
n}B
n�B
n}B
n}B
n�B
n�B
n�B
n�B
oiB
oOB
oiB
oiB
o�B
p;B
pUB
p�B
qAB
q'B
q�B
q�B
r-B
rGB
raB
r�B
r�B
sB
s�B
s�B
s�B
s�B
tB
tB
tTB
t�B
t�B
t�B
uB
uZB
utB
u�B
u�B
u�B
u�B
vB
v+B
vB
vB
vFB
v`B
vzB
v�B
v�B
v�B
v�B
v�B
wB
w2B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xRB
x�B
x�B
x�B
x�B
y	B
y$B
y>B
yXB
yXB
yrB
yrB
y�B
y�B
y�B
zB
zB
zDB
z^B
z^B
zxB
z�B
z�B
z�B
z�B
z�B
{B
{B
{B
{B
{0B
{JB
{B
{�B
{�B
{�B
|B
|6B
|�B
|�B
|�B
}B
}"B
}<B
}qB
}qB
}qB
}�B
}�B
~B
~�B
~�B
~wB
~�B
~�B
~�B
.B
�B
�B
�B
�OB
��B
��B
�B
��B
�;B
�oB
�oB
��B
��B
��B
�B
�[B
�AB
�uB
�uB
�u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�0B��BɆBɆB�lB˒B��B�DB�jB�jB�PB�B��B��B�HBЗB� B�BݘB�BB��B�BB.cBQ Bj�B{�B	��B	��B	�oB	z^B	~�B	��B	��B	�B	�B
(�B
L�B
5�B
c�B
[�B
aB
�B	�HB	��B
KB

=B	��B	�,B	߾B	��B	�_B	B	�	B	��B	�/B	��B	�hB	�7B	��B	�PB	�=B	��B	m�B	IB	C�B	<�B	6FB	0!B	'B	�B	�B	�B�XB�B��B�BżBðB��B�B��BĶB�%BƨB�_B��B�3B��B��B�B�	B�{BԕB�BچB�BܬB�BB� B��B��B�B��B	EB	PB	+kB	%zB	#�B	1AB	;�B	D�B	T�B	^B	^�B	`�B	Y�B	N�B	H1B	_B	kB	u�B	�KB	�tB	�9B	�vB	��B	��B	�B	��B	��B	��B	�GB	�XB	��B	�SB	�)B	�B	ĶB	��B	��B	ּB	׍B	׍B	��B	��B	��B	خB	��B	�
B	רB	��B	��B	��B	ΊB	ϑB	��B	�NB	�B	�B	�/B	�B	�vB	�|B	� B	�B	�TB	��B	�dB	�IB	�5B	�\B	��B	�NB	�B	��B	��B	�B	�5B	��B	�OB	�CB	ۦB	�kB	ٚB	�yB	��B	׍B	רB	��B	��B	��B	רB	�$B	׍B	ּB	�B	׍B	�sB	ּB	ּB	֡B	��B	֡B	�YB	��B	��B	��B	��B	�B	�CB	ݘB	ބB	�B	�'B	ߤB	ߤB	�!B	��B	�OB	�dB	��B	��B	�xB	�CB	��B	�B	�B	��B	�]B	�CB	�CB	�)B	��B	�]B	ܒB	ݲB	�;B	�B	�bB	�mB	�kB	�)B	��B	�B	�IB	��B	�B	��B	��B	�B	�B	��B	�B	��B	�0B	�B	�KB	�B	�B	�B	�B	�B	�B	��B	�`B	�B	��B	�FB	�B	��B	�LB	�B	��B	��B	�qB	�"B	�"B	�"B	�"B	�B	�B	�KB	��B	�B	�B	�0B	��B	��B	�B	��B	�mB	��B	�B	�B	�LB	��B	�zB	�`B	��B	�B	��B	�B	�0B	�0B	�B	�QB	�B	�B	�eB	�B	�6B	�"B	�B	�B	�*B	�B	��B	�=B	�WB	�B	��B	�)B	��B	��B	� B	��B	�B	��B	�UB	��B	�'B	�UB	�B	��B	�B	�hB	��B	��B	�?B	�B	�B	��B	�?B	��B	��B	�2B	�2B	��B	��B	��B	��B	��B	��B	�JB	��B	�B	��B	��B	�B	�PB	�6B	�B	��B	�xB	��B	��B	�PB	�PB	��B	��B	�"B	��B	��B	��B	�"B	�B	�<B	�B	�"B	��B	�jB	�"B	�<B	��B	��B	��B
 4B
 �B	�cB	��B	�(B	�wB
oB
[B
�B
�B
aB
�B
�B
9B
9B
�B
B
�B
�B
�B
fB
	7B
	RB

	B

=B
<B
�B
�B
�B
<B
PB

rB
)B
DB
DB
)B

�B

�B

�B
DB
^B
0B
JB
�B
jB
�B
�B
�B
�B
VB
(B
�B
B
.B
�B
B
�B
:B
:B
oB
�B
[B
&B
&B
B
�B
&B
[B
�B
�B
@B
�B
�B
[B
uB
,B
�B
�B
2B
2B
�B
�B
�B
SB
�B
YB
sB
YB
?B
�B
B
�B
�B
�B
�B
#B
=B
�B
xB
jB
�B
 �B
!|B
!�B
!�B
"4B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#B
#�B
"�B
$&B
#�B
#�B
$&B
$tB
%FB
&�B
(sB
)yB
)�B
)�B
)�B
)�B
)�B
*eB
*�B
*B
*eB
+kB
*�B
)yB
(�B
(�B
(�B
)DB
)yB
)yB
)�B
*�B
,B
,B
,WB
,WB
+�B
,qB
,WB
,B
,�B
-�B
./B
-�B
.B
.cB
.IB
.IB
.IB
./B
.IB
.cB
.}B
.�B
.�B
/ B
/�B
0UB
0�B
0�B
1�B
2-B
2�B
2�B
2�B
2|B
2�B
2�B
2�B
2�B
2�B
2�B
3hB
3�B
3�B
4B
4B
49B
4B
4�B
4�B
5B
5B
5?B
5�B
6�B
6�B
7LB
7B
7B
72B
7�B
7�B
7�B
8lB
8RB
8�B
9XB
9�B
9�B
9�B
:*B
:^B
:DB
:^B
;0B
;B
;�B
;dB
;�B
<B
<6B
<�B
<�B
=qB
=VB
=�B
=�B
=�B
>(B
>B
>B
>B
>BB
>�B
?B
?B
?cB
?�B
?�B
?�B
?�B
?�B
@ B
?�B
@B
@4B
@B
@ B
@�B
AB
A;B
AUB
A�B
BAB
B�B
CaB
C�B
D3B
DMB
DMB
DMB
D�B
DgB
ESB
F?B
F�B
GB
G+B
HfB
H�B
H�B
HfB
HB
G�B
G�B
G�B
H�B
HfB
H�B
HfB
H�B
H�B
IRB
I7B
IB
H�B
IB
I�B
J	B
J�B
J�B
KxB
K�B
L0B
LJB
L0B
L�B
MB
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O(B
O�B
O�B
O�B
O�B
O�B
PHB
P�B
RB
RoB
R�B
S@B
S�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
W$B
W
B
W�B
W�B
W�B
XB
XB
X+B
XB
XB
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
YKB
Y�B
ZB
ZB
ZB
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
\B
\xB
\�B
\�B
\�B
]B
]/B
]IB
]dB
]dB
]�B
]�B
^5B
^OB
^jB
^�B
^�B
^�B
^�B
_B
_B
_;B
_VB
_�B
_�B
`'B
_�B
`�B
a�B
a�B
a�B
bB
b�B
cB
c B
cnB
c�B
c�B
dB
d@B
dtB
d�B
d�B
dtB
eB
e,B
e,B
e,B
e�B
fB
fLB
fLB
f�B
gB
gB
gB
g�B
g�B
h>B
hXB
h�B
h�B
iB
i*B
i*B
iDB
iDB
i_B
i_B
i�B
i�B
i�B
i�B
jB
jB
j0B
j0B
jB
jB
j�B
j�B
kB
k6B
kB
kB
k6B
kQB
k�B
lB
l"B
l"B
l=B
lWB
l=B
l"B
lqB
lqB
lWB
lWB
lWB
lqB
lqB
l�B
l�B
l�B
m�B
m�B
nB
n/B
n�B
n�B
nIB
nIB
n/B
ncB
n}B
n}B
n}B
n�B
n}B
n}B
n�B
n�B
n�B
n�B
oiB
oOB
oiB
oiB
o�B
p;B
pUB
p�B
qAB
q'B
q�B
q�B
r-B
rGB
raB
r�B
r�B
sB
s�B
s�B
s�B
s�B
tB
tB
tTB
t�B
t�B
t�B
uB
uZB
utB
u�B
u�B
u�B
u�B
vB
v+B
vB
vB
vFB
v`B
vzB
v�B
v�B
v�B
v�B
v�B
wB
w2B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xRB
x�B
x�B
x�B
x�B
y	B
y$B
y>B
yXB
yXB
yrB
yrB
y�B
y�B
y�B
zB
zB
zDB
z^B
z^B
zxB
z�B
z�B
z�B
z�B
z�B
{B
{B
{B
{B
{0B
{JB
{B
{�B
{�B
{�B
|B
|6B
|�B
|�B
|�B
}B
}"B
}<B
}qB
}qB
}qB
}�B
}�B
~B
~�B
~�B
~wB
~�B
~�B
~�B
.B
�B
�B
�B
�OB
��B
��B
�B
��B
�;B
�oB
�oB
��B
��B
��B
�B
�[B
�AB
�uB
�uB
�u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104853  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172716  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172716  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172716                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022724  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022724  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                