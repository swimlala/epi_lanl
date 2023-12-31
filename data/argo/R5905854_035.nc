CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:50:46Z creation;2022-06-04T17:50:46Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175046  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               #A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��q�\�1   @��qq��@0s33333�c)&�x��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�ffB���B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�33B���B˙�B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4�C6  C8�C9��C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Du��Dvy�Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @p�@��@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh=qBp=qBx��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B�Q�B��B˸RB��B��B��B��B��B��B��B��B��B��B��B��C \C(�C\C\C\C
\C\C\C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2(�C4(�C6\C8(�C9�)C<\C>\C@\CB\CD\CF\CH\CJ\CK��CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd(�Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D}qD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D}qD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D �qD!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA
=DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Du�qDv}qDw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D��D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�ED���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AʐbAʓ�AʒAʑ4Aʊ�Aʎ�Aʓ�Aʛ�AʖSAʗ�Aʟ�Aʟ�Aʝ~Aʝ�Aʞ�Aʜ�Aʞ�Aʡ�AʞAʟ!Aʖ�AʌA�`A���Aɀ�A�(XA�@A�A�{A��A�:A�eA�.A�?HA�O�AɯA��sAɲ�A�h
A�O�A�1'A�#�A�S�A�\]A���A���A�ԕA���A��A�?}A���A��1A�"A�C�A���A��A��uA���A��A���A��A�5?A�W�A�~A��nA��A�:*A�6�A���A��A���A�9�A��aA��A���A��A���A��A���A�+�A� iA���A��]A�aA�uZA���A�?A���A��vA�;0A~�Az�DAvU2AnzAk�Ahr�Af?�Ab}VA_p;A]�6A\qAYZ�AT�AO�5AM��AJcAG�>AF��AFN�AD�AA�A?�fA=A;,=A8jA71A6��A6��A5��A4A3�*A3X�A2��A1`BA0�A.�A,��A+	lA*�gA*�A(+kA'�$A&��A&4nA$��A$>BA$1�A$��A$3�A"��A"�gA"rGA!y�A!M�A �fA�rA�_A�9A�oA��A!�A�8Az�A��A�'Am]A7�A��A�A�
AoA{AxA��A�Ar�A�EA>�ADgAJ�A'�A�A��Az�A�$A  A:�A�A��A�HA�A[�A�AϫA�A��A�	A�LA#�A�AOA!�A}�A��A
�A
@�A	��A	A�A��A�ATaA��AcA�`A \A+�A��A�A/�AY�AU�AhsAz�AI�AHA+�AںA4nAS&AA�Ao A!�A �ZA �A �2A z@��~@��z@��@��@�Z�@��e@��+@�Ov@��X@���@�C-@�҉@�@�@��@��'@�R�@�C@�ݘ@���@�6@��Z@�Q�@�ߤ@�T�@�N�@�V@�kQ@��U@��@�u@�"h@���@�J�@�o�@��@�b�@��U@�x@���@�ߤ@��@�/�@�֡@��@���@���@���@��6@�,�@��M@�8�@�s@�֡@���@ל@ק�@��Z@��@��@�@�9X@؂A@؊r@�p;@�J�@��@��@��v@���@��5@��?@�h�@նF@�;d@��@��"@Կ�@�Z�@ӄM@�Y@Ғ�@�h�@�ƨ@�/�@��2@И_@��@϶F@��Q@��T@�7L@ν<@�S�@�b@�_@���@��@�6�@�	@͓�@�!-@̌@�  @˼@�Vm@���@�6�@���@��X@ȁo@��@�*�@���@�t�@��s@�(�@×$@�L�@�`�@�� @�x@�F@��@��@�o@���@���@�x@��t@�Z�@��5@�I�@�b�@�
=@�֡@�u�@�zx@���@���@��@���@���@�J�@��A@��@�1�@���@��;@��6@�K�@�@O@�'R@��)@�9�@��@��Z@�9�@�~(@��@�{J@���@���@�x@�+@��@�1�@��]@��q@���@�`B@���@��F@��@��#@��@��d@��V@�%F@�o@��@��M@��2@��B@���@�!@��@�b�@�C@���@���@���@��@�i�@�@���@�G�@���@�҉@�~�@���@��3@���@��{@�y�@�Dg@��_@��[@�T�@�\)@��@�|�@�K^@��W@�X@��@�E�@�s�@�!@�@���@���@���@�qv@�9�@���@��c@�q@�-�@�@��Q@�y�@��@���@��4@��@��^@�t�@�V@���@��z@�\�@�!@��)@���@�rG@�+�@�o@��@��@��@�]d@�-�@��@���@���@��@��@�}V@��@�� @��M@�0�@��@�Z@�5?@�!�@��@��.@��;@��@�g�@�>�@��@��5@�ѷ@���@�d�@���@���@�zx@�O�@�Y@��/@��@�y>@�!@��)@�ϫ@���@��t@���@��~@�e�@�=@�+@���@���@�I�@��*@�5�@���@��_@��@�	@��o@��@��@��:@�x@�IR@��@���@�i�@�4n@��@���@�S�@�%F@���@���@�-@���@��{@�rG@�\�@��@��'@���@�oi@�1�@�G@��#@��@�|@�\)@�;d@���@��}@��I@���@�a|@�?�@��@���@�c@�.I@��@���@���@�j@�C-@�'R@��.@��:@�[W@�F@���@���@���@���@�^5@�U2@�?@��@�k@~ߤ@~n�@~?@~{@}�X@|��@|e�@{�+@{��@{j�@{1�@{S@z��@zn�@zR�@z=q@z)�@y�o@y�z@y\�@x�P@x�@x�@w�F@w�	@w=@v��@v�@v��@vC�@u��@u��@uc�@u�@t��@t��@t?�@t1@s� @s��@sF�@sS@ri�@q��@q�j@q��@q/@p��@p�P@p�@p|�@o��@oC@n�X@n�}@n� @n�@m0�@l9X@k�m@k��@k6z@j��@jv�@jTa@j�@ju@iԕ@i7L@h�U@h`�@g�@g�@f��@fL0@e�j@ef�@d��@dl"@d2�@c��@cl�@bn�@a�@a	l@`�e@`_@_s@^�<@^@]m]@]�@[�+@[\)@[/�@Z�,@Z��@Z\�@Z1�@Y��@Yq@X��@X�I@Xg8@W�*@W�{@WW?@V��@U�@U�~@UL�@T�P@T�`@T�@T��@Ty>@Tj@TFt@T!@S�m@S��@S��@S�{@S+@R�R@R��@ROv@Q�9@Q=�@Q&�@Q�@P�U@PZ@O��@O�6@O��@Oy�@OMj@N҉@M��@MQ�@M�@M@L��@L��@L��@Ly>@LFt@L�@K�V@J�c@J($@I�@Ic@I�@Hy>@G��@F��@F�@Fl�@F-@E��@E�@D�[@D�D@D:�@D7@C�Q@Cb�@B�R@B:*@BC�@A�@A��@A��@AG�@@�@@��@@ѷ@@�p@@��@@tT@?�}@?33@>��@>҉@>z@>{@=�#@=�@<��@<��@<j@<S�@<<�@<'R@;�;@;qv@;33@:�"@:�<@:p;@:?@:�@9�X@9x�@9[W@9(�@8�K@8��@8M@7ݘ@7l�@7�@6��@6�s@6B[@5�3@5s�@5:�@5@4��@4I�@4G@3�@3��@3��@3{J@3>�@3C@2�2@2ff@1��@1��@1|@1�@0��@/��@/��@/O@/C@.��@.8�@.e@-�.@-�@-k�@-+�@-@,�@,�@,�v@,��@,��@,q@+��@+��@+/�@*�X@*�L@*�\@*z@*l�@*R�@*#:@*�@)��@)#�@(�@(��@(��@(,=@'�@'�*@'e�@''�@'�@&��@&��@&xl@&p;@&5?@%�@%7L@%�@$��@$�@$�E@$�U@$tT@#�@#��@#W?@#o@"ߤ@"�@"� @"�+@"YK@".�@"e@"	@!�@!��@!<6@ ��@ �j@ �@ oi@ :�@ @�@|�@�@�"@�y@ߤ@��@ȴ@��@z@kQ@\�@1�@	@�@�H@�~@hs@L�@!�@��@K^@��@��@j�@1�@C@��@�F@Ov@3�@$�@�@��@�d@|@T�@L�@B�@�@Ĝ@�u@Ft@7@�@��@|�@S�@RT@
=@��@��@v�@L0@�Z@�z@��@�S@k�@X@T�@%@�@��@�z@�@bN@Ft@,=@�@��@�@@t�@�@�@�!@�r@i�@J�@�@��@�9@�'@��@�@��@hs@5�@�@��@�Y@�@�@l"@/�@~@  @�A@�;@� @��@8@�@S1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AʐbAʓ�AʒAʑ4Aʊ�Aʎ�Aʓ�Aʛ�AʖSAʗ�Aʟ�Aʟ�Aʝ~Aʝ�Aʞ�Aʜ�Aʞ�Aʡ�AʞAʟ!Aʖ�AʌA�`A���Aɀ�A�(XA�@A�A�{A��A�:A�eA�.A�?HA�O�AɯA��sAɲ�A�h
A�O�A�1'A�#�A�S�A�\]A���A���A�ԕA���A��A�?}A���A��1A�"A�C�A���A��A��uA���A��A���A��A�5?A�W�A�~A��nA��A�:*A�6�A���A��A���A�9�A��aA��A���A��A���A��A���A�+�A� iA���A��]A�aA�uZA���A�?A���A��vA�;0A~�Az�DAvU2AnzAk�Ahr�Af?�Ab}VA_p;A]�6A\qAYZ�AT�AO�5AM��AJcAG�>AF��AFN�AD�AA�A?�fA=A;,=A8jA71A6��A6��A5��A4A3�*A3X�A2��A1`BA0�A.�A,��A+	lA*�gA*�A(+kA'�$A&��A&4nA$��A$>BA$1�A$��A$3�A"��A"�gA"rGA!y�A!M�A �fA�rA�_A�9A�oA��A!�A�8Az�A��A�'Am]A7�A��A�A�
AoA{AxA��A�Ar�A�EA>�ADgAJ�A'�A�A��Az�A�$A  A:�A�A��A�HA�A[�A�AϫA�A��A�	A�LA#�A�AOA!�A}�A��A
�A
@�A	��A	A�A��A�ATaA��AcA�`A \A+�A��A�A/�AY�AU�AhsAz�AI�AHA+�AںA4nAS&AA�Ao A!�A �ZA �A �2A z@��~@��z@��@��@�Z�@��e@��+@�Ov@��X@���@�C-@�҉@�@�@��@��'@�R�@�C@�ݘ@���@�6@��Z@�Q�@�ߤ@�T�@�N�@�V@�kQ@��U@��@�u@�"h@���@�J�@�o�@��@�b�@��U@�x@���@�ߤ@��@�/�@�֡@��@���@���@���@��6@�,�@��M@�8�@�s@�֡@���@ל@ק�@��Z@��@��@�@�9X@؂A@؊r@�p;@�J�@��@��@��v@���@��5@��?@�h�@նF@�;d@��@��"@Կ�@�Z�@ӄM@�Y@Ғ�@�h�@�ƨ@�/�@��2@И_@��@϶F@��Q@��T@�7L@ν<@�S�@�b@�_@���@��@�6�@�	@͓�@�!-@̌@�  @˼@�Vm@���@�6�@���@��X@ȁo@��@�*�@���@�t�@��s@�(�@×$@�L�@�`�@�� @�x@�F@��@��@�o@���@���@�x@��t@�Z�@��5@�I�@�b�@�
=@�֡@�u�@�zx@���@���@��@���@���@�J�@��A@��@�1�@���@��;@��6@�K�@�@O@�'R@��)@�9�@��@��Z@�9�@�~(@��@�{J@���@���@�x@�+@��@�1�@��]@��q@���@�`B@���@��F@��@��#@��@��d@��V@�%F@�o@��@��M@��2@��B@���@�!@��@�b�@�C@���@���@���@��@�i�@�@���@�G�@���@�҉@�~�@���@��3@���@��{@�y�@�Dg@��_@��[@�T�@�\)@��@�|�@�K^@��W@�X@��@�E�@�s�@�!@�@���@���@���@�qv@�9�@���@��c@�q@�-�@�@��Q@�y�@��@���@��4@��@��^@�t�@�V@���@��z@�\�@�!@��)@���@�rG@�+�@�o@��@��@��@�]d@�-�@��@���@���@��@��@�}V@��@�� @��M@�0�@��@�Z@�5?@�!�@��@��.@��;@��@�g�@�>�@��@��5@�ѷ@���@�d�@���@���@�zx@�O�@�Y@��/@��@�y>@�!@��)@�ϫ@���@��t@���@��~@�e�@�=@�+@���@���@�I�@��*@�5�@���@��_@��@�	@��o@��@��@��:@�x@�IR@��@���@�i�@�4n@��@���@�S�@�%F@���@���@�-@���@��{@�rG@�\�@��@��'@���@�oi@�1�@�G@��#@��@�|@�\)@�;d@���@��}@��I@���@�a|@�?�@��@���@�c@�.I@��@���@���@�j@�C-@�'R@��.@��:@�[W@�F@���@���@���@���@�^5@�U2@�?@��@�k@~ߤ@~n�@~?@~{@}�X@|��@|e�@{�+@{��@{j�@{1�@{S@z��@zn�@zR�@z=q@z)�@y�o@y�z@y\�@x�P@x�@x�@w�F@w�	@w=@v��@v�@v��@vC�@u��@u��@uc�@u�@t��@t��@t?�@t1@s� @s��@sF�@sS@ri�@q��@q�j@q��@q/@p��@p�P@p�@p|�@o��@oC@n�X@n�}@n� @n�@m0�@l9X@k�m@k��@k6z@j��@jv�@jTa@j�@ju@iԕ@i7L@h�U@h`�@g�@g�@f��@fL0@e�j@ef�@d��@dl"@d2�@c��@cl�@bn�@a�@a	l@`�e@`_@_s@^�<@^@]m]@]�@[�+@[\)@[/�@Z�,@Z��@Z\�@Z1�@Y��@Yq@X��@X�I@Xg8@W�*@W�{@WW?@V��@U�@U�~@UL�@T�P@T�`@T�@T��@Ty>@Tj@TFt@T!@S�m@S��@S��@S�{@S+@R�R@R��@ROv@Q�9@Q=�@Q&�@Q�@P�U@PZ@O��@O�6@O��@Oy�@OMj@N҉@M��@MQ�@M�@M@L��@L��@L��@Ly>@LFt@L�@K�V@J�c@J($@I�@Ic@I�@Hy>@G��@F��@F�@Fl�@F-@E��@E�@D�[@D�D@D:�@D7@C�Q@Cb�@B�R@B:*@BC�@A�@A��@A��@AG�@@�@@��@@ѷ@@�p@@��@@tT@?�}@?33@>��@>҉@>z@>{@=�#@=�@<��@<��@<j@<S�@<<�@<'R@;�;@;qv@;33@:�"@:�<@:p;@:?@:�@9�X@9x�@9[W@9(�@8�K@8��@8M@7ݘ@7l�@7�@6��@6�s@6B[@5�3@5s�@5:�@5@4��@4I�@4G@3�@3��@3��@3{J@3>�@3C@2�2@2ff@1��@1��@1|@1�@0��@/��@/��@/O@/C@.��@.8�@.e@-�.@-�@-k�@-+�@-@,�@,�@,�v@,��@,��@,q@+��@+��@+/�@*�X@*�L@*�\@*z@*l�@*R�@*#:@*�@)��@)#�@(�@(��@(��@(,=@'�@'�*@'e�@''�@'�@&��@&��@&xl@&p;@&5?@%�@%7L@%�@$��@$�@$�E@$�U@$tT@#�@#��@#W?@#o@"ߤ@"�@"� @"�+@"YK@".�@"e@"	@!�@!��@!<6@ ��@ �j@ �@ oi@ :�@ @�@|�@�@�"@�y@ߤ@��@ȴ@��@z@kQ@\�@1�@	@�@�H@�~@hs@L�@!�@��@K^@��@��@j�@1�@C@��@�F@Ov@3�@$�@�@��@�d@|@T�@L�@B�@�@Ĝ@�u@Ft@7@�@��@|�@S�@RT@
=@��@��@v�@L0@�Z@�z@��@�S@k�@X@T�@%@�@��@�z@�@bN@Ft@,=@�@��@�@@t�@�@�@�!@�r@i�@J�@�@��@�9@�'@��@�@��@hs@5�@�@��@�Y@�@�@l"@/�@~@  @�A@�;@� @��@8@�@S1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�UB�oB��B��B��B��B�oB��B�oB�oB� B� B�B��B��B��B��B��B��B��B� B��B��B�<B��B�NB�FBɺB�
B�`B�wB	
�B	;0B	��B	�JB
6`B
�oB
�B
��B
�B
�B
�8B
�GB
��B
�OBTB2�Bc Bx�B��B��B��B�B�DB��B�B�]B�B�B	�B�B �B�B�B�>B��B�zB��B��B�B��B� B��B��B��B�zB}VBfBESBBB
��B
�DB
��B
�B
�4B
��B
a�B
7�B
!�B
B	��B	�B	�tB	��B	|�B	d�B	N�B	9�B	$@B	�B	�B	tB�!B�DB��B�B�#B�EB��B��B��B�~BٴB��B�B�B�XB	[B	4B	:B	"4B	$tB	&fB	(
B	%zB	�B	B	�B	$�B	0UB	-�B	C-B	J�B	F�B	B�B	GzB	Q�B	tB	cB	��B	��B	��B	��B	��B	�2B	��B	�
B	��B	��B	�5B	�cB	� B	��B	�!B	�TB	�B	�tB	��B	ˬB	�(B	ŢB	żB	ªB	��B	��B	ԕB	҉B	ϫB	οB	� B	֡B	�B	�B	�pB	��B	ߊB	ۦB	�5B	�\B	�nB	��B	�B	�QB	�B	�B	��B	�/B	��B	��B	�B	�B	�'B	�B	خB	ԕB	��B	׍B	өB	�B	��B	��B	�B	��B	��B	�\B	�B	�hB	�B	�DB	�}B	�AB	��B
�B
�B
1B
�B
YB
B	�cB
	7B
�B
�B
�B
�B
tB
zB
aB	��B	��B	�"B	�MB	�QB	�kB	��B	��B	�]B	�B	�@B	�B	�EB	�6B	��B	�>B	�9B	�-B	�hB	�zB	�(B	��B	�JB	��B	��B	�iB	ʦB	�B	�~B	�)B	�^B	�vB	�xB	��B	�-B	�|B	�B	��B	�B	�-B	�|B	��B	��B	�B	�B	یB	�$B	��B	��B	ԯB	�oB	�@B	өB	��B	�?B	��B	ߊB	�B	�4B	�@B	�B	�8B	��B	�yB	�_B	�B	�RB	��B	�B	�B	��B	�6B	�WB	��B	�B	��B	�=B	��B	�B	��B	�wB	��B	��B	�B	�}B	�5B	�UB	�B	�tB	�tB	�TB	�ZB	�RB	�*B	�B	��B	�}B
�B
 �B	��B	��B	�wB	��B	�wB	��B	�<B	��B	�$B	��B	�tB	��B	��B	�UB	�B	�B	��B	��B	�iB	�B	�IB	��B	�)B	��B	��B	�B	��B	�WB	�B	�)B	�wB	�]B	��B	��B	��B	��B	�/B	��B	�B	�B	��B	��B	��B	��B	��B	�fB	��B	��B	�LB	��B	��B	�wB
'B
�B
�B
aB
�B
B
-B
�B
{B
-B
�B
�B
AB
�B
�B
B
�B
�B
aB
�B
�B
�B
�B
�B
�B
SB
�B
�B
�B
?B
�B
B
B
zB
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
�B
+B
EB
�B
	lB
	B
	�B
�B
�B
	�B
�B
�B

�B

�B
)B
�B
�B
�B
jB
6B
B
�B
�B
�B
B
6B
�B
�B
VB
�B
pB
BB
�B
�B
B
�B
bB
�B
B
 B
�B
�B
�B
�B
NB
hB
�B
�B
�B
�B
B
TB
�B
�B
uB
�B
MB
�B
�B
�B
�B
�B
�B
�B
SB
�B
�B
�B
�B
�B
�B
?B
�B
�B
�B
B
yB
�B
KB
�B
KB
B
�B
=B
�B
B
xB
�B
�B
�B
]B
B
�B
CB
)B
�B
xB
dB
dB
dB
B
�B
!B
pB
�B
 �B
 �B
!bB
!�B
!�B
!�B
"hB
"�B
#B
#�B
$ZB
%`B
%`B
%FB
%�B
%�B
&�B
'B
(XB
(�B
)B
)DB
)�B
*0B
*B
)�B
*B
*�B
+�B
,B
,"B
,=B
,�B
-CB
.cB
.cB
.}B
.}B
/ B
/5B
/iB
/�B
0B
/�B
0B
0B
0B
/�B
0;B
0UB
0B
0;B
0!B
0�B
0�B
0�B
0�B
0�B
0�B
1[B
1�B
1�B
1�B
1�B
2B
2B
2|B
2�B
2�B
2�B
2�B
2�B
2�B
33B
3�B
3�B
4B
4TB
4�B
4�B
5B
4�B
5ZB
6+B
6�B
6�B
7�B
8RB
8RB
8RB
8�B
9>B
9XB
9rB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:DB
9�B
9rB
9$B
9$B
9	B
9	B
9$B
9	B
8�B
9$B
9rB
9�B
9�B
9�B
:�B
;dB
;dB
;B
:�B
:�B
;dB
;�B
<PB
<PB
<jB
<B
;B
:�B
;�B
<B
;�B
;�B
;B
;�B
;�B
;�B
<�B
="B
=�B
=�B
=�B
=�B
=�B
=�B
>B
>(B
>�B
>�B
?HB
?}B
?}B
?}B
@�B
B�B
B�B
B�B
B'B
@�B
@�B
@�B
@�B
@�B
A B
AUB
AUB
A;B
AUB
AUB
A�B
B'B
B�B
B�B
B�B
CB
C�B
DgB
D�B
D�B
E9B
ESB
E�B
E�B
FtB
F�B
G+B
GzB
G�B
HfB
IlB
I�B
I�B
I�B
I�B
J	B
J	B
J	B
J=B
JXB
J�B
K^B
K�B
K�B
K�B
K�B
LJB
L�B
LJB
LJB
L~B
L�B
MB
MPB
M�B
M�B
OBB
P}B
P�B
PbB
O�B
PbB
Q�B
R�B
R�B
S@B
S�B
S�B
S�B
S�B
S�B
S�B
TB
T�B
UMB
UgB
UgB
U�B
VB
U�B
W
B
W
B
W$B
W$B
W?B
W?B
W?B
WsB
W�B
W�B
XB
XEB
X�B
X�B
X�B
Y1B
YKB
YeB
Y�B
Y�B
Z7B
Z�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
]IB
]�B
]�B
^B
^B
^B
^5B
^5B
^5B
^5B
^�B
_!B
_;B
_;B
_�B
_�B
`vB
`�B
`�B
`�B
abB
a�B
a�B
a�B
a�B
bB
bhB
bhB
b�B
b�B
b�B
b�B
b�B
c B
cnB
cTB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
d@B
d&B
dZB
d�B
eB
e�B
e�B
e�B
e�B
e�B
fLB
fLB
f2B
fLB
gB
gRB
gmB
g�B
g�B
g�B
g�B
g�B
h>B
hsB
h�B
iB
iDB
iyB
i�B
i�B
i�B
jB
j0B
jKB
jKB
j�B
kB
kkB
kkB
k�B
k�B
k�B
lB
lWB
l�B
l�B
mB
mB
m)B
m)B
m)B
mwB
m�B
m�B
m�B
nB
n/B
nB
nIB
n}B
n�B
n�B
n�B
oB
o�B
o�B
poB
p�B
p�B
p�B
q'B
qvB
q�B
q�B
q�B
q�B
q�B
rB
raB
r|B
r|B
r|B
r�B
r�B
sB
shB
shB
s�B
s�B
tB
tB
tB
t�B
t�B
t�B
t�B
t�B
uZB
utB
u�B
u�B
u�B
u�B
u�B
vFB
vFB
v`B
v�B
v�B
v�B
v�B
v�B
v�B
wLB
wfB
w�B
xB
xB
xlB
xlB
x�B
x�B
x�B
y	B
y	B
yXB
yrB
yrB
yXB
yrB
y�B
y�B
z*B
y�B
y�B
y�B
zB
zDB
z*B
z^B
z^B
z^B
z^B
z�B
z�B
z�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�UB�oB��B��B��B��B�oB��B�oB�oB� B� B�B��B��B��B��B��B��B��B� B��B��B�<B��B�NB�FBɺB�
B�`B�wB	
�B	;0B	��B	�JB
6`B
�oB
�B
��B
�B
�B
�8B
�GB
��B
�OBTB2�Bc Bx�B��B��B��B�B�DB��B�B�]B�B�B	�B�B �B�B�B�>B��B�zB��B��B�B��B� B��B��B��B�zB}VBfBESBBB
��B
�DB
��B
�B
�4B
��B
a�B
7�B
!�B
B	��B	�B	�tB	��B	|�B	d�B	N�B	9�B	$@B	�B	�B	tB�!B�DB��B�B�#B�EB��B��B��B�~BٴB��B�B�B�XB	[B	4B	:B	"4B	$tB	&fB	(
B	%zB	�B	B	�B	$�B	0UB	-�B	C-B	J�B	F�B	B�B	GzB	Q�B	tB	cB	��B	��B	��B	��B	��B	�2B	��B	�
B	��B	��B	�5B	�cB	� B	��B	�!B	�TB	�B	�tB	��B	ˬB	�(B	ŢB	żB	ªB	��B	��B	ԕB	҉B	ϫB	οB	� B	֡B	�B	�B	�pB	��B	ߊB	ۦB	�5B	�\B	�nB	��B	�B	�QB	�B	�B	��B	�/B	��B	��B	�B	�B	�'B	�B	خB	ԕB	��B	׍B	өB	�B	��B	��B	�B	��B	��B	�\B	�B	�hB	�B	�DB	�}B	�AB	��B
�B
�B
1B
�B
YB
B	�cB
	7B
�B
�B
�B
�B
tB
zB
aB	��B	��B	�"B	�MB	�QB	�kB	��B	��B	�]B	�B	�@B	�B	�EB	�6B	��B	�>B	�9B	�-B	�hB	�zB	�(B	��B	�JB	��B	��B	�iB	ʦB	�B	�~B	�)B	�^B	�vB	�xB	��B	�-B	�|B	�B	��B	�B	�-B	�|B	��B	��B	�B	�B	یB	�$B	��B	��B	ԯB	�oB	�@B	өB	��B	�?B	��B	ߊB	�B	�4B	�@B	�B	�8B	��B	�yB	�_B	�B	�RB	��B	�B	�B	��B	�6B	�WB	��B	�B	��B	�=B	��B	�B	��B	�wB	��B	��B	�B	�}B	�5B	�UB	�B	�tB	�tB	�TB	�ZB	�RB	�*B	�B	��B	�}B
�B
 �B	��B	��B	�wB	��B	�wB	��B	�<B	��B	�$B	��B	�tB	��B	��B	�UB	�B	�B	��B	��B	�iB	�B	�IB	��B	�)B	��B	��B	�B	��B	�WB	�B	�)B	�wB	�]B	��B	��B	��B	��B	�/B	��B	�B	�B	��B	��B	��B	��B	��B	�fB	��B	��B	�LB	��B	��B	�wB
'B
�B
�B
aB
�B
B
-B
�B
{B
-B
�B
�B
AB
�B
�B
B
�B
�B
aB
�B
�B
�B
�B
�B
�B
SB
�B
�B
�B
?B
�B
B
B
zB
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
�B
+B
EB
�B
	lB
	B
	�B
�B
�B
	�B
�B
�B

�B

�B
)B
�B
�B
�B
jB
6B
B
�B
�B
�B
B
6B
�B
�B
VB
�B
pB
BB
�B
�B
B
�B
bB
�B
B
 B
�B
�B
�B
�B
NB
hB
�B
�B
�B
�B
B
TB
�B
�B
uB
�B
MB
�B
�B
�B
�B
�B
�B
�B
SB
�B
�B
�B
�B
�B
�B
?B
�B
�B
�B
B
yB
�B
KB
�B
KB
B
�B
=B
�B
B
xB
�B
�B
�B
]B
B
�B
CB
)B
�B
xB
dB
dB
dB
B
�B
!B
pB
�B
 �B
 �B
!bB
!�B
!�B
!�B
"hB
"�B
#B
#�B
$ZB
%`B
%`B
%FB
%�B
%�B
&�B
'B
(XB
(�B
)B
)DB
)�B
*0B
*B
)�B
*B
*�B
+�B
,B
,"B
,=B
,�B
-CB
.cB
.cB
.}B
.}B
/ B
/5B
/iB
/�B
0B
/�B
0B
0B
0B
/�B
0;B
0UB
0B
0;B
0!B
0�B
0�B
0�B
0�B
0�B
0�B
1[B
1�B
1�B
1�B
1�B
2B
2B
2|B
2�B
2�B
2�B
2�B
2�B
2�B
33B
3�B
3�B
4B
4TB
4�B
4�B
5B
4�B
5ZB
6+B
6�B
6�B
7�B
8RB
8RB
8RB
8�B
9>B
9XB
9rB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:DB
9�B
9rB
9$B
9$B
9	B
9	B
9$B
9	B
8�B
9$B
9rB
9�B
9�B
9�B
:�B
;dB
;dB
;B
:�B
:�B
;dB
;�B
<PB
<PB
<jB
<B
;B
:�B
;�B
<B
;�B
;�B
;B
;�B
;�B
;�B
<�B
="B
=�B
=�B
=�B
=�B
=�B
=�B
>B
>(B
>�B
>�B
?HB
?}B
?}B
?}B
@�B
B�B
B�B
B�B
B'B
@�B
@�B
@�B
@�B
@�B
A B
AUB
AUB
A;B
AUB
AUB
A�B
B'B
B�B
B�B
B�B
CB
C�B
DgB
D�B
D�B
E9B
ESB
E�B
E�B
FtB
F�B
G+B
GzB
G�B
HfB
IlB
I�B
I�B
I�B
I�B
J	B
J	B
J	B
J=B
JXB
J�B
K^B
K�B
K�B
K�B
K�B
LJB
L�B
LJB
LJB
L~B
L�B
MB
MPB
M�B
M�B
OBB
P}B
P�B
PbB
O�B
PbB
Q�B
R�B
R�B
S@B
S�B
S�B
S�B
S�B
S�B
S�B
TB
T�B
UMB
UgB
UgB
U�B
VB
U�B
W
B
W
B
W$B
W$B
W?B
W?B
W?B
WsB
W�B
W�B
XB
XEB
X�B
X�B
X�B
Y1B
YKB
YeB
Y�B
Y�B
Z7B
Z�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
]IB
]�B
]�B
^B
^B
^B
^5B
^5B
^5B
^5B
^�B
_!B
_;B
_;B
_�B
_�B
`vB
`�B
`�B
`�B
abB
a�B
a�B
a�B
a�B
bB
bhB
bhB
b�B
b�B
b�B
b�B
b�B
c B
cnB
cTB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
d@B
d&B
dZB
d�B
eB
e�B
e�B
e�B
e�B
e�B
fLB
fLB
f2B
fLB
gB
gRB
gmB
g�B
g�B
g�B
g�B
g�B
h>B
hsB
h�B
iB
iDB
iyB
i�B
i�B
i�B
jB
j0B
jKB
jKB
j�B
kB
kkB
kkB
k�B
k�B
k�B
lB
lWB
l�B
l�B
mB
mB
m)B
m)B
m)B
mwB
m�B
m�B
m�B
nB
n/B
nB
nIB
n}B
n�B
n�B
n�B
oB
o�B
o�B
poB
p�B
p�B
p�B
q'B
qvB
q�B
q�B
q�B
q�B
q�B
rB
raB
r|B
r|B
r|B
r�B
r�B
sB
shB
shB
s�B
s�B
tB
tB
tB
t�B
t�B
t�B
t�B
t�B
uZB
utB
u�B
u�B
u�B
u�B
u�B
vFB
vFB
v`B
v�B
v�B
v�B
v�B
v�B
v�B
wLB
wfB
w�B
xB
xB
xlB
xlB
x�B
x�B
x�B
y	B
y	B
yXB
yrB
yrB
yXB
yrB
y�B
y�B
z*B
y�B
y�B
y�B
zB
zDB
z*B
z^B
z^B
z^B
z^B
z�B
z�B
z�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104949  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175046  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175046  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175046                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025053  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025053  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                