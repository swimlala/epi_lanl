CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:46:28Z creation;2022-06-04T17:46:29Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174628  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��>�1   @�k��@-V�u�d���l�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B�33B�ffB�33B�  B�  B�  B�  B�  B�33B�  B�33B�  B���B���B���B�ffB���B���B�  B�  B�  B�  B�  B�  B�ffB�  B�33B���B���B�  B�  B�  C   C  C  C  C33C	�fC  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{�fD|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @p�@��@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh��Bp��Bx=qB�Q�B��B�Q�B��B��B��B��B��B�Q�B��B�Q�B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��C \C\C\C\CB�C	��C\C��C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.(�C0\C2\C3��C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D}qD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DK�qDL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR�=DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{�=D|�D|��D}�D}��D~�D~��D�D��D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D���D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�~�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�>�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D�~�D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D�D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�x8A�x�A�{A�t�A�u�A��A�}�A؀�A؁;A؃A؃�A؂A؂AA؃GA؃GA؁�A؄�A؇+A�.A�{A�~�A�{�A�x�A�t�A�IRA�"�A�qA���A���A�`A�+�A��A֣�A�	�A�j�A���A��A�(�Aη�A̛	A̅SA�GEA�~�AʜxA�c A��pA��A���A�kA�xA��;A��DA�x8A��A��SA�a�A�49A���A�ѷA�{�A��0A�ǮA�OvA���A�*�A���A��A��A�HA�f2A��JA��`A��A�6A��A��jA��A�҉A���A��$A��gA��/A�C�A�&LA���A���A~��A{L�AzC�Aw��As�Ap��Am�xAjAg�Ad�A`�A\�}AY�`AUT�AR��AO�AK��AI��AHF�AFAD@OAA�?A@B[A=K^A;�uA;E9A:�A:R�A:�A9�*A9~�A9��A9'RA6�.A5�A3O�A0ԕA1�A18A0��A/+kA.b�A.%A-�A-�A+�tA*�LA(�[A(��A(�A)jA+$tA)�vA)"�A(  A'>BA&�}A&[�A%�A#|�A"_�A!�A!�A!��A!��A!h
A �vA .IA��ASA�OAϫA�PA�:A��A*�A�XA�A��AjA�A��A�4A�A7�AQ�A+Aj�A�pAXyA��AԕA9XA��A��A�AjA0�Ao�AK�A�AoiA�A��AzAb�A��A��A,�A/A
u�A
XyA
A A
~A	�hA	&�Ay>A%A�A_As�A�AB[A�TA�A_pA(�A��A��APHA$tA��Al�AR�A�A��A<6A ͟A oiA rG@��@��@���@��}@�D�@���@�H@��@��@��d@�s�@��@��@�S&@�@@��#@��@��Z@�s�@�5�@�ȴ@���@�-�@�Y�@��m@�p�@�W�@�}�@���@�n�@��@���@��@�s�@�%�@�n@�u@��@旍@�!@�q�@�
�@��@�"@嫟@�Y@�l�@��@�Q�@� @���@�m�@��f@�e@�xl@��M@�ԕ@���@�;�@�!�@��@���@�PH@�;�@��g@�RT@�;@غ�@؆Y@��@�e,@֢4@ֆY@֨�@֦L@֥z@�g8@�e@�T�@���@�w�@�a|@�V�@�@ӎ"@��@�ی@�*�@�_@�<6@��`@о@��a@�C@� \@��p@�L0@���@ͶF@͡�@�$t@̘_@̌�@�\�@�4@ˌ~@�ی@��@ɾw@�T�@���@�	@Ǧ�@�7L@�xl@�{@ŷ@��@ç�@�o@µ@�kQ@��T@�p�@��!@�6�@�c@��L@�0U@��N@�a�@��@��_@�bN@�	�@��@��"@�X�@��f@��+@�$@��;@��c@��@�8�@��@���@�]d@�*�@�@��d@�o�@�8�@��K@�^5@���@�(�@���@�%@���@��D@�h�@�%�@���@�t�@�W?@�o@��U@��@�E�@��n@�#�@�Ta@���@���@�l�@�8@��2@�1'@���@�4@��c@��2@��@�͟@���@�Ta@��@��^@�^�@�&�@�
=@���@�}V@�Ta@��@��}@��@��@���@�i�@�2�@�  @��X@�8�@��v@��?@���@�%�@���@�|�@�'�@���@��m@�z@�j@�d�@�S�@�6@��&@�m]@��@��@��o@���@�hs@��@�ی@�bN@�;�@��0@�A @��M@��)@���@�:*@�!�@��&@��F@��@���@�j@���@���@�D�@���@��a@��4@�s@�k�@�F@��X@�{�@�%�@�� @��{@��@�oi@�2�@�4@��@���@�\�@��@��@�q�@��@��@�&�@��U@�n�@��@�p�@�4@�ی@�q@��@��@��u@�H@���@�v`@�4�@��"@��@��@�G@�|�@�>�@��@���@��@�:�@�*�@��@��@@���@�IR@�q@�ی@���@�/�@��@��f@�a�@�7L@��@��@��@�p;@�Xy@�N�@�~@��F@�o�@�q@��@�~�@�J�@�+k@��o@���@��@�b�@�33@�@��E@��b@�p;@��@���@���@�c@�Q�@�%@�p;@�oi@�p;@�K^@� �@�$@@~�@~��@~�F@~e@}�)@}��@}�'@}��@}/@|�K@|��@|��@|�O@|��@|y>@{�
@{O@{�@z�s@z��@z+k@y:�@x�.@w�@w�F@wb�@v��@vh
@u��@u��@uJ�@t�5@t�I@tK^@tx@s�;@s��@sy�@sE9@r�"@r��@rTa@r
�@q��@qQ�@q�@pی@p�j@pq@p2�@o�a@ox@o>�@o�@n��@n$�@m�>@m�'@lĜ@lI�@lx@kݘ@k��@k=@jl�@i�M@iG�@h�@h�9@h|�@h!@g�0@g�[@g��@f�s@f��@f+k@f_@e�t@e\�@e�@d�U@c�@cv`@cX�@cS@b�h@bxl@b4@a�'@a%F@a�@`��@`��@`j@`%�@_�+@_�@_\)@^�@^�@^�@]�@]O�@]-w@]�@\��@\��@\��@\9X@[خ@[�:@[
=@Z��@Zq�@Zd�@ZE�@Z#:@Y�@Y�@X�@X��@Xh�@XZ@X,=@Wt�@WMj@W@O@W1�@W�@V�b@VB[@U�t@UV@TɆ@T��@T��@T/�@S��@Sx@S"�@R�1@RGE@R	@Q��@Q�@Q��@Q��@Qc@P��@P�@O�@O��@O�{@OX�@O1�@O@O@O/�@O$t@N�@N�b@NQ@NJ@M�C@MO�@L�.@Lx@K��@K��@Kx@KY@J�8@J��@I��@I��@I#�@H�|@H��@H��@H|�@HV�@HG@Gv`@F��@Fe@E�~@EQ�@D��@C�m@C@O@B��@Bu%@A��@A*0@A�@@��@@M@@	�@?�$@?b�@>�@>h
@>($@> �@=|@=�@<��@<?�@;� @:͟@:��@:p;@:YK@9�.@9zx@8�e@8�@7�&@7��@7�K@7��@74�@6��@5��@5=�@4��@4K^@3��@3J#@2�,@2�R@2�@2^5@25?@2
�@1�7@0�I@0'R@01@/��@/��@/��@/�V@/P�@.��@.�@-�@-��@-�-@-}�@-IR@-�@,�O@+�}@+��@+t�@+�@*��@*�@*��@*E�@*�@)�@)��@)k�@(�5@(l"@(  @'�Q@'��@'��@'�	@'qv@'U�@'RT@')_@&�"@&��@&}V@%�@%�@%S&@$�v@$�E@$�?@$w�@$�@#�@#>�@"ߤ@"�b@"i�@":*@"3�@")�@"J@!ϫ@!��@!Y�@!Q�@!Dg@!�@!+@!�@!�@ �@ �[@ ��@ �o@ l"@ 6@ b@ 	�@�@�@��@��@�@@O@�6@3�@�.@�D@��@�t@��@[W@@%@�?@��@/�@�@��@=@�@�@��@�\@W�@_@��@�Z@�@u�@&�@��@�)@�@e�@H@�@ƨ@|�@>�@�@�c@҉@�\@0U@��@��@&�@��@�@�@��@�@�F@��@n/@&@ߤ@��@��@��@�R@�R@�R@�@�@-@@�~@[W@8�@��@��@�_@-�@��@� @�[@{J@8@�8@�m@�@\�@C�@J@�@ԕ@��@�~@#�@�@��@��@��@y>@Ft@�@��@�@RT@.I@�@
�m@
}V@
M�@
Ov@
L0@
3�@
4@
�@	��@	�@	�@	zx@	Y�@	+�@	�@��@�|@�@��@�@bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�x8A�x�A�{A�t�A�u�A��A�}�A؀�A؁;A؃A؃�A؂A؂AA؃GA؃GA؁�A؄�A؇+A�.A�{A�~�A�{�A�x�A�t�A�IRA�"�A�qA���A���A�`A�+�A��A֣�A�	�A�j�A���A��A�(�Aη�A̛	A̅SA�GEA�~�AʜxA�c A��pA��A���A�kA�xA��;A��DA�x8A��A��SA�a�A�49A���A�ѷA�{�A��0A�ǮA�OvA���A�*�A���A��A��A�HA�f2A��JA��`A��A�6A��A��jA��A�҉A���A��$A��gA��/A�C�A�&LA���A���A~��A{L�AzC�Aw��As�Ap��Am�xAjAg�Ad�A`�A\�}AY�`AUT�AR��AO�AK��AI��AHF�AFAD@OAA�?A@B[A=K^A;�uA;E9A:�A:R�A:�A9�*A9~�A9��A9'RA6�.A5�A3O�A0ԕA1�A18A0��A/+kA.b�A.%A-�A-�A+�tA*�LA(�[A(��A(�A)jA+$tA)�vA)"�A(  A'>BA&�}A&[�A%�A#|�A"_�A!�A!�A!��A!��A!h
A �vA .IA��ASA�OAϫA�PA�:A��A*�A�XA�A��AjA�A��A�4A�A7�AQ�A+Aj�A�pAXyA��AԕA9XA��A��A�AjA0�Ao�AK�A�AoiA�A��AzAb�A��A��A,�A/A
u�A
XyA
A A
~A	�hA	&�Ay>A%A�A_As�A�AB[A�TA�A_pA(�A��A��APHA$tA��Al�AR�A�A��A<6A ͟A oiA rG@��@��@���@��}@�D�@���@�H@��@��@��d@�s�@��@��@�S&@�@@��#@��@��Z@�s�@�5�@�ȴ@���@�-�@�Y�@��m@�p�@�W�@�}�@���@�n�@��@���@��@�s�@�%�@�n@�u@��@旍@�!@�q�@�
�@��@�"@嫟@�Y@�l�@��@�Q�@� @���@�m�@��f@�e@�xl@��M@�ԕ@���@�;�@�!�@��@���@�PH@�;�@��g@�RT@�;@غ�@؆Y@��@�e,@֢4@ֆY@֨�@֦L@֥z@�g8@�e@�T�@���@�w�@�a|@�V�@�@ӎ"@��@�ی@�*�@�_@�<6@��`@о@��a@�C@� \@��p@�L0@���@ͶF@͡�@�$t@̘_@̌�@�\�@�4@ˌ~@�ی@��@ɾw@�T�@���@�	@Ǧ�@�7L@�xl@�{@ŷ@��@ç�@�o@µ@�kQ@��T@�p�@��!@�6�@�c@��L@�0U@��N@�a�@��@��_@�bN@�	�@��@��"@�X�@��f@��+@�$@��;@��c@��@�8�@��@���@�]d@�*�@�@��d@�o�@�8�@��K@�^5@���@�(�@���@�%@���@��D@�h�@�%�@���@�t�@�W?@�o@��U@��@�E�@��n@�#�@�Ta@���@���@�l�@�8@��2@�1'@���@�4@��c@��2@��@�͟@���@�Ta@��@��^@�^�@�&�@�
=@���@�}V@�Ta@��@��}@��@��@���@�i�@�2�@�  @��X@�8�@��v@��?@���@�%�@���@�|�@�'�@���@��m@�z@�j@�d�@�S�@�6@��&@�m]@��@��@��o@���@�hs@��@�ی@�bN@�;�@��0@�A @��M@��)@���@�:*@�!�@��&@��F@��@���@�j@���@���@�D�@���@��a@��4@�s@�k�@�F@��X@�{�@�%�@�� @��{@��@�oi@�2�@�4@��@���@�\�@��@��@�q�@��@��@�&�@��U@�n�@��@�p�@�4@�ی@�q@��@��@��u@�H@���@�v`@�4�@��"@��@��@�G@�|�@�>�@��@���@��@�:�@�*�@��@��@@���@�IR@�q@�ی@���@�/�@��@��f@�a�@�7L@��@��@��@�p;@�Xy@�N�@�~@��F@�o�@�q@��@�~�@�J�@�+k@��o@���@��@�b�@�33@�@��E@��b@�p;@��@���@���@�c@�Q�@�%@�p;@�oi@�p;@�K^@� �@�$@@~�@~��@~�F@~e@}�)@}��@}�'@}��@}/@|�K@|��@|��@|�O@|��@|y>@{�
@{O@{�@z�s@z��@z+k@y:�@x�.@w�@w�F@wb�@v��@vh
@u��@u��@uJ�@t�5@t�I@tK^@tx@s�;@s��@sy�@sE9@r�"@r��@rTa@r
�@q��@qQ�@q�@pی@p�j@pq@p2�@o�a@ox@o>�@o�@n��@n$�@m�>@m�'@lĜ@lI�@lx@kݘ@k��@k=@jl�@i�M@iG�@h�@h�9@h|�@h!@g�0@g�[@g��@f�s@f��@f+k@f_@e�t@e\�@e�@d�U@c�@cv`@cX�@cS@b�h@bxl@b4@a�'@a%F@a�@`��@`��@`j@`%�@_�+@_�@_\)@^�@^�@^�@]�@]O�@]-w@]�@\��@\��@\��@\9X@[خ@[�:@[
=@Z��@Zq�@Zd�@ZE�@Z#:@Y�@Y�@X�@X��@Xh�@XZ@X,=@Wt�@WMj@W@O@W1�@W�@V�b@VB[@U�t@UV@TɆ@T��@T��@T/�@S��@Sx@S"�@R�1@RGE@R	@Q��@Q�@Q��@Q��@Qc@P��@P�@O�@O��@O�{@OX�@O1�@O@O@O/�@O$t@N�@N�b@NQ@NJ@M�C@MO�@L�.@Lx@K��@K��@Kx@KY@J�8@J��@I��@I��@I#�@H�|@H��@H��@H|�@HV�@HG@Gv`@F��@Fe@E�~@EQ�@D��@C�m@C@O@B��@Bu%@A��@A*0@A�@@��@@M@@	�@?�$@?b�@>�@>h
@>($@> �@=|@=�@<��@<?�@;� @:͟@:��@:p;@:YK@9�.@9zx@8�e@8�@7�&@7��@7�K@7��@74�@6��@5��@5=�@4��@4K^@3��@3J#@2�,@2�R@2�@2^5@25?@2
�@1�7@0�I@0'R@01@/��@/��@/��@/�V@/P�@.��@.�@-�@-��@-�-@-}�@-IR@-�@,�O@+�}@+��@+t�@+�@*��@*�@*��@*E�@*�@)�@)��@)k�@(�5@(l"@(  @'�Q@'��@'��@'�	@'qv@'U�@'RT@')_@&�"@&��@&}V@%�@%�@%S&@$�v@$�E@$�?@$w�@$�@#�@#>�@"ߤ@"�b@"i�@":*@"3�@")�@"J@!ϫ@!��@!Y�@!Q�@!Dg@!�@!+@!�@!�@ �@ �[@ ��@ �o@ l"@ 6@ b@ 	�@�@�@��@��@�@@O@�6@3�@�.@�D@��@�t@��@[W@@%@�?@��@/�@�@��@=@�@�@��@�\@W�@_@��@�Z@�@u�@&�@��@�)@�@e�@H@�@ƨ@|�@>�@�@�c@҉@�\@0U@��@��@&�@��@�@�@��@�@�F@��@n/@&@ߤ@��@��@��@�R@�R@�R@�@�@-@@�~@[W@8�@��@��@�_@-�@��@� @�[@{J@8@�8@�m@�@\�@C�@J@�@ԕ@��@�~@#�@�@��@��@��@y>@Ft@�@��@�@RT@.I@�@
�m@
}V@
M�@
Ov@
L0@
3�@
4@
�@	��@	�@	�@	zx@	Y�@	+�@	�@��@�|@�@��@�@bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B�B�B��B�B��B��B��B��B�B�B�B��B�dB�~B�~B�~B�JB�0B�0B�B��B͟BΥB�pBϫB�B�&BңBӏB�(B� B�B��B��B	 B	NB	=�B	A�B	fB	�ZB	�PB	�<B
�0B
��BYB�B(>BN�BiBs3B��B�'B�?BƎB�jB�rB�:B��B{dB{�B]/BUMB?.B(�B'mB �B�B
�cB
��B
�B
�{B
��B
��B
�IB
�2B
�qB
��B
sMB
I7B
3�B
IB

	B	�B	�B	׍B	�XB	�CB	�B	�^B	�B	�KB	�qB	��B	o B	GzB	1�B	=B	�B	�B��B��B��B�B�KB�B�dB��B�_B�_B��BؓB��BۦB߾B�B�IB�B��B�4BԕB��B��B	B	�B	MB	�B	 B��B�PB��B��B		B	1B	-�B	hXB	w�B	y�B	��B	� B	��B	��B	��B	�QB	��B	��B	�B	��B	�WB	��B	�B	�KB	�zB	�sB	�RB	�*B	�$B	�XB	��B	��B	��B	�eB	��B	�B	��B	�UB	�nB	��B	�B	��B	��B	�8B	�+B	�`B	�%B	��B	�^B	�BB	�;B	ÖB	��B	ÖB	ðB	��B	��B	�zB	��B	�B	�YB	��B	ŢB	��B	�B	�B	�B	��B	�dB	�B	��B	�(B	�BB	�<B	�B	̘B	�DB	�DB	��B	��B	�	B	ɠB	��B	��B	ɆB	ɠB	�=B	�XB	��B	�)B	��B	̘B	��B	�B	�B	�0B	�=B	�0B	�jB	̘B	ʌB	��B	�"B	ΥB	ΊB	ΊB	�"B	͹B	�B	ΊB	�B	��B	�B	�VB	ΊB	��B	�B	��B	��B	��B	��B	�oB	�NB	ѝB	ѷB	бB	�.B	��B	�.B	�B	ЗB	�.B	��B	�(B	��B	��B	ՁB	�B	�mB	ڠB	��B	ݲB	�B	یB	�)B	ۦB	�pB	�B	�fB	�8B	�2B	�|B	��B	�5B	�HB	ޞB	��B	�B	�kB	�]B	ݘB	�/B	�~B	��B	�B	��B	�B	�7B	�=B	�~B	�B	�bB	��B	�|B	��B	�bB	�B	�|B	�HB	�-B	�BB	�\B	�bB	��B	�'B	�;B	��B	��B	߾B	�IB	��B	�BB	��B	�vB	��B	�B	�-B	�|B	�HB	��B	��B	�B	�-B	��B	��B	�HB	��B	�'B	�!B	�VB	�!B	�B	��B	��B	߾B	�4B	��B	�B	�B	�B	�`B	��B	��B	��B	�B	�B	�$B	�B	��B	�*B	�yB	��B	��B	��B	�B	�B	�kB	�B	��B	�B	�)B	�B	��B	�qB	��B	�=B	�qB	�B	�cB	�B	�B	�B	�'B	�vB	�GB	�B	��B	��B	�B	��B	�B	��B	��B	�zB	��B	��B	��B	�>B	��B	�$B	��B	��B	�0B	�JB	�JB	��B	��B	��B	�B	�<B	�qB	��B	�(B	�BB	��B	�(B	�wB	�.B
  B
 B
 �B
 �B
;B
;B
�B
�B
B
GB
{B
�B
gB
3B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
SB
SB
�B
%B
�B
B
�B
zB
�B
�B
�B
KB
�B
	7B
	�B

	B

XB

�B

�B

�B
xB
�B
�B
^B
�B
�B
0B
B
�B
"B
�B
�B
B
(B
�B
�B
4B
4B
hB
NB
�B
oB
�B
�B
,B
2B
�B
�B
�B
�B
9B
�B
YB
�B
+B
yB
_B
�B
�B
B
�B
�B
eB
�B
�B
�B
=B
�B
�B
/B
�B
B
�B
B
jB
B
OB
jB
�B
�B
VB
pB
 B
 �B
!-B
"B
$tB
$�B
%�B
%�B
&B
&�B
&�B
&�B
&�B
'8B
'�B
'�B
(XB
(>B
(�B
(�B
)B
)�B
)�B
*KB
*B
*�B
*�B
*�B
+B
+kB
,B
,=B
,=B
,qB
,�B
-)B
-]B
-]B
-)B
-wB
-�B
./B
.�B
/ B
/ B
/B
/�B
/�B
/�B
/�B
/�B
0;B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1'B
1B
1AB
1vB
2|B
3B
3�B
3�B
4B
4�B
4�B
5ZB
5%B
5%B
5�B
5�B
5�B
6`B
6�B
6�B
6�B
7B
7fB
7�B
7�B
8B
8RB
8�B
8�B
8�B
8�B
9	B
9$B
9rB
9>B
9�B
:^B
;0B
;�B
;�B
;�B
;B
:�B
;0B
;B
;dB
;JB
;�B
;�B
;�B
;�B
;�B
<B
<6B
<�B
<�B
<�B
<�B
=<B
=�B
=�B
>(B
=�B
>B
>(B
?B
?cB
?}B
?�B
@ B
@4B
@�B
AB
A B
A;B
A�B
A�B
BB
B'B
BuB
B�B
CB
C{B
C�B
D3B
D�B
D�B
D�B
D�B
D�B
EB
D�B
E9B
E9B
EB
EB
ESB
E�B
EmB
E�B
E�B
E�B
FB
FtB
F�B
F�B
G+B
GzB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
IRB
IRB
IlB
IRB
I�B
J�B
K)B
KDB
K^B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
KDB
KB
K^B
K�B
K�B
K�B
LdB
MB
M�B
M�B
N�B
N�B
O(B
O(B
O\B
OvB
O�B
P.B
PHB
PHB
PHB
PbB
P}B
PbB
PB
O�B
O�B
O�B
O�B
O�B
O�B
PB
P}B
QNB
QhB
Q�B
Q�B
Q�B
Q�B
RB
R B
R B
RTB
S&B
S&B
S@B
S�B
S�B
S�B
TFB
TFB
T�B
T�B
T�B
T�B
UB
U�B
U�B
VmB
V�B
W?B
WYB
WsB
W�B
W�B
W�B
XyB
X�B
X�B
X�B
X�B
X�B
YeB
Y�B
Z�B
Z�B
Z�B
[WB
[qB
[�B
[�B
[�B
[�B
\B
\B
\B
\CB
]B
]�B
]�B
^B
^B
^B
^B
^OB
_;B
_�B
`B
`'B
`BB
`vB
`�B
a-B
a�B
b�B
b�B
b�B
cB
cB
c B
cB
c:B
cnB
c�B
c�B
c�B
dtB
d�B
eFB
ezB
e�B
e�B
e�B
e�B
e�B
e�B
fB
fB
f2B
f�B
gB
gmB
gRB
g�B
g�B
g�B
h>B
h�B
h�B
i�B
i�B
i�B
jKB
jeB
jeB
jeB
jeB
j�B
kB
kB
kB
kB
kQB
k6B
kQB
k6B
kQB
kkB
kkB
k�B
k�B
lB
lB
lB
l"B
l"B
l"B
lWB
l=B
l�B
mB
m�B
m�B
m�B
m�B
m�B
m�B
nIB
ncB
ncB
n�B
n�B
oB
o5B
o�B
o�B
o�B
o�B
p;B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
qB
qAB
q[B
q�B
q�B
q�B
q�B
raB
r�B
r�B
r�B
s3B
s3B
shB
s�B
tB
tTB
t�B
t�B
uB
u�B
u�B
u�B
vB
v+B
v+B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wfB
w�B
xB
xB
xB
xRB
xRB
x�B
y$B
yXB
y�B
y�B
y�B
y�B
zDB
zxB
z�B
z�B
z�B
{B
{0B
{0B
{0B
{dB
{�B
{�B
|6B
|�B
|�B
|�B
|�B
}B
}B
}VB
}qB
}�B
}�B
~B
~]B
~wB
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
�B
}B
�B
�B
� B
� B
� B
�4B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B�B�B��B�B��B��B��B��B�B�B�B��B�dB�~B�~B�~B�JB�0B�0B�B��B͟BΥB�pBϫB�B�&BңBӏB�(B� B�B��B��B	 B	NB	=�B	A�B	fB	�ZB	�PB	�<B
�0B
��BYB�B(>BN�BiBs3B��B�'B�?BƎB�jB�rB�:B��B{dB{�B]/BUMB?.B(�B'mB �B�B
�cB
��B
�B
�{B
��B
��B
�IB
�2B
�qB
��B
sMB
I7B
3�B
IB

	B	�B	�B	׍B	�XB	�CB	�B	�^B	�B	�KB	�qB	��B	o B	GzB	1�B	=B	�B	�B��B��B��B�B�KB�B�dB��B�_B�_B��BؓB��BۦB߾B�B�IB�B��B�4BԕB��B��B	B	�B	MB	�B	 B��B�PB��B��B		B	1B	-�B	hXB	w�B	y�B	��B	� B	��B	��B	��B	�QB	��B	��B	�B	��B	�WB	��B	�B	�KB	�zB	�sB	�RB	�*B	�$B	�XB	��B	��B	��B	�eB	��B	�B	��B	�UB	�nB	��B	�B	��B	��B	�8B	�+B	�`B	�%B	��B	�^B	�BB	�;B	ÖB	��B	ÖB	ðB	��B	��B	�zB	��B	�B	�YB	��B	ŢB	��B	�B	�B	�B	��B	�dB	�B	��B	�(B	�BB	�<B	�B	̘B	�DB	�DB	��B	��B	�	B	ɠB	��B	��B	ɆB	ɠB	�=B	�XB	��B	�)B	��B	̘B	��B	�B	�B	�0B	�=B	�0B	�jB	̘B	ʌB	��B	�"B	ΥB	ΊB	ΊB	�"B	͹B	�B	ΊB	�B	��B	�B	�VB	ΊB	��B	�B	��B	��B	��B	��B	�oB	�NB	ѝB	ѷB	бB	�.B	��B	�.B	�B	ЗB	�.B	��B	�(B	��B	��B	ՁB	�B	�mB	ڠB	��B	ݲB	�B	یB	�)B	ۦB	�pB	�B	�fB	�8B	�2B	�|B	��B	�5B	�HB	ޞB	��B	�B	�kB	�]B	ݘB	�/B	�~B	��B	�B	��B	�B	�7B	�=B	�~B	�B	�bB	��B	�|B	��B	�bB	�B	�|B	�HB	�-B	�BB	�\B	�bB	��B	�'B	�;B	��B	��B	߾B	�IB	��B	�BB	��B	�vB	��B	�B	�-B	�|B	�HB	��B	��B	�B	�-B	��B	��B	�HB	��B	�'B	�!B	�VB	�!B	�B	��B	��B	߾B	�4B	��B	�B	�B	�B	�`B	��B	��B	��B	�B	�B	�$B	�B	��B	�*B	�yB	��B	��B	��B	�B	�B	�kB	�B	��B	�B	�)B	�B	��B	�qB	��B	�=B	�qB	�B	�cB	�B	�B	�B	�'B	�vB	�GB	�B	��B	��B	�B	��B	�B	��B	��B	�zB	��B	��B	��B	�>B	��B	�$B	��B	��B	�0B	�JB	�JB	��B	��B	��B	�B	�<B	�qB	��B	�(B	�BB	��B	�(B	�wB	�.B
  B
 B
 �B
 �B
;B
;B
�B
�B
B
GB
{B
�B
gB
3B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
SB
SB
�B
%B
�B
B
�B
zB
�B
�B
�B
KB
�B
	7B
	�B

	B

XB

�B

�B

�B
xB
�B
�B
^B
�B
�B
0B
B
�B
"B
�B
�B
B
(B
�B
�B
4B
4B
hB
NB
�B
oB
�B
�B
,B
2B
�B
�B
�B
�B
9B
�B
YB
�B
+B
yB
_B
�B
�B
B
�B
�B
eB
�B
�B
�B
=B
�B
�B
/B
�B
B
�B
B
jB
B
OB
jB
�B
�B
VB
pB
 B
 �B
!-B
"B
$tB
$�B
%�B
%�B
&B
&�B
&�B
&�B
&�B
'8B
'�B
'�B
(XB
(>B
(�B
(�B
)B
)�B
)�B
*KB
*B
*�B
*�B
*�B
+B
+kB
,B
,=B
,=B
,qB
,�B
-)B
-]B
-]B
-)B
-wB
-�B
./B
.�B
/ B
/ B
/B
/�B
/�B
/�B
/�B
/�B
0;B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1'B
1B
1AB
1vB
2|B
3B
3�B
3�B
4B
4�B
4�B
5ZB
5%B
5%B
5�B
5�B
5�B
6`B
6�B
6�B
6�B
7B
7fB
7�B
7�B
8B
8RB
8�B
8�B
8�B
8�B
9	B
9$B
9rB
9>B
9�B
:^B
;0B
;�B
;�B
;�B
;B
:�B
;0B
;B
;dB
;JB
;�B
;�B
;�B
;�B
;�B
<B
<6B
<�B
<�B
<�B
<�B
=<B
=�B
=�B
>(B
=�B
>B
>(B
?B
?cB
?}B
?�B
@ B
@4B
@�B
AB
A B
A;B
A�B
A�B
BB
B'B
BuB
B�B
CB
C{B
C�B
D3B
D�B
D�B
D�B
D�B
D�B
EB
D�B
E9B
E9B
EB
EB
ESB
E�B
EmB
E�B
E�B
E�B
FB
FtB
F�B
F�B
G+B
GzB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
IRB
IRB
IlB
IRB
I�B
J�B
K)B
KDB
K^B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
KDB
KB
K^B
K�B
K�B
K�B
LdB
MB
M�B
M�B
N�B
N�B
O(B
O(B
O\B
OvB
O�B
P.B
PHB
PHB
PHB
PbB
P}B
PbB
PB
O�B
O�B
O�B
O�B
O�B
O�B
PB
P}B
QNB
QhB
Q�B
Q�B
Q�B
Q�B
RB
R B
R B
RTB
S&B
S&B
S@B
S�B
S�B
S�B
TFB
TFB
T�B
T�B
T�B
T�B
UB
U�B
U�B
VmB
V�B
W?B
WYB
WsB
W�B
W�B
W�B
XyB
X�B
X�B
X�B
X�B
X�B
YeB
Y�B
Z�B
Z�B
Z�B
[WB
[qB
[�B
[�B
[�B
[�B
\B
\B
\B
\CB
]B
]�B
]�B
^B
^B
^B
^B
^OB
_;B
_�B
`B
`'B
`BB
`vB
`�B
a-B
a�B
b�B
b�B
b�B
cB
cB
c B
cB
c:B
cnB
c�B
c�B
c�B
dtB
d�B
eFB
ezB
e�B
e�B
e�B
e�B
e�B
e�B
fB
fB
f2B
f�B
gB
gmB
gRB
g�B
g�B
g�B
h>B
h�B
h�B
i�B
i�B
i�B
jKB
jeB
jeB
jeB
jeB
j�B
kB
kB
kB
kB
kQB
k6B
kQB
k6B
kQB
kkB
kkB
k�B
k�B
lB
lB
lB
l"B
l"B
l"B
lWB
l=B
l�B
mB
m�B
m�B
m�B
m�B
m�B
m�B
nIB
ncB
ncB
n�B
n�B
oB
o5B
o�B
o�B
o�B
o�B
p;B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
qB
qAB
q[B
q�B
q�B
q�B
q�B
raB
r�B
r�B
r�B
s3B
s3B
shB
s�B
tB
tTB
t�B
t�B
uB
u�B
u�B
u�B
vB
v+B
v+B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wfB
w�B
xB
xB
xB
xRB
xRB
x�B
y$B
yXB
y�B
y�B
y�B
y�B
zDB
zxB
z�B
z�B
z�B
{B
{0B
{0B
{0B
{dB
{�B
{�B
|6B
|�B
|�B
|�B
|�B
}B
}B
}VB
}qB
}�B
}�B
~B
~]B
~wB
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
�B
}B
�B
�B
� B
� B
� B
�4B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104939  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174628  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174629  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174629                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024636  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024636  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                