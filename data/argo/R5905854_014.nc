CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:46:52Z creation;2022-06-04T17:46:52Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174652  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�ǝ�g��1   @�Ǟ2@y]@-��/���c�vȴ91   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B��B  B   B*  B/33B8  B?��BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�33B�ffB���BB���B�  B�  C   C  C  C  C  C
�C�C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>L�C?�fCA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd33Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|fD|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A��A�z�A�z�A�z�B =qB=qB�B=qB =qB*=qB/p�B8=qB?�BH=qBP=qBX=qB`=qBh=qBo�Bx=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B�B��B�RB��B��B��C \C\C\C\C\C
(�C(�C\C\C\C\C\C\C\C\C\C��C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\)C?��CA��CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb(�CdB�Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D�=D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP�=DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|
=D|��D}�D}��D~�D~��D�D��D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�~�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D���D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D�D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�(XA�)�A�(XA�%�A�'RA�&�A�)*A�*�A�,qA�*�A�VA��A��A��A���A��WA��HAۢhA�wfA�iA�L�A���AھAٙ�A�MA׳hA׋�A�MA�xA�\]A��A�G�A�*eA�,�Aʰ�AɋxA�W
A�?}A��KA�A��(A�e,A�[�Aň1A��A��A�v+A�o�A��A�e�A�U�A��zA��;A�ȴA�� A�tA��{A��A�F?A�8RA�|�A��iA�&�A�*�A���A���A�fA��?A���A���A�`vA�T,A�-wA�\�A�xA���A�/�A���A�'�A�B[A��BA�>wA�A��A�tTA��@A��9A�\]A��fA��A���A��KA���A�TaA���A�� A��AbA{�Ay6Aq��Ak�@AhVAdA`iDA\^�AVB�AQ�AN�LAK�AH��AF#:AE{JAD�wAB;�A>p;A<��A;�A;�A:�A8�rA7l�A6�)A5ںA3�TA3e�A2��A2oiA1�NA0��A0MA/��A.��A-�XA-�A+�A*��A*%A)^5A(jA&��A&xlA#�<A!��A �	A�A	lA͟A>BA0�AY�A�hA�A@OA�oA��A��AXA,�A�Ay>A�QAl�A'�A]�A �Am�AB[AA�bA��AzAh
A�A+A6Av�A_AخA��A�A��A�EAs�A�A|�Aa�Ak�A�A
��A
.�A	�A	�SA	�6A	W�AS�A��An�A�A��A��An�A�Ax�A�A��AxAu�AsA�A��A\�A�A�A�eA�MAMA�pA�qA~�Al�A[WA8�A.IA �A ��A {JA DgA -�A �@���@�L�@�GE@��~@�S&@��@���@�U�@��D@���@��9@�>B@���@��^@���@��f@���@���@��&@�2a@��)@��@��@��\@��@�"@�6z@@�#:@��D@�4�@��o@�Y@�tT@鐗@�D@�e�@�u@���@凔@�.I@��@�  @�o�@�33@�@��)@�N�@���@�B�@���@��@���@�l�@ް!@��m@�F@܀�@�`B@ڻ�@�W�@�e@�)�@َ"@�C@ر�@�_@�0�@֟�@�j@�>B@� �@��@�@@�C-@ө*@Ҿ@�͟@��@��H@ғu@ѨX@�#�@К�@�9�@�ی@Κ@΅�@��o@�;d@���@�ff@���@� \@ʇ+@���@�X@��@�7�@ǅ@�+@��@Ƣ4@��6@��@�l�@��@Ï�@�o@�4n@�[W@��|@��f@��@�($@���@���@���@�6�@�O@�	�@�x�@�C@�%F@��y@��4@�_@��P@�j�@�/@��@�u%@�X�@��@��M@��[@���@�\�@��Y@�:�@�@�O@��h@�p;@�PH@�8�@�7@�4@���@��a@�~�@��@���@��j@���@�^5@�
�@��]@�~�@��@�xl@�:*@��@��d@�l�@���@�H�@��@��@���@�a@�L�@�8�@��@��y@��L@�.�@��)@��@���@�@���@���@��"@�^�@�(�@��E@�tT@���@�j@���@���@���@�oi@��+@���@�x@�;@�ѷ@��_@��@��h@�	l@��@�3�@���@�G�@���@�D�@� �@�RT@���@�͟@���@�b@��$@�Vm@�:�@��@��@�	l@�@��,@�!@���@��@��@��@���@��e@���@�`�@���@��H@���@�Z�@��)@��@�GE@�@���@�l�@�8@��8@���@�kQ@��@�m]@�9�@��"@���@��u@�H�@��j@��V@�g�@��@�҉@��\@�Z�@��@���@���@�c�@��@��@��?@���@��@�Ta@��@��#@���@���@�\�@�)_@��@��@��)@��I@�%�@��@��@���@�N<@�/�@��@��@�h�@�Ov@�~@��Z@��K@���@�G�@��]@���@�I�@�.�@��@��@���@��=@�zx@�qv@�0�@��v@��u@�\�@�B[@�4n@�$�@�@��0@�x@��f@��B@���@���@�:*@��@���@��'@�\�@�A @�8@��@���@��.@���@�>B@�!�@���@�x�@�e�@�Y�@���@��4@�w�@�H@�7@��@���@��@�e,@�8@��@���@��@�4n@��@�f@�@~�,@~��@~.�@}�S@}0�@}�@|��@|g8@{�@{��@{F�@z��@zQ@y��@yc@y@xq@w��@w33@v��@vGE@v@uu�@u@t�)@ttT@s� @sy�@sA�@s�@s�@r��@r�@r��@qVm@q%@p��@p��@pFt@ox@nxl@n0U@n�@m�@m!�@l��@l|�@lS�@l1@k�@k]�@k9�@k�@j�@jZ�@j�@i�7@i%F@h��@hA�@g�0@g=@f�'@fW�@e��@eX@e/@d��@d��@d-�@c�@c�4@b�@a��@aG�@aV@`�e@`�4@`�4@_�K@_�@^��@^M�@^!�@]��@]A @]	l@\�9@\e�@\tT@\[�@\7�@[��@[��@[+@ZR�@YIR@X�9@XS�@X:�@W��@W\)@V��@V��@VM�@V5?@V�@U��@U��@UrG@T�f@T]d@S�K@S]�@Rߤ@R��@ROv@Q��@Q��@Q�"@Q	l@P֡@P��@P��@P��@P,=@O��@O�V@O+@N��@Nz@N@�@N�@M��@M��@Mp�@L�j@L[�@L@Kݘ@K�{@K�@J�h@JZ�@J �@I��@H�P@H��@H<�@H:�@H<�@H6@G�*@G,�@F��@F�X@FV@E��@Eq@D�$@Du�@DFt@DM@C�F@Cqv@B��@B4@A�-@Ax�@AF@A�@@�@@��@@PH@@�@?�F@?x@?Z�@?�@>�2@>�<@>kQ@>
�@=�"@=#�@<ی@<�@;�Q@;�k@:��@:�}@:C�@9�j@9�7@9%@8�[@8�$@8c�@7�+@7��@7RT@7&@7Y@6�c@6�!@6��@6�@5j@5%F@4|�@4�@3��@3\)@3H�@2�@2O@1��@1�~@1?}@0��@0�`@0�z@0N�@0@/خ@/�P@/F�@.��@.�@.Q@.
�@-��@-�3@-�=@-	l@,�@,j@, �@+��@+�6@+�[@+�	@+C�@*��@*i�@)��@)O�@(�f@(�O@(w�@(Z@(1'@'�@'�*@'X�@'�@&��@&��@&v�@&@�@%�N@%�@%s�@%Dg@$�@$h�@$<�@$"h@$�@#��@#�@#� @#��@#1�@"�m@"��@"��@"n�@"W�@")�@!�>@!�@!�'@!x�@!:�@!�@ �@ �p@ �9@ w�@ 1'@ݘ@��@��@iD@P�@;d@�@q�@Ov@O@��@m]@S&@+@�@�z@w�@A�@7�@	�@�;@�a@��@v`@O@H�@4�@��@��@u%@a|@YK@�@�>@|@IR@�@�@�$@�@'R@�&@��@=@!-@҉@��@GE@�@��@�T@�#@�@zx@S&@(�@�@�z@|�@PH@*�@��@��@{J@_p@K�@�@��@�@�L@�F@^5@&�@4@��@�@��@�@��@[W@0�@;@Ĝ@�D@�@[�@PH@$@�;@��@~�@\)@S�@33@�@�H@�@Z�@GE@:*@+k@��@��@hs@Dg@�@�/@��@��@�4@��@$@�@��@�q@|�@E9@!-@@
�@
��@
��@	�.@	��@	J�@	%F@	�@�@�p@�I@��@u�@g811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�(XA�)�A�(XA�%�A�'RA�&�A�)*A�*�A�,qA�*�A�VA��A��A��A���A��WA��HAۢhA�wfA�iA�L�A���AھAٙ�A�MA׳hA׋�A�MA�xA�\]A��A�G�A�*eA�,�Aʰ�AɋxA�W
A�?}A��KA�A��(A�e,A�[�Aň1A��A��A�v+A�o�A��A�e�A�U�A��zA��;A�ȴA�� A�tA��{A��A�F?A�8RA�|�A��iA�&�A�*�A���A���A�fA��?A���A���A�`vA�T,A�-wA�\�A�xA���A�/�A���A�'�A�B[A��BA�>wA�A��A�tTA��@A��9A�\]A��fA��A���A��KA���A�TaA���A�� A��AbA{�Ay6Aq��Ak�@AhVAdA`iDA\^�AVB�AQ�AN�LAK�AH��AF#:AE{JAD�wAB;�A>p;A<��A;�A;�A:�A8�rA7l�A6�)A5ںA3�TA3e�A2��A2oiA1�NA0��A0MA/��A.��A-�XA-�A+�A*��A*%A)^5A(jA&��A&xlA#�<A!��A �	A�A	lA͟A>BA0�AY�A�hA�A@OA�oA��A��AXA,�A�Ay>A�QAl�A'�A]�A �Am�AB[AA�bA��AzAh
A�A+A6Av�A_AخA��A�A��A�EAs�A�A|�Aa�Ak�A�A
��A
.�A	�A	�SA	�6A	W�AS�A��An�A�A��A��An�A�Ax�A�A��AxAu�AsA�A��A\�A�A�A�eA�MAMA�pA�qA~�Al�A[WA8�A.IA �A ��A {JA DgA -�A �@���@�L�@�GE@��~@�S&@��@���@�U�@��D@���@��9@�>B@���@��^@���@��f@���@���@��&@�2a@��)@��@��@��\@��@�"@�6z@@�#:@��D@�4�@��o@�Y@�tT@鐗@�D@�e�@�u@���@凔@�.I@��@�  @�o�@�33@�@��)@�N�@���@�B�@���@��@���@�l�@ް!@��m@�F@܀�@�`B@ڻ�@�W�@�e@�)�@َ"@�C@ر�@�_@�0�@֟�@�j@�>B@� �@��@�@@�C-@ө*@Ҿ@�͟@��@��H@ғu@ѨX@�#�@К�@�9�@�ی@Κ@΅�@��o@�;d@���@�ff@���@� \@ʇ+@���@�X@��@�7�@ǅ@�+@��@Ƣ4@��6@��@�l�@��@Ï�@�o@�4n@�[W@��|@��f@��@�($@���@���@���@�6�@�O@�	�@�x�@�C@�%F@��y@��4@�_@��P@�j�@�/@��@�u%@�X�@��@��M@��[@���@�\�@��Y@�:�@�@�O@��h@�p;@�PH@�8�@�7@�4@���@��a@�~�@��@���@��j@���@�^5@�
�@��]@�~�@��@�xl@�:*@��@��d@�l�@���@�H�@��@��@���@�a@�L�@�8�@��@��y@��L@�.�@��)@��@���@�@���@���@��"@�^�@�(�@��E@�tT@���@�j@���@���@���@�oi@��+@���@�x@�;@�ѷ@��_@��@��h@�	l@��@�3�@���@�G�@���@�D�@� �@�RT@���@�͟@���@�b@��$@�Vm@�:�@��@��@�	l@�@��,@�!@���@��@��@��@���@��e@���@�`�@���@��H@���@�Z�@��)@��@�GE@�@���@�l�@�8@��8@���@�kQ@��@�m]@�9�@��"@���@��u@�H�@��j@��V@�g�@��@�҉@��\@�Z�@��@���@���@�c�@��@��@��?@���@��@�Ta@��@��#@���@���@�\�@�)_@��@��@��)@��I@�%�@��@��@���@�N<@�/�@��@��@�h�@�Ov@�~@��Z@��K@���@�G�@��]@���@�I�@�.�@��@��@���@��=@�zx@�qv@�0�@��v@��u@�\�@�B[@�4n@�$�@�@��0@�x@��f@��B@���@���@�:*@��@���@��'@�\�@�A @�8@��@���@��.@���@�>B@�!�@���@�x�@�e�@�Y�@���@��4@�w�@�H@�7@��@���@��@�e,@�8@��@���@��@�4n@��@�f@�@~�,@~��@~.�@}�S@}0�@}�@|��@|g8@{�@{��@{F�@z��@zQ@y��@yc@y@xq@w��@w33@v��@vGE@v@uu�@u@t�)@ttT@s� @sy�@sA�@s�@s�@r��@r�@r��@qVm@q%@p��@p��@pFt@ox@nxl@n0U@n�@m�@m!�@l��@l|�@lS�@l1@k�@k]�@k9�@k�@j�@jZ�@j�@i�7@i%F@h��@hA�@g�0@g=@f�'@fW�@e��@eX@e/@d��@d��@d-�@c�@c�4@b�@a��@aG�@aV@`�e@`�4@`�4@_�K@_�@^��@^M�@^!�@]��@]A @]	l@\�9@\e�@\tT@\[�@\7�@[��@[��@[+@ZR�@YIR@X�9@XS�@X:�@W��@W\)@V��@V��@VM�@V5?@V�@U��@U��@UrG@T�f@T]d@S�K@S]�@Rߤ@R��@ROv@Q��@Q��@Q�"@Q	l@P֡@P��@P��@P��@P,=@O��@O�V@O+@N��@Nz@N@�@N�@M��@M��@Mp�@L�j@L[�@L@Kݘ@K�{@K�@J�h@JZ�@J �@I��@H�P@H��@H<�@H:�@H<�@H6@G�*@G,�@F��@F�X@FV@E��@Eq@D�$@Du�@DFt@DM@C�F@Cqv@B��@B4@A�-@Ax�@AF@A�@@�@@��@@PH@@�@?�F@?x@?Z�@?�@>�2@>�<@>kQ@>
�@=�"@=#�@<ی@<�@;�Q@;�k@:��@:�}@:C�@9�j@9�7@9%@8�[@8�$@8c�@7�+@7��@7RT@7&@7Y@6�c@6�!@6��@6�@5j@5%F@4|�@4�@3��@3\)@3H�@2�@2O@1��@1�~@1?}@0��@0�`@0�z@0N�@0@/خ@/�P@/F�@.��@.�@.Q@.
�@-��@-�3@-�=@-	l@,�@,j@, �@+��@+�6@+�[@+�	@+C�@*��@*i�@)��@)O�@(�f@(�O@(w�@(Z@(1'@'�@'�*@'X�@'�@&��@&��@&v�@&@�@%�N@%�@%s�@%Dg@$�@$h�@$<�@$"h@$�@#��@#�@#� @#��@#1�@"�m@"��@"��@"n�@"W�@")�@!�>@!�@!�'@!x�@!:�@!�@ �@ �p@ �9@ w�@ 1'@ݘ@��@��@iD@P�@;d@�@q�@Ov@O@��@m]@S&@+@�@�z@w�@A�@7�@	�@�;@�a@��@v`@O@H�@4�@��@��@u%@a|@YK@�@�>@|@IR@�@�@�$@�@'R@�&@��@=@!-@҉@��@GE@�@��@�T@�#@�@zx@S&@(�@�@�z@|�@PH@*�@��@��@{J@_p@K�@�@��@�@�L@�F@^5@&�@4@��@�@��@�@��@[W@0�@;@Ĝ@�D@�@[�@PH@$@�;@��@~�@\)@S�@33@�@�H@�@Z�@GE@:*@+k@��@��@hs@Dg@�@�/@��@��@�4@��@$@�@��@�q@|�@E9@!-@@
�@
��@
��@	�.@	��@	J�@	%F@	�@�@�p@�I@��@u�@g811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	kQB	kB	j�B	jKB	i�B	i�B	i_B	i�B	i�B	k�B	m)B	uB	�B	�B	~�B	~(B	z�B	raB	z�B	�UB	�B	��B	��B	�jB	��B	��B	��B	��B	�B	ԕB

#B
4�B
C�B
u�B
B
��B
�B
�B
�B
�B�B�B5�BE�B[=BmCByrB��B��B�_B�RB��B��B�QB��B��B�B��B�mB��B��B�bB�pB�_B�B��B��B�{BrBj�BU�BDB<6B0�B&fB�BB
��B
خB
�B
�	B
�MB
��B
j�B
7�B
HB	��B	�B	�rB	�bB	y�B	b�B	I�B	3B	1B	[B�B�6B��B�oB�"B	�B	'mB	*�B	-)B	/5B	8B	HB	NVB	XyB	[�B	`�B	kQB	q�B	s�B	u�B	w�B	y�B	�RB	��B	�B	�8B	��B	�"B	��B	�aB	��B	��B	�B	�`B	��B	�NB	�B	�/B	��B	��B	�B	� B	�pB	ȀB	�[B	��B	�B	޸B	�B	�B	��B	��B	��B	��B	�B	�KB	ΊB	ĜB	�0B	�^B	��B	�{B	�#B	�#B	�7B	��B	��B	�B	�VB	�B	��B	��B	�B	��B	��B	ĶB	��B	� B	��B	�#B	��B	�DB	�KB	��B	ĶB	�pB	ˬB	�KB	�gB	�GB	ÖB	�mB	ȴB	�#B	ɠB	�KB	��B	�)B	�B	�(B	�[B	��B	՛B	��B	�sB	��B	�dB	�;B	�|B	�B	�`B	�B	�B	�B	�=B	�oB	��B	��B	�|B	�B	��B	�+B	�%B	��B	��B	��B	�RB	��B	�B	�3B	�;B	�B	�B	�B	�]B	��B	��B	��B	�MB	�[B	�AB	�B	�B	�}B	�)B	��B	�B	�cB	�IB	�OB	�B	�3B	�B	�[B	�B	�IB	�B	�B	�B	�B	�B	�B	�;B	��B	�vB	�B	��B	�B	�B	�B	�5B	�B	�5B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�]B	�B	� B	�UB	�B	�/B	�B	�}B	��B	��B	�qB	�B	��B	�WB	�QB	�KB	�qB	�wB	�OB	�OB	�B	�/B	�=B	�B	��B	��B	��B	��B	�QB	�=B	�B	�B	��B	�B	��B	�B	�/B	��B	�;B	��B	��B	�)B	�qB	��B	��B	�cB	�}B	��B	��B	�B	�sB	�B	��B	��B	�=B	��B	�B	�B	�wB	�]B	�)B	��B	��B	�B	�B	�B	��B	�ZB	�?B	�ZB	�B	��B	�nB	�B	�B	�GB	�AB	�;B	�B	��B	�|B	�B	��B	�GB	�aB	�-B	��B	��B	�B	�B	��B	�-B	�aB	�B	�B	��B	�MB	�B	��B	��B	��B	��B	�%B	�2B	��B	�B	�fB	��B	��B	�B	��B	��B	��B	��B	��B	�>B	��B	��B	��B	��B	��B	�dB	�qB	��B	�(B	��B	��B	�B	��B
  B
 �B
 �B
 �B
B
UB
�B
�B
[B
AB
uB
aB
{B
MB
gB
�B
9B
�B
B
tB
�B
�B
1B
KB
KB
	�B
	lB
	lB
	�B
	�B
	�B
	�B
	RB
	RB

�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
<B
BB
\B
�B
�B
�B
�B
TB
TB
:B
�B
�B
�B
uB
�B
,B
aB
�B
gB
�B
�B
�B
�B
YB
YB
�B
�B
B
�B
�B
1B
KB
1B
1B
�B
7B
QB
QB
kB
B
�B
�B
�B
�B
�B
QB
�B
�B
#B
�B
�B
)B
�B
�B
B
;B
VB
pB
VB
�B
 'B
�B
�B
VB
B
�B
�B
�B
B
;B
 BB
!�B
"NB
"�B
#B
"�B
"�B
#TB
$tB
$�B
&fB
&B
%�B
&2B
&fB
'B
'8B
'mB
'�B
'�B
'�B
(
B
(�B
(�B
(�B
)_B
)B
*eB
*�B
*�B
*�B
+�B
,B
,=B
,�B
,�B
,�B
-)B
-CB
-wB
-�B
.IB
.}B
.�B
/B
/ B
/5B
/�B
/�B
/�B
0oB
1[B
1�B
1�B
1�B
2-B
2aB
2aB
2�B
2�B
3MB
3�B
3�B
4B
4�B
4�B
5ZB
5�B
6`B
6�B
6�B
6�B
72B
7fB
8B
8B
8B
8B
8B
8B
7�B
8B
9$B
9XB
9�B
9�B
9�B
:*B
:�B
:�B
:�B
;B
;�B
;�B
;�B
;�B
<B
<PB
<PB
<PB
<jB
<�B
<�B
<�B
<�B
<�B
=qB
=qB
="B
=�B
>�B
?.B
?}B
?}B
?cB
?cB
?�B
@ B
@ B
?�B
@iB
A B
A B
A�B
B�B
B�B
B�B
C-B
B�B
B�B
CaB
C{B
DB
D3B
DgB
D�B
ESB
FYB
F%B
F?B
F%B
FB
E�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
EB
ESB
E�B
E�B
F%B
F�B
F�B
G+B
GzB
G�B
G�B
HB
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J#B
JXB
J�B
K)B
KDB
K�B
K�B
K�B
K�B
K�B
K�B
L~B
LdB
L�B
L�B
MB
MPB
M�B
M�B
N<B
NVB
N�B
O(B
O�B
O�B
O�B
O�B
P.B
PbB
P}B
P}B
P�B
P�B
QhB
Q�B
RB
R:B
RoB
R�B
R�B
S[B
S�B
TB
T,B
TFB
TaB
T{B
T�B
T�B
T�B
UgB
UgB
U�B
U�B
U�B
U�B
VB
VB
V�B
VmB
VmB
V�B
V�B
V�B
WsB
W?B
W�B
W�B
X�B
X�B
YeB
YKB
Y�B
Z7B
ZQB
ZQB
ZQB
ZkB
Z�B
Z�B
Z�B
[=B
[qB
[�B
\xB
\xB
\�B
]/B
]B
]dB
]�B
]�B
^B
^�B
_;B
_!B
_pB
_�B
`B
`\B
`�B
`�B
`�B
a-B
abB
a�B
a�B
a�B
a�B
bhB
b�B
b�B
cB
c:B
cTB
cTB
cTB
c�B
c�B
dB
d�B
d�B
e,B
eFB
e�B
e�B
e�B
fB
fLB
f�B
f�B
f�B
gB
g8B
g8B
g�B
gRB
gRB
g�B
g�B
h$B
hsB
h�B
h�B
h�B
h�B
i*B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j0B
jKB
jeB
j�B
j�B
kB
kQB
k6B
kkB
k�B
lB
lqB
l�B
l�B
l�B
l�B
l�B
m]B
m�B
m�B
m�B
nB
n}B
n}B
n�B
oB
oB
oOB
o�B
o�B
o�B
o�B
o�B
o�B
p!B
pUB
pUB
pUB
p�B
p�B
q'B
qB
qB
q[B
qvB
q�B
q�B
r-B
rGB
raB
r�B
r�B
r�B
sMB
s�B
s�B
tB
t9B
tnB
t�B
t�B
t�B
t�B
t�B
u%B
u?B
utB
u�B
vB
v+B
vFB
v`B
v�B
v�B
v�B
w2B
w2B
wfB
w�B
w�B
w�B
w�B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y$B
y>B
yrB
y�B
y�B
y�B
zB
zB
zDB
zxB
z�B
z�B
z�B
z�B
{B
{0B
{0B
{dB
{�B
{�B
{�B
{�B
|B
|PB
|jB
|jB
|�B
|�B
}B
}"B
}"B
}"B
}VB
}qB
}�B
}�B
}�B
~B
~(B
~BB
~BB
~BB
~�B
.B
HB
�B
�B
�B
�B
� B
�4B
�4B
�OB
�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	kQB	kB	j�B	jKB	i�B	i�B	i_B	i�B	i�B	k�B	m)B	uB	�B	�B	~�B	~(B	z�B	raB	z�B	�UB	�B	��B	��B	�jB	��B	��B	��B	��B	�B	ԕB

#B
4�B
C�B
u�B
B
��B
�B
�B
�B
�B�B�B5�BE�B[=BmCByrB��B��B�_B�RB��B��B�QB��B��B�B��B�mB��B��B�bB�pB�_B�B��B��B�{BrBj�BU�BDB<6B0�B&fB�BB
��B
خB
�B
�	B
�MB
��B
j�B
7�B
HB	��B	�B	�rB	�bB	y�B	b�B	I�B	3B	1B	[B�B�6B��B�oB�"B	�B	'mB	*�B	-)B	/5B	8B	HB	NVB	XyB	[�B	`�B	kQB	q�B	s�B	u�B	w�B	y�B	�RB	��B	�B	�8B	��B	�"B	��B	�aB	��B	��B	�B	�`B	��B	�NB	�B	�/B	��B	��B	�B	� B	�pB	ȀB	�[B	��B	�B	޸B	�B	�B	��B	��B	��B	��B	�B	�KB	ΊB	ĜB	�0B	�^B	��B	�{B	�#B	�#B	�7B	��B	��B	�B	�VB	�B	��B	��B	�B	��B	��B	ĶB	��B	� B	��B	�#B	��B	�DB	�KB	��B	ĶB	�pB	ˬB	�KB	�gB	�GB	ÖB	�mB	ȴB	�#B	ɠB	�KB	��B	�)B	�B	�(B	�[B	��B	՛B	��B	�sB	��B	�dB	�;B	�|B	�B	�`B	�B	�B	�B	�=B	�oB	��B	��B	�|B	�B	��B	�+B	�%B	��B	��B	��B	�RB	��B	�B	�3B	�;B	�B	�B	�B	�]B	��B	��B	��B	�MB	�[B	�AB	�B	�B	�}B	�)B	��B	�B	�cB	�IB	�OB	�B	�3B	�B	�[B	�B	�IB	�B	�B	�B	�B	�B	�B	�;B	��B	�vB	�B	��B	�B	�B	�B	�5B	�B	�5B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�]B	�B	� B	�UB	�B	�/B	�B	�}B	��B	��B	�qB	�B	��B	�WB	�QB	�KB	�qB	�wB	�OB	�OB	�B	�/B	�=B	�B	��B	��B	��B	��B	�QB	�=B	�B	�B	��B	�B	��B	�B	�/B	��B	�;B	��B	��B	�)B	�qB	��B	��B	�cB	�}B	��B	��B	�B	�sB	�B	��B	��B	�=B	��B	�B	�B	�wB	�]B	�)B	��B	��B	�B	�B	�B	��B	�ZB	�?B	�ZB	�B	��B	�nB	�B	�B	�GB	�AB	�;B	�B	��B	�|B	�B	��B	�GB	�aB	�-B	��B	��B	�B	�B	��B	�-B	�aB	�B	�B	��B	�MB	�B	��B	��B	��B	��B	�%B	�2B	��B	�B	�fB	��B	��B	�B	��B	��B	��B	��B	��B	�>B	��B	��B	��B	��B	��B	�dB	�qB	��B	�(B	��B	��B	�B	��B
  B
 �B
 �B
 �B
B
UB
�B
�B
[B
AB
uB
aB
{B
MB
gB
�B
9B
�B
B
tB
�B
�B
1B
KB
KB
	�B
	lB
	lB
	�B
	�B
	�B
	�B
	RB
	RB

�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
<B
BB
\B
�B
�B
�B
�B
TB
TB
:B
�B
�B
�B
uB
�B
,B
aB
�B
gB
�B
�B
�B
�B
YB
YB
�B
�B
B
�B
�B
1B
KB
1B
1B
�B
7B
QB
QB
kB
B
�B
�B
�B
�B
�B
QB
�B
�B
#B
�B
�B
)B
�B
�B
B
;B
VB
pB
VB
�B
 'B
�B
�B
VB
B
�B
�B
�B
B
;B
 BB
!�B
"NB
"�B
#B
"�B
"�B
#TB
$tB
$�B
&fB
&B
%�B
&2B
&fB
'B
'8B
'mB
'�B
'�B
'�B
(
B
(�B
(�B
(�B
)_B
)B
*eB
*�B
*�B
*�B
+�B
,B
,=B
,�B
,�B
,�B
-)B
-CB
-wB
-�B
.IB
.}B
.�B
/B
/ B
/5B
/�B
/�B
/�B
0oB
1[B
1�B
1�B
1�B
2-B
2aB
2aB
2�B
2�B
3MB
3�B
3�B
4B
4�B
4�B
5ZB
5�B
6`B
6�B
6�B
6�B
72B
7fB
8B
8B
8B
8B
8B
8B
7�B
8B
9$B
9XB
9�B
9�B
9�B
:*B
:�B
:�B
:�B
;B
;�B
;�B
;�B
;�B
<B
<PB
<PB
<PB
<jB
<�B
<�B
<�B
<�B
<�B
=qB
=qB
="B
=�B
>�B
?.B
?}B
?}B
?cB
?cB
?�B
@ B
@ B
?�B
@iB
A B
A B
A�B
B�B
B�B
B�B
C-B
B�B
B�B
CaB
C{B
DB
D3B
DgB
D�B
ESB
FYB
F%B
F?B
F%B
FB
E�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
EB
ESB
E�B
E�B
F%B
F�B
F�B
G+B
GzB
G�B
G�B
HB
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J#B
JXB
J�B
K)B
KDB
K�B
K�B
K�B
K�B
K�B
K�B
L~B
LdB
L�B
L�B
MB
MPB
M�B
M�B
N<B
NVB
N�B
O(B
O�B
O�B
O�B
O�B
P.B
PbB
P}B
P}B
P�B
P�B
QhB
Q�B
RB
R:B
RoB
R�B
R�B
S[B
S�B
TB
T,B
TFB
TaB
T{B
T�B
T�B
T�B
UgB
UgB
U�B
U�B
U�B
U�B
VB
VB
V�B
VmB
VmB
V�B
V�B
V�B
WsB
W?B
W�B
W�B
X�B
X�B
YeB
YKB
Y�B
Z7B
ZQB
ZQB
ZQB
ZkB
Z�B
Z�B
Z�B
[=B
[qB
[�B
\xB
\xB
\�B
]/B
]B
]dB
]�B
]�B
^B
^�B
_;B
_!B
_pB
_�B
`B
`\B
`�B
`�B
`�B
a-B
abB
a�B
a�B
a�B
a�B
bhB
b�B
b�B
cB
c:B
cTB
cTB
cTB
c�B
c�B
dB
d�B
d�B
e,B
eFB
e�B
e�B
e�B
fB
fLB
f�B
f�B
f�B
gB
g8B
g8B
g�B
gRB
gRB
g�B
g�B
h$B
hsB
h�B
h�B
h�B
h�B
i*B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j0B
jKB
jeB
j�B
j�B
kB
kQB
k6B
kkB
k�B
lB
lqB
l�B
l�B
l�B
l�B
l�B
m]B
m�B
m�B
m�B
nB
n}B
n}B
n�B
oB
oB
oOB
o�B
o�B
o�B
o�B
o�B
o�B
p!B
pUB
pUB
pUB
p�B
p�B
q'B
qB
qB
q[B
qvB
q�B
q�B
r-B
rGB
raB
r�B
r�B
r�B
sMB
s�B
s�B
tB
t9B
tnB
t�B
t�B
t�B
t�B
t�B
u%B
u?B
utB
u�B
vB
v+B
vFB
v`B
v�B
v�B
v�B
w2B
w2B
wfB
w�B
w�B
w�B
w�B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y$B
y>B
yrB
y�B
y�B
y�B
zB
zB
zDB
zxB
z�B
z�B
z�B
z�B
{B
{0B
{0B
{dB
{�B
{�B
{�B
{�B
|B
|PB
|jB
|jB
|�B
|�B
}B
}"B
}"B
}"B
}VB
}qB
}�B
}�B
}�B
~B
~(B
~BB
~BB
~BB
~�B
.B
HB
�B
�B
�B
�B
� B
�4B
�4B
�OB
�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104940  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174652  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174652  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174652                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024659  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024659  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                