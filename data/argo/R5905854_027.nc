CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:49:18Z creation;2022-06-04T17:49:18Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174918  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��S���1   @��S�-!�@/t�j~��c1V�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ��B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bh  Bp��Bw��B�  B�  B�  B�33B�  B�33B�  B�  B�  B���B���B�  B�  B���B�  B�  B�  B���B���B�  B�  B�33B���B�  B�  B�  B�ffB���B���B�  B�  B�  C   C  C  C  C  C
�C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>�C@  CA�fCD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @p�@��@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B
>B=qB=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB_�Bh=qBq
>Bw�B��B��B��B�Q�B��B�Q�B��B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B�B��B��B��B��B��C \C\C\C\C\C
(�C(�C\C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<(�C>(�C@\CA��CD\CF\CH\CJ\CL\CM��CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D�=D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF
=DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�>�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D꾹D��D�A�D��D��D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A�XAᨍA��A�4A� A�qA�'�A��PA��oA��A��)A���A��BA��A�A�<A��A�0A�dA��AऩA��A���A��qA���A�.A�T�A��A��A��
A�|A�7�A�[�A�o�A̧RA��Aę1A��A�?HA�E�A��&A�,qA���A�|�A��zA�\�A��A��fA��1A�>A�ZA�kA���A��}A�n�A���A��*A���A��/A���A�s�A�*A{��AzAw��As��Ap�$Al�Ah(�Ad8�Ab9XA_��A\$AX�AU�MAP<6AMc AI
=AG�AD�RABHA=ϫA:kQA8��A7�A5	A2$�A1�A/ݘA.�A,D�A*�CA)�A)B[A(҉A(�hA(0UA'^5A&�6A&��A'6zA'$�A&�.A$��A#˒A"�aA"��A"w�A"MjA#$�A#}VA#FtA#8�A"�cA"�PA"ԕA"�A!hsA �A �RA ȴA sA �A�VA�A@OA+A��A�uA�A�A�}A�AL0A�Ae�Av`A�MA]�AqA�yA��A�oAA�A��AFA�fA�AS&A�A�A;A�:AzAi�A4A�eA_AJ�A�qA}VA?�A	A�gA��A{JA�Av�A�A�tAaA9XA]�A��A�pAw2A�kA  A
��A	�4A�A��A��A(�A��A˒At�A*0A�A~�A��ArGAA�xAF�AA��ADgA#�A �PA ��A [WA �@��@�S�@��@�qv@��@��,@���@�v�@�v`@��$@�� @��0@��f@�'�@���@�1'@��@�-�@��j@�,�@���@�N<@��@�?}@�Ov@��@�~�@�Ɇ@�YK@��d@�E9@��@�r@�J@뫟@�L�@��@�>�@�ȴ@�S�@�w@�+@扠@��@�I@�@�[W@���@�e�@�(�@��@�P@�(@��H@��@ߘ�@�Dg@��y@�tT@ݞ�@��}@ۨX@��|@ڤ�@ٯ�@��@�g8@פ@@�dZ@� \@֖�@�&�@���@���@Հ4@�\�@�-w@���@��W@�$t@ҽ<@���@ї�@��@�y>@��z@�j@��@ΝI@�G@͕�@̹$@�Q@˴�@�P�@��@ʱ�@ʆY@��}@�F@ȗ�@�@�@Ǒh@�S@�y>@�/�@��r@ŵt@�e,@���@�9X@���@å�@�}�@�c@��@·�@�@�خ@��@�E9@��@��r@�7�@�1@�ԕ@���@�P�@� i@���@�7�@��F@�o@��R@�e�@�)�@�	�@��a@�7L@��@�z�@��@�l�@���@�ff@�>B@�#:@��9@��@�@@�֡@��@�c@�Dg@�C@�O@�A @��@�V@���@��o@��+@��o@���@���@��&@�s�@�~@���@�x�@�zx@�w2@�s@�4�@�w�@�&�@��@���@��@��@��X@��@�A�@���@�Dg@���@��@�2�@��@���@�C�@��y@��x@�A�@�	�@��#@���@��8@�
�@��}@��@�z@�	�@���@�}�@�e,@�=@��@�w�@�>B@��~@�+@��@���@�z�@�GE@�($@��@��@�x@��&@���@�o�@�]�@�!-@���@���@�V@�@�@�*�@���@��h@�X@�K�@�4@��"@��B@���@�Q@��@���@�S&@�;@���@�\�@�-�@��@��@���@���@�dZ@�E9@��@�c�@��T@��$@���@�Ĝ@��F@�C-@�4@��@�˒@�\�@��@���@��e@���@�q�@�9X@��C@�T�@��@��@��E@�Ĝ@�{�@�J�@�.�@��@��w@�iD@��"@��@�C�@�?@�8�@���@��Q@��n@�*0@��@���@�l"@�C�@�;�@�9X@��@�c�@�@���@�Ĝ@�_@��@��H@���@�<6@�S@��O@�N�@��T@�4�@��.@�N�@�J�@�A�@�+k@���@��4@�33@��@��[@�U2@��@�	�@�@�2�@�V@�
�@�c@�Ɇ@�{@���@��d@���@�rG@�F@�>�@� \@�
=@��p@���@�u�@�W�@�4n@��@��[@�33@�֡@���@�,=@�@��@��@��@��@\)@�@~��@~��@}�@|�f@|��@|6@|  @{v`@{=@z�@ze@y�@yDg@x�5@x�E@x��@x�D@xFt@w�r@w˒@w��@wH�@v��@v�r@u�@u��@u��@uc�@uQ�@u&�@t��@tK^@s�K@s�@r��@rTa@q��@q�@p�@poi@p"h@o�A@o�@o�f@o"�@n� @nOv@m�@mDg@lɆ@lN�@k˒@k��@k&@jq�@j-@i�#@i��@i/@h��@hU2@g��@gO@g�@f��@f;�@e�@eS&@e�@d�@d9X@c�g@c�*@cMj@b��@b~�@b@aJ�@a�@`�@`��@`��@`��@`|�@`PH@`:�@`$@`  @_t�@_�@^�@^Q@]�@]�"@]<6@\�P@\�D@[�@[��@[9�@[�@Z�s@Z��@Z�@Y�'@Yx�@X��@XZ@W��@W+@V��@VC�@U�@T�@TN�@T4n@T!@S�W@S�{@R�@R��@R	@Q��@Q��@Q��@Q�'@Q��@Q(�@P�@P��@P!@Pb@O��@Ox@O'�@N�x@N�@M�-@MF@M�@L��@LFt@K��@KH�@K�@J�@J��@Jv�@JL0@J1�@I��@I�@H�@H  @G��@G@O@G
=@F�8@F�@F��@F?@E�@EB�@E�@D�@D��@D6@C�@C�$@CU�@B�'@B��@A�-@A��@Ac@A?}@@�[@@2�@?v`@?RT@?.I@>��@>��@>s�@>@�@>_@=�9@=��@=�@=��@=c�@<��@<C-@;�@;�w@;�f@;"�@:��@:��@9��@9=�@8��@8�[@8�o@8C-@8*�@8@7�W@7��@7~�@7O@7�@6҉@6��@6h
@5�)@5��@5-w@4��@4]d@3��@3��@3X�@3H�@3�@2s�@1�9@1�@1�@1��@1p�@14@0��@0l"@0<�@0@0�@/˒@/~�@/X�@/Mj@//�@/@.R�@-�z@-��@-c�@-N<@-�@,��@,�@+��@+l�@+dZ@+>�@+'�@+6z@+6z@*�]@*J@)O�@)S&@)A @)�@(��@(��@(֡@(w�@(I�@(  @'��@'W?@'Y@&�,@&��@&O@%�#@%�@%f�@%L�@%Dg@%�@$�[@$�@$tT@$D�@$4n@$�@#�@#�m@#�0@#��@#�$@#a@#$t@"�@"��@"c @"?@"u@!�)@!��@!p�@!O�@ �`@ |�@ `�@ I�@��@_p@E9@C@�@�1@�+@GE@e@�@��@��@�t@f�@@��@�@h�@!@�K@��@l�@@O@�@@��@�2@�h@� @kQ@i�@YK@E�@)�@@�@�@��@�=@c�@+�@�[@��@��@y>@j@_@!@�a@�F@�@@��@�@� @v�@O@�@��@@��@��@A @�@��@ѷ@�U@�@h�@%�@�@�@�6@��@dZ@o@�y@�B@�6@��@^5@;�@ԕ@��@�~@w2@Y�@-w@�E@Ɇ@u�@Q�@"h@x@��@�:@j�@O@"�@�2@�@xl@R�@6�@&�@J@�@��@��@k�@%F@�@�@��@��@�4@oi@�@�@]�@.I@
�@
�s@
͟@
~�@
GE@
;�@
5?@
0U@
($@
!�@	�#@	��@	u�@	S&@	J�@	F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A�XAᨍA��A�4A� A�qA�'�A��PA��oA��A��)A���A��BA��A�A�<A��A�0A�dA��AऩA��A���A��qA���A�.A�T�A��A��A��
A�|A�7�A�[�A�o�A̧RA��Aę1A��A�?HA�E�A��&A�,qA���A�|�A��zA�\�A��A��fA��1A�>A�ZA�kA���A��}A�n�A���A��*A���A��/A���A�s�A�*A{��AzAw��As��Ap�$Al�Ah(�Ad8�Ab9XA_��A\$AX�AU�MAP<6AMc AI
=AG�AD�RABHA=ϫA:kQA8��A7�A5	A2$�A1�A/ݘA.�A,D�A*�CA)�A)B[A(҉A(�hA(0UA'^5A&�6A&��A'6zA'$�A&�.A$��A#˒A"�aA"��A"w�A"MjA#$�A#}VA#FtA#8�A"�cA"�PA"ԕA"�A!hsA �A �RA ȴA sA �A�VA�A@OA+A��A�uA�A�A�}A�AL0A�Ae�Av`A�MA]�AqA�yA��A�oAA�A��AFA�fA�AS&A�A�A;A�:AzAi�A4A�eA_AJ�A�qA}VA?�A	A�gA��A{JA�Av�A�A�tAaA9XA]�A��A�pAw2A�kA  A
��A	�4A�A��A��A(�A��A˒At�A*0A�A~�A��ArGAA�xAF�AA��ADgA#�A �PA ��A [WA �@��@�S�@��@�qv@��@��,@���@�v�@�v`@��$@�� @��0@��f@�'�@���@�1'@��@�-�@��j@�,�@���@�N<@��@�?}@�Ov@��@�~�@�Ɇ@�YK@��d@�E9@��@�r@�J@뫟@�L�@��@�>�@�ȴ@�S�@�w@�+@扠@��@�I@�@�[W@���@�e�@�(�@��@�P@�(@��H@��@ߘ�@�Dg@��y@�tT@ݞ�@��}@ۨX@��|@ڤ�@ٯ�@��@�g8@פ@@�dZ@� \@֖�@�&�@���@���@Հ4@�\�@�-w@���@��W@�$t@ҽ<@���@ї�@��@�y>@��z@�j@��@ΝI@�G@͕�@̹$@�Q@˴�@�P�@��@ʱ�@ʆY@��}@�F@ȗ�@�@�@Ǒh@�S@�y>@�/�@��r@ŵt@�e,@���@�9X@���@å�@�}�@�c@��@·�@�@�خ@��@�E9@��@��r@�7�@�1@�ԕ@���@�P�@� i@���@�7�@��F@�o@��R@�e�@�)�@�	�@��a@�7L@��@�z�@��@�l�@���@�ff@�>B@�#:@��9@��@�@@�֡@��@�c@�Dg@�C@�O@�A @��@�V@���@��o@��+@��o@���@���@��&@�s�@�~@���@�x�@�zx@�w2@�s@�4�@�w�@�&�@��@���@��@��@��X@��@�A�@���@�Dg@���@��@�2�@��@���@�C�@��y@��x@�A�@�	�@��#@���@��8@�
�@��}@��@�z@�	�@���@�}�@�e,@�=@��@�w�@�>B@��~@�+@��@���@�z�@�GE@�($@��@��@�x@��&@���@�o�@�]�@�!-@���@���@�V@�@�@�*�@���@��h@�X@�K�@�4@��"@��B@���@�Q@��@���@�S&@�;@���@�\�@�-�@��@��@���@���@�dZ@�E9@��@�c�@��T@��$@���@�Ĝ@��F@�C-@�4@��@�˒@�\�@��@���@��e@���@�q�@�9X@��C@�T�@��@��@��E@�Ĝ@�{�@�J�@�.�@��@��w@�iD@��"@��@�C�@�?@�8�@���@��Q@��n@�*0@��@���@�l"@�C�@�;�@�9X@��@�c�@�@���@�Ĝ@�_@��@��H@���@�<6@�S@��O@�N�@��T@�4�@��.@�N�@�J�@�A�@�+k@���@��4@�33@��@��[@�U2@��@�	�@�@�2�@�V@�
�@�c@�Ɇ@�{@���@��d@���@�rG@�F@�>�@� \@�
=@��p@���@�u�@�W�@�4n@��@��[@�33@�֡@���@�,=@�@��@��@��@��@\)@�@~��@~��@}�@|�f@|��@|6@|  @{v`@{=@z�@ze@y�@yDg@x�5@x�E@x��@x�D@xFt@w�r@w˒@w��@wH�@v��@v�r@u�@u��@u��@uc�@uQ�@u&�@t��@tK^@s�K@s�@r��@rTa@q��@q�@p�@poi@p"h@o�A@o�@o�f@o"�@n� @nOv@m�@mDg@lɆ@lN�@k˒@k��@k&@jq�@j-@i�#@i��@i/@h��@hU2@g��@gO@g�@f��@f;�@e�@eS&@e�@d�@d9X@c�g@c�*@cMj@b��@b~�@b@aJ�@a�@`�@`��@`��@`��@`|�@`PH@`:�@`$@`  @_t�@_�@^�@^Q@]�@]�"@]<6@\�P@\�D@[�@[��@[9�@[�@Z�s@Z��@Z�@Y�'@Yx�@X��@XZ@W��@W+@V��@VC�@U�@T�@TN�@T4n@T!@S�W@S�{@R�@R��@R	@Q��@Q��@Q��@Q�'@Q��@Q(�@P�@P��@P!@Pb@O��@Ox@O'�@N�x@N�@M�-@MF@M�@L��@LFt@K��@KH�@K�@J�@J��@Jv�@JL0@J1�@I��@I�@H�@H  @G��@G@O@G
=@F�8@F�@F��@F?@E�@EB�@E�@D�@D��@D6@C�@C�$@CU�@B�'@B��@A�-@A��@Ac@A?}@@�[@@2�@?v`@?RT@?.I@>��@>��@>s�@>@�@>_@=�9@=��@=�@=��@=c�@<��@<C-@;�@;�w@;�f@;"�@:��@:��@9��@9=�@8��@8�[@8�o@8C-@8*�@8@7�W@7��@7~�@7O@7�@6҉@6��@6h
@5�)@5��@5-w@4��@4]d@3��@3��@3X�@3H�@3�@2s�@1�9@1�@1�@1��@1p�@14@0��@0l"@0<�@0@0�@/˒@/~�@/X�@/Mj@//�@/@.R�@-�z@-��@-c�@-N<@-�@,��@,�@+��@+l�@+dZ@+>�@+'�@+6z@+6z@*�]@*J@)O�@)S&@)A @)�@(��@(��@(֡@(w�@(I�@(  @'��@'W?@'Y@&�,@&��@&O@%�#@%�@%f�@%L�@%Dg@%�@$�[@$�@$tT@$D�@$4n@$�@#�@#�m@#�0@#��@#�$@#a@#$t@"�@"��@"c @"?@"u@!�)@!��@!p�@!O�@ �`@ |�@ `�@ I�@��@_p@E9@C@�@�1@�+@GE@e@�@��@��@�t@f�@@��@�@h�@!@�K@��@l�@@O@�@@��@�2@�h@� @kQ@i�@YK@E�@)�@@�@�@��@�=@c�@+�@�[@��@��@y>@j@_@!@�a@�F@�@@��@�@� @v�@O@�@��@@��@��@A @�@��@ѷ@�U@�@h�@%�@�@�@�6@��@dZ@o@�y@�B@�6@��@^5@;�@ԕ@��@�~@w2@Y�@-w@�E@Ɇ@u�@Q�@"h@x@��@�:@j�@O@"�@�2@�@xl@R�@6�@&�@J@�@��@��@k�@%F@�@�@��@��@�4@oi@�@�@]�@.I@
�@
�s@
͟@
~�@
GE@
;�@
5?@
0U@
($@
!�@	�#@	��@	u�@	S&@	J�@	F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�,B�B�,B�B��B�,B�FBԯB�B�SBյB՛BٴB�5B�bB��B�B�B�B�B��B��B�+B��B �B B��B�BZ�B	�B	��B	��B	�JB	�B	�-B	��B	�B	��B	�B	�?B	��B	�B	��B	�B	��B	��B	�hB	�GB	��B	��B	��B	�mB	��B	�\B	�B	�B	��B	�pB	�NB	��B	��B	�7B	�fB	�B	�mB	��B	�VB	��B	y�B	a�B	L�B	;�B	2�B	'8B	1B	�B�wB�B��B�^B�EB��B��B��B�6B�$B�*B�yB��B��B��B��B�>B�PB��B�qB��B��BѷB�"B�B̈́BݲB�|B	B�8B��B�5B�B�B	�B	/B	.}B	?HB	T{B	_�B	jeB	� B	��B	�%B	��B	��B	��B	�XB	�B	��B	�CB	��B	��B	�?B	�B	�HB	�4B	�B	ŢB	�7B	ևB	׍B	�	B	�/B	�B	��B	ܒB	�pB	�B	�CB	�WB	ּB	҉B	��B	�jB	�pB	�6B	��B	��B	ǔB	��B	�DB	�"B	̳B	�B	��B	�B	՛B	՛B	�9B	�sB	��B	��B	��B	ՁB	׍B	�?B	ּB	�+B	��B	�=B	�CB	��B	�5B	��B	�WB	ٚB	�B	خB	�$B	��B	��B	֡B	��B	��B	ڠB	�/B	�#B	�QB	�qB	�WB	��B	�B	�CB	�]B	�CB	��B	�IB	�B	�B	�vB	�B	� B	�B	�B	�fB	�8B	�$B	�B	�B	�B	��B	�RB	�B	��B	��B	��B	�B	�eB	��B	��B	�B	�8B	��B	��B	�B	�B	�B	�B	�B	�8B	��B	�$B	��B	�$B	��B	�RB	�B	�mB	�mB	�B	�>B	��B	�8B	�mB	�
B	�sB	��B	��B	��B	�B	�B	�B	�B	�=B	�WB	�B	�wB	�QB	��B	��B	�B	�B	�B	�B	�]B	��B	�CB	��B	�B	�B	�B	�AB	��B	�[B	�-B	�vB	� B	�IB	�B	�/B	�5B	�5B	�cB	�"B	�B	��B	��B	�WB	�"B	�]B	��B	�B	�oB	�[B	�AB	�[B	��B	��B	��B	��B	�B	�9B	�B	��B	�%B	�ZB	��B	��B	�zB	�B	��B	�B	��B	��B	��B	�RB	��B	�$B	�rB	��B	�DB	��B	�0B	�0B	�0B	�B	��B	��B	��B	�xB	�DB	�*B	�DB	��B	�*B	��B	��B	��B	�B	�B	��B	�jB	��B	�<B	��B	�B	�B	��B	��B	�B	��B	�PB	�6B	��B	�B	��B	�"B	��B	��B	��B
  B
B
�B
�B
B	��B	��B	�"B	�B	�B	��B	�PB	��B	�B	��B	�"B	��B	��B	�6B	��B	��B	��B	�dB	�B	�B	�B	��B	�qB	��B	�(B	��B	�.B	�}B	��B	��B
 �B
�B
B
�B
aB
�B
�B
�B
�B
-B
MB
�B
�B
9B
�B
�B
%B
YB
�B
�B
�B
�B
�B
EB
�B
�B
�B
B
�B
�B
	7B
	7B
	RB
	lB

=B

=B

XB

rB

�B

�B
)B
DB
0B
B
dB
�B
B
PB
�B
�B
B
<B
�B
�B
�B
�B
NB
bB
B
4B
B
B
�B
�B
�B
�B
�B
[B
[B
@B
@B
[B
�B
aB
�B
�B
�B
B
�B
�B
�B
mB
�B
?B
�B
�B
�B
�B
B
+B
�B
�B
�B
eB
1B
�B
�B
B
B
�B
B
qB
�B
#B
#B
)B
xB
�B
/B
�B
B
;B
VB
�B
 �B
"B
"hB
"hB
"hB
"�B
#:B
$B
$�B
$�B
%`B
&B
&fB
'8B
(
B
)�B
*�B
+�B
+6B
*eB
)DB
)�B
*0B
*�B
+6B
+B
+B
+kB
+�B
,�B
,qB
,WB
,=B
,"B
,�B
-)B
.B
.�B
/5B
/�B
0�B
0�B
0�B
1AB
1B
1[B
1[B
1'B
2-B
2�B
2�B
3hB
2�B
3�B
33B
3�B
2�B
3hB
3�B
4nB
4�B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5tB
5�B
6B
6+B
6+B
6FB
6`B
6FB
6FB
6�B
6�B
7fB
7�B
7�B
7�B
8B
7�B
8�B
9rB
:*B
:�B
:�B
:�B
:�B
:�B
;0B
<B
<PB
<�B
<�B
<�B
<�B
<6B
;�B
;�B
<B
<jB
<�B
=B
="B
=<B
=�B
=�B
>B
>�B
>�B
>BB
>wB
>�B
?HB
?�B
?�B
@ B
@�B
A�B
B[B
B�B
B�B
CB
B�B
C-B
CGB
C�B
C�B
DB
DB
C�B
D�B
E9B
E9B
E�B
F�B
FtB
F�B
F�B
G+B
G�B
G�B
G�B
HKB
HKB
HfB
H�B
H�B
H�B
IlB
I7B
J	B
J#B
I�B
J=B
J�B
J�B
K�B
K�B
K^B
KxB
K�B
LJB
L~B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L~B
L�B
MB
M�B
M�B
NB
N"B
NpB
N�B
N�B
N�B
N�B
N�B
OB
O�B
O�B
OvB
OvB
O\B
O�B
P.B
PbB
PHB
P�B
QB
QNB
Q�B
Q�B
RTB
R�B
R�B
R�B
S@B
S[B
T,B
TFB
TaB
T�B
UgB
U�B
U�B
VB
VSB
V9B
V�B
V�B
VmB
VB
VB
V9B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W
B
W�B
X+B
X+B
XEB
XEB
X�B
X�B
X�B
Y�B
Y�B
Y�B
ZB
Z7B
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[=B
[WB
[�B
[�B
\CB
\�B
]B
]dB
]�B
^B
^�B
^�B
_B
^�B
^�B
_�B
`'B
`'B
`BB
`BB
`\B
`�B
`�B
aHB
abB
a|B
a|B
a�B
a�B
a�B
a�B
a�B
a�B
bhB
b�B
b�B
b�B
cTB
c�B
dB
d&B
d�B
e,B
e,B
e,B
eFB
e`B
e`B
e`B
ezB
e�B
e�B
e�B
e�B
e�B
fB
f2B
f�B
gRB
gmB
g�B
g�B
g�B
gmB
g�B
g�B
g�B
g�B
h
B
h$B
h�B
i*B
iyB
i�B
jKB
jB
jB
j�B
j�B
j�B
kB
kB
kB
kkB
k6B
kB
kkB
kkB
kkB
k�B
k�B
k�B
k�B
lB
l=B
l�B
l�B
l�B
m�B
m�B
m�B
n/B
nIB
nIB
ncB
n�B
n�B
n�B
n�B
n�B
o B
oiB
o�B
p�B
qAB
q[B
q�B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
r-B
r�B
r�B
s3B
sMB
s�B
s�B
tB
tB
tB
tB
t9B
t9B
tB
tB
tB
tTB
t�B
t�B
t�B
uB
u?B
u%B
uB
u%B
u?B
uZB
u�B
v+B
vFB
vFB
vFB
vFB
wB
w2B
w2B
w2B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x8B
x�B
x�B
x�B
x�B
y	B
yXB
y>B
y�B
y�B
z*B
zB
z*B
z�B
z�B
z�B
{B
{0B
{JB
{dB
{�B
{�B
{�B
|B
|B
|PB
|�B
|�B
|�B
|�B
|�B
}B
}"B
}<B
}VB
}�B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
HB
}B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�;B
�oB
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�,B�B�,B�B��B�,B�FBԯB�B�SBյB՛BٴB�5B�bB��B�B�B�B�B��B��B�+B��B �B B��B�BZ�B	�B	��B	��B	�JB	�B	�-B	��B	�B	��B	�B	�?B	��B	�B	��B	�B	��B	��B	�hB	�GB	��B	��B	��B	�mB	��B	�\B	�B	�B	��B	�pB	�NB	��B	��B	�7B	�fB	�B	�mB	��B	�VB	��B	y�B	a�B	L�B	;�B	2�B	'8B	1B	�B�wB�B��B�^B�EB��B��B��B�6B�$B�*B�yB��B��B��B��B�>B�PB��B�qB��B��BѷB�"B�B̈́BݲB�|B	B�8B��B�5B�B�B	�B	/B	.}B	?HB	T{B	_�B	jeB	� B	��B	�%B	��B	��B	��B	�XB	�B	��B	�CB	��B	��B	�?B	�B	�HB	�4B	�B	ŢB	�7B	ևB	׍B	�	B	�/B	�B	��B	ܒB	�pB	�B	�CB	�WB	ּB	҉B	��B	�jB	�pB	�6B	��B	��B	ǔB	��B	�DB	�"B	̳B	�B	��B	�B	՛B	՛B	�9B	�sB	��B	��B	��B	ՁB	׍B	�?B	ּB	�+B	��B	�=B	�CB	��B	�5B	��B	�WB	ٚB	�B	خB	�$B	��B	��B	֡B	��B	��B	ڠB	�/B	�#B	�QB	�qB	�WB	��B	�B	�CB	�]B	�CB	��B	�IB	�B	�B	�vB	�B	� B	�B	�B	�fB	�8B	�$B	�B	�B	�B	��B	�RB	�B	��B	��B	��B	�B	�eB	��B	��B	�B	�8B	��B	��B	�B	�B	�B	�B	�B	�8B	��B	�$B	��B	�$B	��B	�RB	�B	�mB	�mB	�B	�>B	��B	�8B	�mB	�
B	�sB	��B	��B	��B	�B	�B	�B	�B	�=B	�WB	�B	�wB	�QB	��B	��B	�B	�B	�B	�B	�]B	��B	�CB	��B	�B	�B	�B	�AB	��B	�[B	�-B	�vB	� B	�IB	�B	�/B	�5B	�5B	�cB	�"B	�B	��B	��B	�WB	�"B	�]B	��B	�B	�oB	�[B	�AB	�[B	��B	��B	��B	��B	�B	�9B	�B	��B	�%B	�ZB	��B	��B	�zB	�B	��B	�B	��B	��B	��B	�RB	��B	�$B	�rB	��B	�DB	��B	�0B	�0B	�0B	�B	��B	��B	��B	�xB	�DB	�*B	�DB	��B	�*B	��B	��B	��B	�B	�B	��B	�jB	��B	�<B	��B	�B	�B	��B	��B	�B	��B	�PB	�6B	��B	�B	��B	�"B	��B	��B	��B
  B
B
�B
�B
B	��B	��B	�"B	�B	�B	��B	�PB	��B	�B	��B	�"B	��B	��B	�6B	��B	��B	��B	�dB	�B	�B	�B	��B	�qB	��B	�(B	��B	�.B	�}B	��B	��B
 �B
�B
B
�B
aB
�B
�B
�B
�B
-B
MB
�B
�B
9B
�B
�B
%B
YB
�B
�B
�B
�B
�B
EB
�B
�B
�B
B
�B
�B
	7B
	7B
	RB
	lB

=B

=B

XB

rB

�B

�B
)B
DB
0B
B
dB
�B
B
PB
�B
�B
B
<B
�B
�B
�B
�B
NB
bB
B
4B
B
B
�B
�B
�B
�B
�B
[B
[B
@B
@B
[B
�B
aB
�B
�B
�B
B
�B
�B
�B
mB
�B
?B
�B
�B
�B
�B
B
+B
�B
�B
�B
eB
1B
�B
�B
B
B
�B
B
qB
�B
#B
#B
)B
xB
�B
/B
�B
B
;B
VB
�B
 �B
"B
"hB
"hB
"hB
"�B
#:B
$B
$�B
$�B
%`B
&B
&fB
'8B
(
B
)�B
*�B
+�B
+6B
*eB
)DB
)�B
*0B
*�B
+6B
+B
+B
+kB
+�B
,�B
,qB
,WB
,=B
,"B
,�B
-)B
.B
.�B
/5B
/�B
0�B
0�B
0�B
1AB
1B
1[B
1[B
1'B
2-B
2�B
2�B
3hB
2�B
3�B
33B
3�B
2�B
3hB
3�B
4nB
4�B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5tB
5�B
6B
6+B
6+B
6FB
6`B
6FB
6FB
6�B
6�B
7fB
7�B
7�B
7�B
8B
7�B
8�B
9rB
:*B
:�B
:�B
:�B
:�B
:�B
;0B
<B
<PB
<�B
<�B
<�B
<�B
<6B
;�B
;�B
<B
<jB
<�B
=B
="B
=<B
=�B
=�B
>B
>�B
>�B
>BB
>wB
>�B
?HB
?�B
?�B
@ B
@�B
A�B
B[B
B�B
B�B
CB
B�B
C-B
CGB
C�B
C�B
DB
DB
C�B
D�B
E9B
E9B
E�B
F�B
FtB
F�B
F�B
G+B
G�B
G�B
G�B
HKB
HKB
HfB
H�B
H�B
H�B
IlB
I7B
J	B
J#B
I�B
J=B
J�B
J�B
K�B
K�B
K^B
KxB
K�B
LJB
L~B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L~B
L�B
MB
M�B
M�B
NB
N"B
NpB
N�B
N�B
N�B
N�B
N�B
OB
O�B
O�B
OvB
OvB
O\B
O�B
P.B
PbB
PHB
P�B
QB
QNB
Q�B
Q�B
RTB
R�B
R�B
R�B
S@B
S[B
T,B
TFB
TaB
T�B
UgB
U�B
U�B
VB
VSB
V9B
V�B
V�B
VmB
VB
VB
V9B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W
B
W�B
X+B
X+B
XEB
XEB
X�B
X�B
X�B
Y�B
Y�B
Y�B
ZB
Z7B
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[=B
[WB
[�B
[�B
\CB
\�B
]B
]dB
]�B
^B
^�B
^�B
_B
^�B
^�B
_�B
`'B
`'B
`BB
`BB
`\B
`�B
`�B
aHB
abB
a|B
a|B
a�B
a�B
a�B
a�B
a�B
a�B
bhB
b�B
b�B
b�B
cTB
c�B
dB
d&B
d�B
e,B
e,B
e,B
eFB
e`B
e`B
e`B
ezB
e�B
e�B
e�B
e�B
e�B
fB
f2B
f�B
gRB
gmB
g�B
g�B
g�B
gmB
g�B
g�B
g�B
g�B
h
B
h$B
h�B
i*B
iyB
i�B
jKB
jB
jB
j�B
j�B
j�B
kB
kB
kB
kkB
k6B
kB
kkB
kkB
kkB
k�B
k�B
k�B
k�B
lB
l=B
l�B
l�B
l�B
m�B
m�B
m�B
n/B
nIB
nIB
ncB
n�B
n�B
n�B
n�B
n�B
o B
oiB
o�B
p�B
qAB
q[B
q�B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
r-B
r�B
r�B
s3B
sMB
s�B
s�B
tB
tB
tB
tB
t9B
t9B
tB
tB
tB
tTB
t�B
t�B
t�B
uB
u?B
u%B
uB
u%B
u?B
uZB
u�B
v+B
vFB
vFB
vFB
vFB
wB
w2B
w2B
w2B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x8B
x�B
x�B
x�B
x�B
y	B
yXB
y>B
y�B
y�B
z*B
zB
z*B
z�B
z�B
z�B
{B
{0B
{JB
{dB
{�B
{�B
{�B
|B
|B
|PB
|�B
|�B
|�B
|�B
|�B
}B
}"B
}<B
}VB
}�B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
HB
}B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�;B
�oB
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104946  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174918  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174918  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174918                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024925  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024925  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                