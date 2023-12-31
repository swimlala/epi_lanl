CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:24:16Z creation;2022-06-04T17:24:16Z conversion to V3.1      
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pD   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t0   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �(   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �0   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �8   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �x   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604172416  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               
A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ؽc	+<1   @ؽcw�	@,St�j~��dV�u1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�  A   A   A>ffA^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B̙�B�  B���B���B�  Bߙ�B�  B�  B�  B�  B�ffB�  B�  C   C  C  C  C  C
  C�C33C�fC�fC  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ Dü�D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @=p�@��@��A ��A ��A?\)A_\)A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh=qBp=qBx=qB��B��B��B��B�Q�B�Q�B��B��B��B��B��B��B��B��B��B��B��B��B��B̸RB��B��B��B��B߸RB��B��B��B��B�B��B��C \C\C\C\C\C
\C(�CB�C��C��C\C\C\C(�C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6(�C8\C:\C<(�C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch(�Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C�{C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D]�qD^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�A�D���D���D��D�A�D���D���D�D�ED���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�~�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�Dþ�D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�.�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�MA��A��A���A���A��A��A�kA�!�A� �A�,qA�.�A��AՔFA��?A�A�.�A�[#A��MA�>�A�Z�A�V9A�/�A��A�A�IA�6zA�-A�  AշA�b�A�%A�MjA�8�A��}A���A�"4A�`BA�OA��Aҡ�A��gA�!A���A�ĜAЅA�tTA��xAϣ�Aʟ�AɵtA���A���A���A���A���A�>�A���A��A���A��XA��2A��HA�>BA��OA�AUA���A��@A���A���A�5�A�1�A���A�aHA�ΥA��mA�e�A�~�A��wA�$@A�G�A��
A�w2A�.A���A~҉Axl�Aw�wAv.IAq�Ao�AkT�Ag�.Ae��A`��A[�DAYXyAVtTAQ�]AN_�AMU�AK�'AG��AC�AA@�A?��A=�A;�A9�>A9qA8<�A7�A6�	A5��A5*0A4��A3��A3m]A2�PA/�"A.��A-��A.��A0ɆA0m�A-�mA+ϫA)��A(��A)��A)�hA)��A)��A)��A)�_A*��A*�)A)�FA)8�A(��A'4A%�OA$�$A$�=A$oA#?�A!��A!_pA xlA =�A A�0A��A{�A�yAP�AA�A�KA��AcAxA7LA�
A;A�IA�.AeA��AV�A�APHA�yAQA�PA�dAj�A��A��A��A5?A��Al�A�A��Al�A(�A8�A�jA,=A�Az�A�.A�}AOvA,�A��A+AѷAj�A��AO�A	lA
iDA
�A	��A	A A�ArGA?AA�
A��A�A��A�;A��AS&A�>A��A:*A�A�,A�6AQ�AMA1A��A��AA��AV�A�A�As�A��AoA PHA �@��k@��@�0U@�_@���@��o@���@���@�;@��o@�b@���@��@�6@���@�d�@�˒@��*@�rG@��E@�tT@���@�x@��@��U@��I@�~�@�2�@��@�4@�<�@�@�33@�Ĝ@�r�@��@�U�@�j@�M@�F�@���@�D@�	�@�@��@�(@��@�tT@�ݘ@��U@���@�7@��B@�/�@��d@�M@�i�@�A @�.@��;@�{@�Dg@���@߹�@�x@�&�@���@ޝI@�7@�a@�@��U@�C�@� �@��N@ۙ�@�j�@�P�@�Ɇ@���@�J�@�l"@�>B@��j@�B�@֞@֚@�s�@�$�@լq@�p�@��`@Գh@�c�@�5?@���@ӭC@һ�@�{�@���@О@��Q@�Mj@��@͓@�O@��@���@̤�@̇�@�i�@�	@��6@˥�@ˌ~@�e,@��K@ʛ�@�xl@�'R@��@ɷ�@Ɇ�@�A�@ȱ�@��@ǈf@�n/@�T�@���@ƆY@�M@���@ų�@�m]@ľ�@�@íC@ãn@Ï�@�X�@�O@�Q�@��@�;d@�~(@�l�@�a|@�"h@���@�Mj@�2a@���@���@���@�xl@��A@�c�@�<6@��@��@��e@�s�@�?@�M@���@��7@�#�@��b@�-�@�	l@��H@�w�@� �@�ݘ@�u�@�%F@��	@��}@�Xy@�_@��*@�X@�IR@��@��!@�M@�I�@�(�@��&@��g@��h@�S&@�S@��s@���@��j@��1@�I�@��@�  @�خ@��@��@���@��+@�bN@�u@��~@�O�@��@�3�@���@�e�@���@�q@��0@�E9@�ߤ@���@�4@��~@�4@��@� i@���@���@�m�@��@��@@���@�Q�@���@�_�@�˒@���@�C@��@�1'@��@��0@���@�RT@��[@�
�@���@��@�J�@��@��4@�l"@�V@��D@���@�9�@��/@�l"@��j@���@�f�@��[@�A�@��@��o@��
@��^@��k@�x@�0�@��P@���@���@��@��:@��P@��@�!@�s�@�$t@���@�x@��@�k�@��@�%@��@�{�@�!@��m@�~�@�=�@��@��2@���@�_�@�#:@���@�@��X@�p�@�;d@�@��@�h�@�-�@��@��@�rG@���@���@�h�@�!@���@�F�@���@���@�PH@� �@�
�@���@�خ@��@���@�a@���@��4@�u�@�?�@�4@���@���@�x�@�dZ@�U�@�Y@���@��Y@�Xy@�x@��6@��@���@�o�@�@@�Ɇ@���@�m�@�YK@�B[@�4n@�)�@��}@���@��F@�8�@�˒@���@�g�@��	@���@���@�I�@�ݘ@���@��P@��O@���@�V�@�A�@˒@~�8@~��@~	@}��@}�@|r�@{��@{�@{~�@{)_@zff@y��@y@x�_@xV�@xb@w�[@w)_@vں@v��@vM�@u�9@u%@tq@t  @st�@rp;@r
�@p��@o�W@o;d@nM�@mk�@m0�@l�?@l$@k��@k��@k�@k�k@k"�@j�b@jH�@i�M@i�@h�v@h�@h��@hh�@g��@g~�@gS�@g!-@f�@f�@fߤ@f͟@f�R@f�1@f��@fv�@f	@e�j@e|@dD�@cb�@b�@b��@bH�@a�X@a&�@`�@`q@`9X@`	�@_�P@^+k@]��@]=�@\�@\e�@[�g@Z͟@Z�@Y��@YJ�@Y+@X�@X�?@X��@X�I@X��@X$@W��@W4�@V��@VW�@VTa@V$�@U�9@U\�@T�5@T�@T��@Th�@TM@So�@S,�@SS@R��@Ru@Q�@Q�"@Q`B@P�@P"h@O~�@O,�@O@N�@NZ�@M�D@M��@M�@M��@LA�@K�:@K{J@J��@J��@Jv�@J&�@I*0@H��@Hz�@G��@G��@Gqv@Gl�@F��@F�}@FV@E�@E�-@E�M@ET�@E!�@D�.@C�W@C�@C1�@Bȴ@Bc @B&�@@�P@@(�@?�g@?��@?a@?>�@? i@>��@=�T@=��@=Vm@=@@<�@<��@<tT@;�@;��@;��@;~�@;]�@;.I@:�8@:��@:��@:��@:z@:�@9�M@8��@8|�@8$@81@7e�@6ߤ@6n�@6�@5@5��@5��@5\�@5!�@4��@4��@4�4@4`�@49X@4  @3�6@3�P@3a@3>�@2�M@2��@2V@2�@1�C@1Y�@1L�@1<6@0�@0@/E9@.�m@.i�@.1�@._@-�>@-��@-S&@,�)@,�@,7�@+�@+�@+)_@*��@*�@*($@)��@)�H@)�h@):�@)-w@(�@(��@(��@(Xy@(7@'�+@'�w@'�@@'qv@'33@'�@&�y@&�x@&Q@&-@&@%zx@%B�@%�@%	l@$��@$��@$K^@$@#��@#��@#_p@#H�@#@"�s@"p;@"�@!�@!�@!�"@!L�@ ی@ r�@ C-@ 1'@ �@خ@��@j�@@O@�M@�!@��@xl@Z�@{@u@��@�@�#@@��@L�@V@��@��@bN@6@ �@�@�0@y�@�2@�6@��@_�@�@�3@��@hs@+�@%@�`@y>@V�@2�@@o�@(@5?@�@zx@5�@�K@Ɇ@j@7@�w@~�@P�@�@ߤ@��@GE@�@�@��@��@�h@m]@B�@@@ی@��@l"@"h@�@�@��@n/@Mj@�@�@��@��@��@xl@c @+k@��@�H@��@w2@^�@<6@�@�K@��@U2@I�@H@7�@�@��@U�@33@
��@
z@
l�@
L0@
8�@
	@	��@	\�@	&�@	�@�@��@�[11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�MA��A��A���A���A��A��A�kA�!�A� �A�,qA�.�A��AՔFA��?A�A�.�A�[#A��MA�>�A�Z�A�V9A�/�A��A�A�IA�6zA�-A�  AշA�b�A�%A�MjA�8�A��}A���A�"4A�`BA�OA��Aҡ�A��gA�!A���A�ĜAЅA�tTA��xAϣ�Aʟ�AɵtA���A���A���A���A���A�>�A���A��A���A��XA��2A��HA�>BA��OA�AUA���A��@A���A���A�5�A�1�A���A�aHA�ΥA��mA�e�A�~�A��wA�$@A�G�A��
A�w2A�.A���A~҉Axl�Aw�wAv.IAq�Ao�AkT�Ag�.Ae��A`��A[�DAYXyAVtTAQ�]AN_�AMU�AK�'AG��AC�AA@�A?��A=�A;�A9�>A9qA8<�A7�A6�	A5��A5*0A4��A3��A3m]A2�PA/�"A.��A-��A.��A0ɆA0m�A-�mA+ϫA)��A(��A)��A)�hA)��A)��A)��A)�_A*��A*�)A)�FA)8�A(��A'4A%�OA$�$A$�=A$oA#?�A!��A!_pA xlA =�A A�0A��A{�A�yAP�AA�A�KA��AcAxA7LA�
A;A�IA�.AeA��AV�A�APHA�yAQA�PA�dAj�A��A��A��A5?A��Al�A�A��Al�A(�A8�A�jA,=A�Az�A�.A�}AOvA,�A��A+AѷAj�A��AO�A	lA
iDA
�A	��A	A A�ArGA?AA�
A��A�A��A�;A��AS&A�>A��A:*A�A�,A�6AQ�AMA1A��A��AA��AV�A�A�As�A��AoA PHA �@��k@��@�0U@�_@���@��o@���@���@�;@��o@�b@���@��@�6@���@�d�@�˒@��*@�rG@��E@�tT@���@�x@��@��U@��I@�~�@�2�@��@�4@�<�@�@�33@�Ĝ@�r�@��@�U�@�j@�M@�F�@���@�D@�	�@�@��@�(@��@�tT@�ݘ@��U@���@�7@��B@�/�@��d@�M@�i�@�A @�.@��;@�{@�Dg@���@߹�@�x@�&�@���@ޝI@�7@�a@�@��U@�C�@� �@��N@ۙ�@�j�@�P�@�Ɇ@���@�J�@�l"@�>B@��j@�B�@֞@֚@�s�@�$�@լq@�p�@��`@Գh@�c�@�5?@���@ӭC@һ�@�{�@���@О@��Q@�Mj@��@͓@�O@��@���@̤�@̇�@�i�@�	@��6@˥�@ˌ~@�e,@��K@ʛ�@�xl@�'R@��@ɷ�@Ɇ�@�A�@ȱ�@��@ǈf@�n/@�T�@���@ƆY@�M@���@ų�@�m]@ľ�@�@íC@ãn@Ï�@�X�@�O@�Q�@��@�;d@�~(@�l�@�a|@�"h@���@�Mj@�2a@���@���@���@�xl@��A@�c�@�<6@��@��@��e@�s�@�?@�M@���@��7@�#�@��b@�-�@�	l@��H@�w�@� �@�ݘ@�u�@�%F@��	@��}@�Xy@�_@��*@�X@�IR@��@��!@�M@�I�@�(�@��&@��g@��h@�S&@�S@��s@���@��j@��1@�I�@��@�  @�خ@��@��@���@��+@�bN@�u@��~@�O�@��@�3�@���@�e�@���@�q@��0@�E9@�ߤ@���@�4@��~@�4@��@� i@���@���@�m�@��@��@@���@�Q�@���@�_�@�˒@���@�C@��@�1'@��@��0@���@�RT@��[@�
�@���@��@�J�@��@��4@�l"@�V@��D@���@�9�@��/@�l"@��j@���@�f�@��[@�A�@��@��o@��
@��^@��k@�x@�0�@��P@���@���@��@��:@��P@��@�!@�s�@�$t@���@�x@��@�k�@��@�%@��@�{�@�!@��m@�~�@�=�@��@��2@���@�_�@�#:@���@�@��X@�p�@�;d@�@��@�h�@�-�@��@��@�rG@���@���@�h�@�!@���@�F�@���@���@�PH@� �@�
�@���@�خ@��@���@�a@���@��4@�u�@�?�@�4@���@���@�x�@�dZ@�U�@�Y@���@��Y@�Xy@�x@��6@��@���@�o�@�@@�Ɇ@���@�m�@�YK@�B[@�4n@�)�@��}@���@��F@�8�@�˒@���@�g�@��	@���@���@�I�@�ݘ@���@��P@��O@���@�V�@�A�@˒@~�8@~��@~	@}��@}�@|r�@{��@{�@{~�@{)_@zff@y��@y@x�_@xV�@xb@w�[@w)_@vں@v��@vM�@u�9@u%@tq@t  @st�@rp;@r
�@p��@o�W@o;d@nM�@mk�@m0�@l�?@l$@k��@k��@k�@k�k@k"�@j�b@jH�@i�M@i�@h�v@h�@h��@hh�@g��@g~�@gS�@g!-@f�@f�@fߤ@f͟@f�R@f�1@f��@fv�@f	@e�j@e|@dD�@cb�@b�@b��@bH�@a�X@a&�@`�@`q@`9X@`	�@_�P@^+k@]��@]=�@\�@\e�@[�g@Z͟@Z�@Y��@YJ�@Y+@X�@X�?@X��@X�I@X��@X$@W��@W4�@V��@VW�@VTa@V$�@U�9@U\�@T�5@T�@T��@Th�@TM@So�@S,�@SS@R��@Ru@Q�@Q�"@Q`B@P�@P"h@O~�@O,�@O@N�@NZ�@M�D@M��@M�@M��@LA�@K�:@K{J@J��@J��@Jv�@J&�@I*0@H��@Hz�@G��@G��@Gqv@Gl�@F��@F�}@FV@E�@E�-@E�M@ET�@E!�@D�.@C�W@C�@C1�@Bȴ@Bc @B&�@@�P@@(�@?�g@?��@?a@?>�@? i@>��@=�T@=��@=Vm@=@@<�@<��@<tT@;�@;��@;��@;~�@;]�@;.I@:�8@:��@:��@:��@:z@:�@9�M@8��@8|�@8$@81@7e�@6ߤ@6n�@6�@5@5��@5��@5\�@5!�@4��@4��@4�4@4`�@49X@4  @3�6@3�P@3a@3>�@2�M@2��@2V@2�@1�C@1Y�@1L�@1<6@0�@0@/E9@.�m@.i�@.1�@._@-�>@-��@-S&@,�)@,�@,7�@+�@+�@+)_@*��@*�@*($@)��@)�H@)�h@):�@)-w@(�@(��@(��@(Xy@(7@'�+@'�w@'�@@'qv@'33@'�@&�y@&�x@&Q@&-@&@%zx@%B�@%�@%	l@$��@$��@$K^@$@#��@#��@#_p@#H�@#@"�s@"p;@"�@!�@!�@!�"@!L�@ ی@ r�@ C-@ 1'@ �@خ@��@j�@@O@�M@�!@��@xl@Z�@{@u@��@�@�#@@��@L�@V@��@��@bN@6@ �@�@�0@y�@�2@�6@��@_�@�@�3@��@hs@+�@%@�`@y>@V�@2�@@o�@(@5?@�@zx@5�@�K@Ɇ@j@7@�w@~�@P�@�@ߤ@��@GE@�@�@��@��@�h@m]@B�@@@ی@��@l"@"h@�@�@��@n/@Mj@�@�@��@��@��@xl@c @+k@��@�H@��@w2@^�@<6@�@�K@��@U2@I�@H@7�@�@��@U�@33@
��@
z@
l�@
L0@
8�@
	@	��@	\�@	&�@	�@�@��@�[11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�~B��B�IB��B�/B��B��B�jB�vB��B��B��B�OB��B�iB�BBʌB�FB�xB	VB	mB	yB	9B	,B	�B	�B	;B	�B	
B	^B��B��B�0B��B�PB��B�B�MB��B�+B�iB��B|�Bu�Bm�BkB~(B��B��B	=�B	MB	n}B	��B	�B
Q�B
��B
ªB#�Bd�B�+B��B��B�4B��B��B��B�)Br�B[�B2B�B	�B
�CB
�sB
��B
�{B
��B
x�B
0;B
�B
4B
NB
HB
�B
mB	�B	��B	��B	�B	�	B	��B	t�B	eFB	ZkB	?}B	$@B	9B	YB�+B�RB�hB��B�9B�3B��B��B�AB��B��B�dB��B�fB	�B	,�B	ESB	VB	gmB	jeB	o�B	X�B	PB	R B	i�B	�kB	�NB	�hB	��B	�tB	�+B	�B	��B	�*B
 B
{B
YB
B
 'B
 �B
(�B
)�B
!�B
�B
�B
+B
qB
YB
�B
7B
�B
�B
# B
,"B
,�B
0UB
3�B
49B
7�B
8RB
9rB
@OB
DgB
F�B
J	B
LB
I�B
H�B
HB
H�B
G�B
F�B
ESB
E�B
G_B
G+B
F�B
H�B
J�B
HfB
LdB
LB
J#B
KB
F�B
C�B
CGB
B�B
E�B
E�B
D�B
D�B
DB
B�B
@ B
>�B
=�B
<�B
9rB
6`B
4nB
1�B
.�B
-�B
-�B
.cB
-�B
-]B
-CB
-B
-]B
-�B
0;B
0�B
0�B
4�B
8B
:DB
:*B
9�B
8B
7LB
6FB
5�B
4�B
4�B
5�B
5�B
5tB
4�B
3MB
1�B
0�B
/�B
.�B
/�B
.cB
-)B
*�B
*�B
)*B
+B
)�B
(�B
(�B
(sB
(sB
(XB
'�B
'�B
&�B
&2B
%�B
%FB
#B
"�B
"�B
!�B
!�B
!-B
 \B
�B
VB
�B
�B
B
�B
~B
�B
�B
�B
�B
�B
#B
WB
�B
�B
7B
�B
�B
�B
{B
�B
�B
2B
�B
�B
FB
uB
�B
�B
�B
�B
(B
�B
<B
�B
B
B
dB
~B
�B
�B
JB
�B
^B

�B

�B

#B
	�B
KB
�B
�B
�B
�B
	B
	�B

�B

�B
xB

	B
	�B
�B
	B

	B
�B
�B
�B
�B
YB
SB
B
mB
B
B
�B
�B
�B
�B
�B
3B
MB
aB
GB
�B
B
�B
AB
B
B
AB
AB
�B
B
B
B
�B
B
�B
�B
�B
B
9B
B
B
mB
?B
B
�B
�B
�B
�B
+B
�B
�B
�B
�B
�B
�B
B
B
fB
1B
�B
�B
�B
KB
B
�B
�B
1B
�B
�B
�B
�B
�B
�B
�B
1B
�B
B
KB
�B
�B
	B
	B
	lB
	�B

#B

XB

�B
B

�B
0B
~B
B
0B
�B
6B
�B
�B
"B
pB
VB
VB
VB
�B
VB
pB
pB
�B
<B
�B
�B
BB
vB
�B
�B
�B
�B
BB
B
VB
"B
�B
(B
(B
�B
(B
\B
BB
B
vB
�B
�B
�B
�B
jB
�B
6B
�B
B
pB
�B
B
�B
�B
�B
.B
HB
}B
bB
�B
�B
B
�B
B
TB
�B
�B
�B
[B
[B
�B
�B
�B
�B
FB
FB
aB
MB
B
�B
?B
YB
EB
_B
KB
B
B
KB
�B
qB
�B
�B
�B
�B
)B
)B
)B
�B
�B
�B
�B
]B
�B
�B
�B
CB
CB
�B
dB
dB
�B
OB
OB
�B
VB
 \B
 �B
!HB
!�B
!�B
"hB
#:B
#�B
#�B
#�B
#�B
#�B
$tB
$�B
$�B
%FB
&LB
&fB
&fB
&�B
&�B
&�B
'8B
'�B
(
B
(sB
(�B
)_B
)�B
*KB
*B
*B
*�B
*�B
*�B
*�B
+B
+QB
+�B
+�B
+�B
+�B
,"B
,qB
,WB
,qB
,WB
,�B
,�B
-]B
-)B
-�B
-�B
-�B
-�B
-�B
./B
.cB
.}B
.�B
.�B
.�B
.�B
.cB
.�B
/�B
/ B
.cB
-�B
/iB
0UB
1B
1�B
1�B
1AB
1vB
0�B
0;B
/5B
.�B
.�B
/iB
/B
.cB
.B
.IB
.�B
/�B
0UB
1'B
0�B
0�B
1B
1�B
1�B
2B
2aB
2aB
2aB
2�B
33B
3hB
3�B
3�B
3�B
49B
4TB
4�B
4�B
5?B
5ZB
6`B
7B
7�B
8�B
9�B
9�B
:DB
:�B
;dB
<6B
<B
<B
;�B
<B
<B
<B
<B
<6B
<B
<6B
<PB
<�B
<�B
<�B
="B
=<B
=qB
=�B
=�B
=�B
>wB
>wB
>�B
?HB
?}B
@ B
@B
?�B
?�B
?�B
@iB
@�B
A B
B�B
C�B
C�B
C�B
DB
CGB
CaB
B�B
B�B
B�B
C-B
C�B
DgB
D�B
D�B
EB
E9B
E9B
ESB
E9B
E9B
E�B
E�B
E�B
FtB
FtB
F?B
F�B
F�B
G+B
GEB
G�B
G�B
G�B
H�B
IRB
I�B
JXB
KB
J�B
K^B
KxB
K�B
LB
L�B
MB
MPB
MPB
MjB
M�B
M�B
M�B
M�B
M�B
N�B
M�B
M�B
N"B
M�B
M�B
N"B
O(B
O\B
OvB
O�B
O�B
O�B
O�B
PHB
PbB
P�B
Q4B
Q4B
QB
Q B
Q4B
QhB
Q�B
Q�B
RB
R:B
R�B
R:B
SuB
S�B
S�B
T,B
TaB
TaB
T{B
T�B
T�B
T�B
U2B
UgB
UgB
U�B
U�B
VSB
VSB
VSB
V�B
WsB
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
XB
X�B
YeB
Y�B
ZQB
Z7B
Z�B
Z�B
[qB
\)B
]B
]B
]dB
]~B
]�B
]�B
]�B
]�B
^B
^5B
^�B
^�B
^�B
_VB
_�B
_�B
_�B
`BB
`'B
`BB
`'B
`\B
`vB
`�B
aB
abB
abB
a�B
a�B
b4B
bhB
cB
cB
cTB
c�B
c�B
dZB
d@B
d�B
eB
eFB
eFB
ezB
e�B
e�B
fB
e�B
e�B
ffB
f�B
f�B
f�B
f�B
gB
g8B
gmB
gmB
g�B
h
B
h$B
h
B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
jeB
jeB
jB
j�B
j�B
kQB
k�B
k�B
k�B
k�B
lWB
l�B
l�B
l�B
l�B
m)B
m]B
m]B
m�B
m�B
m�B
n/B
n/B
nIB
n}B
n}B
n�B
n�B
n}B
n�B
n�B
o5B
oiB
o�B
o�B
pB
o�B
pB
p!B
poB
pUB
q'B
q'B
qAB
qvB
q�B
rB
rGB
r-B
r�B
r�B
r�B
s3B
s3B
sMB
sMB
s�B
tnB
u%B
uZB
u�B
u�B
v+B
v+B
v�B
v�B
v�B
w2B
wLB
wfB
w�B
w�B
xB
xRB
xlB
x�B
x�B
x�B
x�B
y	B
y$B
y>B
yrB
yXB
y�B
y�B
z*B
zxB
z�B
z�B
{B
{dB
{�B
{�B
{�B
{�B
|B
|B
|B
|�B
|�B
|�B
|�B
|�B
}B
}<B
}�B
}�B
}�B
}�B
}�B
~(B
~�B
~�B
~�B
cB
�B
� B
�4B
�4B
�iB
��B
�oB
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�~B��B�IB��B�/B��B��B�jB�vB��B��B��B�OB��B�iB�BBʌB�FB�xB	VB	mB	yB	9B	,B	�B	�B	;B	�B	
B	^B��B��B�0B��B�PB��B�B�MB��B�+B�iB��B|�Bu�Bm�BkB~(B��B��B	=�B	MB	n}B	��B	�B
Q�B
��B
ªB#�Bd�B�+B��B��B�4B��B��B��B�)Br�B[�B2B�B	�B
�CB
�sB
��B
�{B
��B
x�B
0;B
�B
4B
NB
HB
�B
mB	�B	��B	��B	�B	�	B	��B	t�B	eFB	ZkB	?}B	$@B	9B	YB�+B�RB�hB��B�9B�3B��B��B�AB��B��B�dB��B�fB	�B	,�B	ESB	VB	gmB	jeB	o�B	X�B	PB	R B	i�B	�kB	�NB	�hB	��B	�tB	�+B	�B	��B	�*B
 B
{B
YB
B
 'B
 �B
(�B
)�B
!�B
�B
�B
+B
qB
YB
�B
7B
�B
�B
# B
,"B
,�B
0UB
3�B
49B
7�B
8RB
9rB
@OB
DgB
F�B
J	B
LB
I�B
H�B
HB
H�B
G�B
F�B
ESB
E�B
G_B
G+B
F�B
H�B
J�B
HfB
LdB
LB
J#B
KB
F�B
C�B
CGB
B�B
E�B
E�B
D�B
D�B
DB
B�B
@ B
>�B
=�B
<�B
9rB
6`B
4nB
1�B
.�B
-�B
-�B
.cB
-�B
-]B
-CB
-B
-]B
-�B
0;B
0�B
0�B
4�B
8B
:DB
:*B
9�B
8B
7LB
6FB
5�B
4�B
4�B
5�B
5�B
5tB
4�B
3MB
1�B
0�B
/�B
.�B
/�B
.cB
-)B
*�B
*�B
)*B
+B
)�B
(�B
(�B
(sB
(sB
(XB
'�B
'�B
&�B
&2B
%�B
%FB
#B
"�B
"�B
!�B
!�B
!-B
 \B
�B
VB
�B
�B
B
�B
~B
�B
�B
�B
�B
�B
#B
WB
�B
�B
7B
�B
�B
�B
{B
�B
�B
2B
�B
�B
FB
uB
�B
�B
�B
�B
(B
�B
<B
�B
B
B
dB
~B
�B
�B
JB
�B
^B

�B

�B

#B
	�B
KB
�B
�B
�B
�B
	B
	�B

�B

�B
xB

	B
	�B
�B
	B

	B
�B
�B
�B
�B
YB
SB
B
mB
B
B
�B
�B
�B
�B
�B
3B
MB
aB
GB
�B
B
�B
AB
B
B
AB
AB
�B
B
B
B
�B
B
�B
�B
�B
B
9B
B
B
mB
?B
B
�B
�B
�B
�B
+B
�B
�B
�B
�B
�B
�B
B
B
fB
1B
�B
�B
�B
KB
B
�B
�B
1B
�B
�B
�B
�B
�B
�B
�B
1B
�B
B
KB
�B
�B
	B
	B
	lB
	�B

#B

XB

�B
B

�B
0B
~B
B
0B
�B
6B
�B
�B
"B
pB
VB
VB
VB
�B
VB
pB
pB
�B
<B
�B
�B
BB
vB
�B
�B
�B
�B
BB
B
VB
"B
�B
(B
(B
�B
(B
\B
BB
B
vB
�B
�B
�B
�B
jB
�B
6B
�B
B
pB
�B
B
�B
�B
�B
.B
HB
}B
bB
�B
�B
B
�B
B
TB
�B
�B
�B
[B
[B
�B
�B
�B
�B
FB
FB
aB
MB
B
�B
?B
YB
EB
_B
KB
B
B
KB
�B
qB
�B
�B
�B
�B
)B
)B
)B
�B
�B
�B
�B
]B
�B
�B
�B
CB
CB
�B
dB
dB
�B
OB
OB
�B
VB
 \B
 �B
!HB
!�B
!�B
"hB
#:B
#�B
#�B
#�B
#�B
#�B
$tB
$�B
$�B
%FB
&LB
&fB
&fB
&�B
&�B
&�B
'8B
'�B
(
B
(sB
(�B
)_B
)�B
*KB
*B
*B
*�B
*�B
*�B
*�B
+B
+QB
+�B
+�B
+�B
+�B
,"B
,qB
,WB
,qB
,WB
,�B
,�B
-]B
-)B
-�B
-�B
-�B
-�B
-�B
./B
.cB
.}B
.�B
.�B
.�B
.�B
.cB
.�B
/�B
/ B
.cB
-�B
/iB
0UB
1B
1�B
1�B
1AB
1vB
0�B
0;B
/5B
.�B
.�B
/iB
/B
.cB
.B
.IB
.�B
/�B
0UB
1'B
0�B
0�B
1B
1�B
1�B
2B
2aB
2aB
2aB
2�B
33B
3hB
3�B
3�B
3�B
49B
4TB
4�B
4�B
5?B
5ZB
6`B
7B
7�B
8�B
9�B
9�B
:DB
:�B
;dB
<6B
<B
<B
;�B
<B
<B
<B
<B
<6B
<B
<6B
<PB
<�B
<�B
<�B
="B
=<B
=qB
=�B
=�B
=�B
>wB
>wB
>�B
?HB
?}B
@ B
@B
?�B
?�B
?�B
@iB
@�B
A B
B�B
C�B
C�B
C�B
DB
CGB
CaB
B�B
B�B
B�B
C-B
C�B
DgB
D�B
D�B
EB
E9B
E9B
ESB
E9B
E9B
E�B
E�B
E�B
FtB
FtB
F?B
F�B
F�B
G+B
GEB
G�B
G�B
G�B
H�B
IRB
I�B
JXB
KB
J�B
K^B
KxB
K�B
LB
L�B
MB
MPB
MPB
MjB
M�B
M�B
M�B
M�B
M�B
N�B
M�B
M�B
N"B
M�B
M�B
N"B
O(B
O\B
OvB
O�B
O�B
O�B
O�B
PHB
PbB
P�B
Q4B
Q4B
QB
Q B
Q4B
QhB
Q�B
Q�B
RB
R:B
R�B
R:B
SuB
S�B
S�B
T,B
TaB
TaB
T{B
T�B
T�B
T�B
U2B
UgB
UgB
U�B
U�B
VSB
VSB
VSB
V�B
WsB
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
XB
X�B
YeB
Y�B
ZQB
Z7B
Z�B
Z�B
[qB
\)B
]B
]B
]dB
]~B
]�B
]�B
]�B
]�B
^B
^5B
^�B
^�B
^�B
_VB
_�B
_�B
_�B
`BB
`'B
`BB
`'B
`\B
`vB
`�B
aB
abB
abB
a�B
a�B
b4B
bhB
cB
cB
cTB
c�B
c�B
dZB
d@B
d�B
eB
eFB
eFB
ezB
e�B
e�B
fB
e�B
e�B
ffB
f�B
f�B
f�B
f�B
gB
g8B
gmB
gmB
g�B
h
B
h$B
h
B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
jeB
jeB
jB
j�B
j�B
kQB
k�B
k�B
k�B
k�B
lWB
l�B
l�B
l�B
l�B
m)B
m]B
m]B
m�B
m�B
m�B
n/B
n/B
nIB
n}B
n}B
n�B
n�B
n}B
n�B
n�B
o5B
oiB
o�B
o�B
pB
o�B
pB
p!B
poB
pUB
q'B
q'B
qAB
qvB
q�B
rB
rGB
r-B
r�B
r�B
r�B
s3B
s3B
sMB
sMB
s�B
tnB
u%B
uZB
u�B
u�B
v+B
v+B
v�B
v�B
v�B
w2B
wLB
wfB
w�B
w�B
xB
xRB
xlB
x�B
x�B
x�B
x�B
y	B
y$B
y>B
yrB
yXB
y�B
y�B
z*B
zxB
z�B
z�B
{B
{dB
{�B
{�B
{�B
{�B
|B
|B
|B
|�B
|�B
|�B
|�B
|�B
}B
}<B
}�B
}�B
}�B
}�B
}�B
~(B
~�B
~�B
~�B
cB
�B
� B
�4B
�4B
�iB
��B
�oB
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104846  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172416  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172416  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172416                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022423  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022423  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                