CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:23:01Z creation;2022-06-04T17:23:01Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604172301  20220610121506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ث�;���1   @ث�J%*�@+�$�/��d�~��"�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���BB�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C33C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP33CR�CS� CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̃3D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@#�
@��@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh=qBp=qBx=qB��B�Q�B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B�RB��B��B��C \C\C\C\C\C
\C\C\C\C\C\C\C\C(�CB�C\C��C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C7��C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN(�CPB�CR(�CS�\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C�{C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D
=D�=D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�~�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D���D�A�D���D���D��D�A�D���D���D��D�A�D��D���D��D�A�D�~�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D̅D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D��D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��dA���A��A��`A���A���A��A��A��DA���A���A��A��A��A��"A��A���A��A��A���A���A���A��A�%FA�6�A�8RA�&�A��A�҉Aʱ�AʉA�S�A�&A��A��.A���A�ںAɹ�AɥA�{JA�h>A�՛A���A�B�AöA�ߤA��A��A�T�A�N<A�=�A�=A�ŢA��sA���A�\]A��jA�#�A���A���A�9$A��xA�PHA�aHA�H�A�NpA�V9A��A���A�#A�m�A�y	A�>wA�_�A�K�A�|A���A�xA�aHA��uA��;A�d�A�{A��zA�2�A�E�A�v`A��pA�YA�� A�.A���A��A~�A}{Az��AxS�Au"hAr�_AnB�Aj�AfA`�IAZVAWp�AQC�AE�AC��AB-AAB[A?tTA;ϫA7�zA6�	A5�kA2*�A0x�A.�A-�A->BA,�A,kQA,)_A+~�A*�?A)�OA'y�A%�A$Q�A$2�A#��A#rGA"�A!�A!$�AS�A��A \A�*A�+A�A�jA S�A g8A D�A�A�$AQ�A˒Ak�A\�AیA�FAE9A�A��A��AA�AYA�bA'�A�mA"�Ac�A��A�|A��A	�Ae,A�A[WA��AA4�A�A��A�HAV�A�pA`BAy�A�A
�XA
��A	�A	��A	W?A��A��APHA�VA>�A�A�vA��A2aA��AA�A��A9XA�=A�A��A��A�A �}A 7L@��@�Ĝ@��@��@�ی@��@�I�@���@��@��"@�s�@�Y@���@�+@��`@���@�?@�\)@���@��\@�<�@��>@��@�H�@�'@�|�@�Z�@���@�|�@�q@�@���@�7@�خ@�b�@���@�w�@��@�L@��@�2a@�}V@�+@��@�H�@�33@�S�@�u�@⤩@���@�}�@��	@�W�@ߨX@ޕ�@�7�@��@ݻ0@�=@��8@�c @�@�[W@ڭ�@�hs@؃�@���@�ƨ@�&@�j@�{@ճ�@�K�@��@�3�@�Y�@�͟@қ�@��@шf@мj@�"h@�V@ή}@ΔF@��&@�g8@��@�-@ɐ�@�5�@ȹ�@�h
@�>B@�G@���@Ƿ�@�~�@�Vm@���@�h�@�@ŀ4@�q@��@�M@ô�@Û=@��@���@���@���@��K@�<�@��@���@���@��u@�?@�!�@��7@�4@��@���@���@� i@��@��@�Q@�4@��@�Y�@�Mj@��	@���@�{�@�,=@��^@��4@�>�@�h
@��@��@���@���@�s�@��@�|�@�:�@��Z@��3@�{J@�#�@��@��L@�C�@��@�P�@��@��@��_@��@�V@��O@�u�@���@�\)@��@�bN@�T�@���@�l�@���@�n/@�E9@���@�s�@�8�@���@���@�t�@�Q�@���@���@���@�xl@�?�@� �@�
�@���@�RT@��@��"@�ߤ@�ی@���@���@�kQ@�M@��T@���@�/@�ی@��j@��+@���@�dZ@�*0@��	@�`�@��6@�a�@��<@�c�@���@��w@�@O@�a|@���@�_p@�Vm@�.I@�ی@���@�.�@�$@��@��.@��@�{J@�x@�c�@��@���@�Ov@���@���@��S@�_p@�+@��@�W�@�O@��@��f@��@��r@�-@��@�Y@��v@�Ft@���@��@��$@��4@�V�@��@�_p@��@�S@���@�ȴ@���@�Q�@�!�@�@���@���@�w2@�Mj@�(�@��@���@�K^@�خ@���@��	@�\)@�2a@�S@���@�}V@�=q@��@���@���@�zx@�"�@��b@�|�@�J�@��H@��p@���@�?�@�3�@�1'@��A@�'�@��@�>B@��@@�C@���@���@���@�h
@�-�@���@�A @�!�@�q@��@��@�@��]@���@��O@���@�7�@��@��g@���@��[@�o @�Q�@���@��m@�oi@�=q@�&�@�O@�_@�
@_p@~҉@~�@~~�@~q�@}�9@}�@|�P@|�E@|c�@{��@{'�@z�+@zTa@z($@z�@zO@zJ@y��@y�@y�@y�M@y5�@y�@x��@x�@x~(@x�@w��@wU�@v��@v�F@vd�@v-@u�@u#�@t�|@t��@t�U@t�u@t(�@s�6@sW?@s�@r��@r��@rh
@q�Z@q#�@p��@pu�@pbN@pD�@p/�@p'R@p!@o�@o�@o8@n3�@m�-@m4@l�I@l4n@kƨ@k�{@j�y@js�@jYK@j�@i��@i*0@i+@i�@hS�@g��@gC@f�,@f��@f�\@fTa@e��@e�T@e�#@e�^@e�@e0�@d֡@d�z@dK^@c��@cx@b��@b��@b=q@b_@a��@aJ�@`��@`oi@`!@_��@_y�@_a@_/�@_'�@_�@^�H@^��@^Ov@^)�@^!�@^�@]�C@]5�@\��@\�v@\�I@\|�@[�@Z��@Y��@Y��@YQ�@X�@W�@WK�@W
=@V�!@Vu%@V-@Uc�@T��@TG@S�q@S]�@R��@RL0@RO@Q��@Q��@Q^�@Qq@P�u@Pz�@Poi@PD�@P�@PG@O{J@N}V@N{@M�@M�C@Mc@MJ�@M \@L��@Ll"@LV�@L�@K��@K�@J�@J��@J?@I��@H�`@H��@H6@G��@G i@F��@Fs�@F4@E@E`B@D��@Dh�@D�@Cv`@B�y@B�}@B_�@Aԕ@A��@A�~@A*0@@]d@?�@?�@>��@>_�@>�@=ϫ@=4@<�P@<�@<��@<`�@<2�@<�@;�@:��@:kQ@:&�@9�X@9hs@9A @8�@8|�@8K^@8*�@8�@7�+@7�F@7n/@7F�@6��@6��@63�@5�@5��@5�^@5�@5}�@5J�@5+�@5%F@4�f@4��@4��@42�@3�@3��@3S@2��@2:*@1�9@1�X@1`B@1=�@1/@1(�@1�@1%@0ѷ@0��@0�@0"h@/�@.�@.�6@.� @._�@-��@-�@,��@,�@,j@,/�@, �@,x@+�W@+�Q@+��@+�4@+dZ@+P�@+33@*�@*v�@*?@*	@)8�@(�/@(��@(N�@(/�@(*�@(�@'��@'g�@'RT@'+@&ں@&\�@&@%��@%�@%zx@%o @%j@%O�@%;@$|�@#�&@#�K@#�@#�@#�[@#��@#��@#��@#��@#��@#��@#+@"�@"}V@"6�@!�-@!��@!�"@!|@!x�@!p�@!@ w�@ Q�@ H@ A�@ :�@ ~@�@��@��@��@X�@��@�x@($@�Z@�-@��@[W@N<@A @5�@/@%@�@�K@ی@��@�$@r�@S�@6@	�@�;@s@H�@F�@)_@
=@�y@��@_�@.�@��@ԕ@�@j@J�@IR@8�@�@�`@��@`�@/�@~@b@��@�@��@�:@U�@@O@1�@,�@+@C@͟@�\@~�@ff@:*@.�@{@��@p�@Y�@T�@c�@O�@ \@�@֡@�@��@�_@�o@2�@��@��@��@iD@W?@+@�}@~�@�Z@�S@^�@IR@!�@ی@�.@bN@C-@�@�m@˒@j�@U�@+@�@ i@�2@��@��@��@�r@i�@3�@�.@�)@�@p�@Vm@8�@!�@�f@�@�.@�o@~(@u�@H@�m@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��dA���A��A��`A���A���A��A��A��DA���A���A��A��A��A��"A��A���A��A��A���A���A���A��A�%FA�6�A�8RA�&�A��A�҉Aʱ�AʉA�S�A�&A��A��.A���A�ںAɹ�AɥA�{JA�h>A�՛A���A�B�AöA�ߤA��A��A�T�A�N<A�=�A�=A�ŢA��sA���A�\]A��jA�#�A���A���A�9$A��xA�PHA�aHA�H�A�NpA�V9A��A���A�#A�m�A�y	A�>wA�_�A�K�A�|A���A�xA�aHA��uA��;A�d�A�{A��zA�2�A�E�A�v`A��pA�YA�� A�.A���A��A~�A}{Az��AxS�Au"hAr�_AnB�Aj�AfA`�IAZVAWp�AQC�AE�AC��AB-AAB[A?tTA;ϫA7�zA6�	A5�kA2*�A0x�A.�A-�A->BA,�A,kQA,)_A+~�A*�?A)�OA'y�A%�A$Q�A$2�A#��A#rGA"�A!�A!$�AS�A��A \A�*A�+A�A�jA S�A g8A D�A�A�$AQ�A˒Ak�A\�AیA�FAE9A�A��A��AA�AYA�bA'�A�mA"�Ac�A��A�|A��A	�Ae,A�A[WA��AA4�A�A��A�HAV�A�pA`BAy�A�A
�XA
��A	�A	��A	W?A��A��APHA�VA>�A�A�vA��A2aA��AA�A��A9XA�=A�A��A��A�A �}A 7L@��@�Ĝ@��@��@�ی@��@�I�@���@��@��"@�s�@�Y@���@�+@��`@���@�?@�\)@���@��\@�<�@��>@��@�H�@�'@�|�@�Z�@���@�|�@�q@�@���@�7@�خ@�b�@���@�w�@��@�L@��@�2a@�}V@�+@��@�H�@�33@�S�@�u�@⤩@���@�}�@��	@�W�@ߨX@ޕ�@�7�@��@ݻ0@�=@��8@�c @�@�[W@ڭ�@�hs@؃�@���@�ƨ@�&@�j@�{@ճ�@�K�@��@�3�@�Y�@�͟@қ�@��@шf@мj@�"h@�V@ή}@ΔF@��&@�g8@��@�-@ɐ�@�5�@ȹ�@�h
@�>B@�G@���@Ƿ�@�~�@�Vm@���@�h�@�@ŀ4@�q@��@�M@ô�@Û=@��@���@���@���@��K@�<�@��@���@���@��u@�?@�!�@��7@�4@��@���@���@� i@��@��@�Q@�4@��@�Y�@�Mj@��	@���@�{�@�,=@��^@��4@�>�@�h
@��@��@���@���@�s�@��@�|�@�:�@��Z@��3@�{J@�#�@��@��L@�C�@��@�P�@��@��@��_@��@�V@��O@�u�@���@�\)@��@�bN@�T�@���@�l�@���@�n/@�E9@���@�s�@�8�@���@���@�t�@�Q�@���@���@���@�xl@�?�@� �@�
�@���@�RT@��@��"@�ߤ@�ی@���@���@�kQ@�M@��T@���@�/@�ی@��j@��+@���@�dZ@�*0@��	@�`�@��6@�a�@��<@�c�@���@��w@�@O@�a|@���@�_p@�Vm@�.I@�ی@���@�.�@�$@��@��.@��@�{J@�x@�c�@��@���@�Ov@���@���@��S@�_p@�+@��@�W�@�O@��@��f@��@��r@�-@��@�Y@��v@�Ft@���@��@��$@��4@�V�@��@�_p@��@�S@���@�ȴ@���@�Q�@�!�@�@���@���@�w2@�Mj@�(�@��@���@�K^@�خ@���@��	@�\)@�2a@�S@���@�}V@�=q@��@���@���@�zx@�"�@��b@�|�@�J�@��H@��p@���@�?�@�3�@�1'@��A@�'�@��@�>B@��@@�C@���@���@���@�h
@�-�@���@�A @�!�@�q@��@��@�@��]@���@��O@���@�7�@��@��g@���@��[@�o @�Q�@���@��m@�oi@�=q@�&�@�O@�_@�
@_p@~҉@~�@~~�@~q�@}�9@}�@|�P@|�E@|c�@{��@{'�@z�+@zTa@z($@z�@zO@zJ@y��@y�@y�@y�M@y5�@y�@x��@x�@x~(@x�@w��@wU�@v��@v�F@vd�@v-@u�@u#�@t�|@t��@t�U@t�u@t(�@s�6@sW?@s�@r��@r��@rh
@q�Z@q#�@p��@pu�@pbN@pD�@p/�@p'R@p!@o�@o�@o8@n3�@m�-@m4@l�I@l4n@kƨ@k�{@j�y@js�@jYK@j�@i��@i*0@i+@i�@hS�@g��@gC@f�,@f��@f�\@fTa@e��@e�T@e�#@e�^@e�@e0�@d֡@d�z@dK^@c��@cx@b��@b��@b=q@b_@a��@aJ�@`��@`oi@`!@_��@_y�@_a@_/�@_'�@_�@^�H@^��@^Ov@^)�@^!�@^�@]�C@]5�@\��@\�v@\�I@\|�@[�@Z��@Y��@Y��@YQ�@X�@W�@WK�@W
=@V�!@Vu%@V-@Uc�@T��@TG@S�q@S]�@R��@RL0@RO@Q��@Q��@Q^�@Qq@P�u@Pz�@Poi@PD�@P�@PG@O{J@N}V@N{@M�@M�C@Mc@MJ�@M \@L��@Ll"@LV�@L�@K��@K�@J�@J��@J?@I��@H�`@H��@H6@G��@G i@F��@Fs�@F4@E@E`B@D��@Dh�@D�@Cv`@B�y@B�}@B_�@Aԕ@A��@A�~@A*0@@]d@?�@?�@>��@>_�@>�@=ϫ@=4@<�P@<�@<��@<`�@<2�@<�@;�@:��@:kQ@:&�@9�X@9hs@9A @8�@8|�@8K^@8*�@8�@7�+@7�F@7n/@7F�@6��@6��@63�@5�@5��@5�^@5�@5}�@5J�@5+�@5%F@4�f@4��@4��@42�@3�@3��@3S@2��@2:*@1�9@1�X@1`B@1=�@1/@1(�@1�@1%@0ѷ@0��@0�@0"h@/�@.�@.�6@.� @._�@-��@-�@,��@,�@,j@,/�@, �@,x@+�W@+�Q@+��@+�4@+dZ@+P�@+33@*�@*v�@*?@*	@)8�@(�/@(��@(N�@(/�@(*�@(�@'��@'g�@'RT@'+@&ں@&\�@&@%��@%�@%zx@%o @%j@%O�@%;@$|�@#�&@#�K@#�@#�@#�[@#��@#��@#��@#��@#��@#��@#+@"�@"}V@"6�@!�-@!��@!�"@!|@!x�@!p�@!@ w�@ Q�@ H@ A�@ :�@ ~@�@��@��@��@X�@��@�x@($@�Z@�-@��@[W@N<@A @5�@/@%@�@�K@ی@��@�$@r�@S�@6@	�@�;@s@H�@F�@)_@
=@�y@��@_�@.�@��@ԕ@�@j@J�@IR@8�@�@�`@��@`�@/�@~@b@��@�@��@�:@U�@@O@1�@,�@+@C@͟@�\@~�@ff@:*@.�@{@��@p�@Y�@T�@c�@O�@ \@�@֡@�@��@�_@�o@2�@��@��@��@iD@W?@+@�}@~�@�Z@�S@^�@IR@!�@ی@�.@bN@C-@�@�m@˒@j�@U�@+@�@ i@�2@��@��@��@�r@i�@3�@�.@�)@�@p�@Vm@8�@!�@�f@�@�.@�o@~(@u�@H@�m@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	�fB	�2B	�B	��B	��B	��B	��B	��B	�B	�8B	�XB	��B	�0B	�PB	� B	�[B	�B	�MB	ڠB	�B
,�B
bB
�?B
��B
ңB
�B
͹B
̳B
�#B
��B
��B
�RB
ɆB
�	B
��B
�JB
��B
͹B
�B
��BH�Bs3B��B�kB��B��B�fB�}B�nB�B$B�B�BB!BVB�B"4B�B&B�"B��B�kB�,B�JB�BB�`B�B�B��B{�Bd�B]dB]�BFtB1�B �B�B
�B�B
��B
�B
�eB
��B
�kB
��B
�MB
z�B
X�B
1AB
�B
[B	�B	ݲB	��B	�6B	��B	n/B	OvB	/5B	B�VB��B՛B�bB��B�FBҽB��B�tB�oB��B��B�NB�]B�B�%B��B�*B��B�B	 iB	 B	+B	hB	yB	7B	�B	"�B	%FB	&2B	$�B	(sB	A�B	T�B	r|B	{JB	��B	�}B	�6B	��B	ևB	�7B	�;B	�4B	�tB	��B	��B	�B	�=B	�QB	��B	�B	��B	ބB	��B	�#B	�B	�sB	�?B	��B	�7B	چB	��B	��B	�B	߾B	��B	��B	̘B	�%B	��B	�)B	��B	�_B	ԕB	�hB	��B	��B	�QB	ٚB	�
B	�;B	�B	�&B	�B	��B	�`B	��B	�ZB	�ZB	�,B	�:B	�TB	�B	�ZB	�2B	�fB	��B	�,B	�B	�B	�hB	�\B	�]B	��B	�YB	��B	�B	��B	�RB	�B	�B	�B	�B	�mB	�mB	�mB	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�$B	�
B	�B	�B	��B	�B	��B	�8B	�B	��B	�fB	�B	��B	�B	�B	�B	�B	�B	�fB	�RB	�B	�B	�8B	�B	�mB	�B	�
B	�B	��B	�DB	�B	�eB	��B	�B	�B	��B	�]B	�iB	��B	�B	�B	�oB	��B	��B	�oB	�UB	�B	��B	�B	�B	�iB	�!B	�oB	�;B	�;B	�!B	�!B	��B	�B	��B	�B	� B	�5B	��B	�B	�B	�AB	�B	��B	��B	�B	�-B	�-B	�GB	�GB	�-B	�|B	�B	��B	�MB	�hB	�MB	�B	�B	�9B	�tB	�+B	��B	��B	�8B	��B	�B	�B	�lB	��B	��B	�B	�B	�wB	��B	�]B	��B
  B
 OB
 �B
[B
AB
�B
B
 �B
 B
UB
�B
uB
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
GB
GB
-B
GB
B
GB
{B
aB
�B
�B
B
gB
3B
3B
�B
�B
SB
SB
�B
%B
YB
�B
�B
�B
�B
�B
�B
1B
�B
	B
	RB
	�B
	�B
	�B

#B

rB

�B
DB
xB
xB
xB
)B
0B
~B
�B
�B
�B
�B
�B
�B
B
jB
B
�B
B
(B
BB
(B
B
HB
�B
B
�B
NB
�B
�B
�B
�B
[B
�B
B
B
�B
@B
[B
�B
aB
�B
�B
B
�B
�B
�B
�B
�B
KB
B
7B
�B
�B
�B
#B
WB
�B
CB
)B
]B
�B
B
�B
�B
�B
�B
VB
 \B
!B
"�B
"B
"B
"NB
#:B
#�B
#�B
#nB
#�B
#�B
#�B
$tB
$�B
$�B
$�B
%B
%FB
%`B
%zB
%�B
%�B
&B
&�B
&�B
&�B
&�B
&�B
'B
'mB
'�B
'�B
(
B
(XB
(sB
(sB
(�B
)_B
)*B
)DB
)�B
+6B
+kB
+�B
+�B
+�B
+�B
-B
-wB
.cB
/OB
0B
0B
0oB
0�B
0�B
1B
2|B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3MB
3�B
3�B
3�B
3�B
4B
4B
4�B
4�B
5?B
5�B
5�B
5�B
6B
6+B
6�B
7B
72B
72B
7B
7�B
88B
8RB
8lB
8�B
8�B
8�B
9>B
9rB
9rB
9rB
9rB
9�B
9�B
9�B
9�B
9�B
9�B
:B
:DB
:B
:B
:xB
:�B
:�B
:�B
:�B
;B
;0B
;�B
;�B
;�B
;�B
;�B
;�B
<B
<PB
<�B
=B
="B
=<B
="B
=qB
>B
>BB
>BB
>BB
>wB
>]B
>]B
>]B
>BB
>]B
>�B
?HB
?�B
?�B
@B
@4B
@iB
@iB
AUB
A;B
AB
A;B
A�B
A�B
A�B
A�B
BB
BuB
B�B
B�B
B�B
B�B
CB
C-B
C-B
C-B
CGB
C{B
C�B
C�B
C�B
C�B
DB
D3B
D�B
D�B
ESB
EB
EB
E�B
E�B
FB
FYB
FtB
F�B
F�B
GB
F�B
GB
G+B
G+B
G�B
G�B
G�B
G�B
H1B
H�B
H�B
H�B
H�B
HKB
IRB
I�B
J=B
JXB
JXB
J�B
KxB
K�B
K�B
K�B
K�B
K�B
L~B
L�B
MB
L�B
L�B
MjB
M�B
M�B
M�B
M�B
N"B
N<B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
PB
PB
P.B
P.B
PHB
P�B
P�B
P�B
P�B
QNB
Q�B
Q�B
Q�B
Q�B
RoB
R�B
R�B
S&B
S�B
TFB
TaB
T�B
UMB
UgB
U�B
VB
V�B
V�B
V�B
W?B
WYB
WYB
WsB
XB
XEB
X�B
Y1B
Y1B
Y�B
ZQB
Z�B
Z�B
[	B
[qB
[�B
[�B
[�B
\B
\B
\B
\�B
]/B
]IB
]dB
]�B
^B
]�B
^OB
^�B
^�B
^�B
^�B
_B
_;B
_pB
_pB
_�B
_�B
`\B
`vB
`\B
`vB
`vB
`�B
`�B
`�B
`�B
`�B
a-B
a-B
a�B
a�B
a�B
bhB
b�B
c B
cTB
c�B
c�B
dB
dB
dB
d&B
d&B
dZB
dZB
dZB
d�B
d�B
e�B
ezB
ezB
ezB
e�B
f2B
f�B
gmB
g�B
g�B
g�B
g�B
g�B
h
B
h$B
hXB
hXB
hXB
h>B
h�B
h�B
iB
iB
j0B
jeB
j�B
j�B
j�B
j�B
j�B
k�B
kkB
kkB
kkB
k�B
k�B
l"B
l=B
lWB
lWB
lWB
lWB
lWB
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
ncB
ncB
o B
o5B
o�B
o�B
o�B
o�B
o�B
o�B
p;B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q'B
q�B
q�B
r-B
rB
rGB
rGB
r|B
r|B
r|B
r|B
r|B
r�B
sMB
s�B
s�B
s�B
s�B
tB
tB
t9B
t9B
t9B
t9B
t9B
t�B
t�B
t�B
t�B
t�B
uB
u?B
u?B
u%B
uB
u�B
u�B
utB
u�B
u�B
u�B
u�B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w2B
wfB
w�B
wfB
wLB
wLB
w�B
w�B
w�B
xB
xRB
x8B
xRB
xlB
x�B
x�B
y�B
zB
zDB
z�B
z�B
{JB
{�B
{B
{�B
|B
|�B
}"B
}<B
}�B
}�B
}�B
}�B
}VB
}�B
~(B
~�B
~�B
~wB
~�B
~�B
~�B
B
B
B
B
~�B
~�B
�B
�B
� B
�B
��B
��B
�B
��B
��B
��B
��B
�B
�;B
��B
��B
��B
��B
�B
�[B
�B
�GB
�{B
�aB
�{B
�{B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	�fB	�2B	�B	��B	��B	��B	��B	��B	�B	�8B	�XB	��B	�0B	�PB	� B	�[B	�B	�MB	ڠB	�B
,�B
bB
�?B
��B
ңB
�B
͹B
̳B
�#B
��B
��B
�RB
ɆB
�	B
��B
�JB
��B
͹B
�B
��BH�Bs3B��B�kB��B��B�fB�}B�nB�B$B�B�BB!BVB�B"4B�B&B�"B��B�kB�,B�JB�BB�`B�B�B��B{�Bd�B]dB]�BFtB1�B �B�B
�B�B
��B
�B
�eB
��B
�kB
��B
�MB
z�B
X�B
1AB
�B
[B	�B	ݲB	��B	�6B	��B	n/B	OvB	/5B	B�VB��B՛B�bB��B�FBҽB��B�tB�oB��B��B�NB�]B�B�%B��B�*B��B�B	 iB	 B	+B	hB	yB	7B	�B	"�B	%FB	&2B	$�B	(sB	A�B	T�B	r|B	{JB	��B	�}B	�6B	��B	ևB	�7B	�;B	�4B	�tB	��B	��B	�B	�=B	�QB	��B	�B	��B	ބB	��B	�#B	�B	�sB	�?B	��B	�7B	چB	��B	��B	�B	߾B	��B	��B	̘B	�%B	��B	�)B	��B	�_B	ԕB	�hB	��B	��B	�QB	ٚB	�
B	�;B	�B	�&B	�B	��B	�`B	��B	�ZB	�ZB	�,B	�:B	�TB	�B	�ZB	�2B	�fB	��B	�,B	�B	�B	�hB	�\B	�]B	��B	�YB	��B	�B	��B	�RB	�B	�B	�B	�B	�mB	�mB	�mB	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�$B	�
B	�B	�B	��B	�B	��B	�8B	�B	��B	�fB	�B	��B	�B	�B	�B	�B	�B	�fB	�RB	�B	�B	�8B	�B	�mB	�B	�
B	�B	��B	�DB	�B	�eB	��B	�B	�B	��B	�]B	�iB	��B	�B	�B	�oB	��B	��B	�oB	�UB	�B	��B	�B	�B	�iB	�!B	�oB	�;B	�;B	�!B	�!B	��B	�B	��B	�B	� B	�5B	��B	�B	�B	�AB	�B	��B	��B	�B	�-B	�-B	�GB	�GB	�-B	�|B	�B	��B	�MB	�hB	�MB	�B	�B	�9B	�tB	�+B	��B	��B	�8B	��B	�B	�B	�lB	��B	��B	�B	�B	�wB	��B	�]B	��B
  B
 OB
 �B
[B
AB
�B
B
 �B
 B
UB
�B
uB
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
GB
GB
-B
GB
B
GB
{B
aB
�B
�B
B
gB
3B
3B
�B
�B
SB
SB
�B
%B
YB
�B
�B
�B
�B
�B
�B
1B
�B
	B
	RB
	�B
	�B
	�B

#B

rB

�B
DB
xB
xB
xB
)B
0B
~B
�B
�B
�B
�B
�B
�B
B
jB
B
�B
B
(B
BB
(B
B
HB
�B
B
�B
NB
�B
�B
�B
�B
[B
�B
B
B
�B
@B
[B
�B
aB
�B
�B
B
�B
�B
�B
�B
�B
KB
B
7B
�B
�B
�B
#B
WB
�B
CB
)B
]B
�B
B
�B
�B
�B
�B
VB
 \B
!B
"�B
"B
"B
"NB
#:B
#�B
#�B
#nB
#�B
#�B
#�B
$tB
$�B
$�B
$�B
%B
%FB
%`B
%zB
%�B
%�B
&B
&�B
&�B
&�B
&�B
&�B
'B
'mB
'�B
'�B
(
B
(XB
(sB
(sB
(�B
)_B
)*B
)DB
)�B
+6B
+kB
+�B
+�B
+�B
+�B
-B
-wB
.cB
/OB
0B
0B
0oB
0�B
0�B
1B
2|B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3MB
3�B
3�B
3�B
3�B
4B
4B
4�B
4�B
5?B
5�B
5�B
5�B
6B
6+B
6�B
7B
72B
72B
7B
7�B
88B
8RB
8lB
8�B
8�B
8�B
9>B
9rB
9rB
9rB
9rB
9�B
9�B
9�B
9�B
9�B
9�B
:B
:DB
:B
:B
:xB
:�B
:�B
:�B
:�B
;B
;0B
;�B
;�B
;�B
;�B
;�B
;�B
<B
<PB
<�B
=B
="B
=<B
="B
=qB
>B
>BB
>BB
>BB
>wB
>]B
>]B
>]B
>BB
>]B
>�B
?HB
?�B
?�B
@B
@4B
@iB
@iB
AUB
A;B
AB
A;B
A�B
A�B
A�B
A�B
BB
BuB
B�B
B�B
B�B
B�B
CB
C-B
C-B
C-B
CGB
C{B
C�B
C�B
C�B
C�B
DB
D3B
D�B
D�B
ESB
EB
EB
E�B
E�B
FB
FYB
FtB
F�B
F�B
GB
F�B
GB
G+B
G+B
G�B
G�B
G�B
G�B
H1B
H�B
H�B
H�B
H�B
HKB
IRB
I�B
J=B
JXB
JXB
J�B
KxB
K�B
K�B
K�B
K�B
K�B
L~B
L�B
MB
L�B
L�B
MjB
M�B
M�B
M�B
M�B
N"B
N<B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
PB
PB
P.B
P.B
PHB
P�B
P�B
P�B
P�B
QNB
Q�B
Q�B
Q�B
Q�B
RoB
R�B
R�B
S&B
S�B
TFB
TaB
T�B
UMB
UgB
U�B
VB
V�B
V�B
V�B
W?B
WYB
WYB
WsB
XB
XEB
X�B
Y1B
Y1B
Y�B
ZQB
Z�B
Z�B
[	B
[qB
[�B
[�B
[�B
\B
\B
\B
\�B
]/B
]IB
]dB
]�B
^B
]�B
^OB
^�B
^�B
^�B
^�B
_B
_;B
_pB
_pB
_�B
_�B
`\B
`vB
`\B
`vB
`vB
`�B
`�B
`�B
`�B
`�B
a-B
a-B
a�B
a�B
a�B
bhB
b�B
c B
cTB
c�B
c�B
dB
dB
dB
d&B
d&B
dZB
dZB
dZB
d�B
d�B
e�B
ezB
ezB
ezB
e�B
f2B
f�B
gmB
g�B
g�B
g�B
g�B
g�B
h
B
h$B
hXB
hXB
hXB
h>B
h�B
h�B
iB
iB
j0B
jeB
j�B
j�B
j�B
j�B
j�B
k�B
kkB
kkB
kkB
k�B
k�B
l"B
l=B
lWB
lWB
lWB
lWB
lWB
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
ncB
ncB
o B
o5B
o�B
o�B
o�B
o�B
o�B
o�B
p;B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q'B
q�B
q�B
r-B
rB
rGB
rGB
r|B
r|B
r|B
r|B
r|B
r�B
sMB
s�B
s�B
s�B
s�B
tB
tB
t9B
t9B
t9B
t9B
t9B
t�B
t�B
t�B
t�B
t�B
uB
u?B
u?B
u%B
uB
u�B
u�B
utB
u�B
u�B
u�B
u�B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w2B
wfB
w�B
wfB
wLB
wLB
w�B
w�B
w�B
xB
xRB
x8B
xRB
xlB
x�B
x�B
y�B
zB
zDB
z�B
z�B
{JB
{�B
{B
{�B
|B
|�B
}"B
}<B
}�B
}�B
}�B
}�B
}VB
}�B
~(B
~�B
~�B
~wB
~�B
~�B
~�B
B
B
B
B
~�B
~�B
�B
�B
� B
�B
��B
��B
�B
��B
��B
��B
��B
�B
�;B
��B
��B
��B
��B
�B
�[B
�B
�GB
�{B
�aB
�{B
�{B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104843  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172301  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172301  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172301                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022309  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022309  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610121506                      G�O�G�O�G�O�                