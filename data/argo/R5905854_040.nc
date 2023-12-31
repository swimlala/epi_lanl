CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:51:42Z creation;2022-06-04T17:51:43Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604175142  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               (A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�	նlx1   @�	Q���@0z��vȴ�cZM���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH��BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�33B�  B�  B�33B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C;�fC=�fC@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  CzL�C{�fC}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D¼�D���D�@ DÀ D�� D�  D�@ D�|�D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�3D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@���@���A z�A z�A@z�A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BW�RB`�Bh�Bp�Bx�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�u�B��)B�\B�\B�B�B�\B�\B�B�B�\B��)B�\B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C!HC�C�C�C�C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:!HC;�C=�C@�CB�CD�CF�CH�CJ�CL�CN�CO�CR�CT�CV�CW�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�CzT{C{�C}�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��
C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�RD�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw�RDxRDx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D½�D���D�@�DÀ�D���D� �D�@�D�}�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D�)D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�0UA�1�A�,�A�!bA��A��rA��jAļ�Aĩ�AĤ@AĢ�Ağ�AĞ�Aĝ~AĕMAĒ�Aď�Aď\AĎ�AēAđ4AčPAċ�AČ~Ač�Aď(Aē�A�MA���A�ݘA��A��A��+A��A�	�A��A�#A�N<A�^jA�d�A�kA�_pA�B[A�"�A� 'A�)_A��A��>A��iA���A�a�A�x�A���A��EA��A� �A�6A�z�A�[#A�&�A��A���A�OA��A�p�A��YA��PA���A��CA�h
A��IA�4nA�~(A��A�.A�(�A��uA���A�V�A�Q�A�#A�_A��xA���A��#A��A�V�A��A��PA��A�wfA���A�0�A�{A���A���A~:�Ayw2Au~�AsL�AoZ�Aj�AAh	Act�A[g8AZjAZAX�	ARy�AO	�AM�eAKѷAI��AI/�AH��AH,�AG+�AF1AD�"AC�AC�AB�A@�:A?`BA>�A=ZA:��A9{JA9_A8��A7�HA7v�A6ĜA4H�A1�'A/�<A/�A0PHA0ĜA0��A/�A/8�A.(�A*��A(��A'�mA'�A'SA&cA%\�A%A$cA$>BA$uA#ĜA#u%A#I�A#9XA#-A#CA"��A"�+A"1A �sA�AA�A?}A>BA�A�fA iA~A��A�AJ�A��A^5A�~A�bAzxA�dA��A�`A�A��A#�A�cAݘA�kA��A6�A�A�-AI�A�]A�DAA�A�0A,�A�A�A��A_pA
�A
��A	�A	�HA	�
A	K�A��A��AT�A��A�XA6�A�`A�IA��AQ�A=A&�AݘA��A[�A�A8�A�NA��A%FA�yAW�A0�AA �@��@���@�rG@���@���@���@�Z@��0@�q@�W?@��v@�~(@���@���@��"@���@��@�,�@���@�b@��Z@�s�@��`@�0U@�S@�e�@� i@��@�Mj@��P@��@��m@�^�@�@�,�@�_@��d@�z�@���@�9@��@��@�y>@�B[@�$@߅�@޸R@�-�@ݼ@�RT@�1'@�8�@��/@ڰ!@�B[@��@�($@ٕ�@�O@�@���@�r�@��@���@�j@�8�@� �@��>@��d@�=�@�.I@�+�@ԍ�@���@Ә�@�x�@�4�@�`�@�C�@Е�@�l"@�m�@��o@�)_@�-�@��@��@́@�o@̿�@��@���@�N<@ʌ@�!�@��.@���@���@��c@�d�@��@��@��@Ɵ�@ƣ�@��@��z@� \@��@�zx@Ÿ�@ŝ�@�s@�o@ĳh@��W@�(@��	@½<@�V�@�2�@�0U@�{@��@���@�s�@�Mj@��K@�u%@���@�0�@��[@�%�@���@��F@�zx@�Mj@�>�@�q@��9@��@�v`@��j@�q@��T@�4@���@���@�;�@��@��+@���@�&�@�=q@��a@�U�@���@�($@��T@��a@��@�]�@�6z@��P@��[@���@�ff@�$�@��@�@�@�-w@�_@��N@���@��@��[@���@���@�z@�C-@��Q@��X@��@��^@��k@���@�_p@�F�@��@���@��@�M@��@��@�Dg@���@�N�@�"h@��@���@�Y@��)@��@�e@���@���@�\�@���@�}�@�f�@�]�@�O�@�K�@�F�@�9�@��@��B@�y>@�U2@�1'@��W@���@��?@�{@��@�@��r@�=@���@��x@�I�@�M@��@��)@���@�=�@��x@�H�@��&@��@� \@�V@��@�@��P@�ں@�~(@��@��Q@��z@���@���@�w2@�!-@��A@�+k@��@�^�@�ی@��o@���@���@�X�@�"�@���@���@��P@���@��@�,=@��>@��Q@��^@�(�@��E@�xl@�/�@���@���@�c�@��@�~(@�I�@�/�@���@�IR@��@�g8@�	@��@��"@�S�@�+�@��@���@�H@�<�@�*�@���@���@�j@��O@���@�~(@�`�@�K^@�%�@���@���@�qv@�G�@�;d@�/@��@���@���@���@���@���@��Y@��A@�Z�@���@�\�@�%F@��c@��R@�]d@��H@�|�@�dZ@�8�@�%F@���@�H@��]@��&@�ƨ@��S@�J�@��@���@�y>@�=q@�� @�L�@�:�@�Y@�S@���@�z�@�~@��@���@��P@�a�@�0�@��@�z�@��@��@��@y�@\)@9�@~�b@}@}%@|�)@|�4@|g8@{��@{Z�@{)_@z�]@yԕ@x`�@w�a@wj�@v�c@v�<@v��@v;�@v{@v
�@u��@t�@t*�@s�K@s��@s�[@s��@sn/@s1�@r�L@rJ@q�@pI�@p �@p1@oƨ@o�[@o��@oiD@n�@nV@m�.@l��@kt�@kH�@k�@j�M@jߤ@j��@j�\@jL0@i�@i�@h��@hh�@h'R@g��@gS�@g i@f{�@e�T@e<6@dq@d�@c�6@c��@cS�@b�2@b��@b.�@a�^@aG�@`ی@`�@`m�@`"h@_� @_dZ@_�@^͟@^��@^Z�@^B[@]�@]Dg@\��@\N�@[��@[1�@Z��@ZE�@Y�H@Y��@YrG@YY�@Y�@X��@X��@Xq@XPH@W�Q@Wy�@V��@Vu%@V8�@V)�@Uԕ@U��@U-w@T�)@TN�@S�Q@S$t@R� @R_@Q��@Q}�@Q�@P�@Pѷ@P��@P�@P�u@O��@O�@O��@O$t@N��@N�@N�L@Np;@N0U@M��@M`B@M�@L��@L_@L�@K�@Kخ@K�0@K�f@K9�@K�@K i@Jߤ@J��@J	@Ix�@I7L@H��@H�?@H��@Hw�@HXy@H!@G� @Gn/@G6z@F�"@F�@F8�@E�z@E��@EG�@E4@E&�@E@D��@D�e@Dw�@C�6@CP�@CA�@C"�@B�'@B��@Bh
@B+k@A��@A�^@A^�@@�u@?�r@?��@?dZ@>�8@>��@>��@>��@>i�@>u@=@=��@=�-@=c�@<�	@<��@<�O@<��@<��@<��@<�D@;�]@;s@; i@:d�@:4@9�d@9a�@9[W@9O�@98�@9*0@9�@8��@8<�@7��@7�[@7l�@7"�@6�@6��@6�@5k�@4��@4m�@4*�@4@3��@3�&@3�[@3�{@3dZ@3
=@2:*@1��@1@0��@0�@01@/�W@/خ@/��@/��@/�4@/C@.�,@.��@.s�@.\�@.C�@.�@-��@-��@-}�@-e,@-q@,��@,tT@,2�@,�@+��@+y�@+y�@+W?@+�@*�H@*��@*5?@*u@)�@)�#@)�H@)�C@)e,@)0�@)@(�@(�j@(�@(bN@'�a@'~�@'P�@&��@&�,@&��@&0U@%�@%k�@%O�@%Dg@%�@$��@$��@$��@$u�@$A�@#�F@#�k@#�P@#�f@#o�@#J#@#$t@#@"��@"��@"��@"E�@"O@!�@!��@!e,@!<6@!�@ �9@ �o@ h�@ I�@ 2�@ �@�g@��@��@��@e�@_p@_p@>�@�c@��@c @W�@YK@^5@C�@�@=�@!�@�|@֡@��@��@c�@`�@_@��@4�@��@kQ@Ov@H�@O@��@�z@�-@��@j@N<@<6@��@�@tT@/�@�:@F�@
=@�8@�@��@��@a|@:*@��@�t@�S@}�@J�@&�@��@�j@�@K^@4n@�@�@�
@˒@�*@�P@4�@��@��@\�@@�>@�9@�~@Y�@*0@+@�@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�0UA�1�A�,�A�!bA��A��rA��jAļ�Aĩ�AĤ@AĢ�Ağ�AĞ�Aĝ~AĕMAĒ�Aď�Aď\AĎ�AēAđ4AčPAċ�AČ~Ač�Aď(Aē�A�MA���A�ݘA��A��A��+A��A�	�A��A�#A�N<A�^jA�d�A�kA�_pA�B[A�"�A� 'A�)_A��A��>A��iA���A�a�A�x�A���A��EA��A� �A�6A�z�A�[#A�&�A��A���A�OA��A�p�A��YA��PA���A��CA�h
A��IA�4nA�~(A��A�.A�(�A��uA���A�V�A�Q�A�#A�_A��xA���A��#A��A�V�A��A��PA��A�wfA���A�0�A�{A���A���A~:�Ayw2Au~�AsL�AoZ�Aj�AAh	Act�A[g8AZjAZAX�	ARy�AO	�AM�eAKѷAI��AI/�AH��AH,�AG+�AF1AD�"AC�AC�AB�A@�:A?`BA>�A=ZA:��A9{JA9_A8��A7�HA7v�A6ĜA4H�A1�'A/�<A/�A0PHA0ĜA0��A/�A/8�A.(�A*��A(��A'�mA'�A'SA&cA%\�A%A$cA$>BA$uA#ĜA#u%A#I�A#9XA#-A#CA"��A"�+A"1A �sA�AA�A?}A>BA�A�fA iA~A��A�AJ�A��A^5A�~A�bAzxA�dA��A�`A�A��A#�A�cAݘA�kA��A6�A�A�-AI�A�]A�DAA�A�0A,�A�A�A��A_pA
�A
��A	�A	�HA	�
A	K�A��A��AT�A��A�XA6�A�`A�IA��AQ�A=A&�AݘA��A[�A�A8�A�NA��A%FA�yAW�A0�AA �@��@���@�rG@���@���@���@�Z@��0@�q@�W?@��v@�~(@���@���@��"@���@��@�,�@���@�b@��Z@�s�@��`@�0U@�S@�e�@� i@��@�Mj@��P@��@��m@�^�@�@�,�@�_@��d@�z�@���@�9@��@��@�y>@�B[@�$@߅�@޸R@�-�@ݼ@�RT@�1'@�8�@��/@ڰ!@�B[@��@�($@ٕ�@�O@�@���@�r�@��@���@�j@�8�@� �@��>@��d@�=�@�.I@�+�@ԍ�@���@Ә�@�x�@�4�@�`�@�C�@Е�@�l"@�m�@��o@�)_@�-�@��@��@́@�o@̿�@��@���@�N<@ʌ@�!�@��.@���@���@��c@�d�@��@��@��@Ɵ�@ƣ�@��@��z@� \@��@�zx@Ÿ�@ŝ�@�s@�o@ĳh@��W@�(@��	@½<@�V�@�2�@�0U@�{@��@���@�s�@�Mj@��K@�u%@���@�0�@��[@�%�@���@��F@�zx@�Mj@�>�@�q@��9@��@�v`@��j@�q@��T@�4@���@���@�;�@��@��+@���@�&�@�=q@��a@�U�@���@�($@��T@��a@��@�]�@�6z@��P@��[@���@�ff@�$�@��@�@�@�-w@�_@��N@���@��@��[@���@���@�z@�C-@��Q@��X@��@��^@��k@���@�_p@�F�@��@���@��@�M@��@��@�Dg@���@�N�@�"h@��@���@�Y@��)@��@�e@���@���@�\�@���@�}�@�f�@�]�@�O�@�K�@�F�@�9�@��@��B@�y>@�U2@�1'@��W@���@��?@�{@��@�@��r@�=@���@��x@�I�@�M@��@��)@���@�=�@��x@�H�@��&@��@� \@�V@��@�@��P@�ں@�~(@��@��Q@��z@���@���@�w2@�!-@��A@�+k@��@�^�@�ی@��o@���@���@�X�@�"�@���@���@��P@���@��@�,=@��>@��Q@��^@�(�@��E@�xl@�/�@���@���@�c�@��@�~(@�I�@�/�@���@�IR@��@�g8@�	@��@��"@�S�@�+�@��@���@�H@�<�@�*�@���@���@�j@��O@���@�~(@�`�@�K^@�%�@���@���@�qv@�G�@�;d@�/@��@���@���@���@���@���@��Y@��A@�Z�@���@�\�@�%F@��c@��R@�]d@��H@�|�@�dZ@�8�@�%F@���@�H@��]@��&@�ƨ@��S@�J�@��@���@�y>@�=q@�� @�L�@�:�@�Y@�S@���@�z�@�~@��@���@��P@�a�@�0�@��@�z�@��@��@��@y�@\)@9�@~�b@}@}%@|�)@|�4@|g8@{��@{Z�@{)_@z�]@yԕ@x`�@w�a@wj�@v�c@v�<@v��@v;�@v{@v
�@u��@t�@t*�@s�K@s��@s�[@s��@sn/@s1�@r�L@rJ@q�@pI�@p �@p1@oƨ@o�[@o��@oiD@n�@nV@m�.@l��@kt�@kH�@k�@j�M@jߤ@j��@j�\@jL0@i�@i�@h��@hh�@h'R@g��@gS�@g i@f{�@e�T@e<6@dq@d�@c�6@c��@cS�@b�2@b��@b.�@a�^@aG�@`ی@`�@`m�@`"h@_� @_dZ@_�@^͟@^��@^Z�@^B[@]�@]Dg@\��@\N�@[��@[1�@Z��@ZE�@Y�H@Y��@YrG@YY�@Y�@X��@X��@Xq@XPH@W�Q@Wy�@V��@Vu%@V8�@V)�@Uԕ@U��@U-w@T�)@TN�@S�Q@S$t@R� @R_@Q��@Q}�@Q�@P�@Pѷ@P��@P�@P�u@O��@O�@O��@O$t@N��@N�@N�L@Np;@N0U@M��@M`B@M�@L��@L_@L�@K�@Kخ@K�0@K�f@K9�@K�@K i@Jߤ@J��@J	@Ix�@I7L@H��@H�?@H��@Hw�@HXy@H!@G� @Gn/@G6z@F�"@F�@F8�@E�z@E��@EG�@E4@E&�@E@D��@D�e@Dw�@C�6@CP�@CA�@C"�@B�'@B��@Bh
@B+k@A��@A�^@A^�@@�u@?�r@?��@?dZ@>�8@>��@>��@>��@>i�@>u@=@=��@=�-@=c�@<�	@<��@<�O@<��@<��@<��@<�D@;�]@;s@; i@:d�@:4@9�d@9a�@9[W@9O�@98�@9*0@9�@8��@8<�@7��@7�[@7l�@7"�@6�@6��@6�@5k�@4��@4m�@4*�@4@3��@3�&@3�[@3�{@3dZ@3
=@2:*@1��@1@0��@0�@01@/�W@/خ@/��@/��@/�4@/C@.�,@.��@.s�@.\�@.C�@.�@-��@-��@-}�@-e,@-q@,��@,tT@,2�@,�@+��@+y�@+y�@+W?@+�@*�H@*��@*5?@*u@)�@)�#@)�H@)�C@)e,@)0�@)@(�@(�j@(�@(bN@'�a@'~�@'P�@&��@&�,@&��@&0U@%�@%k�@%O�@%Dg@%�@$��@$��@$��@$u�@$A�@#�F@#�k@#�P@#�f@#o�@#J#@#$t@#@"��@"��@"��@"E�@"O@!�@!��@!e,@!<6@!�@ �9@ �o@ h�@ I�@ 2�@ �@�g@��@��@��@e�@_p@_p@>�@�c@��@c @W�@YK@^5@C�@�@=�@!�@�|@֡@��@��@c�@`�@_@��@4�@��@kQ@Ov@H�@O@��@�z@�-@��@j@N<@<6@��@�@tT@/�@�:@F�@
=@�8@�@��@��@a|@:*@��@�t@�S@}�@J�@&�@��@�j@�@K^@4n@�@�@�
@˒@�*@�P@4�@��@��@\�@@�>@�9@�~@Y�@*0@+@�@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�*B��B�>B�$B�B�LB��B�B��B��B��B��B�3B�3B�3B�3B�hB�B�B�%B��B��B�zB��B�	B��B	�B	gB	�B	��B	��B	�$B	��B	��B	��B	��B	�	B	�gB	��B	�B	�.B	� B	�\B	�6B	��B	�?B	��B	��B
?B
_�B
p�B
�ZB�BA�Bd�Bp!B~�B�RB��B��B�rB��B��B�~B�B�BDBB�B�B�B�BYBB��B�B��B�B��B�B׍BɺB�gB�qB�pB�.By�B_�B;�B+B
�B
�"B
xB
%`B
�B
3B	�qB	ɠB	��B	�B	{�B	YeB	GB	,�B	�B	{B	�B��B��B��B��B�yB�B�B�5B�WB�_B��B�hB�B��B�B�YB�[B��B��B�3B�.B�B�YB�=B�kB�B		B�}B�B	�B	3MB	`�B	~�B	��B	�-B	�B	��B	|jB	|�B	�B	|B	z�B	x�B	{�B	�B	�;B	�uB	��B	�B	��B	��B	�"B	��B	�\B	��B	�vB	�\B	�DB	��B	��B	�dB	�B	�oB	��B	��B	��B	��B	�zB	��B	��B	��B	�xB	�jB	��B	��B	�xB	�<B	��B	�dB	ϑB	�NB	��B	ҽB	ԯB	�,B	�B	�B	��B	��B	��B	�YB	�+B	�eB	��B	ބB	��B	�'B	�B	�B	��B	�B	�B	�\B	��B	��B	��B	�vB	�B	��B	�B	��B	�bB	��B	�-B	��B	��B	ߤB	�vB	�qB	ٴB	�kB	�B	�B	�B	��B	�EB	��B	��B	�
B	�B	ںB	�WB	��B	�#B	ܬB	�)B	ٴB	ؓB	��B	�sB	׍B	֡B	�B	�yB	ּB	��B	�9B	ּB	֡B	��B	��B	�mB	�B	�SB	ּB	�mB	�B	�B	�SB	ևB	��B	�sB	�?B	�$B	��B	خB	�?B	׍B	�B	�EB	�+B	��B	�EB	��B	�B	ۦB	�qB	�)B	��B	��B	�)B	��B	�B	�;B	�|B	�4B	�B	�B	��B	�B	�|B	�\B	߾B	��B	�hB	�B	�zB	�zB	�B	�`B	�B	�B	�B	�nB	�B	��B	�mB	�B	��B	��B	�/B	��B	�B	��B	�B	��B	��B	�OB	��B	�B	� B	��B	�/B	��B	�oB	�AB	��B	�5B	�B	��B	�-B	�B	�GB	�[B	��B	�B	��B	��B	��B	�	B	��B	��B	�B	��B	��B	�zB	�lB	�8B	�$B	��B	�B	��B	��B	��B	�0B	��B	��B	��B	�B	�lB	�*B	��B	��B	�0B	�B	��B	��B	�RB	�B	��B	�B	��B	��B	�+B	�B	�B	��B	��B	��B	�B	��B	�qB	�B	�B	�B	�?B	��B	�FB	�zB	��B	�B	��B	��B	�fB	�	B	��B	�VB	��B	�DB	�B	��B	��B	��B	�>B	��B	�0B	��B	��B	�B	��B
�B
�B
�B
MB
�B
MB
�B
�B
mB
SB
B
�B
9B
SB
mB
SB
�B
�B
9B
9B
�B
SB
9B
�B
SB
�B
�B
�B
�B
�B
B
�B
�B
�B
	B
	7B
	�B

	B

XB
	�B
	7B

�B
xB
~B
�B
�B
�B
(B
.B
HB
�B
�B
�B
oB
 B
 B
NB
NB
�B
B
uB
FB
�B
�B
B
gB
�B
yB
�B
�B
�B
sB
9B
�B
gB
�B
�B
�B
�B
�B
uB
�B
aB
�B
SB
�B
�B
�B
�B
�B
�B
�B
�B

B

B
sB
_B
_B
�B
YB
$B
?B
�B
yB
�B
KB
1B
�B
QB
7B
�B
B
xB
]B
�B
B
~B
B
�B
B
VB
�B
�B
 BB
 �B
!B
!�B
!�B
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
"hB
"�B
"�B
"�B
"�B
"�B
#�B
%B
%�B
%�B
&�B
'�B
'�B
(�B
(
B
(�B
)�B
*eB
*�B
+B
+�B
,WB
,"B
-CB
.B
-�B
-�B
-�B
-�B
.}B
.�B
/�B
/�B
0�B
1'B
1vB
1�B
1�B
2-B
1�B
2B
2GB
2B
2B
2|B
3MB
3�B
4B
49B
49B
4�B
4nB
4nB
4nB
5?B
5�B
6`B
6�B
6�B
6�B
7B
7fB
7�B
7fB
7�B
8B
8RB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9>B
:*B
:�B
:�B
:�B
;B
:�B
:�B
:�B
<B
;�B
;�B
=<B
=�B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
>(B
>wB
>�B
>�B
>�B
?HB
?HB
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A B
A B
AUB
AoB
A�B
BB
A�B
B'B
BuB
B�B
B�B
CB
CB
CaB
CaB
CaB
C{B
D3B
DMB
DMB
D�B
EB
E9B
E�B
E�B
E�B
E�B
E�B
F?B
FYB
FtB
F�B
F�B
GB
G+B
G�B
HB
HKB
HKB
H�B
IB
IB
I7B
I�B
J=B
J�B
K)B
K�B
K�B
K�B
L0B
LJB
LdB
LJB
LdB
LJB
L�B
MB
MB
MPB
MjB
M�B
M�B
M�B
M�B
NVB
NVB
N�B
N�B
OB
OvB
OvB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P.B
PHB
P}B
PHB
P}B
P}B
P�B
P�B
Q4B
QNB
Q�B
RB
RB
R B
R�B
R�B
SB
S&B
S�B
SuB
S�B
SuB
SuB
S�B
SuB
TaB
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
V�B
V�B
W
B
W$B
XB
X_B
X_B
X�B
X�B
Y1B
YB
YeB
YB
Y�B
ZB
ZB
Z7B
ZQB
ZQB
ZB
ZB
Z�B
Z�B
Z�B
[=B
[WB
[�B
[�B
[�B
[�B
\B
\B
[�B
\�B
\�B
]B
]/B
]B
]IB
]�B
]�B
^B
^jB
^OB
^�B
^�B
^�B
^�B
^�B
_!B
_!B
_B
_;B
_�B
`B
`BB
`\B
`vB
`�B
`�B
`�B
a-B
a-B
a-B
a�B
a�B
a�B
bB
bB
bB
b4B
b4B
bhB
b�B
b�B
c B
cB
cTB
cnB
c�B
dB
dZB
dZB
d�B
d�B
d�B
eFB
e`B
e�B
e�B
e�B
e�B
e�B
e�B
fB
fLB
fLB
ffB
f�B
ffB
f�B
f�B
gB
gRB
g8B
gmB
g�B
h>B
hsB
hsB
hsB
h�B
h�B
h�B
h�B
iB
iB
i_B
i_B
iyB
iyB
i�B
i�B
i�B
i�B
jB
j0B
jB
j�B
j�B
j�B
kB
k6B
k�B
k�B
lB
lB
k�B
k�B
k�B
l"B
lWB
lqB
lqB
lWB
lqB
lqB
l=B
lWB
l�B
l�B
mwB
mwB
mwB
m]B
m�B
ncB
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
oOB
o�B
pB
pB
p!B
p;B
p�B
p�B
p�B
p�B
qB
qB
p�B
q'B
qvB
qvB
q�B
raB
r�B
r�B
r�B
sB
sB
sMB
s�B
s�B
s�B
tTB
tnB
tnB
t�B
t�B
u%B
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v`B
v�B
wLB
w�B
x8B
x8B
x8B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�*B��B�>B�$B�B�LB��B�B��B��B��B��B�3B�3B�3B�3B�hB�B�B�%B��B��B�zB��B�	B��B	�B	gB	�B	��B	��B	�$B	��B	��B	��B	��B	�	B	�gB	��B	�B	�.B	� B	�\B	�6B	��B	�?B	��B	��B
?B
_�B
p�B
�ZB�BA�Bd�Bp!B~�B�RB��B��B�rB��B��B�~B�B�BDBB�B�B�B�BYBB��B�B��B�B��B�B׍BɺB�gB�qB�pB�.By�B_�B;�B+B
�B
�"B
xB
%`B
�B
3B	�qB	ɠB	��B	�B	{�B	YeB	GB	,�B	�B	{B	�B��B��B��B��B�yB�B�B�5B�WB�_B��B�hB�B��B�B�YB�[B��B��B�3B�.B�B�YB�=B�kB�B		B�}B�B	�B	3MB	`�B	~�B	��B	�-B	�B	��B	|jB	|�B	�B	|B	z�B	x�B	{�B	�B	�;B	�uB	��B	�B	��B	��B	�"B	��B	�\B	��B	�vB	�\B	�DB	��B	��B	�dB	�B	�oB	��B	��B	��B	��B	�zB	��B	��B	��B	�xB	�jB	��B	��B	�xB	�<B	��B	�dB	ϑB	�NB	��B	ҽB	ԯB	�,B	�B	�B	��B	��B	��B	�YB	�+B	�eB	��B	ބB	��B	�'B	�B	�B	��B	�B	�B	�\B	��B	��B	��B	�vB	�B	��B	�B	��B	�bB	��B	�-B	��B	��B	ߤB	�vB	�qB	ٴB	�kB	�B	�B	�B	��B	�EB	��B	��B	�
B	�B	ںB	�WB	��B	�#B	ܬB	�)B	ٴB	ؓB	��B	�sB	׍B	֡B	�B	�yB	ּB	��B	�9B	ּB	֡B	��B	��B	�mB	�B	�SB	ּB	�mB	�B	�B	�SB	ևB	��B	�sB	�?B	�$B	��B	خB	�?B	׍B	�B	�EB	�+B	��B	�EB	��B	�B	ۦB	�qB	�)B	��B	��B	�)B	��B	�B	�;B	�|B	�4B	�B	�B	��B	�B	�|B	�\B	߾B	��B	�hB	�B	�zB	�zB	�B	�`B	�B	�B	�B	�nB	�B	��B	�mB	�B	��B	��B	�/B	��B	�B	��B	�B	��B	��B	�OB	��B	�B	� B	��B	�/B	��B	�oB	�AB	��B	�5B	�B	��B	�-B	�B	�GB	�[B	��B	�B	��B	��B	��B	�	B	��B	��B	�B	��B	��B	�zB	�lB	�8B	�$B	��B	�B	��B	��B	��B	�0B	��B	��B	��B	�B	�lB	�*B	��B	��B	�0B	�B	��B	��B	�RB	�B	��B	�B	��B	��B	�+B	�B	�B	��B	��B	��B	�B	��B	�qB	�B	�B	�B	�?B	��B	�FB	�zB	��B	�B	��B	��B	�fB	�	B	��B	�VB	��B	�DB	�B	��B	��B	��B	�>B	��B	�0B	��B	��B	�B	��B
�B
�B
�B
MB
�B
MB
�B
�B
mB
SB
B
�B
9B
SB
mB
SB
�B
�B
9B
9B
�B
SB
9B
�B
SB
�B
�B
�B
�B
�B
B
�B
�B
�B
	B
	7B
	�B

	B

XB
	�B
	7B

�B
xB
~B
�B
�B
�B
(B
.B
HB
�B
�B
�B
oB
 B
 B
NB
NB
�B
B
uB
FB
�B
�B
B
gB
�B
yB
�B
�B
�B
sB
9B
�B
gB
�B
�B
�B
�B
�B
uB
�B
aB
�B
SB
�B
�B
�B
�B
�B
�B
�B
�B

B

B
sB
_B
_B
�B
YB
$B
?B
�B
yB
�B
KB
1B
�B
QB
7B
�B
B
xB
]B
�B
B
~B
B
�B
B
VB
�B
�B
 BB
 �B
!B
!�B
!�B
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
"hB
"�B
"�B
"�B
"�B
"�B
#�B
%B
%�B
%�B
&�B
'�B
'�B
(�B
(
B
(�B
)�B
*eB
*�B
+B
+�B
,WB
,"B
-CB
.B
-�B
-�B
-�B
-�B
.}B
.�B
/�B
/�B
0�B
1'B
1vB
1�B
1�B
2-B
1�B
2B
2GB
2B
2B
2|B
3MB
3�B
4B
49B
49B
4�B
4nB
4nB
4nB
5?B
5�B
6`B
6�B
6�B
6�B
7B
7fB
7�B
7fB
7�B
8B
8RB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9>B
:*B
:�B
:�B
:�B
;B
:�B
:�B
:�B
<B
;�B
;�B
=<B
=�B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
>(B
>wB
>�B
>�B
>�B
?HB
?HB
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A B
A B
AUB
AoB
A�B
BB
A�B
B'B
BuB
B�B
B�B
CB
CB
CaB
CaB
CaB
C{B
D3B
DMB
DMB
D�B
EB
E9B
E�B
E�B
E�B
E�B
E�B
F?B
FYB
FtB
F�B
F�B
GB
G+B
G�B
HB
HKB
HKB
H�B
IB
IB
I7B
I�B
J=B
J�B
K)B
K�B
K�B
K�B
L0B
LJB
LdB
LJB
LdB
LJB
L�B
MB
MB
MPB
MjB
M�B
M�B
M�B
M�B
NVB
NVB
N�B
N�B
OB
OvB
OvB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P.B
PHB
P}B
PHB
P}B
P}B
P�B
P�B
Q4B
QNB
Q�B
RB
RB
R B
R�B
R�B
SB
S&B
S�B
SuB
S�B
SuB
SuB
S�B
SuB
TaB
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
V�B
V�B
W
B
W$B
XB
X_B
X_B
X�B
X�B
Y1B
YB
YeB
YB
Y�B
ZB
ZB
Z7B
ZQB
ZQB
ZB
ZB
Z�B
Z�B
Z�B
[=B
[WB
[�B
[�B
[�B
[�B
\B
\B
[�B
\�B
\�B
]B
]/B
]B
]IB
]�B
]�B
^B
^jB
^OB
^�B
^�B
^�B
^�B
^�B
_!B
_!B
_B
_;B
_�B
`B
`BB
`\B
`vB
`�B
`�B
`�B
a-B
a-B
a-B
a�B
a�B
a�B
bB
bB
bB
b4B
b4B
bhB
b�B
b�B
c B
cB
cTB
cnB
c�B
dB
dZB
dZB
d�B
d�B
d�B
eFB
e`B
e�B
e�B
e�B
e�B
e�B
e�B
fB
fLB
fLB
ffB
f�B
ffB
f�B
f�B
gB
gRB
g8B
gmB
g�B
h>B
hsB
hsB
hsB
h�B
h�B
h�B
h�B
iB
iB
i_B
i_B
iyB
iyB
i�B
i�B
i�B
i�B
jB
j0B
jB
j�B
j�B
j�B
kB
k6B
k�B
k�B
lB
lB
k�B
k�B
k�B
l"B
lWB
lqB
lqB
lWB
lqB
lqB
l=B
lWB
l�B
l�B
mwB
mwB
mwB
m]B
m�B
ncB
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
oOB
o�B
pB
pB
p!B
p;B
p�B
p�B
p�B
p�B
qB
qB
p�B
q'B
qvB
qvB
q�B
raB
r�B
r�B
r�B
sB
sB
sMB
s�B
s�B
s�B
tTB
tnB
tnB
t�B
t�B
u%B
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v`B
v�B
wLB
w�B
x8B
x8B
x8B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104951  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175142  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175142  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175143                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025150  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025150  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                