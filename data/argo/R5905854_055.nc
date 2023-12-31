CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:54:25Z creation;2022-06-04T17:54:25Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175425  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               7A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�.�Dt��1   @�.�Q�m�@/{dZ��c�S���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�  A   AffA>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh��Bo��Bx  B�  B�33B�  B���B���B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  CL�C�fC  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>L�C?�fCA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch� Ci�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @!�@���@���A z�A�GA>�GA`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bo�RBx�B�\B�B�B�\B���B��)B�\B�\B���B���B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B���B��)B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�CT{C�C�C �C"�C$�C&�C(�C*�C,�C-�C0�C2�C4�C6�C8�C:�C<�C>T{C?�CA�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch��Ci�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��
C��
C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�D)D���D���D� �D�@�D���D���D�)D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�D)D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�G�A�D�A�B�A�CaA�C�A�A�A�A�A�A�A�B�A�DgA�D3A�B�A�C�A�<6A�9XA�2aA�(�A��Aѧ�A�s�A�OBA��A�oiA��A��A�!�A�WsA��,A�U�A�%�A˥�A�h>A�/�AʢhA�Z�A�a|AVA�w�A��FA��HA��gA���A�7A���A�K�A���A���A�)�A��wA�_A��A���A�
=A�gA��A�a�A��A���A��A�~�A�6�A���A�9�A��9A�\)A��A���A� �A��BA��RA�QNA���A�O�A�^5A�'�A�+kA��xA�;dA��A��ZA�b�A���A���A�33A�9XA�1�A��5A��EA�
=A���A��9A���A��kA��cA���A�+�A�RTA}>�A{�AzoAu� Ap��An�HAk�	Af�A`<6A]6AZ�_AW�AR|AO�AM��AJoiAI�bAH��AF�WAE�CAB'�AAh
A?�KA>�\A=^�A;�TA:��A9_pA7%�A5ѷA4qA0��A._A-�WA-�]A,�fA+��A+�A*�_A)S�A(��A&��A%�DA$qA"+�A!Z�A ��A��A��A�A�pA{�A��A^�A?}A��AcAXAs�A�Ai�Ag�A�YA�AaA5�AĜA�A}VA��A�MA�zAL0A�*A��AVA`�A҉A��A]�An�AA�A��A/A�"A�oA�A\)A(�A��AC�A�)A�hAZ�A�A`BA�6A�{AK�A�&AFA
�A
�YA
#:A	�>A	��A	�A��A�sA�	AVmAQ�A
�A�A��A��Aw2AkQAH�A�"A��A[WA�A�&A�A�aA��A:�A��A;�A��A��A�AFA�MAJ#A �sA �VA ��A Dg@��@���@���@�ں@��@��@�o@�6�@�+@�!@���@� \@��@�a�@�YK@�@�0@�@�p;@�:�@﹌@���@�Q@��Q@�s@���@�@�;@�Z�@�4@��@��@�S�@���@�7@�@��a@��@��@�2�@�c@��K@�Q@ߢ�@�5�@�r�@���@ݤ@@�dZ@�S@ܹ�@�|�@��g@�:�@ړu@�@ق�@�͟@�H@���@�	l@�Z@���@Շ�@�`B@��@Ԟ@�:*@ӹ�@�Vm@��@ҡb@�g8@��N@��@Ї+@�@Ϫ�@Ϙ�@�m]@�G�@�Y@��@ή}@�n�@��@�֡@̌�@�6@���@�j�@��@��@ɋ�@�@�Xy@��@ǶF@�S�@�L0@�zx@�0�@��@�tT@�@�o @��	@�@�7�@���@���@�$�@�@�Y@�֡@�Z@��@�:�@�oi@��@�|@�_p@�2a@��@�kQ@�E�@��;@��P@�j@�/@��@�`�@�+k@��@�
�@��"@�/�@��@���@�h
@���@��9@��@��#@���@��@�{J@�9�@��@��@��$@�C�@���@��K@��[@�u�@�!@���@��@��'@�P�@��@��2@��h@���@��p@�,=@��@�N<@��@��z@�{�@��@�j@�ں@�c @�-@���@��P@�`B@��@�!�@���@���@�g�@��@���@�H@���@�A @�%@��@���@��@��q@��$@�Vm@���@�~(@�ff@�Q�@�-�@�@��Q@�m]@�"�@��@��s@���@��o@�`�@�7@���@��@�Dg@��@�҉@�}V@�1�@��@���@��4@�F�@��@�~(@�D�@�-@��@��F@�Vm@�/@��E@�Z�@���@��M@�=�@��|@��9@���@�r�@�D�@�O@�ϫ@���@���@�v`@�/@�@���@�l�@�.�@�ϫ@��S@�G�@��v@��D@�r�@�H�@�b@��@���@�A @�Y@��v@���@�D�@��.@��a@��M@�S&@�,�@�@��@�r�@�B[@��@���@�@��@���@��M@�hs@�H�@�=�@�4@�o@���@���@�ff@�"h@���@���@�E9@��@��8@��/@��R@�y>@�n�@�0U@���@��=@���@�N<@�Ĝ@�!�@��T@��^@�`B@�%@��u@�6�@�Z�@� \@��@��p@��@���@�i�@�@�_p@�	l@��@��o@��@��S@�F@�=@��@��$@�#:@���@���@�s@�+@���@���@�%�@�r@W?@~��@~��@~
�@}a�@}q@|��@|_@{��@{)_@z�+@y�D@yx�@y<6@y�@x��@x �@w�Q@wZ�@v�H@v��@v�@u�7@u	l@t��@t7@se�@s@r��@r��@rQ@r�@q�N@q��@q?}@p��@p �@o��@o˒@o9�@o
=@nߤ@n�@nJ�@m�)@m�'@m��@m8�@l��@l�@l�@k�K@k��@j��@j��@j6�@j
�@i��@i \@h�$@h|�@hU2@g�]@g�$@f�@f\�@f@em]@d�5@d��@dD�@d�@c�@c��@c"�@b�@b�6@bOv@a��@a}�@`�@`h�@_�@_�	@_s@_l�@_K�@^E�@]�@]�h@]f�@]O�@]/@\�U@[��@[X�@Z��@Z!�@Y��@Yx�@YVm@Y+�@X��@X��@X��@X�Y@Xb@W�@W�@Vc @U��@U�^@UrG@UQ�@U@@T�@T�D@TN�@S��@So�@S"�@S@R�X@Rq�@R6�@Q�9@Qu�@Q�@P�K@P�[@P��@PS�@P,=@O�[@Ox@Oo@N�'@Ns�@N!�@M�@Mj@M%F@L��@L��@L�|@L��@LbN@L`�@L[�@K�@Kn/@KY@J�@J��@J� @J8�@I@IJ�@Hѷ@H��@HtT@HM@G��@G��@Gx@G"�@G�@F�2@F��@Fv�@F-@F	@E�@Ej@E8�@EV@DtT@D�@C��@C�	@C|�@Cb�@CRT@C,�@B��@B�h@B_�@A�@A��@A�@@�@@V�@@�@@x@@x@@b@?�W@?��@?a@?�@>��@=�Z@<�@<�@<7�@<M@;��@;@O@:�@:H�@9��@9p�@9T�@8��@8u�@8N�@8,=@8M@7�@6҉@6�1@6p;@6V@6�@5��@5�"@5<6@5�@4�|@4��@4q@3�]@3s@3S�@3�@2�@2��@2{�@2R�@1��@1O�@1�@0�@0�@0u�@0PH@0	�@/�@/S�@.��@.��@.�<@._�@.!�@.�@.�@.
�@-��@-L�@,��@,tT@+ݘ@+�q@*�M@*� @*#:@)rG@)7L@)#�@)�@(Ĝ@(Z@(�@'�@'�K@'O@&��@&�<@&Ov@&!�@%�@%�>@%�@%�3@%�"@%f�@%-w@$��@$��@$��@$��@$m�@$H@$(�@$$@$�@#�@#�K@#��@#o�@#9�@#&@#Y@"�s@"z@"Q@"=q@"($@"@!�@!�@!�d@!zx@!F@ �@ ��@ ��@ �@ r�@ U2@ ?�@ �@�@˒@�Q@��@�@s@U�@33@�y@�s@��@GE@$�@ �@��@��@�@��@Vm@�@�@�`@��@2�@b@خ@X�@!-@Y@ i@��@�B@��@�'@��@�+@	@�>@�@�9@�N@��@/@��@�v@�$@��@�@[�@6@��@��@{J@g�@RT@)_@�2@��@�@i�@C�@#:@u@�o@��@�X@��@zx@?}@*0@�f@�z@r�@]d@-�@@��@�a@t�@J#@�@��@��@��@<6@�@�@��@g8@Ft@�@�@�F@qv@$t@�@�@�1@q�@�@ �@��@�H@��@�"@J�@&�@@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�G�A�D�A�B�A�CaA�C�A�A�A�A�A�A�A�B�A�DgA�D3A�B�A�C�A�<6A�9XA�2aA�(�A��Aѧ�A�s�A�OBA��A�oiA��A��A�!�A�WsA��,A�U�A�%�A˥�A�h>A�/�AʢhA�Z�A�a|AVA�w�A��FA��HA��gA���A�7A���A�K�A���A���A�)�A��wA�_A��A���A�
=A�gA��A�a�A��A���A��A�~�A�6�A���A�9�A��9A�\)A��A���A� �A��BA��RA�QNA���A�O�A�^5A�'�A�+kA��xA�;dA��A��ZA�b�A���A���A�33A�9XA�1�A��5A��EA�
=A���A��9A���A��kA��cA���A�+�A�RTA}>�A{�AzoAu� Ap��An�HAk�	Af�A`<6A]6AZ�_AW�AR|AO�AM��AJoiAI�bAH��AF�WAE�CAB'�AAh
A?�KA>�\A=^�A;�TA:��A9_pA7%�A5ѷA4qA0��A._A-�WA-�]A,�fA+��A+�A*�_A)S�A(��A&��A%�DA$qA"+�A!Z�A ��A��A��A�A�pA{�A��A^�A?}A��AcAXAs�A�Ai�Ag�A�YA�AaA5�AĜA�A}VA��A�MA�zAL0A�*A��AVA`�A҉A��A]�An�AA�A��A/A�"A�oA�A\)A(�A��AC�A�)A�hAZ�A�A`BA�6A�{AK�A�&AFA
�A
�YA
#:A	�>A	��A	�A��A�sA�	AVmAQ�A
�A�A��A��Aw2AkQAH�A�"A��A[WA�A�&A�A�aA��A:�A��A;�A��A��A�AFA�MAJ#A �sA �VA ��A Dg@��@���@���@�ں@��@��@�o@�6�@�+@�!@���@� \@��@�a�@�YK@�@�0@�@�p;@�:�@﹌@���@�Q@��Q@�s@���@�@�;@�Z�@�4@��@��@�S�@���@�7@�@��a@��@��@�2�@�c@��K@�Q@ߢ�@�5�@�r�@���@ݤ@@�dZ@�S@ܹ�@�|�@��g@�:�@ړu@�@ق�@�͟@�H@���@�	l@�Z@���@Շ�@�`B@��@Ԟ@�:*@ӹ�@�Vm@��@ҡb@�g8@��N@��@Ї+@�@Ϫ�@Ϙ�@�m]@�G�@�Y@��@ή}@�n�@��@�֡@̌�@�6@���@�j�@��@��@ɋ�@�@�Xy@��@ǶF@�S�@�L0@�zx@�0�@��@�tT@�@�o @��	@�@�7�@���@���@�$�@�@�Y@�֡@�Z@��@�:�@�oi@��@�|@�_p@�2a@��@�kQ@�E�@��;@��P@�j@�/@��@�`�@�+k@��@�
�@��"@�/�@��@���@�h
@���@��9@��@��#@���@��@�{J@�9�@��@��@��$@�C�@���@��K@��[@�u�@�!@���@��@��'@�P�@��@��2@��h@���@��p@�,=@��@�N<@��@��z@�{�@��@�j@�ں@�c @�-@���@��P@�`B@��@�!�@���@���@�g�@��@���@�H@���@�A @�%@��@���@��@��q@��$@�Vm@���@�~(@�ff@�Q�@�-�@�@��Q@�m]@�"�@��@��s@���@��o@�`�@�7@���@��@�Dg@��@�҉@�}V@�1�@��@���@��4@�F�@��@�~(@�D�@�-@��@��F@�Vm@�/@��E@�Z�@���@��M@�=�@��|@��9@���@�r�@�D�@�O@�ϫ@���@���@�v`@�/@�@���@�l�@�.�@�ϫ@��S@�G�@��v@��D@�r�@�H�@�b@��@���@�A @�Y@��v@���@�D�@��.@��a@��M@�S&@�,�@�@��@�r�@�B[@��@���@�@��@���@��M@�hs@�H�@�=�@�4@�o@���@���@�ff@�"h@���@���@�E9@��@��8@��/@��R@�y>@�n�@�0U@���@��=@���@�N<@�Ĝ@�!�@��T@��^@�`B@�%@��u@�6�@�Z�@� \@��@��p@��@���@�i�@�@�_p@�	l@��@��o@��@��S@�F@�=@��@��$@�#:@���@���@�s@�+@���@���@�%�@�r@W?@~��@~��@~
�@}a�@}q@|��@|_@{��@{)_@z�+@y�D@yx�@y<6@y�@x��@x �@w�Q@wZ�@v�H@v��@v�@u�7@u	l@t��@t7@se�@s@r��@r��@rQ@r�@q�N@q��@q?}@p��@p �@o��@o˒@o9�@o
=@nߤ@n�@nJ�@m�)@m�'@m��@m8�@l��@l�@l�@k�K@k��@j��@j��@j6�@j
�@i��@i \@h�$@h|�@hU2@g�]@g�$@f�@f\�@f@em]@d�5@d��@dD�@d�@c�@c��@c"�@b�@b�6@bOv@a��@a}�@`�@`h�@_�@_�	@_s@_l�@_K�@^E�@]�@]�h@]f�@]O�@]/@\�U@[��@[X�@Z��@Z!�@Y��@Yx�@YVm@Y+�@X��@X��@X��@X�Y@Xb@W�@W�@Vc @U��@U�^@UrG@UQ�@U@@T�@T�D@TN�@S��@So�@S"�@S@R�X@Rq�@R6�@Q�9@Qu�@Q�@P�K@P�[@P��@PS�@P,=@O�[@Ox@Oo@N�'@Ns�@N!�@M�@Mj@M%F@L��@L��@L�|@L��@LbN@L`�@L[�@K�@Kn/@KY@J�@J��@J� @J8�@I@IJ�@Hѷ@H��@HtT@HM@G��@G��@Gx@G"�@G�@F�2@F��@Fv�@F-@F	@E�@Ej@E8�@EV@DtT@D�@C��@C�	@C|�@Cb�@CRT@C,�@B��@B�h@B_�@A�@A��@A�@@�@@V�@@�@@x@@x@@b@?�W@?��@?a@?�@>��@=�Z@<�@<�@<7�@<M@;��@;@O@:�@:H�@9��@9p�@9T�@8��@8u�@8N�@8,=@8M@7�@6҉@6�1@6p;@6V@6�@5��@5�"@5<6@5�@4�|@4��@4q@3�]@3s@3S�@3�@2�@2��@2{�@2R�@1��@1O�@1�@0�@0�@0u�@0PH@0	�@/�@/S�@.��@.��@.�<@._�@.!�@.�@.�@.
�@-��@-L�@,��@,tT@+ݘ@+�q@*�M@*� @*#:@)rG@)7L@)#�@)�@(Ĝ@(Z@(�@'�@'�K@'O@&��@&�<@&Ov@&!�@%�@%�>@%�@%�3@%�"@%f�@%-w@$��@$��@$��@$��@$m�@$H@$(�@$$@$�@#�@#�K@#��@#o�@#9�@#&@#Y@"�s@"z@"Q@"=q@"($@"@!�@!�@!�d@!zx@!F@ �@ ��@ ��@ �@ r�@ U2@ ?�@ �@�@˒@�Q@��@�@s@U�@33@�y@�s@��@GE@$�@ �@��@��@�@��@Vm@�@�@�`@��@2�@b@خ@X�@!-@Y@ i@��@�B@��@�'@��@�+@	@�>@�@�9@�N@��@/@��@�v@�$@��@�@[�@6@��@��@{J@g�@RT@)_@�2@��@�@i�@C�@#:@u@�o@��@�X@��@zx@?}@*0@�f@�z@r�@]d@-�@@��@�a@t�@J#@�@��@��@��@<6@�@�@��@g8@Ft@�@�@�F@qv@$t@�@�@�1@q�@�@ �@��@�H@��@�"@J�@&�@@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�TB	�TB	�B	��B	�SB	�$B	��B	�FB	�@B	��B	��B	�~B	y>B	v�B	vB	tB	r�B	q�B	qAB	qB	�B	��B	��B	��B	�FB
�B
	lB
�B
�B
dB
#�B
+�B
4�B
A�B
MB
Y�B
^�B
jKB
o5B
u�B
cB
�XB
��B
�
B
�B
�BdB4�BC{BLJBP�Ba�Br�Bv�By$B�B��B��B�)B��B�_B�B��B��B�B~�BxBw�B��B`�B,�B
�B
dB
oB
�B
C�B
2GB

	B	�*B	�*B	ںB	�B	��B	��B	��B	��B	�B	u?B	fLB	O�B	,WB	�B	}B	�B�B�B�4BٴBרB�B��B�6B�xB�KB��B��B��B��B��BҽB�LB�B�B�LB�$B�B		�B		lB	�B	5B	'B	#B	5B	�B	!�B	"�B	 �B	,�B	Q4B	O�B	B�B	>�B	?.B	@OB	C{B	E�B	LJB	N�B	J�B	S�B	ezB	r-B	raB	xB	x�B	��B	�xB	�:B	��B	��B	�nB	��B	��B	��B	�	B	� B	��B	�*B	�zB	�B	��B	��B	�8B	ǮB	��B	�BB	�aB	�EB	�kB	��B	�B	��B	�IB	��B	�vB	��B	�B	�~B	ٚB	��B	޸B	ߤB	�B	�!B	��B	�VB	��B	��B	�	B	��B	�B	�7B	�B	�B	��B	�dB	ݘB	��B	��B	�-B	�HB	�|B	��B	�B	�B	��B	�B	�B	�hB	�B	��B	�|B	��B	��B	�HB	��B	�B	��B	�B	�\B	�HB	�B	�4B	��B	�'B	��B	�!B	��B	�OB	��B	ߤB	�B	�/B	�xB	��B	ۦB	�)B	ܬB	��B	��B	�]B	�CB	�CB	�]B	�CB	�)B	�B	ܒB	�dB	��B	��B	�]B	�CB	�OB	�5B	��B	�dB	��B	ݲB	ޞB	ߤB	�bB	�B	�B	�B	�hB	�NB	�B	�4B	�B	�4B	�nB	�B	�B	�zB	�,B	�zB	�@B	�B	��B	�NB	�B	�hB	�B	��B	��B	��B	�4B	�B	�4B	�B	�B	�B	�B	��B	�B	�B	�B	�@B	�@B	�tB	�B	�B	��B	�,B	�B	�`B	�B	��B	�B	�B	�8B	�B	��B	�KB	��B	�B	��B	�B	�kB	�kB	��B	�B	�eB	�B	�6B	��B	��B	�WB	�B	�B	��B	��B	�cB	��B	�B	�B	�B	��B	��B	��B	��B	�'B	��B	�hB	��B	��B	�GB	�B	�nB	�B	�?B	�?B	��B	��B	��B	�zB	�B	�B	�8B	�rB	�6B	��B	�"B	��B
 B	��B	�(B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	��B	�B	��B
 �B
 4B	��B
 �B
�B
�B
oB
�B
 �B	��B	��B	��B	��B	�.B	��B	��B	�}B	�cB	�cB	�HB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
oB
 �B
UB
�B
�B
�B
�B
;B
 OB
B
�B
aB
MB
B
�B
�B
�B
YB
�B
+B
�B
�B
B
�B
B
1B
	B
	lB
	�B
	lB
	�B

=B

�B

�B
xB
0B
~B
~B
B
�B
�B
�B
B
�B
(B
vB
\B
�B
vB
�B
�B
HB
�B
B
4B
4B
�B
 B
oB
oB
�B
B
&B
B
B
�B
,B
aB
B
MB
�B
B
B
SB
9B
mB
$B
sB
�B
+B
EB
yB
yB
�B
�B
KB
KB
B
�B
7B
	B
�B
xB
�B
�B
�B
�B
�B
�B
�B
B
�B
OB
B
pB
VB
�B
 \B
 'B
�B
 �B
!B
!|B
!bB
 �B
 B
�B
 vB
 �B
 �B
"NB
#�B
$&B
$@B
$�B
$�B
%`B
%�B
%�B
%zB
%zB
&2B
&�B
&�B
&�B
'B
'B
'�B
'B
'B
'RB
($B
(XB
(sB
(>B
(�B
(�B
)DB
)*B
)*B
*0B
*KB
*�B
+B
+�B
+�B
+�B
,"B
,WB
,WB
,�B
-CB
-CB
-�B
-�B
./B
.cB
.�B
/iB
/�B
/�B
/�B
/�B
0B
0;B
0B
0!B
0�B
0�B
1B
1AB
2-B
2-B
2aB
2|B
2�B
2�B
2�B
2�B
3B
3�B
3�B
4TB
4TB
49B
4�B
5%B
5tB
5%B
5%B
5%B
5ZB
5ZB
5?B
6�B
7�B
72B
6�B
72B
7�B
7�B
7�B
8RB
8�B
9>B
9�B
9�B
:^B
:^B
:�B
;B
;B
;�B
;�B
<B
<6B
<PB
<6B
<6B
="B
="B
=VB
=qB
=VB
=qB
=�B
>�B
>�B
>�B
>�B
?B
?B
?�B
A B
A�B
BAB
BAB
B[B
B�B
C{B
C-B
B�B
B'B
BB
A�B
B[B
BuB
BuB
B�B
B�B
CGB
C�B
C�B
DB
D3B
DgB
DMB
D�B
D�B
EB
EB
EB
E�B
E�B
E�B
FYB
F�B
G�B
HB
H1B
H1B
H1B
HfB
H�B
H�B
H�B
IlB
I�B
JXB
J=B
J#B
J�B
K)B
KxB
K�B
K�B
K�B
K�B
LdB
L�B
M6B
MjB
M�B
M�B
NpB
NpB
O�B
O�B
PB
O�B
O�B
P}B
Q B
P�B
Q4B
QB
QB
Q4B
RB
R:B
RTB
R:B
R:B
RTB
R:B
R:B
R�B
R�B
S@B
S�B
S�B
S�B
TFB
TB
T,B
TB
S�B
S�B
T�B
T�B
T�B
T�B
U�B
U�B
V�B
V�B
V�B
V�B
W
B
WsB
W�B
XEB
X�B
X�B
X�B
YKB
YB
YB
YB
YeB
Y�B
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
[#B
[#B
[	B
[	B
[#B
[qB
[�B
\CB
\]B
\�B
\�B
\�B
]B
]B
]�B
^OB
^�B
^�B
^�B
^�B
_B
_B
_!B
_�B
_�B
`B
`B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
a-B
a�B
a�B
a�B
b�B
cB
cnB
dB
dZB
dtB
dZB
d�B
d�B
e,B
eFB
eFB
e�B
fB
f2B
f�B
f�B
f�B
gB
gB
gB
g8B
gRB
gmB
g�B
g�B
g�B
g�B
g�B
h
B
h$B
h$B
h>B
hsB
hsB
h�B
i�B
i�B
i�B
i�B
i�B
jB
j0B
jeB
jeB
kB
k�B
k�B
k�B
k�B
k�B
l=B
lWB
lqB
lWB
lqB
l�B
l�B
l�B
l�B
l�B
mB
m)B
mCB
m�B
m�B
m�B
m�B
m�B
mwB
m]B
mCB
mCB
mwB
n�B
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
poB
pUB
p�B
qAB
q�B
q�B
q�B
q�B
raB
r|B
r�B
r�B
r�B
sB
sMB
shB
s�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
uB
u?B
utB
utB
utB
utB
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v`B
vzB
v�B
v�B
v�B
v�B
v�B
wB
wB
wfB
w�B
xlB
x�B
x�B
y$B
y>B
yrB
y�B
y�B
y�B
y�B
z*B
zxB
z�B
z�B
{B
{dB
{dB
|B
{�B
{�B
|B
|B
|B
|�B
|�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�TB	�TB	�B	��B	�SB	�$B	��B	�FB	�@B	��B	��B	�~B	y>B	v�B	vB	tB	r�B	q�B	qAB	qB	�B	��B	��B	��B	�FB
�B
	lB
�B
�B
dB
#�B
+�B
4�B
A�B
MB
Y�B
^�B
jKB
o5B
u�B
cB
�XB
��B
�
B
�B
�BdB4�BC{BLJBP�Ba�Br�Bv�By$B�B��B��B�)B��B�_B�B��B��B�B~�BxBw�B��B`�B,�B
�B
dB
oB
�B
C�B
2GB

	B	�*B	�*B	ںB	�B	��B	��B	��B	��B	�B	u?B	fLB	O�B	,WB	�B	}B	�B�B�B�4BٴBרB�B��B�6B�xB�KB��B��B��B��B��BҽB�LB�B�B�LB�$B�B		�B		lB	�B	5B	'B	#B	5B	�B	!�B	"�B	 �B	,�B	Q4B	O�B	B�B	>�B	?.B	@OB	C{B	E�B	LJB	N�B	J�B	S�B	ezB	r-B	raB	xB	x�B	��B	�xB	�:B	��B	��B	�nB	��B	��B	��B	�	B	� B	��B	�*B	�zB	�B	��B	��B	�8B	ǮB	��B	�BB	�aB	�EB	�kB	��B	�B	��B	�IB	��B	�vB	��B	�B	�~B	ٚB	��B	޸B	ߤB	�B	�!B	��B	�VB	��B	��B	�	B	��B	�B	�7B	�B	�B	��B	�dB	ݘB	��B	��B	�-B	�HB	�|B	��B	�B	�B	��B	�B	�B	�hB	�B	��B	�|B	��B	��B	�HB	��B	�B	��B	�B	�\B	�HB	�B	�4B	��B	�'B	��B	�!B	��B	�OB	��B	ߤB	�B	�/B	�xB	��B	ۦB	�)B	ܬB	��B	��B	�]B	�CB	�CB	�]B	�CB	�)B	�B	ܒB	�dB	��B	��B	�]B	�CB	�OB	�5B	��B	�dB	��B	ݲB	ޞB	ߤB	�bB	�B	�B	�B	�hB	�NB	�B	�4B	�B	�4B	�nB	�B	�B	�zB	�,B	�zB	�@B	�B	��B	�NB	�B	�hB	�B	��B	��B	��B	�4B	�B	�4B	�B	�B	�B	�B	��B	�B	�B	�B	�@B	�@B	�tB	�B	�B	��B	�,B	�B	�`B	�B	��B	�B	�B	�8B	�B	��B	�KB	��B	�B	��B	�B	�kB	�kB	��B	�B	�eB	�B	�6B	��B	��B	�WB	�B	�B	��B	��B	�cB	��B	�B	�B	�B	��B	��B	��B	��B	�'B	��B	�hB	��B	��B	�GB	�B	�nB	�B	�?B	�?B	��B	��B	��B	�zB	�B	�B	�8B	�rB	�6B	��B	�"B	��B
 B	��B	�(B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	��B	�B	��B
 �B
 4B	��B
 �B
�B
�B
oB
�B
 �B	��B	��B	��B	��B	�.B	��B	��B	�}B	�cB	�cB	�HB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
oB
 �B
UB
�B
�B
�B
�B
;B
 OB
B
�B
aB
MB
B
�B
�B
�B
YB
�B
+B
�B
�B
B
�B
B
1B
	B
	lB
	�B
	lB
	�B

=B

�B

�B
xB
0B
~B
~B
B
�B
�B
�B
B
�B
(B
vB
\B
�B
vB
�B
�B
HB
�B
B
4B
4B
�B
 B
oB
oB
�B
B
&B
B
B
�B
,B
aB
B
MB
�B
B
B
SB
9B
mB
$B
sB
�B
+B
EB
yB
yB
�B
�B
KB
KB
B
�B
7B
	B
�B
xB
�B
�B
�B
�B
�B
�B
�B
B
�B
OB
B
pB
VB
�B
 \B
 'B
�B
 �B
!B
!|B
!bB
 �B
 B
�B
 vB
 �B
 �B
"NB
#�B
$&B
$@B
$�B
$�B
%`B
%�B
%�B
%zB
%zB
&2B
&�B
&�B
&�B
'B
'B
'�B
'B
'B
'RB
($B
(XB
(sB
(>B
(�B
(�B
)DB
)*B
)*B
*0B
*KB
*�B
+B
+�B
+�B
+�B
,"B
,WB
,WB
,�B
-CB
-CB
-�B
-�B
./B
.cB
.�B
/iB
/�B
/�B
/�B
/�B
0B
0;B
0B
0!B
0�B
0�B
1B
1AB
2-B
2-B
2aB
2|B
2�B
2�B
2�B
2�B
3B
3�B
3�B
4TB
4TB
49B
4�B
5%B
5tB
5%B
5%B
5%B
5ZB
5ZB
5?B
6�B
7�B
72B
6�B
72B
7�B
7�B
7�B
8RB
8�B
9>B
9�B
9�B
:^B
:^B
:�B
;B
;B
;�B
;�B
<B
<6B
<PB
<6B
<6B
="B
="B
=VB
=qB
=VB
=qB
=�B
>�B
>�B
>�B
>�B
?B
?B
?�B
A B
A�B
BAB
BAB
B[B
B�B
C{B
C-B
B�B
B'B
BB
A�B
B[B
BuB
BuB
B�B
B�B
CGB
C�B
C�B
DB
D3B
DgB
DMB
D�B
D�B
EB
EB
EB
E�B
E�B
E�B
FYB
F�B
G�B
HB
H1B
H1B
H1B
HfB
H�B
H�B
H�B
IlB
I�B
JXB
J=B
J#B
J�B
K)B
KxB
K�B
K�B
K�B
K�B
LdB
L�B
M6B
MjB
M�B
M�B
NpB
NpB
O�B
O�B
PB
O�B
O�B
P}B
Q B
P�B
Q4B
QB
QB
Q4B
RB
R:B
RTB
R:B
R:B
RTB
R:B
R:B
R�B
R�B
S@B
S�B
S�B
S�B
TFB
TB
T,B
TB
S�B
S�B
T�B
T�B
T�B
T�B
U�B
U�B
V�B
V�B
V�B
V�B
W
B
WsB
W�B
XEB
X�B
X�B
X�B
YKB
YB
YB
YB
YeB
Y�B
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
[#B
[#B
[	B
[	B
[#B
[qB
[�B
\CB
\]B
\�B
\�B
\�B
]B
]B
]�B
^OB
^�B
^�B
^�B
^�B
_B
_B
_!B
_�B
_�B
`B
`B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
a-B
a�B
a�B
a�B
b�B
cB
cnB
dB
dZB
dtB
dZB
d�B
d�B
e,B
eFB
eFB
e�B
fB
f2B
f�B
f�B
f�B
gB
gB
gB
g8B
gRB
gmB
g�B
g�B
g�B
g�B
g�B
h
B
h$B
h$B
h>B
hsB
hsB
h�B
i�B
i�B
i�B
i�B
i�B
jB
j0B
jeB
jeB
kB
k�B
k�B
k�B
k�B
k�B
l=B
lWB
lqB
lWB
lqB
l�B
l�B
l�B
l�B
l�B
mB
m)B
mCB
m�B
m�B
m�B
m�B
m�B
mwB
m]B
mCB
mCB
mwB
n�B
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
poB
pUB
p�B
qAB
q�B
q�B
q�B
q�B
raB
r|B
r�B
r�B
r�B
sB
sMB
shB
s�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
uB
u?B
utB
utB
utB
utB
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v`B
vzB
v�B
v�B
v�B
v�B
v�B
wB
wB
wfB
w�B
xlB
x�B
x�B
y$B
y>B
yrB
y�B
y�B
y�B
y�B
z*B
zxB
z�B
z�B
{B
{dB
{dB
|B
{�B
{�B
|B
|B
|B
|�B
|�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104957  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175425  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175425  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175425                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025433  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025433  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                