CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:48:12Z creation;2022-06-04T17:48:12Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604174812  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��8+�e1   @��8w`U@/;dZ��c���R1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B���B�  B���B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&33C'��C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}�fD~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���A z�A z�A@z�A`z�A�=qA�=qA�=qA�=qA�=qA�
>A�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�u�B��)B�\B���B�\B���B�B�B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C !HC�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&:�C'�{C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CO�CR�CT�CV�CX�CZ�C\!HC^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}�RD~�D~��D�D��D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��)D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��)D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�[�A�G�A�2�A�\A�A��fA���A���A��,A��A���A��jA��dA��/A��A���A��jA��pA��pA�9A�GA�a�A�U�A�S&A�N�A�K�A�H�A�H�A�FtA�zA��A��A���A���A��A�ZA�NA���A�{A��A�/OAܑ A˼6A�A�A�+�A��jA��5A�A�A�pA���A�lWA�9$A�_A���A���A�h�A��A���A���A�MA�.IA��A���A�D3A���A�|A�0�A�:A�A�A���A���A���A��oA���A�p�A���A��&A�ܒA��A|�Aw��Aq�Ap��Ak�zAh2aAf��Ac�A_�?A[�eAY*0AV~�AU�>AU��AT��AR�+AQ�AN�TAL�cAI��AG��AE  A@�TA=��A:��A9+kA7[�A7�A6$�A4 iA3�A1rGA0��A0	A/C�A.P�A-:*A, \A+��A+OA*��A*A)�A'	lA%��A%��A%@OA$�A#�A"R�A"GA"�A"��A"�A"�pA#ZA#\)A#bNA"�yA"L0A!�VA �oA �SA T�A S�A 7�A $tA��A��AYKA�FA�6A��A\�A�A�$A�A��Aq�A��A��A��AZ�A��A��A��A�A�qA�KA��A#:AAcA=qA�Ae�AA�4AیA�+A��Av�A�	Al"A�Ap�A�5Aa�A�Aa|A
��A
��A
_A	��A	)�A��Ap�AR�AxA8A�ZA�tAiDA�A��A��A�A�@A^5AeA��AOvA�A��A�A�4Ao�A?�A�A�&A�uA`�A;�A�KA �hA a@���@��f@�*0@�E�@�_p@��R@�&�@�ݘ@�Y@�Ɇ@�V�@��r@�@�C@�V@�_p@��@�zx@��@���@�8�@��@�j@�&�@��.@���@�C@�n/@�0�@��@��@��@���@�/�@��@�(�@�{@��@�^@�֡@�+@�l�@��@�ں@姇@�^�@�B�@��H@��@�q�@�^@���@ߊ�@�	l@޻�@��o@�[W@ܭ�@ܘ_@���@ۭC@�c@��@ګ6@ږ�@�Ov@�ϫ@ٍP@�0�@���@��)@ا�@�<�@��@��@�z@Ր�@�A�@�2�@ң�@�Ft@�O@Ѣ�@�dZ@�@Љ�@�خ@�zx@�O�@Ϋ6@�z@�]d@���@�x@�F@�"�@�Y@���@��[@̞�@�`�@�M�@�	�@��@�6z@���@��@ʷ�@� �@�خ@Ɇ�@��@���@Ȅ�@�/�@���@�_p@�'�@�ߤ@�Q@���@�Q�@ĵ@��@å�@¥z@�;�@��
@�f�@��P@���@�I�@�b@��N@���@�O@��@��F@�Q@��@�H�@���@��O@�bN@��$@�RT@��f@���@�_�@�D�@��}@�s@�f�@�@O@��`@���@�W�@�J@��&@��@���@�"�@��I@�ff@��@��T@��N@���@�(@�+@���@�͟@��4@�z�@�L0@�n/@� i@���@��o@�J�@�@�B�@���@���@��@��o@���@���@�@O@�Ɇ@��A@�+k@��@�U�@��@��D@�ff@�$�@��T@��@�F@�@���@���@�;�@�ݘ@���@�@��e@�d�@��@��H@��o@�7�@�	�@��9@��z@�j@�E9@�"�@���@�1'@�O@��@�=@��A@�Xy@�	@���@�zx@�o�@�8@��@�Xy@���@�s@��@��X@�J�@���@�w2@�C@�͟@�i�@���@�O�@��6@�A�@��@��S@�G�@��,@�a|@���@��[@�c@�j@�&�@��b@�1�@���@��N@���@�o�@�Vm@�1�@���@�n�@�-�@�c@���@��@���@���@���@�YK@�<�@��@�c@�9�@�Y@��y@��\@�:*@���@�a�@�4@���@��@��H@��@���@�PH@��@���@�_p@�8�@��@���@�=q@�+k@��@��6@���@�X@�0�@�҉@���@�u%@�c @�?�@�@��@���@��t@�o�@�;d@�!-@��8@��]@��e@��u@�tT@�5?@���@�#�@��@��e@�l�@�GE@�	@��T@��F@��=@�j�@�.I@�@��@���@��2@��]@�Ĝ@�r�@�2�@�O@���@���@�/@��@��2@��m@�� @�N�@�@��T@���@��{@�[W@�#�@�ѷ@��@�j@��@>�@~�@~Z�@~_@}m]@|��@|��@|�o@{�W@{�@{"�@z�1@z3�@y�>@y�@y�3@y��@y0�@x�@x��@x��@x��@x�O@x��@xr�@xG@v҉@v�@vJ�@v�@u�o@u��@u	l@tPH@s��@s/�@r�@r�@r�L@q�)@q%@p�@o�Q@n�@nd�@m[W@l(�@k�*@k��@k��@ke�@kZ�@k"�@j��@j��@j0U@i�"@i5�@hq@h"h@g�6@g��@g�P@g'�@fGE@e�@eϫ@ec@d��@d��@d��@d��@dU2@c�r@c��@c=@b��@bn�@a�D@a��@ax�@aV@`��@_�m@_��@_y�@_�@^�@]��@]��@]|@]o @]J�@]�@\�`@\~(@\N�@\�@[��@[�0@[��@[O@Z0U@Ys�@YB�@Y�@X�@X��@Xz�@W�@W�@VQ@U��@U��@UN<@U�@T�5@Tw�@T�@S��@S�A@S��@S�	@SE9@Rȴ@Rh
@ROv@R	@Q��@Q�@P��@P�_@Ph�@P,=@O�&@Oخ@O��@O_p@OY@N�@N��@N_�@N�@M�@M��@M�=@Mm]@M#�@L��@LPH@L�@K�F@KC@J�B@J�A@J@I�@I#�@H�[@H�u@H`�@H6@H@Gƨ@Gs@G�@F��@Fv�@E�D@E��@E:�@D��@D��@DtT@DPH@D,=@C�&@C��@C��@CiD@C�@B��@B�b@Bp;@B($@Ao @@�4@@�@?��@?H�@>�y@>��@>�'@>��@>��@>�@=�'@<�f@<��@<K^@<�@;��@;!-@:�@:�+@:q�@9�)@9��@9�@8��@8PH@7��@7�f@6�]@6�@6W�@5�)@5 \@4�|@4��@4~(@4~@3� @3��@3v`@3F�@2�@2v�@2?@1�@1ϫ@1��@1e,@0��@0��@04n@0�@0�@/��@.�M@.�b@.J�@-|@- \@,��@,��@,G@+��@+�4@+Z�@+'�@+"�@+S@*�m@*��@*M�@*$�@*#:@*e@*_@)��@)��@)8�@)%@(�@(%�@'�@'��@'l�@'C@&�@&ߤ@&�@&W�@&@�@&=q@&!�@%��@%��@%�@%�@%��@%c�@%�@%;@$��@$u�@$2�@#�@#��@#��@#�4@#g�@#4�@"��@"p;@"L0@"+k@"{@"@!��@!��@!��@!Y�@ �f@ ֡@ �o@ S�@ Ft@ 4n@ G@�q@iD@O@(@�@��@��@6�@��@c@\�@(�@��@��@C-@�@��@�K@��@��@v`@RT@K�@=@ں@�@n�@Ov@H�@GE@�@�#@�@��@��@�@T�@O�@�@��@]d@%�@��@�P@n/@S�@$t@�@��@z@@@}�@F@�@%@�@�E@��@�O@�.@�@g8@A�@/�@�@�&@�:@dZ@�@�8@�2@��@�}@i�@E�@{@��@�@��@J�@�@@@�@�p@Ĝ@bN@ �@b@x@�r@�g@��@8@Y@��@��@a|@Z�@C�@GE@E�@6�@�@{@J@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�[�A�G�A�2�A�\A�A��fA���A���A��,A��A���A��jA��dA��/A��A���A��jA��pA��pA�9A�GA�a�A�U�A�S&A�N�A�K�A�H�A�H�A�FtA�zA��A��A���A���A��A�ZA�NA���A�{A��A�/OAܑ A˼6A�A�A�+�A��jA��5A�A�A�pA���A�lWA�9$A�_A���A���A�h�A��A���A���A�MA�.IA��A���A�D3A���A�|A�0�A�:A�A�A���A���A���A��oA���A�p�A���A��&A�ܒA��A|�Aw��Aq�Ap��Ak�zAh2aAf��Ac�A_�?A[�eAY*0AV~�AU�>AU��AT��AR�+AQ�AN�TAL�cAI��AG��AE  A@�TA=��A:��A9+kA7[�A7�A6$�A4 iA3�A1rGA0��A0	A/C�A.P�A-:*A, \A+��A+OA*��A*A)�A'	lA%��A%��A%@OA$�A#�A"R�A"GA"�A"��A"�A"�pA#ZA#\)A#bNA"�yA"L0A!�VA �oA �SA T�A S�A 7�A $tA��A��AYKA�FA�6A��A\�A�A�$A�A��Aq�A��A��A��AZ�A��A��A��A�A�qA�KA��A#:AAcA=qA�Ae�AA�4AیA�+A��Av�A�	Al"A�Ap�A�5Aa�A�Aa|A
��A
��A
_A	��A	)�A��Ap�AR�AxA8A�ZA�tAiDA�A��A��A�A�@A^5AeA��AOvA�A��A�A�4Ao�A?�A�A�&A�uA`�A;�A�KA �hA a@���@��f@�*0@�E�@�_p@��R@�&�@�ݘ@�Y@�Ɇ@�V�@��r@�@�C@�V@�_p@��@�zx@��@���@�8�@��@�j@�&�@��.@���@�C@�n/@�0�@��@��@��@���@�/�@��@�(�@�{@��@�^@�֡@�+@�l�@��@�ں@姇@�^�@�B�@��H@��@�q�@�^@���@ߊ�@�	l@޻�@��o@�[W@ܭ�@ܘ_@���@ۭC@�c@��@ګ6@ږ�@�Ov@�ϫ@ٍP@�0�@���@��)@ا�@�<�@��@��@�z@Ր�@�A�@�2�@ң�@�Ft@�O@Ѣ�@�dZ@�@Љ�@�خ@�zx@�O�@Ϋ6@�z@�]d@���@�x@�F@�"�@�Y@���@��[@̞�@�`�@�M�@�	�@��@�6z@���@��@ʷ�@� �@�خ@Ɇ�@��@���@Ȅ�@�/�@���@�_p@�'�@�ߤ@�Q@���@�Q�@ĵ@��@å�@¥z@�;�@��
@�f�@��P@���@�I�@�b@��N@���@�O@��@��F@�Q@��@�H�@���@��O@�bN@��$@�RT@��f@���@�_�@�D�@��}@�s@�f�@�@O@��`@���@�W�@�J@��&@��@���@�"�@��I@�ff@��@��T@��N@���@�(@�+@���@�͟@��4@�z�@�L0@�n/@� i@���@��o@�J�@�@�B�@���@���@��@��o@���@���@�@O@�Ɇ@��A@�+k@��@�U�@��@��D@�ff@�$�@��T@��@�F@�@���@���@�;�@�ݘ@���@�@��e@�d�@��@��H@��o@�7�@�	�@��9@��z@�j@�E9@�"�@���@�1'@�O@��@�=@��A@�Xy@�	@���@�zx@�o�@�8@��@�Xy@���@�s@��@��X@�J�@���@�w2@�C@�͟@�i�@���@�O�@��6@�A�@��@��S@�G�@��,@�a|@���@��[@�c@�j@�&�@��b@�1�@���@��N@���@�o�@�Vm@�1�@���@�n�@�-�@�c@���@��@���@���@���@�YK@�<�@��@�c@�9�@�Y@��y@��\@�:*@���@�a�@�4@���@��@��H@��@���@�PH@��@���@�_p@�8�@��@���@�=q@�+k@��@��6@���@�X@�0�@�҉@���@�u%@�c @�?�@�@��@���@��t@�o�@�;d@�!-@��8@��]@��e@��u@�tT@�5?@���@�#�@��@��e@�l�@�GE@�	@��T@��F@��=@�j�@�.I@�@��@���@��2@��]@�Ĝ@�r�@�2�@�O@���@���@�/@��@��2@��m@�� @�N�@�@��T@���@��{@�[W@�#�@�ѷ@��@�j@��@>�@~�@~Z�@~_@}m]@|��@|��@|�o@{�W@{�@{"�@z�1@z3�@y�>@y�@y�3@y��@y0�@x�@x��@x��@x��@x�O@x��@xr�@xG@v҉@v�@vJ�@v�@u�o@u��@u	l@tPH@s��@s/�@r�@r�@r�L@q�)@q%@p�@o�Q@n�@nd�@m[W@l(�@k�*@k��@k��@ke�@kZ�@k"�@j��@j��@j0U@i�"@i5�@hq@h"h@g�6@g��@g�P@g'�@fGE@e�@eϫ@ec@d��@d��@d��@d��@dU2@c�r@c��@c=@b��@bn�@a�D@a��@ax�@aV@`��@_�m@_��@_y�@_�@^�@]��@]��@]|@]o @]J�@]�@\�`@\~(@\N�@\�@[��@[�0@[��@[O@Z0U@Ys�@YB�@Y�@X�@X��@Xz�@W�@W�@VQ@U��@U��@UN<@U�@T�5@Tw�@T�@S��@S�A@S��@S�	@SE9@Rȴ@Rh
@ROv@R	@Q��@Q�@P��@P�_@Ph�@P,=@O�&@Oخ@O��@O_p@OY@N�@N��@N_�@N�@M�@M��@M�=@Mm]@M#�@L��@LPH@L�@K�F@KC@J�B@J�A@J@I�@I#�@H�[@H�u@H`�@H6@H@Gƨ@Gs@G�@F��@Fv�@E�D@E��@E:�@D��@D��@DtT@DPH@D,=@C�&@C��@C��@CiD@C�@B��@B�b@Bp;@B($@Ao @@�4@@�@?��@?H�@>�y@>��@>�'@>��@>��@>�@=�'@<�f@<��@<K^@<�@;��@;!-@:�@:�+@:q�@9�)@9��@9�@8��@8PH@7��@7�f@6�]@6�@6W�@5�)@5 \@4�|@4��@4~(@4~@3� @3��@3v`@3F�@2�@2v�@2?@1�@1ϫ@1��@1e,@0��@0��@04n@0�@0�@/��@.�M@.�b@.J�@-|@- \@,��@,��@,G@+��@+�4@+Z�@+'�@+"�@+S@*�m@*��@*M�@*$�@*#:@*e@*_@)��@)��@)8�@)%@(�@(%�@'�@'��@'l�@'C@&�@&ߤ@&�@&W�@&@�@&=q@&!�@%��@%��@%�@%�@%��@%c�@%�@%;@$��@$u�@$2�@#�@#��@#��@#�4@#g�@#4�@"��@"p;@"L0@"+k@"{@"@!��@!��@!��@!Y�@ �f@ ֡@ �o@ S�@ Ft@ 4n@ G@�q@iD@O@(@�@��@��@6�@��@c@\�@(�@��@��@C-@�@��@�K@��@��@v`@RT@K�@=@ں@�@n�@Ov@H�@GE@�@�#@�@��@��@�@T�@O�@�@��@]d@%�@��@�P@n/@S�@$t@�@��@z@@@}�@F@�@%@�@�E@��@�O@�.@�@g8@A�@/�@�@�&@�:@dZ@�@�8@�2@��@�}@i�@E�@{@��@�@��@J�@�@@@�@�p@Ĝ@bN@ �@b@x@�r@�g@��@8@Y@��@��@a|@Z�@C�@GE@E�@6�@�@{@J@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	1vB	1'B	0�B	/�B	.�B	-�B	-]B	,�B	,WB	,qB	,qB	+�B	+�B	+�B	+�B	,"B	-)B	-�B	.�B	3�B	1vB	,�B	+B	*�B	+kB	,=B	-CB	-�B	0�B	F%B	S�B	X+B	b4B	a|B	Y1B	NVB	P.B	H�B	RoB	b4B	;B	A�B	 B�"B	tB	5%B	q�B	�B
 �B
BU�Bl�B��B�wB��B�oB��B��B��B�^B|Bj�B3�B'�B"4B�B	�B
��B
��B
��B
��B
�B
��B
wfB
V�B
0�B
�B	ϑB	�SB	��B	��B	yXB	wLB	p�B	h�B	_B	H�B	3�B	�B	~B	 B��B	 B	�B��B�B�B�KB�^B��B�FB�B��B��B�#B�KB��B��B��B��B�$B��B��B��B��B�1B��B��B��B��B�"B�uB�KB�B�B�~BٴB�B�1B�B�B	�B	"�B	3hB	N"B	[�B	j�B	m)B	kkB	p�B	{�B	��B	��B	��B	�2B	�tB	��B	��B	�oB	��B	�yB	��B	��B	��B	��B	��B	�	B	��B	�;B	�XB	��B	��B	��B	��B	�]B	�mB	�{B	��B	�aB	�B	�}B	��B	�PB	�dB	��B	��B	��B	�B	�GB	�_B	�1B	ǮB	�%B	��B	�lB	��B	�0B	��B	̈́B	̈́B	̈́B	�"B	�B	�B	ΊB	�VB	�pB	�BB	�.B	�vB	��B	��B	��B	��B	� B	�B	��B	�kB	یB	�WB	�]B	��B	��B	�TB	�B	�B	��B	��B	�B	�sB	�B	�
B	�B	�B	��B	�B	��B	�B	�B	�*B	��B	��B	�sB	�XB	�B	�>B	�B	�_B	�qB	�6B	�B	�B	�B	�>B	�B	�B	�WB	�B	�QB	�QB	��B	�!B	�;B	�B	�B	�-B	�MB	��B	��B	��B	�B	��B	��B	��B	�B	��B	�)B	��B	��B	��B	�eB	�B	�B	�
B	��B	��B	�NB	�B	�HB	�B	��B	�B	��B	��B	��B	�
B	�B	�B	�_B	�B	��B	�B	�B	�B	��B	�B	�B	�B	�B	��B	�0B	�DB	��B	�fB	�NB	�B	��B	�B	�B	�B	�nB	�ZB	�B	�,B	�8B	�RB	�RB	��B	�*B	�_B	�B	�B	�B	��B	�eB	��B	��B	�QB	�B	�B	�B	�B	�B	�/B	�B	�IB	��B	�B	�iB	�B	�B	��B	��B	�!B	��B	�'B	�B	�GB	�|B	�GB	�B	�B	�B	�3B	��B	��B	��B	��B	��B	�B	�B	�hB	��B	��B	�B	��B	�B	�B	�B	��B	�B	��B	�B	��B	�B	�B	�B	��B	�B	�B	�%B	��B	�B	�FB	��B	�fB	�lB	��B	��B	��B	��B	��B	��B	��B	�xB	��B	�JB	�JB	�0B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	�PB	��B	��B	�jB	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
[B
�B
-B
�B
B
B
gB
3B
�B
B
-B
�B
B
�B
�B
B
B
�B
�B
�B
�B
�B
3B
�B
�B
gB
�B
B
�B
tB
�B
�B
�B
B
�B
�B
�B
�B
	7B
�B
	�B
	�B
	�B

�B
)B
�B
B
�B
�B
pB
B
(B
\B
�B
.B
�B
�B
�B
HB
�B
�B
TB
�B
TB
TB
B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
B
SB
9B
B
9B
�B
�B
�B
B
SB

B
�B
�B
_B
�B
�B
�B
�B
�B
B
�B
qB
�B
�B
xB
�B
�B
IB
dB
�B
B
jB
�B
�B
;B
VB
�B
 BB
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
"B
"B
"4B
"�B
#nB
#�B
#�B
$&B
$�B
%,B
%,B
%�B
&B
&LB
&�B
'RB
'�B
'�B
'�B
($B
(XB
(�B
)�B
)�B
)�B
*eB
*�B
+6B
+QB
+kB
+�B
,qB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-wB
-wB
-�B
.}B
/5B
/�B
/�B
0UB
1[B
1�B
2B
2-B
2�B
2�B
2�B
3B
33B
3hB
3hB
3hB
3�B
4B
49B
4�B
4nB
4�B
4nB
4nB
4�B
5B
6B
6B
6`B
6`B
6zB
6�B
6�B
7LB
8B
8B
7�B
7�B
7�B
8lB
8�B
8�B
9XB
9�B
:DB
;B
;�B
;�B
;�B
;�B
<B
;�B
<6B
<jB
<jB
<�B
="B
=qB
>BB
>wB
>�B
>�B
>�B
>�B
?}B
?�B
?�B
@ B
@�B
@�B
@�B
@�B
@�B
A B
AUB
A�B
A�B
B'B
B[B
B�B
B�B
B�B
B�B
CaB
CaB
CB
C�B
D�B
D�B
EB
D�B
D�B
EB
EB
E9B
E�B
ESB
E�B
E�B
FB
E�B
FB
GB
GzB
G�B
G�B
G�B
G�B
G�B
HB
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J=B
J�B
J�B
J�B
JrB
J�B
J�B
KB
K)B
KB
K)B
KDB
J�B
J�B
J�B
J�B
K^B
KxB
K^B
K^B
KxB
K^B
K�B
K�B
K�B
K�B
LB
LJB
L0B
LdB
L�B
MB
MjB
MPB
M�B
M�B
NB
N"B
N�B
N�B
O\B
OBB
OvB
O�B
O�B
O�B
O�B
PHB
P}B
P�B
P�B
Q4B
Q�B
Q�B
R B
R:B
R:B
RoB
R�B
R�B
R�B
R�B
SB
S[B
SuB
S�B
TB
TaB
T�B
T�B
UMB
U�B
U�B
VB
VB
VB
VB
VB
V�B
V�B
W?B
W�B
W�B
W�B
XEB
X�B
X�B
X�B
X�B
YB
Y�B
Y�B
Z7B
ZQB
Z�B
Z�B
[qB
[qB
[�B
[�B
\B
\B
\)B
\CB
\�B
\�B
]B
]B
]/B
]�B
]�B
^B
^5B
^OB
^OB
^�B
^�B
_;B
_VB
_VB
_;B
_�B
`BB
`BB
`vB
a-B
abB
aHB
a�B
bNB
b�B
b�B
b�B
c B
cB
c B
cnB
c�B
c�B
c�B
c�B
c�B
dB
dB
d&B
dZB
d�B
eB
e�B
e�B
e�B
e�B
f2B
ffB
fLB
f�B
f�B
f�B
f�B
g8B
gmB
g�B
g�B
g�B
h
B
h>B
hXB
hXB
hsB
h�B
i*B
iyB
iyB
i�B
i�B
i�B
i�B
jeB
j�B
j�B
j�B
kB
kB
k6B
k6B
kQB
k�B
k�B
k�B
lWB
lqB
lqB
lqB
l�B
l�B
mB
m)B
mwB
mwB
m�B
m�B
m�B
nIB
n�B
n�B
n�B
n�B
oB
oB
o�B
o�B
o�B
o�B
o�B
pB
p!B
p!B
pB
p�B
p�B
p�B
qB
qB
qB
q'B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
raB
r�B
r�B
sMB
sMB
shB
shB
s�B
s�B
s�B
tB
tTB
t�B
t�B
uB
u%B
u?B
u?B
utB
uZB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
vB
v`B
v�B
v�B
v�B
v�B
v�B
w2B
w�B
w�B
w�B
w�B
xB
xB
x�B
x�B
x�B
x�B
x�B
x�B
yXB
y�B
y�B
y�B
y�B
y�B
y�B
zDB
zDB
z^B
z�B
{B
z�B
{B
{B
{B
{0B
{JB
{dB
{dB
{d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	1vB	1'B	0�B	/�B	.�B	-�B	-]B	,�B	,WB	,qB	,qB	+�B	+�B	+�B	+�B	,"B	-)B	-�B	.�B	3�B	1vB	,�B	+B	*�B	+kB	,=B	-CB	-�B	0�B	F%B	S�B	X+B	b4B	a|B	Y1B	NVB	P.B	H�B	RoB	b4B	;B	A�B	 B�"B	tB	5%B	q�B	�B
 �B
BU�Bl�B��B�wB��B�oB��B��B��B�^B|Bj�B3�B'�B"4B�B	�B
��B
��B
��B
��B
�B
��B
wfB
V�B
0�B
�B	ϑB	�SB	��B	��B	yXB	wLB	p�B	h�B	_B	H�B	3�B	�B	~B	 B��B	 B	�B��B�B�B�KB�^B��B�FB�B��B��B�#B�KB��B��B��B��B�$B��B��B��B��B�1B��B��B��B��B�"B�uB�KB�B�B�~BٴB�B�1B�B�B	�B	"�B	3hB	N"B	[�B	j�B	m)B	kkB	p�B	{�B	��B	��B	��B	�2B	�tB	��B	��B	�oB	��B	�yB	��B	��B	��B	��B	��B	�	B	��B	�;B	�XB	��B	��B	��B	��B	�]B	�mB	�{B	��B	�aB	�B	�}B	��B	�PB	�dB	��B	��B	��B	�B	�GB	�_B	�1B	ǮB	�%B	��B	�lB	��B	�0B	��B	̈́B	̈́B	̈́B	�"B	�B	�B	ΊB	�VB	�pB	�BB	�.B	�vB	��B	��B	��B	��B	� B	�B	��B	�kB	یB	�WB	�]B	��B	��B	�TB	�B	�B	��B	��B	�B	�sB	�B	�
B	�B	�B	��B	�B	��B	�B	�B	�*B	��B	��B	�sB	�XB	�B	�>B	�B	�_B	�qB	�6B	�B	�B	�B	�>B	�B	�B	�WB	�B	�QB	�QB	��B	�!B	�;B	�B	�B	�-B	�MB	��B	��B	��B	�B	��B	��B	��B	�B	��B	�)B	��B	��B	��B	�eB	�B	�B	�
B	��B	��B	�NB	�B	�HB	�B	��B	�B	��B	��B	��B	�
B	�B	�B	�_B	�B	��B	�B	�B	�B	��B	�B	�B	�B	�B	��B	�0B	�DB	��B	�fB	�NB	�B	��B	�B	�B	�B	�nB	�ZB	�B	�,B	�8B	�RB	�RB	��B	�*B	�_B	�B	�B	�B	��B	�eB	��B	��B	�QB	�B	�B	�B	�B	�B	�/B	�B	�IB	��B	�B	�iB	�B	�B	��B	��B	�!B	��B	�'B	�B	�GB	�|B	�GB	�B	�B	�B	�3B	��B	��B	��B	��B	��B	�B	�B	�hB	��B	��B	�B	��B	�B	�B	�B	��B	�B	��B	�B	��B	�B	�B	�B	��B	�B	�B	�%B	��B	�B	�FB	��B	�fB	�lB	��B	��B	��B	��B	��B	��B	��B	�xB	��B	�JB	�JB	�0B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	�PB	��B	��B	�jB	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
[B
�B
-B
�B
B
B
gB
3B
�B
B
-B
�B
B
�B
�B
B
B
�B
�B
�B
�B
�B
3B
�B
�B
gB
�B
B
�B
tB
�B
�B
�B
B
�B
�B
�B
�B
	7B
�B
	�B
	�B
	�B

�B
)B
�B
B
�B
�B
pB
B
(B
\B
�B
.B
�B
�B
�B
HB
�B
�B
TB
�B
TB
TB
B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
B
SB
9B
B
9B
�B
�B
�B
B
SB

B
�B
�B
_B
�B
�B
�B
�B
�B
B
�B
qB
�B
�B
xB
�B
�B
IB
dB
�B
B
jB
�B
�B
;B
VB
�B
 BB
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
"B
"B
"4B
"�B
#nB
#�B
#�B
$&B
$�B
%,B
%,B
%�B
&B
&LB
&�B
'RB
'�B
'�B
'�B
($B
(XB
(�B
)�B
)�B
)�B
*eB
*�B
+6B
+QB
+kB
+�B
,qB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-wB
-wB
-�B
.}B
/5B
/�B
/�B
0UB
1[B
1�B
2B
2-B
2�B
2�B
2�B
3B
33B
3hB
3hB
3hB
3�B
4B
49B
4�B
4nB
4�B
4nB
4nB
4�B
5B
6B
6B
6`B
6`B
6zB
6�B
6�B
7LB
8B
8B
7�B
7�B
7�B
8lB
8�B
8�B
9XB
9�B
:DB
;B
;�B
;�B
;�B
;�B
<B
;�B
<6B
<jB
<jB
<�B
="B
=qB
>BB
>wB
>�B
>�B
>�B
>�B
?}B
?�B
?�B
@ B
@�B
@�B
@�B
@�B
@�B
A B
AUB
A�B
A�B
B'B
B[B
B�B
B�B
B�B
B�B
CaB
CaB
CB
C�B
D�B
D�B
EB
D�B
D�B
EB
EB
E9B
E�B
ESB
E�B
E�B
FB
E�B
FB
GB
GzB
G�B
G�B
G�B
G�B
G�B
HB
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J=B
J�B
J�B
J�B
JrB
J�B
J�B
KB
K)B
KB
K)B
KDB
J�B
J�B
J�B
J�B
K^B
KxB
K^B
K^B
KxB
K^B
K�B
K�B
K�B
K�B
LB
LJB
L0B
LdB
L�B
MB
MjB
MPB
M�B
M�B
NB
N"B
N�B
N�B
O\B
OBB
OvB
O�B
O�B
O�B
O�B
PHB
P}B
P�B
P�B
Q4B
Q�B
Q�B
R B
R:B
R:B
RoB
R�B
R�B
R�B
R�B
SB
S[B
SuB
S�B
TB
TaB
T�B
T�B
UMB
U�B
U�B
VB
VB
VB
VB
VB
V�B
V�B
W?B
W�B
W�B
W�B
XEB
X�B
X�B
X�B
X�B
YB
Y�B
Y�B
Z7B
ZQB
Z�B
Z�B
[qB
[qB
[�B
[�B
\B
\B
\)B
\CB
\�B
\�B
]B
]B
]/B
]�B
]�B
^B
^5B
^OB
^OB
^�B
^�B
_;B
_VB
_VB
_;B
_�B
`BB
`BB
`vB
a-B
abB
aHB
a�B
bNB
b�B
b�B
b�B
c B
cB
c B
cnB
c�B
c�B
c�B
c�B
c�B
dB
dB
d&B
dZB
d�B
eB
e�B
e�B
e�B
e�B
f2B
ffB
fLB
f�B
f�B
f�B
f�B
g8B
gmB
g�B
g�B
g�B
h
B
h>B
hXB
hXB
hsB
h�B
i*B
iyB
iyB
i�B
i�B
i�B
i�B
jeB
j�B
j�B
j�B
kB
kB
k6B
k6B
kQB
k�B
k�B
k�B
lWB
lqB
lqB
lqB
l�B
l�B
mB
m)B
mwB
mwB
m�B
m�B
m�B
nIB
n�B
n�B
n�B
n�B
oB
oB
o�B
o�B
o�B
o�B
o�B
pB
p!B
p!B
pB
p�B
p�B
p�B
qB
qB
qB
q'B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
raB
r�B
r�B
sMB
sMB
shB
shB
s�B
s�B
s�B
tB
tTB
t�B
t�B
uB
u%B
u?B
u?B
utB
uZB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
vB
v`B
v�B
v�B
v�B
v�B
v�B
w2B
w�B
w�B
w�B
w�B
xB
xB
x�B
x�B
x�B
x�B
x�B
x�B
yXB
y�B
y�B
y�B
y�B
y�B
y�B
zDB
zDB
z^B
z�B
{B
z�B
{B
{B
{B
{0B
{JB
{dB
{dB
{d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104943  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174812  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174812  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174812                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024819  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024819  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                