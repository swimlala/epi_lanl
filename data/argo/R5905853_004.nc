CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:23:11Z creation;2022-06-04T17:23:11Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20220604172311  20220610121506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @خTxj1N1   @خT��I�@+Ձ$�/�d}O�;dZ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0��B8  B@  BG��BO33BX  B`  Bh  Bp  Bx  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B���B���C   C  C  C  C  C
  C  C  C  C  C  C�C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CI�fCK�fCN  CP�CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@���@���A z�A z�A@z�A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BG�RBOQ�BX�B`�Bh�Bp�Bx�B�u�B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�u�B��)B��)B�\B�\B�\B�\B�\B�\B�\B���B��)C �C�C�C�C�C
�C�C�C�C�C�C!HC!HC�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@!HCB�CD�CF�CH�CI�CK�CN�CP!HCR�CT�CV�CX�CZ�C[�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl!HCn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C�qC��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D�RD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.RD.�RD/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DXRDX�RDY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D� �D�@�D���D���D� �D�@�D�}�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D���D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�'\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A��A�VA�!bA� �A�!�A� �A�A�A�VA�!A��A�OA��A�OA� �A� �A�!�A�$A�&�A�&�A�"�A�!A� �AΪeA�t�A��iAɑ4A�o5A�f2A�iyAɉ�Aɯ�Aɫ�A�t�A�gmA�?}A�7A���A��Aȇ�A���Aǲ-Aǂ�A�AƍPA�L0A���A���A�^A�4A�1�AIA�CaA�!�A��2A�K�A�� A�h
A���A�yrA���A��2A���A��A��A��hA��-A���A��A���A��A�R�A�bA��A��A�W?A�I�A��A�J�A��A�8�A�֡A�t�A�+�A��A��VA���A��A���A�,=A�_AzAv{Aq�An�AlxlAj�Ae �A]�[AW�KAVAS�)AO�AK��AIe,AE��AAߤA=c�A:��A5�A4CA3A2��A3�A4  A4�=A3��A3kQA2��A2�eA1��A.�XA,�A+��A+�.A*:�A(�WA'��A'qA&��A&u%A%�A%<6A"�MA!��A!��A!�bA!��A ��A �AیA��AaA��A�A�A7A�A&�A�A�$AQA�KAMA��A��A>�A�MAm�A�A�A��Ah
AA�AMA�AOvA�oA�yA�A8A
�8A
��A
a|A
"�A
A	�BA	i�A�-A4A�AW�A�AE�A�zAVA�A��A��AE9A��A�AzxA �A��A��A��AqvAZ�A"�A �&A �[A ��A TaA �@��@�s�@�+@��,@�?@�o@��@���@���@��~@���@�;d@��+@�<�@��@��T@���@���@��,@���@��@�@���@��r@�F�@�e,@���@��c@�R@�ƨ@��)@�)_@��@�s�@�v`@��?@�1@���@�*@�P�@���@捹@�PH@�J�@�@��@�]�@�#�@��@��@��X@ⅈ@�6�@�"h@�� @��@�3�@�%F@�xl@�!�@��@ܛ�@�Q@��j@ڰ!@�4n@ٍP@�.I@ؾ�@�~�@�(�@׷�@�f�@�@ְ!@�� @���@ԂA@Ӕ�@�1�@��@Ҝx@�Xy@��@�hs@�F@�/@���@А.@�*�@ϓ@�#�@�}�@̑ @���@�Mj@��@ʥz@��@ɖS@�8@ȏ\@��)@ǫ�@ǁ@�/@��.@��M@�	@×$@�o�@��/@��@��$@���@�u�@��@��@�Vm@��@�Ĝ@�y>@�u@�ݘ@�rG@���@�j@�M@��@��@�xl@�ϫ@�s@���@�/�@��@�x@�\)@�9�@�%@���@��)@��#@���@�X�@���@�:�@��@��{@�qv@�Q�@�B�@�Y@��e@���@�o @�b�@�Mj@��@��@�M@��@��0@�e,@�C@�
=@��@��L@�C-@���@�	l@�m�@�	�@���@��@��N@���@�s@�A�@�8�@��@��6@�l"@��@���@���@��7@���@�zx@�U�@�Y@��5@���@�y>@��g@��k@�F�@��H@�tT@���@���@��d@���@��{@��@���@�n�@�W�@�A�@�?�@� �@��@��@�~�@�j�@�;d@��P@��\@�C�@���@�O�@�!-@��@���@��@�R�@��@��9@��@���@�RT@��@��@���@�5?@��@�u@���@�E9@���@��x@�m�@�J�@�	@��)@��^@���@�?}@�C@��@��@�`�@��@��@�H�@��@��v@�~(@�Z�@�1'@���@�O@�6z@�!-@��2@�N�@��d@���@��P@�Q@�$�@�x@���@�@��j@�IR@��]@��N@���@�j�@�S&@�F�@�#�@���@�h�@��;@�}�@���@��<@��@��u@�5?@��@���@��d@��@��w@��t@��'@�e�@� \@���@�?�@�	�@���@��@�x�@�Mj@�0�@��@��@��@�	l@��@�Ɇ@�q@��@��;@��M@�f�@�Vm@�+�@��@���@�N�@���@��>@���@���@���@���@�rG@�Z�@�U�@�P�@�%@��R@�N�@�1�@��&@�S@��u@�K^@�$@��A@���@�T�@��8@���@�c�@�(�@�@��D@���@�8�@�
=@��z@�S�@��@�:@~�s@}�z@}4@|oi@{�g@{4�@z	@y�@y��@y\�@y�@x֡@xɆ@x�_@xC-@w~�@v�M@v�]@v�,@v��@vff@u�@uc�@u%F@t�5@t�@tFt@s��@r��@q��@qzx@pD�@o��@o��@o�@o�K@o@n�F@nh
@n �@m��@m��@m�~@l��@lZ@lb@k��@k4�@j� @j	@i��@iT�@h�p@g�@g1�@f��@fE�@e�z@e`B@d�@d@c�@c��@c�f@c>�@b��@b�+@b5?@a�#@a��@a��@a�~@ak�@aF@`�9@`~@_�@_Mj@^��@^��@^4@]k�@]�@\��@\�u@\S�@\�@[�:@Z��@Zp;@Y��@YIR@XS�@X  @W��@W�{@WC�@Wo@V�M@V�m@V��@V��@Vh
@V5?@U�'@T��@S�Q@S��@Sg�@Rh
@R�@Q��@P��@Pc�@P/�@P~@P�@O�
@Ot�@OC@N��@N�r@N;�@M��@M��@M5�@L�@L��@L�@K�w@Kt�@K�@J�L@Jv�@J0U@I��@IrG@H��@Hoi@H'R@H�@G�@@F��@Fs�@F)�@F�@E��@E�t@E-w@E�@D�@D�@DPH@C�W@C�0@Ct�@C>�@C+@CY@B�c@B��@B�<@B��@B^5@A�t@@w�@@Q�@@:�@@$@@@?��@?�W@?�@?�@>�L@>��@>+k@=�@=@<�@<�U@<��@<I�@;�@;��@; i@:��@:�b@:ff@:Q@:=q@:	@9�o@9�d@9�"@9�@8Ɇ@8y>@87@7�a@7�F@7�:@6��@6n�@6#:@5zx@5@4�/@4�D@46@4*�@4 �@4  @3��@3O@3�@2Ta@1�@10�@0�[@0h�@/��@/b�@/O@/>�@.��@.�F@.0U@.@-��@-:�@-#�@,�`@,`�@,7@+��@+��@+!-@*��@*�y@*ߤ@*�@*R�@)�7@)O�@)@@(�z@(C-@'��@'O@&��@&��@%��@%��@%�@%T�@$��@$�@$D�@$"h@$G@#��@#� @#��@#�P@#o�@#
=@"҉@"�h@"��@"�\@"kQ@"5?@"_@!ϫ@!w2@!!�@ ��@ ��@ r�@ <�@ 1@˒@9�@��@�6@�b@��@z@c @@��@��@s�@c�@S&@#�@u�@�@�w@qv@X�@H�@�@�@�@��@�s@�h@�+@�9@�n@�@rG@*0@�@�@��@�`@��@g8@K^@�@� @��@�a@�:@]�@RT@C�@=@'�@��@͟@�}@xl@C�@3�@.�@)�@e@��@u�@Vm@!�@�P@�@�@�v@�4@m�@]d@U2@K^@D�@x@��@g�@1�@�@�@S@��@�H@��@	@��@��@��@u�@��@��@��@~(@tT@l"@D�@�]@��@��@t�@U�@4�@ i@��@��@��@�'@��@v�@kQ@Q@?@J@ϫ@�S@a�@F@4@�@��@e�@:�@�@��@��@��@�@��@e�@+@@
=@�@
�c@
�@
��@
��@
��@
z@
a|@
-@

�@	�#@	�^@	zx@	?}@	G�@	B�@	�@ی1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A��A�VA�!bA� �A�!�A� �A�A�A�VA�!A��A�OA��A�OA� �A� �A�!�A�$A�&�A�&�A�"�A�!A� �AΪeA�t�A��iAɑ4A�o5A�f2A�iyAɉ�Aɯ�Aɫ�A�t�A�gmA�?}A�7A���A��Aȇ�A���Aǲ-Aǂ�A�AƍPA�L0A���A���A�^A�4A�1�AIA�CaA�!�A��2A�K�A�� A�h
A���A�yrA���A��2A���A��A��A��hA��-A���A��A���A��A�R�A�bA��A��A�W?A�I�A��A�J�A��A�8�A�֡A�t�A�+�A��A��VA���A��A���A�,=A�_AzAv{Aq�An�AlxlAj�Ae �A]�[AW�KAVAS�)AO�AK��AIe,AE��AAߤA=c�A:��A5�A4CA3A2��A3�A4  A4�=A3��A3kQA2��A2�eA1��A.�XA,�A+��A+�.A*:�A(�WA'��A'qA&��A&u%A%�A%<6A"�MA!��A!��A!�bA!��A ��A �AیA��AaA��A�A�A7A�A&�A�A�$AQA�KAMA��A��A>�A�MAm�A�A�A��Ah
AA�AMA�AOvA�oA�yA�A8A
�8A
��A
a|A
"�A
A	�BA	i�A�-A4A�AW�A�AE�A�zAVA�A��A��AE9A��A�AzxA �A��A��A��AqvAZ�A"�A �&A �[A ��A TaA �@��@�s�@�+@��,@�?@�o@��@���@���@��~@���@�;d@��+@�<�@��@��T@���@���@��,@���@��@�@���@��r@�F�@�e,@���@��c@�R@�ƨ@��)@�)_@��@�s�@�v`@��?@�1@���@�*@�P�@���@捹@�PH@�J�@�@��@�]�@�#�@��@��@��X@ⅈ@�6�@�"h@�� @��@�3�@�%F@�xl@�!�@��@ܛ�@�Q@��j@ڰ!@�4n@ٍP@�.I@ؾ�@�~�@�(�@׷�@�f�@�@ְ!@�� @���@ԂA@Ӕ�@�1�@��@Ҝx@�Xy@��@�hs@�F@�/@���@А.@�*�@ϓ@�#�@�}�@̑ @���@�Mj@��@ʥz@��@ɖS@�8@ȏ\@��)@ǫ�@ǁ@�/@��.@��M@�	@×$@�o�@��/@��@��$@���@�u�@��@��@�Vm@��@�Ĝ@�y>@�u@�ݘ@�rG@���@�j@�M@��@��@�xl@�ϫ@�s@���@�/�@��@�x@�\)@�9�@�%@���@��)@��#@���@�X�@���@�:�@��@��{@�qv@�Q�@�B�@�Y@��e@���@�o @�b�@�Mj@��@��@�M@��@��0@�e,@�C@�
=@��@��L@�C-@���@�	l@�m�@�	�@���@��@��N@���@�s@�A�@�8�@��@��6@�l"@��@���@���@��7@���@�zx@�U�@�Y@��5@���@�y>@��g@��k@�F�@��H@�tT@���@���@��d@���@��{@��@���@�n�@�W�@�A�@�?�@� �@��@��@�~�@�j�@�;d@��P@��\@�C�@���@�O�@�!-@��@���@��@�R�@��@��9@��@���@�RT@��@��@���@�5?@��@�u@���@�E9@���@��x@�m�@�J�@�	@��)@��^@���@�?}@�C@��@��@�`�@��@��@�H�@��@��v@�~(@�Z�@�1'@���@�O@�6z@�!-@��2@�N�@��d@���@��P@�Q@�$�@�x@���@�@��j@�IR@��]@��N@���@�j�@�S&@�F�@�#�@���@�h�@��;@�}�@���@��<@��@��u@�5?@��@���@��d@��@��w@��t@��'@�e�@� \@���@�?�@�	�@���@��@�x�@�Mj@�0�@��@��@��@�	l@��@�Ɇ@�q@��@��;@��M@�f�@�Vm@�+�@��@���@�N�@���@��>@���@���@���@���@�rG@�Z�@�U�@�P�@�%@��R@�N�@�1�@��&@�S@��u@�K^@�$@��A@���@�T�@��8@���@�c�@�(�@�@��D@���@�8�@�
=@��z@�S�@��@�:@~�s@}�z@}4@|oi@{�g@{4�@z	@y�@y��@y\�@y�@x֡@xɆ@x�_@xC-@w~�@v�M@v�]@v�,@v��@vff@u�@uc�@u%F@t�5@t�@tFt@s��@r��@q��@qzx@pD�@o��@o��@o�@o�K@o@n�F@nh
@n �@m��@m��@m�~@l��@lZ@lb@k��@k4�@j� @j	@i��@iT�@h�p@g�@g1�@f��@fE�@e�z@e`B@d�@d@c�@c��@c�f@c>�@b��@b�+@b5?@a�#@a��@a��@a�~@ak�@aF@`�9@`~@_�@_Mj@^��@^��@^4@]k�@]�@\��@\�u@\S�@\�@[�:@Z��@Zp;@Y��@YIR@XS�@X  @W��@W�{@WC�@Wo@V�M@V�m@V��@V��@Vh
@V5?@U�'@T��@S�Q@S��@Sg�@Rh
@R�@Q��@P��@Pc�@P/�@P~@P�@O�
@Ot�@OC@N��@N�r@N;�@M��@M��@M5�@L�@L��@L�@K�w@Kt�@K�@J�L@Jv�@J0U@I��@IrG@H��@Hoi@H'R@H�@G�@@F��@Fs�@F)�@F�@E��@E�t@E-w@E�@D�@D�@DPH@C�W@C�0@Ct�@C>�@C+@CY@B�c@B��@B�<@B��@B^5@A�t@@w�@@Q�@@:�@@$@@@?��@?�W@?�@?�@>�L@>��@>+k@=�@=@<�@<�U@<��@<I�@;�@;��@; i@:��@:�b@:ff@:Q@:=q@:	@9�o@9�d@9�"@9�@8Ɇ@8y>@87@7�a@7�F@7�:@6��@6n�@6#:@5zx@5@4�/@4�D@46@4*�@4 �@4  @3��@3O@3�@2Ta@1�@10�@0�[@0h�@/��@/b�@/O@/>�@.��@.�F@.0U@.@-��@-:�@-#�@,�`@,`�@,7@+��@+��@+!-@*��@*�y@*ߤ@*�@*R�@)�7@)O�@)@@(�z@(C-@'��@'O@&��@&��@%��@%��@%�@%T�@$��@$�@$D�@$"h@$G@#��@#� @#��@#�P@#o�@#
=@"҉@"�h@"��@"�\@"kQ@"5?@"_@!ϫ@!w2@!!�@ ��@ ��@ r�@ <�@ 1@˒@9�@��@�6@�b@��@z@c @@��@��@s�@c�@S&@#�@u�@�@�w@qv@X�@H�@�@�@�@��@�s@�h@�+@�9@�n@�@rG@*0@�@�@��@�`@��@g8@K^@�@� @��@�a@�:@]�@RT@C�@=@'�@��@͟@�}@xl@C�@3�@.�@)�@e@��@u�@Vm@!�@�P@�@�@�v@�4@m�@]d@U2@K^@D�@x@��@g�@1�@�@�@S@��@�H@��@	@��@��@��@u�@��@��@��@~(@tT@l"@D�@�]@��@��@t�@U�@4�@ i@��@��@��@�'@��@v�@kQ@Q@?@J@ϫ@�S@a�@F@4@�@��@e�@:�@�@��@��@��@�@��@e�@+@@
=@�@
�c@
�@
��@
��@
��@
z@
a|@
-@

�@	�#@	�^@	zx@	?}@	G�@	B�@	�@ی1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B�B�RB�RB�mB�mB��B��B��B��B��B��B��B��B��B��B��B�sB�XB��B��B�UB��B�RB	ESB	�B	��B	��B	ɺB	��B
2B
eFB
��B
��B
��B
�+B
�nB
�B
�B
XBB�B(�B5?B;JBG�BcTB�hB�2B��B��B�-B��B�BuB�B$�B$�B:�BB[B?�B!�B�BB�B�B�0B��B��B��B�B�rB�=Bq�B%�BMB
�XB
�kB
��B
��B
��B
��B
��B
�]B
�=B
z�B
q�B
X�B
7�B
�B	��B	ܒB	��B	��B	�qB	�B	lWB	5�B	HB	MB��B�&BңB�	B�'B�<B�DB��B��B��B��BªB��B��B	<B	�B	B	CB	�B		B	B�B��B	�B	:B	�B	^B	�B	VB	�B	aB	dB	=qB	S&B	��B	�0B	��B	��B	��B	�gB	��B	��B	�aB	�bB	�FB	�8B	�B	�B	�B	��B	��B	�!B	��B	�IB	�#B	��B	�kB	�qB	�xB	�B	�IB	�xB	��B	��B	�<B	ɺB	�B	��B	�+B	�9B	��B	��B	��B	ߤB	�B	�LB	�fB	��B	�,B	��B	�B	��B	��B	�B	�B	��B	��B	�hB	��B	�B	�\B	ޞB	�/B	�CB	�)B	ܬB	ܬB	ܒB	��B	�B	�dB	�~B	��B	�B	�jB	ޞB	��B	�!B	��B	�;B	߾B	�;B	�B	޸B	ބB	ܬB	�]B	ܒB	�B	�bB	�TB	�`B	��B	�nB	�VB	�hB	��B	�2B	�B	�B	�B	�LB	�B	��B	��B	�ZB	�B	�ZB	�B	�|B	��B	�bB	�HB	�B	�B	�:B	��B	�&B	�:B	�ZB	�&B	�B	��B	��B	�B	�B	�B	�LB	�fB	�B	�>B	�B	�B	�KB	�B	�B	�_B	�B	��B	��B	�eB	��B	�0B	�B	�eB	��B	��B	�6B	��B	�qB	�)B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	�/B	�B	�B	�B	�B	��B	�!B	�UB	�B	�'B	��B	�-B	�B	�aB	�-B	�-B	��B	�B	�B	�nB	�TB	��B	�ZB	��B	�B	�B	�zB	��B	��B	�B	�2B	��B	��B	��B	�8B	��B	��B	��B	�	B	��B	��B	�^B	�^B	��B	��B	�B	�B	�B	�B	�6B	�B	�<B	�VB	�VB	��B	�]B	��B	�cB	��B	��B	��B	��B
  B
 iB
�B
'B
B
'B
[B
�B
�B
GB
�B
3B
�B
gB
�B
�B
9B
9B
�B
B
_B
_B
_B
EB
EB
�B
B
�B
fB
�B
	lB

=B

rB

�B
B
B
B
�B
B
�B
�B
B
B
B
�B
~B
�B
�B
�B
�B
0B
JB
PB
B
�B
�B
�B
�B
BB
(B
BB
B
�B
B
HB
�B
 B
�B
oB
�B
�B
B
&B
�B
�B
,B
,B
,B
�B
�B
�B
�B
�B
gB
gB
�B
�B
?B
�B
+B
EB
yB
�B
�B
�B
B
�B
�B
�B
7B
�B
WB
qB
�B
�B
]B
]B
CB
B
B
B
B
B
B
�B
�B
�B
 �B
 �B
 �B
!B
 �B
 �B
!�B
"B
#�B
#�B
#�B
#�B
#�B
#nB
#�B
#�B
$@B
%B
%�B
&B
%�B
&2B
&�B
&�B
'B
'B
'mB
'8B
'8B
'RB
'mB
'�B
'�B
(�B
(�B
)�B
)yB
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
*B
*0B
*0B
*0B
*�B
*�B
*�B
*�B
*�B
*�B
,WB
,qB
,�B
,qB
,qB
,�B
,�B
,�B
,�B
,�B
,�B
-]B
-�B
/B
.�B
/iB
0�B
1[B
1�B
1vB
1�B
2-B
2�B
2�B
3MB
3�B
4B
4B
4B
5B
5?B
5tB
5�B
6+B
6zB
6�B
72B
7�B
7�B
8�B
8�B
8�B
9�B
9�B
9�B
:B
:*B
:*B
:*B
:B
:DB
:�B
;0B
;B
;B
:�B
;0B
;�B
;�B
<B
<B
;�B
<6B
<�B
=<B
=VB
=VB
>]B
>(B
>(B
>B
>(B
>�B
>�B
>�B
?.B
?.B
?B
?HB
?}B
?�B
?�B
?�B
@ B
@iB
@iB
@�B
@�B
A B
A�B
BB
BuB
B�B
B�B
B�B
CGB
CGB
C{B
C�B
DB
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
EB
E�B
E�B
FYB
FYB
FtB
F�B
G+B
GzB
G�B
G�B
G�B
HB
HKB
IB
IB
J	B
I�B
J�B
J�B
J�B
KB
K)B
K^B
KDB
KxB
K�B
K�B
K�B
K�B
K�B
L�B
M6B
MB
MB
M�B
M�B
NB
N�B
OB
O(B
O(B
OB
OBB
OvB
O�B
O�B
P.B
PHB
P�B
P�B
Q4B
QhB
QhB
Q�B
RB
R:B
R�B
R�B
R�B
R�B
S@B
S�B
TB
T{B
T�B
T�B
T�B
U2B
U�B
U�B
U�B
VB
U�B
V�B
V�B
V�B
V�B
WYB
W�B
W�B
X+B
XEB
X+B
X+B
XEB
X+B
X+B
X+B
XEB
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
[#B
[�B
\B
\B
\)B
\CB
\�B
\�B
\�B
]�B
]�B
^B
^jB
^jB
^�B
^�B
^�B
^�B
^�B
_VB
_�B
_�B
_�B
`'B
`B
`B
`�B
`�B
`�B
abB
a�B
a�B
a�B
bB
bB
a�B
a�B
a�B
bhB
b�B
cB
cTB
c�B
c�B
d&B
dtB
d�B
d�B
d�B
d�B
eB
eFB
e,B
e�B
e�B
e�B
fLB
gB
g8B
gRB
g�B
h>B
hXB
hsB
hXB
hXB
h�B
iDB
i_B
i�B
i�B
i�B
jKB
jB
j�B
kQB
k�B
k�B
k�B
l"B
l�B
l�B
m)B
mwB
m�B
nIB
nIB
ncB
n�B
n�B
n�B
o5B
oiB
o�B
o�B
o�B
p!B
p!B
pUB
p�B
p�B
q'B
q'B
qvB
q�B
q�B
rB
r�B
s3B
s3B
sMB
sMB
shB
shB
s�B
tB
t9B
tTB
tTB
t9B
tTB
t�B
u%B
uZB
u�B
u�B
u�B
vB
vB
v+B
v+B
v+B
vFB
vFB
vFB
u�B
u�B
u�B
u�B
vB
v�B
v�B
v�B
wB
vzB
v�B
v�B
v�B
v�B
wB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
x8B
xB
x8B
xRB
x�B
x�B
x�B
y	B
yXB
yXB
yXB
yXB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z*B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
|B
|PB
|�B
}B
}�B
}�B
}�B
}�B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
B
HB
cB
cB
cB
cB
HB
HB
HB
cB
�B
�B
� B
��B
��B
��B
��B
��B
��B
��B
��B
�AB
�AB
�[B
�'B
�B
�uB
��B
��B
��B
��B
�B
�GB
�GB
�aB
�aB
�aB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�gB
��B
�B
�mB
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B�B�RB�RB�mB�mB��B��B��B��B��B��B��B��B��B��B��B�sB�XB��B��B�UB��B�RB	ESB	�B	��B	��B	ɺB	��B
2B
eFB
��B
��B
��B
�+B
�nB
�B
�B
XBB�B(�B5?B;JBG�BcTB�hB�2B��B��B�-B��B�BuB�B$�B$�B:�BB[B?�B!�B�BB�B�B�0B��B��B��B�B�rB�=Bq�B%�BMB
�XB
�kB
��B
��B
��B
��B
��B
�]B
�=B
z�B
q�B
X�B
7�B
�B	��B	ܒB	��B	��B	�qB	�B	lWB	5�B	HB	MB��B�&BңB�	B�'B�<B�DB��B��B��B��BªB��B��B	<B	�B	B	CB	�B		B	B�B��B	�B	:B	�B	^B	�B	VB	�B	aB	dB	=qB	S&B	��B	�0B	��B	��B	��B	�gB	��B	��B	�aB	�bB	�FB	�8B	�B	�B	�B	��B	��B	�!B	��B	�IB	�#B	��B	�kB	�qB	�xB	�B	�IB	�xB	��B	��B	�<B	ɺB	�B	��B	�+B	�9B	��B	��B	��B	ߤB	�B	�LB	�fB	��B	�,B	��B	�B	��B	��B	�B	�B	��B	��B	�hB	��B	�B	�\B	ޞB	�/B	�CB	�)B	ܬB	ܬB	ܒB	��B	�B	�dB	�~B	��B	�B	�jB	ޞB	��B	�!B	��B	�;B	߾B	�;B	�B	޸B	ބB	ܬB	�]B	ܒB	�B	�bB	�TB	�`B	��B	�nB	�VB	�hB	��B	�2B	�B	�B	�B	�LB	�B	��B	��B	�ZB	�B	�ZB	�B	�|B	��B	�bB	�HB	�B	�B	�:B	��B	�&B	�:B	�ZB	�&B	�B	��B	��B	�B	�B	�B	�LB	�fB	�B	�>B	�B	�B	�KB	�B	�B	�_B	�B	��B	��B	�eB	��B	�0B	�B	�eB	��B	��B	�6B	��B	�qB	�)B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	�/B	�B	�B	�B	�B	��B	�!B	�UB	�B	�'B	��B	�-B	�B	�aB	�-B	�-B	��B	�B	�B	�nB	�TB	��B	�ZB	��B	�B	�B	�zB	��B	��B	�B	�2B	��B	��B	��B	�8B	��B	��B	��B	�	B	��B	��B	�^B	�^B	��B	��B	�B	�B	�B	�B	�6B	�B	�<B	�VB	�VB	��B	�]B	��B	�cB	��B	��B	��B	��B
  B
 iB
�B
'B
B
'B
[B
�B
�B
GB
�B
3B
�B
gB
�B
�B
9B
9B
�B
B
_B
_B
_B
EB
EB
�B
B
�B
fB
�B
	lB

=B

rB

�B
B
B
B
�B
B
�B
�B
B
B
B
�B
~B
�B
�B
�B
�B
0B
JB
PB
B
�B
�B
�B
�B
BB
(B
BB
B
�B
B
HB
�B
 B
�B
oB
�B
�B
B
&B
�B
�B
,B
,B
,B
�B
�B
�B
�B
�B
gB
gB
�B
�B
?B
�B
+B
EB
yB
�B
�B
�B
B
�B
�B
�B
7B
�B
WB
qB
�B
�B
]B
]B
CB
B
B
B
B
B
B
�B
�B
�B
 �B
 �B
 �B
!B
 �B
 �B
!�B
"B
#�B
#�B
#�B
#�B
#�B
#nB
#�B
#�B
$@B
%B
%�B
&B
%�B
&2B
&�B
&�B
'B
'B
'mB
'8B
'8B
'RB
'mB
'�B
'�B
(�B
(�B
)�B
)yB
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
*B
*0B
*0B
*0B
*�B
*�B
*�B
*�B
*�B
*�B
,WB
,qB
,�B
,qB
,qB
,�B
,�B
,�B
,�B
,�B
,�B
-]B
-�B
/B
.�B
/iB
0�B
1[B
1�B
1vB
1�B
2-B
2�B
2�B
3MB
3�B
4B
4B
4B
5B
5?B
5tB
5�B
6+B
6zB
6�B
72B
7�B
7�B
8�B
8�B
8�B
9�B
9�B
9�B
:B
:*B
:*B
:*B
:B
:DB
:�B
;0B
;B
;B
:�B
;0B
;�B
;�B
<B
<B
;�B
<6B
<�B
=<B
=VB
=VB
>]B
>(B
>(B
>B
>(B
>�B
>�B
>�B
?.B
?.B
?B
?HB
?}B
?�B
?�B
?�B
@ B
@iB
@iB
@�B
@�B
A B
A�B
BB
BuB
B�B
B�B
B�B
CGB
CGB
C{B
C�B
DB
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
EB
E�B
E�B
FYB
FYB
FtB
F�B
G+B
GzB
G�B
G�B
G�B
HB
HKB
IB
IB
J	B
I�B
J�B
J�B
J�B
KB
K)B
K^B
KDB
KxB
K�B
K�B
K�B
K�B
K�B
L�B
M6B
MB
MB
M�B
M�B
NB
N�B
OB
O(B
O(B
OB
OBB
OvB
O�B
O�B
P.B
PHB
P�B
P�B
Q4B
QhB
QhB
Q�B
RB
R:B
R�B
R�B
R�B
R�B
S@B
S�B
TB
T{B
T�B
T�B
T�B
U2B
U�B
U�B
U�B
VB
U�B
V�B
V�B
V�B
V�B
WYB
W�B
W�B
X+B
XEB
X+B
X+B
XEB
X+B
X+B
X+B
XEB
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
[#B
[�B
\B
\B
\)B
\CB
\�B
\�B
\�B
]�B
]�B
^B
^jB
^jB
^�B
^�B
^�B
^�B
^�B
_VB
_�B
_�B
_�B
`'B
`B
`B
`�B
`�B
`�B
abB
a�B
a�B
a�B
bB
bB
a�B
a�B
a�B
bhB
b�B
cB
cTB
c�B
c�B
d&B
dtB
d�B
d�B
d�B
d�B
eB
eFB
e,B
e�B
e�B
e�B
fLB
gB
g8B
gRB
g�B
h>B
hXB
hsB
hXB
hXB
h�B
iDB
i_B
i�B
i�B
i�B
jKB
jB
j�B
kQB
k�B
k�B
k�B
l"B
l�B
l�B
m)B
mwB
m�B
nIB
nIB
ncB
n�B
n�B
n�B
o5B
oiB
o�B
o�B
o�B
p!B
p!B
pUB
p�B
p�B
q'B
q'B
qvB
q�B
q�B
rB
r�B
s3B
s3B
sMB
sMB
shB
shB
s�B
tB
t9B
tTB
tTB
t9B
tTB
t�B
u%B
uZB
u�B
u�B
u�B
vB
vB
v+B
v+B
v+B
vFB
vFB
vFB
u�B
u�B
u�B
u�B
vB
v�B
v�B
v�B
wB
vzB
v�B
v�B
v�B
v�B
wB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
x8B
xB
x8B
xRB
x�B
x�B
x�B
y	B
yXB
yXB
yXB
yXB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z*B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
|B
|PB
|�B
}B
}�B
}�B
}�B
}�B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
B
HB
cB
cB
cB
cB
HB
HB
HB
cB
�B
�B
� B
��B
��B
��B
��B
��B
��B
��B
��B
�AB
�AB
�[B
�'B
�B
�uB
��B
��B
��B
��B
�B
�GB
�GB
�aB
�aB
�aB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�gB
��B
�B
�mB
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104843  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172311  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172311  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172311                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022318  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022318  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610121506                      G�O�G�O�G�O�                