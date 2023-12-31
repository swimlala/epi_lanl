CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:27:05Z creation;2022-06-04T17:27:05Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ͱ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20220604172705  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @����1   @��3c��@.
=p���c;C��%1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB33B��B(  B0  B8  B@  BH  BPffBV  B_��Bh  Bp  Bx  B�  B���B���B���B�  B�  B���B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  C �C�C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C#��C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3D y�D ��D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�3D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D��3D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ D�|�D�� D�  D�@ D߀ D�� D�  D�@ D�� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���A z�A z�A@z�A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�BQ�B�RB(�B0�B8�B@�BH�BP�BV�B_�RBh�Bp�Bx�B�\B��)B��)B���B�\B�\B��)B�\B��)B��)B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�\B�\B�\B�\B�\B�\B�B�B�B�B�\B�\C !HC!HC�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"!HC#�HC%�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT!HCV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��
C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��
D {�D ��D��DRD�RD�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D/��D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6RD6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS�RDT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc{�Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D�)D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D�)D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D���D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D�)D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D��)D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�D�}�D���D� �D�@�D߀�D���D� �D�@�D���D��)D�)D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A���A���A��A��A�x�A�#:A� �A�ѷA��'A��&A��mA��rA��A�hA�!�A�	A���A��"A��A���A�W�AދA�ÖAژ�A�f2A��A�8RA�8�A�}�Aη�A��A�y	A�S�A�pA��A���A���A���A���A��BA�s�A���A�OBA���A�\)A�v+A�^5A��MA�`BA���A�s�A��nA�n�A���A�uA��A�bA�یA���A�w�A�g�A���A���A�R�A�VA�A�o�A�-�A���A�ޞA���A�6�A�|�A�CA~=Au�Al�yAe��AaS�A[�6AQC�AK�QAG��AE<6AB�QAA�AA�A>A9�OA7]dA5�rA5*�A3�fA/�A.<�A-�A+�
A+��A,��A*��A)�A'��A&)_A${JA#��A"v�A ��A ~A ��A B[A }VA �DA &�AU�A�FA�PA��A!�A��A�[A�UAu�A:�A�A�[A��A�gA�A�mA��AϫA�<A�hA��A��AM�A��A�A��A,=A��AɆA��A�A�zA��ADgAj�A�5A.IA��A~(AA��A2�AZA
=A��A_A�A��A�$A�A:*A~�AL�A��A\�A�A�6A��A?}A�}A{�A�A�A��AU2A� A|�A	A
�AA
�A	��A	ZA�FAqA͟A��A�A҉A	lA�>A��A�Av`A��AQAxA͟A��A�uA�4A-A �A ��A c�@��W@���@���@��"@�h�@�K�@��.@� \@�M@��-@��2@�C�@�e�@�%@���@�1�@�h@�4@�q�@�ݘ@�H�@�xl@�
�@�w@�t�@�@�4@�l�@�� @�	@�!-@���@�"h@�M@�C@�?�@�U�@��@�;@�Ɇ@濱@��m@��p@沖@���@�r@�y>@�
�@��@�qv@�q@�;@�6@��@�@�-w@��@���@��U@�8�@ݒ:@ܾ@�?�@�s�@��P@ڬ@�&�@ٔ�@ح�@�	�@���@׼@�+@ְ�@֕@��}@��X@�B[@��K@ӓ@�b�@��@�ƨ@�Mj@��@�q�@���@�@�c @��@�]�@��@���@��@́o@�)�@��3@�6z@���@��@ʏ\@�A�@ɤ@@�+@��8@�֡@Ȣ4@���@�|�@��@ƃ�@�IR@ı�@ĕ�@�tT@��@��#@å�@�j�@�+@�H�@��@�0�@�%F@��@���@�c @�+k@��z@�v`@�j�@�Y�@�.I@��@�_�@��@���@�A�@�͟@��j@�\)@�33@���@��@�Mj@�#�@��@�$t@�4@��y@�n�@���@���@�`B@�s�@�+k@���@�Mj@���@��9@���@�v�@�GE@�4@��j@��S@�rG@�>�@��`@���@��z@�9�@��'@��W@�g�@��]@�@�@��@��e@�6@��z@��	@�Vm@���@�"h@���@���@��6@��
@��H@�|�@�E9@�&�@�z�@��@��q@�IR@���@�M@�ϫ@�u�@���@��@���@�8�@��@���@�|�@�>B@��[@��@���@�o @�2a@��@���@�_�@�A�@�1�@��@���@�4@�S�@��o@��Q@���@��6@��K@��P@�7L@���@��@�p;@�#:@��@���@�T�@�:�@�+@��B@��O@���@�u%@��@��~@���@���@�tT@��r@��k@���@�F�@��`@�� @�`�@�-@��+@��K@��H@���@�A @�@�ߤ@���@�q@���@��=@�`B@�+�@�F�@�8@�?}@��8@�Ĝ@��}@��@��@��T@�ݘ@���@���@�dZ@�A�@���@�]d@�!�@��@�خ@���@�:�@��@��@��@��@���@�c @��>@�m]@�V@���@��4@�y>@�*�@�@��A@��9@���@���@�j�@�^�@�F�@��)@���@�Ta@�6�@�e@���@��z@��M@�'�@��O@���@�I�@���@�ݘ@���@�N<@�%@�ȴ@���@�M�@��@�@�t�@�\)@�C�@��@���@���@�M@���@��@�P�@��@��@��r@�[�@�D�@�G@�@��@�@~�,@~��@~{�@~3�@}��@}��@}&�@|��@|��@|�_@|e�@{�;@{>�@{�@z�@zxl@z($@z@z�@z_@y�7@xl"@w�4@we�@w>�@w!-@v��@vV@u��@u�@t�p@t��@t|�@tN�@t2�@t@s�@sl�@r��@r)�@q�j@q�@q�@q�@q5�@p�@p]d@p,=@p$@p1@o�V@oO@n�h@n.�@m�n@m5�@l�@l�u@l'R@k� @k;d@k�@jV@i��@i�@h�e@h�_@hl"@h,=@g��@g'�@f��@f�H@fc @e�@e8�@d��@d�U@d*�@c��@b��@b8�@b5?@b�@a�H@`��@`��@`�.@`��@`4n@`�@_��@^�]@^�+@^Z�@^@�@^	@]�@]�@]��@]}�@\�I@\r�@[�K@[S�@[S�@[�@Z�b@Z}V@Zc @Z?@Y�Z@Y�M@YO�@X�`@XS�@X�@W��@W�V@W�@V�1@U�@UY�@U�@T�9@Tu�@T[�@TC-@T�@S��@S�6@S�}@S|�@S1�@S(@R�2@R��@Rv�@Q�@Qc@QJ�@Q�@PZ@O��@Oa@N�"@N�6@N�F@Nq�@N-@M�@Lѷ@L�@K+@J�2@J��@J�x@J�@I&�@H��@G��@G�V@G@F�]@F�B@F��@F��@E�@E�@Ek�@E!�@D��@D��@D��@D�@DV�@C�f@Cv`@CS@B�H@Bȴ@B��@Bn�@A�N@@Ɇ@@��@@|�@@e�@@I�@@b@?iD@>�"@>�@>.�@>
�@>�@=�Z@=��@<�5@<j@<@;v`@;�@:͟@:v�@:�@9<6@9%@8�`@8�e@8�u@8Xy@7��@7!-@65?@5��@5�@4�I@4H@3�@3��@3X�@3C@3�@2ߤ@2W�@2@�@1��@1�t@1�~@1^�@0��@0Ɇ@0��@0�Y@0S�@/��@/�4@/y�@/n/@/s@/O@/E9@/�@.͟@.��@.�r@.8�@-��@-�@-�@-�3@-��@-��@-\�@,�O@,2�@+�r@+� @+��@+�:@+~�@+4�@*�c@*��@*�b@*R�@*+k@)�@)p�@)N<@)�@(��@(h�@(Ft@(1'@(�@'�r@'�@'�:@'l�@';d@'�@&{�@&6�@&-@&@%��@%�>@%�z@%�'@%IR@$��@$�U@$�j@$�@$bN@$/�@$	�@#��@#��@#&@"�@"�]@"�@"u%@"Ov@"4@!��@!�=@!`B@!�@!%@!	l@!�@!	l@ �@ A�@ x@�A@�&@�;@�Q@��@��@�{@e�@_p@E9@8@�@��@�!@�@�@�-@c@o @X@5�@@��@�e@��@7�@�A@��@�F@�@~�@b�@=@"�@�X@^5@��@��@��@��@}�@e,@O�@�@�/@�@U2@C-@@��@�@ƨ@x@P�@@O@�@�y@�,@�,@ȴ@��@ff@3�@-@?@;�@4@�@��@��@�t@��@�C@5�@��@��@��@�9@��@<�@�@��@��@�;@��@��@��@�f@{J@o�@Y@��@h
@YK@0U@�@�#@��@Q�@�5@�.@PH@/�@"h@@�@��@K�@�@�R@�!@� @Q@5?@�@
�@�>@��@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A���A���A��A��A�x�A�#:A� �A�ѷA��'A��&A��mA��rA��A�hA�!�A�	A���A��"A��A���A�W�AދA�ÖAژ�A�f2A��A�8RA�8�A�}�Aη�A��A�y	A�S�A�pA��A���A���A���A���A��BA�s�A���A�OBA���A�\)A�v+A�^5A��MA�`BA���A�s�A��nA�n�A���A�uA��A�bA�یA���A�w�A�g�A���A���A�R�A�VA�A�o�A�-�A���A�ޞA���A�6�A�|�A�CA~=Au�Al�yAe��AaS�A[�6AQC�AK�QAG��AE<6AB�QAA�AA�A>A9�OA7]dA5�rA5*�A3�fA/�A.<�A-�A+�
A+��A,��A*��A)�A'��A&)_A${JA#��A"v�A ��A ~A ��A B[A }VA �DA &�AU�A�FA�PA��A!�A��A�[A�UAu�A:�A�A�[A��A�gA�A�mA��AϫA�<A�hA��A��AM�A��A�A��A,=A��AɆA��A�A�zA��ADgAj�A�5A.IA��A~(AA��A2�AZA
=A��A_A�A��A�$A�A:*A~�AL�A��A\�A�A�6A��A?}A�}A{�A�A�A��AU2A� A|�A	A
�AA
�A	��A	ZA�FAqA͟A��A�A҉A	lA�>A��A�Av`A��AQAxA͟A��A�uA�4A-A �A ��A c�@��W@���@���@��"@�h�@�K�@��.@� \@�M@��-@��2@�C�@�e�@�%@���@�1�@�h@�4@�q�@�ݘ@�H�@�xl@�
�@�w@�t�@�@�4@�l�@�� @�	@�!-@���@�"h@�M@�C@�?�@�U�@��@�;@�Ɇ@濱@��m@��p@沖@���@�r@�y>@�
�@��@�qv@�q@�;@�6@��@�@�-w@��@���@��U@�8�@ݒ:@ܾ@�?�@�s�@��P@ڬ@�&�@ٔ�@ح�@�	�@���@׼@�+@ְ�@֕@��}@��X@�B[@��K@ӓ@�b�@��@�ƨ@�Mj@��@�q�@���@�@�c @��@�]�@��@���@��@́o@�)�@��3@�6z@���@��@ʏ\@�A�@ɤ@@�+@��8@�֡@Ȣ4@���@�|�@��@ƃ�@�IR@ı�@ĕ�@�tT@��@��#@å�@�j�@�+@�H�@��@�0�@�%F@��@���@�c @�+k@��z@�v`@�j�@�Y�@�.I@��@�_�@��@���@�A�@�͟@��j@�\)@�33@���@��@�Mj@�#�@��@�$t@�4@��y@�n�@���@���@�`B@�s�@�+k@���@�Mj@���@��9@���@�v�@�GE@�4@��j@��S@�rG@�>�@��`@���@��z@�9�@��'@��W@�g�@��]@�@�@��@��e@�6@��z@��	@�Vm@���@�"h@���@���@��6@��
@��H@�|�@�E9@�&�@�z�@��@��q@�IR@���@�M@�ϫ@�u�@���@��@���@�8�@��@���@�|�@�>B@��[@��@���@�o @�2a@��@���@�_�@�A�@�1�@��@���@�4@�S�@��o@��Q@���@��6@��K@��P@�7L@���@��@�p;@�#:@��@���@�T�@�:�@�+@��B@��O@���@�u%@��@��~@���@���@�tT@��r@��k@���@�F�@��`@�� @�`�@�-@��+@��K@��H@���@�A @�@�ߤ@���@�q@���@��=@�`B@�+�@�F�@�8@�?}@��8@�Ĝ@��}@��@��@��T@�ݘ@���@���@�dZ@�A�@���@�]d@�!�@��@�خ@���@�:�@��@��@��@��@���@�c @��>@�m]@�V@���@��4@�y>@�*�@�@��A@��9@���@���@�j�@�^�@�F�@��)@���@�Ta@�6�@�e@���@��z@��M@�'�@��O@���@�I�@���@�ݘ@���@�N<@�%@�ȴ@���@�M�@��@�@�t�@�\)@�C�@��@���@���@�M@���@��@�P�@��@��@��r@�[�@�D�@�G@�@��@�@~�,@~��@~{�@~3�@}��@}��@}&�@|��@|��@|�_@|e�@{�;@{>�@{�@z�@zxl@z($@z@z�@z_@y�7@xl"@w�4@we�@w>�@w!-@v��@vV@u��@u�@t�p@t��@t|�@tN�@t2�@t@s�@sl�@r��@r)�@q�j@q�@q�@q�@q5�@p�@p]d@p,=@p$@p1@o�V@oO@n�h@n.�@m�n@m5�@l�@l�u@l'R@k� @k;d@k�@jV@i��@i�@h�e@h�_@hl"@h,=@g��@g'�@f��@f�H@fc @e�@e8�@d��@d�U@d*�@c��@b��@b8�@b5?@b�@a�H@`��@`��@`�.@`��@`4n@`�@_��@^�]@^�+@^Z�@^@�@^	@]�@]�@]��@]}�@\�I@\r�@[�K@[S�@[S�@[�@Z�b@Z}V@Zc @Z?@Y�Z@Y�M@YO�@X�`@XS�@X�@W��@W�V@W�@V�1@U�@UY�@U�@T�9@Tu�@T[�@TC-@T�@S��@S�6@S�}@S|�@S1�@S(@R�2@R��@Rv�@Q�@Qc@QJ�@Q�@PZ@O��@Oa@N�"@N�6@N�F@Nq�@N-@M�@Lѷ@L�@K+@J�2@J��@J�x@J�@I&�@H��@G��@G�V@G@F�]@F�B@F��@F��@E�@E�@Ek�@E!�@D��@D��@D��@D�@DV�@C�f@Cv`@CS@B�H@Bȴ@B��@Bn�@A�N@@Ɇ@@��@@|�@@e�@@I�@@b@?iD@>�"@>�@>.�@>
�@>�@=�Z@=��@<�5@<j@<@;v`@;�@:͟@:v�@:�@9<6@9%@8�`@8�e@8�u@8Xy@7��@7!-@65?@5��@5�@4�I@4H@3�@3��@3X�@3C@3�@2ߤ@2W�@2@�@1��@1�t@1�~@1^�@0��@0Ɇ@0��@0�Y@0S�@/��@/�4@/y�@/n/@/s@/O@/E9@/�@.͟@.��@.�r@.8�@-��@-�@-�@-�3@-��@-��@-\�@,�O@,2�@+�r@+� @+��@+�:@+~�@+4�@*�c@*��@*�b@*R�@*+k@)�@)p�@)N<@)�@(��@(h�@(Ft@(1'@(�@'�r@'�@'�:@'l�@';d@'�@&{�@&6�@&-@&@%��@%�>@%�z@%�'@%IR@$��@$�U@$�j@$�@$bN@$/�@$	�@#��@#��@#&@"�@"�]@"�@"u%@"Ov@"4@!��@!�=@!`B@!�@!%@!	l@!�@!	l@ �@ A�@ x@�A@�&@�;@�Q@��@��@�{@e�@_p@E9@8@�@��@�!@�@�@�-@c@o @X@5�@@��@�e@��@7�@�A@��@�F@�@~�@b�@=@"�@�X@^5@��@��@��@��@}�@e,@O�@�@�/@�@U2@C-@@��@�@ƨ@x@P�@@O@�@�y@�,@�,@ȴ@��@ff@3�@-@?@;�@4@�@��@��@�t@��@�C@5�@��@��@��@�9@��@<�@�@��@��@�;@��@��@��@�f@{J@o�@Y@��@h
@YK@0U@�@�#@��@Q�@�5@�.@PH@/�@"h@@�@��@K�@�@�R@�!@� @Q@5?@�@
�@�>@��@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B�nB��B��B��B�AB��B�BG_B��B�BB�,B�6B��B��B��B̳B��B�aB	_�B	��B	�nB	��B	�}B	��B	�B	��B	��B	�]B	�hB	�B	�B	��B	�VB	�;B
�B
�B
�?B
��B
��B
��B
��B
�B
�"B%�B;�BJ#BU�Bk�Bj�Bp�By$B\�B_�BX_B3B6FB@OB��B�*B�B�~Bz^B_�B5�B
�+B
�IB
R�B
+QB
*�B
!B
@B
�B	��B	�B	�@B	�nB	kkB	F�B	-�B	}B��B�hB��B�"B�fB��B�+B��B��B�jB�,B��B�{B�RB��B��B��B�`B��BÖB�%B��BÖB�qB�XB��B��B�bB	 �B		lB	QB	#nB	(�B	*�B	(�B	%B	:B	N"B	^OB	d&B	iDB	l"B	s3B	y	B	��B	�=B	��B	��B	��B	��B	�eB	��B	��B	�tB	�B	��B	��B	�B	�+B	��B	ɆB	�	B	��B	̳B	�B	�B	�6B	��B	�\B	�(B	ϑB	��B	�4B	��B	�?B	�eB	خB	خB	��B	�YB	�YB	�$B	ּB	��B	�?B	��B	�SB	�B	��B	��B	�!B	�dB	�#B	�=B	�B	�,B	�`B	�B	��B	�B	��B	�B	�B	�B	��B	�RB	�B	��B	�B	��B	�`B	�B	��B	�vB	߾B	ބB	��B	�OB	�VB	�jB	�jB	ބB	ބB	��B	�~B	�OB	��B	�B	�IB	��B	�~B	��B	�~B	��B	�jB	ݲB	��B	ݲB	�OB	�OB	ݲB	�B	޸B	�jB	ބB	�VB	��B	�'B	�vB	�'B	��B	��B	��B	�VB	�\B	�B	�B	�B	��B	߾B	�vB	�NB	�B	�B	��B	�B	��B	��B	�bB	�FB	�B	�B	��B	�bB	�B	��B	��B	�B	�B	�bB	�'B	�4B	��B	�B	��B	�B	�B	�4B	�B	��B	�B	�&B	�fB	�B	�FB	�:B	�B	�2B	�RB	�RB	�RB	�B	�B	�LB	�B	�B	�B	�8B	�mB	�eB	��B	�B	�B	��B	�B	��B	�_B	�B	�B	�B	�kB	�WB	�=B	�QB	�B	�B	�/B	�IB	�cB	�B	��B	��B	�B	�B	�AB	�UB	��B	�B	��B	��B	�!B	�GB	��B	��B	�B	�hB	�B	�B	�nB	�B	�aB	�aB	��B	��B	�B	��B	�B	�B	�B	�B	�MB	�MB	�B	�B	�|B	�B	�GB	�|B	�-B	�3B	�B	�B	��B	��B	��B	��B	�	B	�8B	�fB	��B	��B	��B	�xB	�^B	��B	��B	�<B	�VB	��B	��B	�BB	��B	��B	��B	��B	��B	�BB	��B	��B	�qB	�VB	�B	�0B	��B	��B	��B	�HB
 �B
�B
3B
�B
�B
%B
gB
�B
�B
gB
B
B
B
�B
�B
�B
�B
�B
�B
zB
�B
	B

�B

rB
)B
�B
0B
dB
~B
dB
B
<B
"B
"B
�B
�B
�B
�B
�B
�B
HB
bB
bB
 B
�B
�B
�B
�B
NB
�B
�B
�B
B
:B
�B
�B
B
&B
&B
[B
@B
B
{B
uB
�B
,B
aB
�B
�B
MB
�B
gB
�B
�B
9B
�B
�B
YB
?B
B
EB
yB
B
�B
�B
KB
�B
�B
�B
OB
 'B
 �B
 �B
!HB
!-B
!bB
!�B
!�B
!�B
"B
"hB
"�B
"�B
"�B
"�B
#B
"�B
"�B
"�B
$@B
%B
%�B
&B
&B
%�B
&LB
&2B
'B
'�B
($B
(>B
(sB
(�B
(�B
)*B
)_B
)�B
)�B
)�B
)�B
*0B
*B
*�B
+B
+QB
+kB
+�B
,B
-B
,�B
,�B
./B
.cB
-�B
-�B
.cB
.�B
/iB
/OB
/5B
/�B
/�B
0B
0oB
0UB
0UB
0oB
0�B
0!B
/B
-�B
-wB
-�B
-�B
.IB
/ B
/�B
0;B
0�B
0�B
2|B
3�B
4B
4B
49B
4�B
4�B
5?B
5tB
5ZB
5�B
5�B
6`B
6FB
6�B
8B
8RB
8�B
9$B
9	B
9	B
8�B
9$B
9�B
9�B
9�B
9�B
9�B
:B
:�B
;dB
;dB
;B
;dB
;dB
;dB
;dB
;dB
;dB
;�B
;�B
<6B
<6B
<6B
<6B
<B
<PB
<�B
<�B
<�B
<�B
<�B
=B
=B
=VB
=�B
=�B
=�B
=�B
>(B
>]B
>wB
>�B
>�B
?HB
?�B
?�B
@B
@B
@iB
@iB
@�B
AoB
AoB
AoB
A;B
A�B
B�B
B�B
C-B
C�B
C�B
C{B
CaB
DMB
D�B
D�B
EmB
EB
EB
E9B
EB
D�B
D�B
D3B
C�B
C�B
C�B
C�B
C�B
C�B
DB
D�B
E�B
E�B
FB
E�B
E�B
FYB
GB
GEB
G_B
GzB
G�B
H�B
HfB
H�B
IB
IB
IRB
I�B
I�B
I�B
J	B
IRB
IB
H�B
H�B
H�B
IB
IB
IB
H�B
H�B
IB
H�B
H�B
IB
I�B
I�B
JXB
J#B
J�B
KB
K�B
K^B
KxB
LJB
MB
M�B
N<B
N"B
N�B
N�B
OvB
PbB
QB
P�B
QNB
RB
R�B
R�B
R�B
R�B
S&B
S&B
S@B
SB
S&B
S�B
S�B
TB
TFB
T�B
T{B
T�B
T{B
T�B
UgB
UgB
U�B
U�B
U�B
U�B
U�B
V9B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
YB
YB
Y�B
Z7B
Z�B
[#B
[=B
[�B
\xB
\xB
\CB
\xB
\]B
\�B
]~B
^B
_!B
^jB
^�B
^�B
_VB
_B
_VB
_�B
_�B
`\B
`�B
`�B
`�B
`�B
`�B
aB
a|B
b�B
b�B
b�B
b�B
b�B
cnB
c�B
d&B
d@B
d�B
d�B
d�B
eB
d�B
d�B
d�B
ezB
fB
ffB
ffB
f�B
f�B
f�B
f�B
g�B
g�B
h$B
hsB
h�B
h�B
h�B
i*B
i_B
iDB
i�B
jB
j0B
jB
j�B
j�B
j�B
kB
kB
kB
kB
k6B
k6B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mCB
m�B
m�B
m�B
m�B
n/B
nB
n/B
nB
n}B
n�B
n�B
nIB
n}B
n�B
oB
oB
oiB
oOB
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
qB
qB
qAB
qAB
qAB
qAB
qAB
q[B
q�B
q�B
q�B
rB
r-B
r�B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
sMB
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
utB
u�B
u�B
u�B
u�B
u�B
v+B
v�B
v�B
v�B
wB
w2B
wLB
wLB
wLB
w�B
w�B
wLB
wLB
w2B
wB
w2B
wLB
wLB
wfB
wfB
w�B
xB
x�B
x�B
x�B
x�B
x�B
y$B
y$B
y>B
y>B
y$B
y>B
yrB
yXB
yXB
y�B
yrB
yXB
y�B
zxB
z^B
zxB
z�B
z�B
z�B
z�B
z�B
{B
{�B
{�B
|B
{�B
|B
{�B
|B
|PB
|�B
|�B
|�B
|�B
}"B
}VB
}�B
}�B
~BB
~BB
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B�nB��B��B��B�AB��B�BG_B��B�BB�,B�6B��B��B��B̳B��B�aB	_�B	��B	�nB	��B	�}B	��B	�B	��B	��B	�]B	�hB	�B	�B	��B	�VB	�;B
�B
�B
�?B
��B
��B
��B
��B
�B
�"B%�B;�BJ#BU�Bk�Bj�Bp�By$B\�B_�BX_B3B6FB@OB��B�*B�B�~Bz^B_�B5�B
�+B
�IB
R�B
+QB
*�B
!B
@B
�B	��B	�B	�@B	�nB	kkB	F�B	-�B	}B��B�hB��B�"B�fB��B�+B��B��B�jB�,B��B�{B�RB��B��B��B�`B��BÖB�%B��BÖB�qB�XB��B��B�bB	 �B		lB	QB	#nB	(�B	*�B	(�B	%B	:B	N"B	^OB	d&B	iDB	l"B	s3B	y	B	��B	�=B	��B	��B	��B	��B	�eB	��B	��B	�tB	�B	��B	��B	�B	�+B	��B	ɆB	�	B	��B	̳B	�B	�B	�6B	��B	�\B	�(B	ϑB	��B	�4B	��B	�?B	�eB	خB	خB	��B	�YB	�YB	�$B	ּB	��B	�?B	��B	�SB	�B	��B	��B	�!B	�dB	�#B	�=B	�B	�,B	�`B	�B	��B	�B	��B	�B	�B	�B	��B	�RB	�B	��B	�B	��B	�`B	�B	��B	�vB	߾B	ބB	��B	�OB	�VB	�jB	�jB	ބB	ބB	��B	�~B	�OB	��B	�B	�IB	��B	�~B	��B	�~B	��B	�jB	ݲB	��B	ݲB	�OB	�OB	ݲB	�B	޸B	�jB	ބB	�VB	��B	�'B	�vB	�'B	��B	��B	��B	�VB	�\B	�B	�B	�B	��B	߾B	�vB	�NB	�B	�B	��B	�B	��B	��B	�bB	�FB	�B	�B	��B	�bB	�B	��B	��B	�B	�B	�bB	�'B	�4B	��B	�B	��B	�B	�B	�4B	�B	��B	�B	�&B	�fB	�B	�FB	�:B	�B	�2B	�RB	�RB	�RB	�B	�B	�LB	�B	�B	�B	�8B	�mB	�eB	��B	�B	�B	��B	�B	��B	�_B	�B	�B	�B	�kB	�WB	�=B	�QB	�B	�B	�/B	�IB	�cB	�B	��B	��B	�B	�B	�AB	�UB	��B	�B	��B	��B	�!B	�GB	��B	��B	�B	�hB	�B	�B	�nB	�B	�aB	�aB	��B	��B	�B	��B	�B	�B	�B	�B	�MB	�MB	�B	�B	�|B	�B	�GB	�|B	�-B	�3B	�B	�B	��B	��B	��B	��B	�	B	�8B	�fB	��B	��B	��B	�xB	�^B	��B	��B	�<B	�VB	��B	��B	�BB	��B	��B	��B	��B	��B	�BB	��B	��B	�qB	�VB	�B	�0B	��B	��B	��B	�HB
 �B
�B
3B
�B
�B
%B
gB
�B
�B
gB
B
B
B
�B
�B
�B
�B
�B
�B
zB
�B
	B

�B

rB
)B
�B
0B
dB
~B
dB
B
<B
"B
"B
�B
�B
�B
�B
�B
�B
HB
bB
bB
 B
�B
�B
�B
�B
NB
�B
�B
�B
B
:B
�B
�B
B
&B
&B
[B
@B
B
{B
uB
�B
,B
aB
�B
�B
MB
�B
gB
�B
�B
9B
�B
�B
YB
?B
B
EB
yB
B
�B
�B
KB
�B
�B
�B
OB
 'B
 �B
 �B
!HB
!-B
!bB
!�B
!�B
!�B
"B
"hB
"�B
"�B
"�B
"�B
#B
"�B
"�B
"�B
$@B
%B
%�B
&B
&B
%�B
&LB
&2B
'B
'�B
($B
(>B
(sB
(�B
(�B
)*B
)_B
)�B
)�B
)�B
)�B
*0B
*B
*�B
+B
+QB
+kB
+�B
,B
-B
,�B
,�B
./B
.cB
-�B
-�B
.cB
.�B
/iB
/OB
/5B
/�B
/�B
0B
0oB
0UB
0UB
0oB
0�B
0!B
/B
-�B
-wB
-�B
-�B
.IB
/ B
/�B
0;B
0�B
0�B
2|B
3�B
4B
4B
49B
4�B
4�B
5?B
5tB
5ZB
5�B
5�B
6`B
6FB
6�B
8B
8RB
8�B
9$B
9	B
9	B
8�B
9$B
9�B
9�B
9�B
9�B
9�B
:B
:�B
;dB
;dB
;B
;dB
;dB
;dB
;dB
;dB
;dB
;�B
;�B
<6B
<6B
<6B
<6B
<B
<PB
<�B
<�B
<�B
<�B
<�B
=B
=B
=VB
=�B
=�B
=�B
=�B
>(B
>]B
>wB
>�B
>�B
?HB
?�B
?�B
@B
@B
@iB
@iB
@�B
AoB
AoB
AoB
A;B
A�B
B�B
B�B
C-B
C�B
C�B
C{B
CaB
DMB
D�B
D�B
EmB
EB
EB
E9B
EB
D�B
D�B
D3B
C�B
C�B
C�B
C�B
C�B
C�B
DB
D�B
E�B
E�B
FB
E�B
E�B
FYB
GB
GEB
G_B
GzB
G�B
H�B
HfB
H�B
IB
IB
IRB
I�B
I�B
I�B
J	B
IRB
IB
H�B
H�B
H�B
IB
IB
IB
H�B
H�B
IB
H�B
H�B
IB
I�B
I�B
JXB
J#B
J�B
KB
K�B
K^B
KxB
LJB
MB
M�B
N<B
N"B
N�B
N�B
OvB
PbB
QB
P�B
QNB
RB
R�B
R�B
R�B
R�B
S&B
S&B
S@B
SB
S&B
S�B
S�B
TB
TFB
T�B
T{B
T�B
T{B
T�B
UgB
UgB
U�B
U�B
U�B
U�B
U�B
V9B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
YB
YB
Y�B
Z7B
Z�B
[#B
[=B
[�B
\xB
\xB
\CB
\xB
\]B
\�B
]~B
^B
_!B
^jB
^�B
^�B
_VB
_B
_VB
_�B
_�B
`\B
`�B
`�B
`�B
`�B
`�B
aB
a|B
b�B
b�B
b�B
b�B
b�B
cnB
c�B
d&B
d@B
d�B
d�B
d�B
eB
d�B
d�B
d�B
ezB
fB
ffB
ffB
f�B
f�B
f�B
f�B
g�B
g�B
h$B
hsB
h�B
h�B
h�B
i*B
i_B
iDB
i�B
jB
j0B
jB
j�B
j�B
j�B
kB
kB
kB
kB
k6B
k6B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mCB
m�B
m�B
m�B
m�B
n/B
nB
n/B
nB
n}B
n�B
n�B
nIB
n}B
n�B
oB
oB
oiB
oOB
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
qB
qB
qAB
qAB
qAB
qAB
qAB
q[B
q�B
q�B
q�B
rB
r-B
r�B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
sMB
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
utB
u�B
u�B
u�B
u�B
u�B
v+B
v�B
v�B
v�B
wB
w2B
wLB
wLB
wLB
w�B
w�B
wLB
wLB
w2B
wB
w2B
wLB
wLB
wfB
wfB
w�B
xB
x�B
x�B
x�B
x�B
x�B
y$B
y$B
y>B
y>B
y$B
y>B
yrB
yXB
yXB
y�B
yrB
yXB
y�B
zxB
z^B
zxB
z�B
z�B
z�B
z�B
z�B
{B
{�B
{�B
|B
{�B
|B
{�B
|B
|PB
|�B
|�B
|�B
|�B
}"B
}VB
}�B
}�B
~BB
~BB
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104852  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172705  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172705  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172705                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022713  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022713  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                