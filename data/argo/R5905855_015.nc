CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:13:20Z creation;2022-06-04T19:13:20Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191320  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��ޠ#1   @��3c��@.�\(��d�vȴ91   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`��Bg��BpffBw33B��B�  B�  B�33B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�ffB˙�B���B�  B�  B�  B�  B�  B�33B�33B�ffB�B���B�  C   C  C  C  C  C
  C  C  C  CL�C�fC  C�fC�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C633C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�C3DĀ D�� D�  D�@ DŃ3D��3D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�C3D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@���@���A z�A z�A@z�A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bg�RBp�BwQ�B�RB�\B�\B�B�B��)B�\B�\B��)B�\B�\B�\B�\B�\B�\B�\B���B�\B�\B�u�B˨�B��)B�\B�\B�\B�\B�\B�B�B�B�B�u�B��B��)B�\C �C�C�C�C�C
�C�C�C�CT{C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6:�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|!HC~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D{�D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#RD#��D$�D$�RD%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�D)D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�D)D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�D)DĀ�D���D� �D�@�Dń)D��)D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�D)D߀�D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D� �D�D)D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�*A��BA�ϫA��A��BA��dA���A�ɺA���A���A��zA��^A��HA���A���A��AවA�CA��	A��YA���A���A�A��A��YA���A��A�
=AߣnA���Aމ�A�A���A�7A���A�xlAԗ�A�
rAύ�A�J�A�خA�A�~(AƲ�A�ȀA�EA��/AônA�_�A���A���A��A�z�A��A��A��A���A���A��qA�gmA��A�hA�7A�[WA�6zA��A��'A�N�A�F�A��JA��A���A���A�A�h
A��A�C-A�YA��<A�}"A���A��A��sA�ʌA�ӏA��A���A�֡A��9A���A���A��fA�S�A�l�A��[A�xA~{�A|S�Ax֡Av��Ar��An�}Aj�XAi͟AhAd}�A`1�AZ��AX_�AU�AR�_AQT�APuAO6zAN%FAKN<AI��AG�=AD�AB�mA@��A=A9��A7ںA6iDA7bA7J�A5�$A3RTA1ѷA0(�A/:�A.\�A-��A,�vA,^5A+jA)�zA'��A'RTA&��A$\�A#E9A"��A!m�A �`A h�A�SAy>A�fA)�A4A-A{A��A'RA)_A)_A��A�QAqA=Ad�A>�ArGAK^AN�A��A$tA+�A��AIRAR�A�A��A�A�6Av`A�ADgA�A��A8�A� A��A
�&A
�!A
PHA	��A	|�A��A��A��AOA�WA4�AGEAz�A��AoiA�AqvA��Aw�A��A!-A)�A�A�aA ��A `�@��$@�|�@�~(@���@�V@���@�Z�@�H�@��/@��/@�S�@��@�\)@���@�Ta@�S�@���@�Z�@��9@�#�@�y>@�|�@���@�!�@�@�oi@�RT@�S�@��@�)�@���@��@�8@�V@�zx@�o@��2@�{�@��N@�*@�Y@��@���@�X�@�(@�B�@�
�@�P@��]@��@�c�@ݟV@�iD@�C@�h
@���@��@��@ۄM@��@�o�@��p@�Xy@�~�@֓u@�*�@�˒@�m]@�q�@�@Ӂ�@�oi@��@ї$@�^�@�2a@��@��@� i@���@�0U@��@ΔF@�V@�-@͸�@�m]@�Ĝ@�c�@ˠ'@�v�@Ɏ"@�@��U@ȃ�@�h
@�Q@�>B@�,=@�˒@�+@�%@��@��@�_@���@�<6@�S@��	@��	@��@İ!@�H@�@ó�@�k�@»�@�-@���@���@�a�@��]@�1@�e�@�+@���@�.�@��@��@@�S�@�%@��@�E�@��>@���@�K�@�@��[@���@�i�@�� @���@��f@�f�@�\)@��@���@���@�l"@�:�@�/�@�x@���@���@���@�n�@�1@��4@���@�Q�@�"h@���@���@��@��
@�6z@���@��@�=@���@���@��I@���@�J�@���@��[@��o@�i�@�@�@��w@�O�@���@���@�(�@�8�@�#:@���@�c�@�(�@���@��@�Dg@��@���@�?@���@�u�@��@�r�@�+k@���@��5@��U@���@�%�@�u@��@��N@���@�@O@�	l@��@��b@�{@�b�@�4@��@��@��@��,@���@�(�@��W@��F@���@�e�@�'�@��@�Z�@��@��X@�7L@���@���@�z@��@��Q@�@�w2@�.I@���@��h@��4@�g8@�4n@���@�T�@���@���@�8�@�ݘ@��-@��h@�[W@�%F@��c@��m@���@�S�@�5?@��@���@�ϫ@���@���@�<6@��M@�Ɇ@��m@��j@��D@�ff@�5?@���@���@�q@��@��,@��!@�kQ@��@��d@���@���@��@��U@�y>@�?@�1'@�@��T@��z@���@�`B@�A�@���@���@�c�@�O@���@�?}@�;@��@�_�@�J@���@���@�-w@���@���@�Ft@��g@���@���@�,�@�@@�(@��M@���@�`�@�~@��6@�J#@�	l@���@���@�xl@�V@�($@��@���@��$@�33@���@��b@��.@�Z�@�*�@���@��@��@�5�@��@���@��z@�q@�e@�&@~�@Y@@~�@~z@}�o@}�-@}�@}Vm@|�K@|V�@|2�@|1@{�F@{y�@{.I@z�@z��@zp;@z8�@y��@y�t@yx�@y�@x��@x�o@x6@w��@w"�@vOv@vu@uB�@t��@t��@tM@t~@s�@st�@s�@r�@r��@r1�@q�>@q��@r@q�@q}�@q%F@q!�@p�|@p��@p�@o��@n��@n�@m��@m?}@l��@l�u@lV�@l[�@lFt@lG@kƨ@kv`@kC@j��@j�@jl�@j�@i��@i#�@hC-@g�@gx@f��@f�r@e��@ehs@d�	@d��@dXy@c�r@co�@c(@b��@bJ�@b �@aX@a:�@a�@`��@`~@_��@_)_@^5?@]��@]��@]zx@]B�@\S�@[�w@[�$@[_p@[;d@[
=@Z�X@ZJ�@Y��@Y�@X��@W�;@W��@Wx@V��@U��@U�S@Uo @U%F@T�9@TD�@S�]@S��@SC�@Rs�@R
�@Qhs@P�v@P<�@O��@O�@NR�@Ne@M�@Mhs@Lѷ@L�@K�@K��@KW?@J�B@JB[@J-@I�@Io @H�?@H_@H�@G��@GO@Fߤ@F��@F_�@F�@E��@E\�@E@@D�)@D��@D%�@C�+@C��@Ct�@CW?@CK�@C>�@B��@B	@Aϫ@A��@A�n@A^�@Aq@@�@@֡@@��@@�@?�@?iD@?>�@?$t@? i@>�]@>��@>L0@>@=�@=��@=w2@=(�@<�[@<�@<h�@;�@;��@;��@;��@;v`@;E9@:��@:�s@:��@:��@:{�@:kQ@:L0@:�@9��@9u�@9 \@8֡@8��@8Ft@8'R@7ݘ@7�@7�4@7�@6ߤ@6��@6d�@6�@5�9@5|@5c�@5\�@5@4��@4M@3�]@3��@3;d@2�c@2҉@2��@2�\@2h
@2$�@1�N@1u�@1&�@0�E@0~(@0!@/��@/��@/!-@.�R@.�A@.\�@.+k@-��@-�@-�h@-zx@-4@,�|@,PH@+��@+�	@+X�@+"�@*�@*�R@*��@*6�@)�>@)�3@)��@)Y�@(�	@(��@(r�@(7�@(�@'�}@'�q@'��@'@&��@&��@&kQ@&;�@%��@%�^@%[W@%q@$��@$��@$Q�@#��@#��@#�:@#_p@#9�@"҉@"��@"��@"-@!�@!�@!<6@ �5@ �9@ y>@ 4n@�6@�$@x@.I@�X@#:@�T@��@c@e,@T�@�@֡@�@(�@�@��@v`@U�@�@z@^5@L0@;�@$�@��@(�@�@�_@�o@l"@N�@(�@@خ@�k@n/@F�@�@�@��@h
@	@�@�#@��@o @+@%@��@�	@�@u�@j@Q�@9X@�@�+@��@iD@/�@��@Ov@($@��@ԕ@��@c@O�@O�@V@�O@l"@ �@خ@��@'�@�@ں@҉@�@��@z@V@@�@�@�)@�d@��@�S@o @4@q@�f@ی@�U@e�@$@�@˒@��@�:@|�@S�@A�@�@(@
��@
��@
��@
�@
v�@
�@	ϫ@	��@	��@	^�@	5�@	�@�@��@�.@_@U2@_@"h@�@�@�g@� @�}@��@��@H�@8@�@�@S@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�*A��BA�ϫA��A��BA��dA���A�ɺA���A���A��zA��^A��HA���A���A��AවA�CA��	A��YA���A���A�A��A��YA���A��A�
=AߣnA���Aމ�A�A���A�7A���A�xlAԗ�A�
rAύ�A�J�A�خA�A�~(AƲ�A�ȀA�EA��/AônA�_�A���A���A��A�z�A��A��A��A���A���A��qA�gmA��A�hA�7A�[WA�6zA��A��'A�N�A�F�A��JA��A���A���A�A�h
A��A�C-A�YA��<A�}"A���A��A��sA�ʌA�ӏA��A���A�֡A��9A���A���A��fA�S�A�l�A��[A�xA~{�A|S�Ax֡Av��Ar��An�}Aj�XAi͟AhAd}�A`1�AZ��AX_�AU�AR�_AQT�APuAO6zAN%FAKN<AI��AG�=AD�AB�mA@��A=A9��A7ںA6iDA7bA7J�A5�$A3RTA1ѷA0(�A/:�A.\�A-��A,�vA,^5A+jA)�zA'��A'RTA&��A$\�A#E9A"��A!m�A �`A h�A�SAy>A�fA)�A4A-A{A��A'RA)_A)_A��A�QAqA=Ad�A>�ArGAK^AN�A��A$tA+�A��AIRAR�A�A��A�A�6Av`A�ADgA�A��A8�A� A��A
�&A
�!A
PHA	��A	|�A��A��A��AOA�WA4�AGEAz�A��AoiA�AqvA��Aw�A��A!-A)�A�A�aA ��A `�@��$@�|�@�~(@���@�V@���@�Z�@�H�@��/@��/@�S�@��@�\)@���@�Ta@�S�@���@�Z�@��9@�#�@�y>@�|�@���@�!�@�@�oi@�RT@�S�@��@�)�@���@��@�8@�V@�zx@�o@��2@�{�@��N@�*@�Y@��@���@�X�@�(@�B�@�
�@�P@��]@��@�c�@ݟV@�iD@�C@�h
@���@��@��@ۄM@��@�o�@��p@�Xy@�~�@֓u@�*�@�˒@�m]@�q�@�@Ӂ�@�oi@��@ї$@�^�@�2a@��@��@� i@���@�0U@��@ΔF@�V@�-@͸�@�m]@�Ĝ@�c�@ˠ'@�v�@Ɏ"@�@��U@ȃ�@�h
@�Q@�>B@�,=@�˒@�+@�%@��@��@�_@���@�<6@�S@��	@��	@��@İ!@�H@�@ó�@�k�@»�@�-@���@���@�a�@��]@�1@�e�@�+@���@�.�@��@��@@�S�@�%@��@�E�@��>@���@�K�@�@��[@���@�i�@�� @���@��f@�f�@�\)@��@���@���@�l"@�:�@�/�@�x@���@���@���@�n�@�1@��4@���@�Q�@�"h@���@���@��@��
@�6z@���@��@�=@���@���@��I@���@�J�@���@��[@��o@�i�@�@�@��w@�O�@���@���@�(�@�8�@�#:@���@�c�@�(�@���@��@�Dg@��@���@�?@���@�u�@��@�r�@�+k@���@��5@��U@���@�%�@�u@��@��N@���@�@O@�	l@��@��b@�{@�b�@�4@��@��@��@��,@���@�(�@��W@��F@���@�e�@�'�@��@�Z�@��@��X@�7L@���@���@�z@��@��Q@�@�w2@�.I@���@��h@��4@�g8@�4n@���@�T�@���@���@�8�@�ݘ@��-@��h@�[W@�%F@��c@��m@���@�S�@�5?@��@���@�ϫ@���@���@�<6@��M@�Ɇ@��m@��j@��D@�ff@�5?@���@���@�q@��@��,@��!@�kQ@��@��d@���@���@��@��U@�y>@�?@�1'@�@��T@��z@���@�`B@�A�@���@���@�c�@�O@���@�?}@�;@��@�_�@�J@���@���@�-w@���@���@�Ft@��g@���@���@�,�@�@@�(@��M@���@�`�@�~@��6@�J#@�	l@���@���@�xl@�V@�($@��@���@��$@�33@���@��b@��.@�Z�@�*�@���@��@��@�5�@��@���@��z@�q@�e@�&@~�@Y@@~�@~z@}�o@}�-@}�@}Vm@|�K@|V�@|2�@|1@{�F@{y�@{.I@z�@z��@zp;@z8�@y��@y�t@yx�@y�@x��@x�o@x6@w��@w"�@vOv@vu@uB�@t��@t��@tM@t~@s�@st�@s�@r�@r��@r1�@q�>@q��@r@q�@q}�@q%F@q!�@p�|@p��@p�@o��@n��@n�@m��@m?}@l��@l�u@lV�@l[�@lFt@lG@kƨ@kv`@kC@j��@j�@jl�@j�@i��@i#�@hC-@g�@gx@f��@f�r@e��@ehs@d�	@d��@dXy@c�r@co�@c(@b��@bJ�@b �@aX@a:�@a�@`��@`~@_��@_)_@^5?@]��@]��@]zx@]B�@\S�@[�w@[�$@[_p@[;d@[
=@Z�X@ZJ�@Y��@Y�@X��@W�;@W��@Wx@V��@U��@U�S@Uo @U%F@T�9@TD�@S�]@S��@SC�@Rs�@R
�@Qhs@P�v@P<�@O��@O�@NR�@Ne@M�@Mhs@Lѷ@L�@K�@K��@KW?@J�B@JB[@J-@I�@Io @H�?@H_@H�@G��@GO@Fߤ@F��@F_�@F�@E��@E\�@E@@D�)@D��@D%�@C�+@C��@Ct�@CW?@CK�@C>�@B��@B	@Aϫ@A��@A�n@A^�@Aq@@�@@֡@@��@@�@?�@?iD@?>�@?$t@? i@>�]@>��@>L0@>@=�@=��@=w2@=(�@<�[@<�@<h�@;�@;��@;��@;��@;v`@;E9@:��@:�s@:��@:��@:{�@:kQ@:L0@:�@9��@9u�@9 \@8֡@8��@8Ft@8'R@7ݘ@7�@7�4@7�@6ߤ@6��@6d�@6�@5�9@5|@5c�@5\�@5@4��@4M@3�]@3��@3;d@2�c@2҉@2��@2�\@2h
@2$�@1�N@1u�@1&�@0�E@0~(@0!@/��@/��@/!-@.�R@.�A@.\�@.+k@-��@-�@-�h@-zx@-4@,�|@,PH@+��@+�	@+X�@+"�@*�@*�R@*��@*6�@)�>@)�3@)��@)Y�@(�	@(��@(r�@(7�@(�@'�}@'�q@'��@'@&��@&��@&kQ@&;�@%��@%�^@%[W@%q@$��@$��@$Q�@#��@#��@#�:@#_p@#9�@"҉@"��@"��@"-@!�@!�@!<6@ �5@ �9@ y>@ 4n@�6@�$@x@.I@�X@#:@�T@��@c@e,@T�@�@֡@�@(�@�@��@v`@U�@�@z@^5@L0@;�@$�@��@(�@�@�_@�o@l"@N�@(�@@خ@�k@n/@F�@�@�@��@h
@	@�@�#@��@o @+@%@��@�	@�@u�@j@Q�@9X@�@�+@��@iD@/�@��@Ov@($@��@ԕ@��@c@O�@O�@V@�O@l"@ �@خ@��@'�@�@ں@҉@�@��@z@V@@�@�@�)@�d@��@�S@o @4@q@�f@ی@�U@e�@$@�@˒@��@�:@|�@S�@A�@�@(@
��@
��@
��@
�@
v�@
�@	ϫ@	��@	��@	^�@	5�@	�@�@��@�.@_@U2@_@"h@�@�@�g@� @�}@��@��@H�@8@�@�@S@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�&B	�TB	�TB	�oB	��B	�aB	��B	�@B	��B	��B	��B	��B	�B	��B	�nB	�6B	��B	��B	��B	�
B	�B	�B	�B	�;B	��B	�B	� B	�B	�B
3B	��B	�-B	��B	�	B	�SB	�B	��B	}"B	�B	��B	��B	ÖB	ϫB	�KB
jB
?�B
J�B
QB
Y�B
j�B
�oB
��B
ŢB
�nB?B�B#B)yB*�B)yBR�B_�B_�B]IB[�BZQBW�BU2BW�BxB�AB��Bx�Bt�Bo�Bf�BF�BZ�B�TB�+B�vBo5BI�B)�B
��B
�B
�B
�5B
��B
��B
wLB
f�B
^5B
C-B
!-B

�B	�<B	��B	�#B	��B	�GB	�CB	��B	�+B	|�B	]B	@�B	%�B	]B	�B	�B�B�XB�B�IB�B׍BѝB�B��B��B�HB��B��B�6B��B��B�jBخB��BԕB��BՁB��B�{BԯB�sB��BٚB�	BܒB�B��B�B��B�B�;B��B�RB	
	B	%`B	#�B	"�B	*KB	,�B	2aB	9�B	M�B	c�B	x�B	HB	pUB	mwB	qB	��B	��B	��B	��B	z�B	u�B	u�B	u�B	��B	�B	�6B	��B	��B	��B	żB	�B	��B	ƨB	�+B	��B	�zB	�B	��B	�B	��B	ʌB	�B	�1B	�EB	�B	��B	�'B	�IB	��B	�mB	��B	��B	��B	�WB	�;B	�XB	�EB	�B	�jB	��B	ɺB	�+B	ÖB	�gB	ȀB	�VB	�\B	��B	�pB	�<B	̈́B	�bB	ѷB	�oB	�&B	ңB	�oB	�B	�}B	��B	�bB	ЗB	��B	�&B	�[B	�B	өB	��B	�@B	�NB	��B	͟B	�B	�B	ʦB	�B	�PB	��B	��B	�}B	��B	��B	�<B	�8B	�vB	��B	��B	�-B	�B	��B	��B	�^B	ǔB	āB	�9B	�9B	��B	ªB	�=B	��B	�.B	�(B	��B	�B	�JB	�B	�B	��B	˒B	�)B	˒B	��B	�PB	��B	� B	ҽB	�aB	��B	�B	��B	چB	�]B	ݲB	�B	�B	�5B	��B	�B	��B	߾B	ޞB	��B	��B	ߊB	��B	��B	�VB	�VB	�;B	�VB	�VB	�'B	�BB	�B	��B	�bB	�|B	�B	��B	�4B	�hB	�hB	�B	�B	�:B	�B	��B	�&B	��B	�@B	��B	�B	��B	��B	�B	�fB	�fB	�B	��B	�B	�B	�0B	�B	�qB	�WB	�B	�/B	��B	�vB	�aB	�aB	�B	�B	��B	�B	�B	�3B	�B	��B	�B	�B	�TB	�TB	�nB	��B	��B	�B	�`B	��B	��B	��B	�B	�B	�B	��B	��B	�`B	�FB	�?B	�TB	�hB	��B	�B	�3B	�B	�%B	��B	�LB	��B	�RB	��B	�	B	��B	�lB	�8B	��B	��B	��B	�rB	��B	��B	�^B	��B	�JB	��B	��B	��B	�"B	�<B	��B	�]B	��B
 OB
 �B
'B
3B
�B
B
B
9B
�B
%B
�B
�B
�B
�B
1B
fB
fB
fB
�B
�B
�B
	B
	7B
	�B
	lB
	�B
	�B
	�B
	�B

XB

�B
�B
�B
xB
^B
�B
xB
DB
DB
�B
�B
�B
�B
�B
DB
xB
�B
JB
JB
�B
�B
�B
B
PB
�B
�B
"B
�B
�B
B
BB
�B
�B
B
.B
bB
�B
�B
�B
&B
FB
�B
�B
SB
�B
�B
B
B
eB
�B
�B
�B
B
QB
=B
B
B
dB
~B
�B
OB
�B
�B
VB
VB
�B
 'B
 �B
!B
!bB
!HB
!|B
!�B
!�B
!�B
!�B
!�B
"�B
"NB
"4B
"�B
"�B
"hB
"�B
#nB
#nB
#TB
#�B
#�B
#�B
$@B
$�B
$�B
%�B
%�B
&fB
&�B
&�B
&�B
'RB
'RB
'�B
(�B
)DB
)_B
)DB
)�B
)�B
)�B
)�B
*eB
+�B
,"B
,=B
,WB
,�B
-B
-)B
-wB
-�B
./B
.}B
/B
/�B
/�B
0B
0B
0UB
0�B
0�B
0�B
0�B
1'B
1AB
1'B
1AB
1vB
1�B
1�B
1�B
1�B
2B
2aB
2�B
2�B
3B
2�B
2-B
2|B
3B
3MB
3MB
49B
4�B
5%B
5tB
5�B
5�B
6zB
6+B
6`B
7�B
8�B
8�B
88B
7�B
8RB
9$B
9$B
8�B
9$B
8�B
8RB
9XB
:^B
:�B
;�B
<B
<6B
<�B
="B
="B
=VB
="B
=<B
=VB
=qB
=VB
=�B
>B
>�B
>�B
>�B
?}B
?�B
?�B
@B
@OB
@OB
@�B
@�B
@�B
@�B
@�B
A B
A;B
A�B
A�B
AUB
AB
A;B
AUB
A�B
BB
B'B
B'B
BB
B'B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
CGB
CGB
C-B
D3B
D�B
D�B
D�B
EmB
E�B
E�B
FtB
F�B
GB
GzB
G_B
G�B
G�B
H�B
H�B
IRB
IlB
I�B
J	B
JXB
J�B
J�B
J�B
KxB
K�B
LB
K�B
LJB
LdB
MB
MB
MB
M6B
M�B
N<B
NpB
N�B
N�B
O(B
O�B
O�B
O�B
PHB
PHB
P�B
P�B
Q B
Q4B
Q�B
Q�B
RB
R:B
RTB
RoB
RTB
R�B
SuB
S�B
S�B
S�B
S�B
TB
T,B
T,B
T,B
TaB
UB
UMB
UgB
UgB
U�B
U�B
U�B
VB
VB
V9B
V�B
VmB
V�B
V�B
V�B
WYB
W�B
W�B
W�B
XB
X+B
X+B
XEB
X_B
X_B
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
YB
Y�B
Y�B
Y�B
Y�B
Z7B
ZkB
ZkB
Z�B
Z�B
[#B
[=B
[�B
[�B
\)B
\]B
\)B
\�B
\�B
\�B
]B
]dB
]�B
]�B
]�B
^B
^B
^B
^OB
^�B
^�B
_!B
_VB
_�B
_�B
`B
`�B
`�B
a-B
abB
abB
a�B
a�B
a�B
a�B
a�B
bB
bB
b�B
b�B
c B
c B
cTB
cnB
c�B
c�B
c�B
dZB
dZB
dtB
d�B
e,B
e,B
e`B
ezB
e�B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
f�B
gB
g8B
g�B
g�B
g�B
g�B
hXB
h�B
h�B
h�B
h�B
iB
iyB
i_B
i_B
i�B
jB
j0B
j�B
j�B
kB
k6B
kkB
k�B
k�B
lB
l=B
l�B
mB
m)B
mCB
m�B
m�B
m�B
m�B
m�B
n/B
n�B
n�B
n�B
o B
oB
oOB
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q[B
qvB
qvB
qvB
q�B
q�B
q�B
q�B
rB
rGB
r-B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
tnB
t�B
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u�B
u�B
u�B
v`B
vzB
v`B
v`B
vFB
v`B
v�B
v�B
v�B
v�B
wB
w�B
w�B
w�B
w�B
xB
x�B
x�B
yXB
yrB
yrB
y�B
y�B
y�B
z*B
z^B
zxB
z�B
z�B
z�B
{JB
{dB
{�B
|B
{�B
|B
|B
|PB
|�B
|PB
|6B
|6B
|6B
|6B
|6B
|jB
|�B
|�B
|�B
|�B
}"B
}VB
}<B
}<B
}�B
~B
}�B
~B
~]B
~wB
~�B
~�B
~�B
~�B
B
.B
cB
�B
}B
�B
�B
�B
�B
�B
� B
�4B
�4B
�4B
�4B
�4B
�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�&B	�TB	�TB	�oB	��B	�aB	��B	�@B	��B	��B	��B	��B	�B	��B	�nB	�6B	��B	��B	��B	�
B	�B	�B	�B	�;B	��B	�B	� B	�B	�B
3B	��B	�-B	��B	�	B	�SB	�B	��B	}"B	�B	��B	��B	ÖB	ϫB	�KB
jB
?�B
J�B
QB
Y�B
j�B
�oB
��B
ŢB
�nB?B�B#B)yB*�B)yBR�B_�B_�B]IB[�BZQBW�BU2BW�BxB�AB��Bx�Bt�Bo�Bf�BF�BZ�B�TB�+B�vBo5BI�B)�B
��B
�B
�B
�5B
��B
��B
wLB
f�B
^5B
C-B
!-B

�B	�<B	��B	�#B	��B	�GB	�CB	��B	�+B	|�B	]B	@�B	%�B	]B	�B	�B�B�XB�B�IB�B׍BѝB�B��B��B�HB��B��B�6B��B��B�jBخB��BԕB��BՁB��B�{BԯB�sB��BٚB�	BܒB�B��B�B��B�B�;B��B�RB	
	B	%`B	#�B	"�B	*KB	,�B	2aB	9�B	M�B	c�B	x�B	HB	pUB	mwB	qB	��B	��B	��B	��B	z�B	u�B	u�B	u�B	��B	�B	�6B	��B	��B	��B	żB	�B	��B	ƨB	�+B	��B	�zB	�B	��B	�B	��B	ʌB	�B	�1B	�EB	�B	��B	�'B	�IB	��B	�mB	��B	��B	��B	�WB	�;B	�XB	�EB	�B	�jB	��B	ɺB	�+B	ÖB	�gB	ȀB	�VB	�\B	��B	�pB	�<B	̈́B	�bB	ѷB	�oB	�&B	ңB	�oB	�B	�}B	��B	�bB	ЗB	��B	�&B	�[B	�B	өB	��B	�@B	�NB	��B	͟B	�B	�B	ʦB	�B	�PB	��B	��B	�}B	��B	��B	�<B	�8B	�vB	��B	��B	�-B	�B	��B	��B	�^B	ǔB	āB	�9B	�9B	��B	ªB	�=B	��B	�.B	�(B	��B	�B	�JB	�B	�B	��B	˒B	�)B	˒B	��B	�PB	��B	� B	ҽB	�aB	��B	�B	��B	چB	�]B	ݲB	�B	�B	�5B	��B	�B	��B	߾B	ޞB	��B	��B	ߊB	��B	��B	�VB	�VB	�;B	�VB	�VB	�'B	�BB	�B	��B	�bB	�|B	�B	��B	�4B	�hB	�hB	�B	�B	�:B	�B	��B	�&B	��B	�@B	��B	�B	��B	��B	�B	�fB	�fB	�B	��B	�B	�B	�0B	�B	�qB	�WB	�B	�/B	��B	�vB	�aB	�aB	�B	�B	��B	�B	�B	�3B	�B	��B	�B	�B	�TB	�TB	�nB	��B	��B	�B	�`B	��B	��B	��B	�B	�B	�B	��B	��B	�`B	�FB	�?B	�TB	�hB	��B	�B	�3B	�B	�%B	��B	�LB	��B	�RB	��B	�	B	��B	�lB	�8B	��B	��B	��B	�rB	��B	��B	�^B	��B	�JB	��B	��B	��B	�"B	�<B	��B	�]B	��B
 OB
 �B
'B
3B
�B
B
B
9B
�B
%B
�B
�B
�B
�B
1B
fB
fB
fB
�B
�B
�B
	B
	7B
	�B
	lB
	�B
	�B
	�B
	�B

XB

�B
�B
�B
xB
^B
�B
xB
DB
DB
�B
�B
�B
�B
�B
DB
xB
�B
JB
JB
�B
�B
�B
B
PB
�B
�B
"B
�B
�B
B
BB
�B
�B
B
.B
bB
�B
�B
�B
&B
FB
�B
�B
SB
�B
�B
B
B
eB
�B
�B
�B
B
QB
=B
B
B
dB
~B
�B
OB
�B
�B
VB
VB
�B
 'B
 �B
!B
!bB
!HB
!|B
!�B
!�B
!�B
!�B
!�B
"�B
"NB
"4B
"�B
"�B
"hB
"�B
#nB
#nB
#TB
#�B
#�B
#�B
$@B
$�B
$�B
%�B
%�B
&fB
&�B
&�B
&�B
'RB
'RB
'�B
(�B
)DB
)_B
)DB
)�B
)�B
)�B
)�B
*eB
+�B
,"B
,=B
,WB
,�B
-B
-)B
-wB
-�B
./B
.}B
/B
/�B
/�B
0B
0B
0UB
0�B
0�B
0�B
0�B
1'B
1AB
1'B
1AB
1vB
1�B
1�B
1�B
1�B
2B
2aB
2�B
2�B
3B
2�B
2-B
2|B
3B
3MB
3MB
49B
4�B
5%B
5tB
5�B
5�B
6zB
6+B
6`B
7�B
8�B
8�B
88B
7�B
8RB
9$B
9$B
8�B
9$B
8�B
8RB
9XB
:^B
:�B
;�B
<B
<6B
<�B
="B
="B
=VB
="B
=<B
=VB
=qB
=VB
=�B
>B
>�B
>�B
>�B
?}B
?�B
?�B
@B
@OB
@OB
@�B
@�B
@�B
@�B
@�B
A B
A;B
A�B
A�B
AUB
AB
A;B
AUB
A�B
BB
B'B
B'B
BB
B'B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
CGB
CGB
C-B
D3B
D�B
D�B
D�B
EmB
E�B
E�B
FtB
F�B
GB
GzB
G_B
G�B
G�B
H�B
H�B
IRB
IlB
I�B
J	B
JXB
J�B
J�B
J�B
KxB
K�B
LB
K�B
LJB
LdB
MB
MB
MB
M6B
M�B
N<B
NpB
N�B
N�B
O(B
O�B
O�B
O�B
PHB
PHB
P�B
P�B
Q B
Q4B
Q�B
Q�B
RB
R:B
RTB
RoB
RTB
R�B
SuB
S�B
S�B
S�B
S�B
TB
T,B
T,B
T,B
TaB
UB
UMB
UgB
UgB
U�B
U�B
U�B
VB
VB
V9B
V�B
VmB
V�B
V�B
V�B
WYB
W�B
W�B
W�B
XB
X+B
X+B
XEB
X_B
X_B
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
YB
Y�B
Y�B
Y�B
Y�B
Z7B
ZkB
ZkB
Z�B
Z�B
[#B
[=B
[�B
[�B
\)B
\]B
\)B
\�B
\�B
\�B
]B
]dB
]�B
]�B
]�B
^B
^B
^B
^OB
^�B
^�B
_!B
_VB
_�B
_�B
`B
`�B
`�B
a-B
abB
abB
a�B
a�B
a�B
a�B
a�B
bB
bB
b�B
b�B
c B
c B
cTB
cnB
c�B
c�B
c�B
dZB
dZB
dtB
d�B
e,B
e,B
e`B
ezB
e�B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
f�B
gB
g8B
g�B
g�B
g�B
g�B
hXB
h�B
h�B
h�B
h�B
iB
iyB
i_B
i_B
i�B
jB
j0B
j�B
j�B
kB
k6B
kkB
k�B
k�B
lB
l=B
l�B
mB
m)B
mCB
m�B
m�B
m�B
m�B
m�B
n/B
n�B
n�B
n�B
o B
oB
oOB
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q[B
qvB
qvB
qvB
q�B
q�B
q�B
q�B
rB
rGB
r-B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
tnB
t�B
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u�B
u�B
u�B
v`B
vzB
v`B
v`B
vFB
v`B
v�B
v�B
v�B
v�B
wB
w�B
w�B
w�B
w�B
xB
x�B
x�B
yXB
yrB
yrB
y�B
y�B
y�B
z*B
z^B
zxB
z�B
z�B
z�B
{JB
{dB
{�B
|B
{�B
|B
|B
|PB
|�B
|PB
|6B
|6B
|6B
|6B
|6B
|jB
|�B
|�B
|�B
|�B
}"B
}VB
}<B
}<B
}�B
~B
}�B
~B
~]B
~wB
~�B
~�B
~�B
~�B
B
.B
cB
�B
}B
�B
�B
�B
�B
�B
� B
�4B
�4B
�4B
�4B
�4B
�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105229  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191320  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191320  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191320                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041328  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041328  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                