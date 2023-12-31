CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:57:00Z creation;2022-06-04T17:57:00Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175700  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               EA   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�Q���1   @�Q�>F�@/��E����c�`A�71   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&�C(  C*  C,  C.  C0�C2�C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx�fDy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D���D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @5�@���@���A z�A z�A@z�A`z�A�=qA�=qA�=qA�=qA�
>A�p�A�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�\B�\B�\B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B��)B��)B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$!HC&!HC(�C*�C,�C.�C0!HC2!HC4�C5�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL!HCN!HCP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"RD"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DNRDN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Df��Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dp��Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx�RDy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D���D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��)D���D� �D�@�D�`�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�e,A�d�A�d�A�c�A�`vA�_pA�a|A�a|A�b�A�c�A�f�A�q�A�(�AɂAɢhAɜCA�u�A� 'A��A��HA���A�ŢAȾBAȱ�Aȟ�AȓuAȐ�Aȏ\AȆ�A�tAȁ�Aȇ+AȂA�oiA�P�A�I�A��"AſA��HA�{�A��DA���A�%FA��A�2aA��A��.A��wA�y	A�G�A��A���A���A��A��3A�!bA� �A��A��2A�s�A��A�-wA���A���A��VA���A��
A��A�MA��A���A���A���A�`vA�GA���A���A�e�A��nA�ܒA���A�e�A�	7A��"A~Ayb�Av�"At_Aq]dAj��Aa�AY8�AS��AP�AKt�AHxAD��ABjA@�SA@XyA?��A>	A;jA;7A9֡A8��A8��A7�KA7�A5B�A2�6A1��A04�A/_pA/J�A/ffA/�!A/��A0bNA0�A2��A2b�A2��A2�5A2�A2A1L�A0t�A0qA/q�A/�A.�A.g8A,\)A*��A)��A(�HA'��A'_A'aA&�A%��A%R�A$�'A$_A%a|A$��A"p�A!qvA!�A ��A ��A ��A :�A�A�fA4AxAqvA��A#:A�A�5A��AB[AA-wAm�A�A&�AW�A;�A��A��A�]A��A  APHA�Aa|A��A�A��AC-A�MA�?A?}A?�AVmAXyA>�A
�rA	��A	;dA�	A�HAe,A,=A��A��A��AxlAGEA#�A�8A�'Av`A�A��AffA33AȴA�\Au�A>BA�3A��A=qA�A+kA��A��A�A �rA7A�A �`A �kA I�@��@�+�@��'@��.@��$@���@��b@���@��@��V@��_@�GE@��m@��N@��c@��@�˒@���@�@��@��@@��@�@�G�@���@�-w@���@�V@�M�@�1�@���@��`@�Ĝ@�7@�@�4@��@��@�V@�:�@��a@�	l@��&@��@�b�@�u%@ޡb@�/�@�  @ݩ*@�=@ܥz@�o�@ه�@�Z�@�X@�Y�@�2a@�֡@ؐ.@�bN@�6�@��d@�"�@�@��@��9@�J�@��@ԔF@���@���@���@҈�@�*�@�x@�b�@о�@��'@���@���@��X@�^5@��o@ϲ-@�U�@��@�z�@���@�<6@̮}@��@ˮ�@��>@��o@˵t@�"�@�l�@ɔ�@�p�@��@�D�@ǭC@�2a@ƴ9@��@�q@�4@��@Ų�@�b�@�A�@�+�@�.I@�h
@�1@�S&@��@�!�@���@���@�J�@�*0@�
=@���@�@�Dg@��@�@�K�@� \@���@��?@��u@���@���@�a�@�$t@��X@�1�@�˒@�e,@�S�@���@���@�i�@��S@��@���@�Dg@���@���@��o@�/�@���@�ȴ@��@���@�o @��@�y>@��@��P@�K�@��@��v@���@�($@��]@���@�@���@�~�@�@�@���@�K�@���@���@�N�@��z@�7L@��@��@��@�Y@��/@�I�@��-@�S�@�:�@�;@���@�7�@���@�O@�/�@��@�g8@��@��j@��'@�qv@�6z@��j@��@�Ft@��@�r�@��@���@���@��@��m@�ϫ@��3@���@�	l@��@�Ov@�(�@��@�ϫ@���@��@��@�S�@�7@���@�qv@�@��)@��1@�>B@��}@�dZ@��M@��p@��@�-�@��D@��@���@�a@��c@���@���@�|�@�:�@�"h@��@���@�P�@�ѷ@��_@�h�@�.�@���@��N@���@�N<@��@���@��@��W@�خ@�j�@�V@��)@��b@�h
@�3�@��@���@�x@�Y�@�%F@�(@��@�֡@��'@���@�_�@��@���@���@�qv@�K�@�@�Ɇ@�� @�Q�@�:*@��@��W@��*@�qv@��@��@���@��z@��r@�q�@���@���@��P@�o @�C�@���@�W�@�@���@���@�P�@�1�@���@�h
@�6@��]@��@��f@�X�@�"�@���@�`�@�9X@�7@���@���@�*0@���@��b@�(�@��Z@�ƨ@��V@�rG@�O@��@��@���@��I@�I�@��@��@@~�R@~�F@~��@~W�@~4@}�@}��@}!�@|�E@|��@|�@{�0@{RT@z҉@z�@y��@y�@y��@y��@y�@yG�@x�@x��@x7�@w�V@w{J@w_p@w;d@v�,@v��@v�@vC�@u�#@u�h@u5�@t�[@tN�@t,=@s�m@s��@sH�@r�8@r��@r��@re@q��@q�@q��@qm]@qA @q�@pɆ@p�.@pw�@p  @o��@ox@nߤ@n{�@m��@m�@l�@k�@k��@kX�@j��@jJ�@i�3@h��@h��@hc�@g��@g�@f��@fn�@e�T@e2a@d�@c�
@c_p@cS�@c@b��@bR�@a�z@aDg@`�@`r�@_��@_E9@^�X@^}V@^	@]�@]c@]Dg@\�E@\2�@[��@[��@[�{@[RT@Z�'@Zh
@Z@Y�-@Y:�@Y!�@X��@XtT@X?�@W�m@W�@W@O@W�@V��@V�B@VZ�@U��@U��@Up�@U+@Tѷ@T��@T"h@S�w@S�k@Sj�@SC@R�H@R�1@RkQ@Q�N@Q��@Qx�@QF@P��@P�@O��@N�6@Nv�@N	@M�z@M��@M��@M��@M�7@M�7@Mc@Mf�@M+�@M+@Lی@L�@L��@LH@K�&@K��@KMj@J��@J�@J�@J@�@I�d@Ik�@I?}@H�@H��@Hr�@H(�@G��@G��@GE9@F�"@F�'@F�1@FTa@F#:@E�Z@E�S@D�`@D��@DQ�@C�@C�[@Cl�@C)_@Bߤ@B~�@A�@Ap�@AX@AJ�@A5�@A@@�`@@Ɇ@@�$@@�@@<�@?��@?l�@?!-@?�@>�@>�h@>&�@=�3@=��@=�'@=s�@=?}@=+�@<�@<��@<c�@<7�@<�@;�@;��@;�@:��@:1�@9�@9�^@9u�@9*0@8֡@8~(@8M@89X@8-�@7˒@7j�@7>�@6��@5��@5�t@5S&@4��@4tT@4!@3�r@3�}@3��@3��@3�@2��@2� @2B[@1�@1��@1��@1IR@0�D@0 �@/��@/�	@/dZ@/+@/S@.��@.�@.R�@.:*@.�@.u@-�@-f�@-�@,��@,�@,c�@,/�@,�@+�@+��@+��@+��@++@*^5@)��@)��@)��@)Vm@(�E@(�I@( �@'�0@'y�@&�@&��@&�@%�@%[W@%2a@$�@$��@$�@$>B@#�@#C�@"��@"�1@":*@!�o@!��@!?}@ ��@ �@ �@ ��@ h�@�m@K�@)_@Y@ں@��@s�@ff@#:@�^@�h@|@�@�E@�4@w�@�@��@��@F�@.I@@�]@�h@\�@�9@��@��@S&@A @5�@&�@��@��@`�@2�@7@�+@خ@��@{J@n/@J#@��@�@-@ϫ@�S@k�@/@;@~(@q@oi@`�@D�@��@�V@Z�@@ں@��@��@��@��@l�@�@��@zx@7L@�@��@_@PH@H@C-@>B@7�@1'@(�@7@@�@P�@1�@�@��@��@�\@{�@i�@Z�@Q@L0@8�@��@�=@��@�@�@V@�@��@j@N�@*�@	�@�w@~�@iD@X�@K�@,�@!-@�@
҉@
�'@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�e,A�d�A�d�A�c�A�`vA�_pA�a|A�a|A�b�A�c�A�f�A�q�A�(�AɂAɢhAɜCA�u�A� 'A��A��HA���A�ŢAȾBAȱ�Aȟ�AȓuAȐ�Aȏ\AȆ�A�tAȁ�Aȇ+AȂA�oiA�P�A�I�A��"AſA��HA�{�A��DA���A�%FA��A�2aA��A��.A��wA�y	A�G�A��A���A���A��A��3A�!bA� �A��A��2A�s�A��A�-wA���A���A��VA���A��
A��A�MA��A���A���A���A�`vA�GA���A���A�e�A��nA�ܒA���A�e�A�	7A��"A~Ayb�Av�"At_Aq]dAj��Aa�AY8�AS��AP�AKt�AHxAD��ABjA@�SA@XyA?��A>	A;jA;7A9֡A8��A8��A7�KA7�A5B�A2�6A1��A04�A/_pA/J�A/ffA/�!A/��A0bNA0�A2��A2b�A2��A2�5A2�A2A1L�A0t�A0qA/q�A/�A.�A.g8A,\)A*��A)��A(�HA'��A'_A'aA&�A%��A%R�A$�'A$_A%a|A$��A"p�A!qvA!�A ��A ��A ��A :�A�A�fA4AxAqvA��A#:A�A�5A��AB[AA-wAm�A�A&�AW�A;�A��A��A�]A��A  APHA�Aa|A��A�A��AC-A�MA�?A?}A?�AVmAXyA>�A
�rA	��A	;dA�	A�HAe,A,=A��A��A��AxlAGEA#�A�8A�'Av`A�A��AffA33AȴA�\Au�A>BA�3A��A=qA�A+kA��A��A�A �rA7A�A �`A �kA I�@��@�+�@��'@��.@��$@���@��b@���@��@��V@��_@�GE@��m@��N@��c@��@�˒@���@�@��@��@@��@�@�G�@���@�-w@���@�V@�M�@�1�@���@��`@�Ĝ@�7@�@�4@��@��@�V@�:�@��a@�	l@��&@��@�b�@�u%@ޡb@�/�@�  @ݩ*@�=@ܥz@�o�@ه�@�Z�@�X@�Y�@�2a@�֡@ؐ.@�bN@�6�@��d@�"�@�@��@��9@�J�@��@ԔF@���@���@���@҈�@�*�@�x@�b�@о�@��'@���@���@��X@�^5@��o@ϲ-@�U�@��@�z�@���@�<6@̮}@��@ˮ�@��>@��o@˵t@�"�@�l�@ɔ�@�p�@��@�D�@ǭC@�2a@ƴ9@��@�q@�4@��@Ų�@�b�@�A�@�+�@�.I@�h
@�1@�S&@��@�!�@���@���@�J�@�*0@�
=@���@�@�Dg@��@�@�K�@� \@���@��?@��u@���@���@�a�@�$t@��X@�1�@�˒@�e,@�S�@���@���@�i�@��S@��@���@�Dg@���@���@��o@�/�@���@�ȴ@��@���@�o @��@�y>@��@��P@�K�@��@��v@���@�($@��]@���@�@���@�~�@�@�@���@�K�@���@���@�N�@��z@�7L@��@��@��@�Y@��/@�I�@��-@�S�@�:�@�;@���@�7�@���@�O@�/�@��@�g8@��@��j@��'@�qv@�6z@��j@��@�Ft@��@�r�@��@���@���@��@��m@�ϫ@��3@���@�	l@��@�Ov@�(�@��@�ϫ@���@��@��@�S�@�7@���@�qv@�@��)@��1@�>B@��}@�dZ@��M@��p@��@�-�@��D@��@���@�a@��c@���@���@�|�@�:�@�"h@��@���@�P�@�ѷ@��_@�h�@�.�@���@��N@���@�N<@��@���@��@��W@�خ@�j�@�V@��)@��b@�h
@�3�@��@���@�x@�Y�@�%F@�(@��@�֡@��'@���@�_�@��@���@���@�qv@�K�@�@�Ɇ@�� @�Q�@�:*@��@��W@��*@�qv@��@��@���@��z@��r@�q�@���@���@��P@�o @�C�@���@�W�@�@���@���@�P�@�1�@���@�h
@�6@��]@��@��f@�X�@�"�@���@�`�@�9X@�7@���@���@�*0@���@��b@�(�@��Z@�ƨ@��V@�rG@�O@��@��@���@��I@�I�@��@��@@~�R@~�F@~��@~W�@~4@}�@}��@}!�@|�E@|��@|�@{�0@{RT@z҉@z�@y��@y�@y��@y��@y�@yG�@x�@x��@x7�@w�V@w{J@w_p@w;d@v�,@v��@v�@vC�@u�#@u�h@u5�@t�[@tN�@t,=@s�m@s��@sH�@r�8@r��@r��@re@q��@q�@q��@qm]@qA @q�@pɆ@p�.@pw�@p  @o��@ox@nߤ@n{�@m��@m�@l�@k�@k��@kX�@j��@jJ�@i�3@h��@h��@hc�@g��@g�@f��@fn�@e�T@e2a@d�@c�
@c_p@cS�@c@b��@bR�@a�z@aDg@`�@`r�@_��@_E9@^�X@^}V@^	@]�@]c@]Dg@\�E@\2�@[��@[��@[�{@[RT@Z�'@Zh
@Z@Y�-@Y:�@Y!�@X��@XtT@X?�@W�m@W�@W@O@W�@V��@V�B@VZ�@U��@U��@Up�@U+@Tѷ@T��@T"h@S�w@S�k@Sj�@SC@R�H@R�1@RkQ@Q�N@Q��@Qx�@QF@P��@P�@O��@N�6@Nv�@N	@M�z@M��@M��@M��@M�7@M�7@Mc@Mf�@M+�@M+@Lی@L�@L��@LH@K�&@K��@KMj@J��@J�@J�@J@�@I�d@Ik�@I?}@H�@H��@Hr�@H(�@G��@G��@GE9@F�"@F�'@F�1@FTa@F#:@E�Z@E�S@D�`@D��@DQ�@C�@C�[@Cl�@C)_@Bߤ@B~�@A�@Ap�@AX@AJ�@A5�@A@@�`@@Ɇ@@�$@@�@@<�@?��@?l�@?!-@?�@>�@>�h@>&�@=�3@=��@=�'@=s�@=?}@=+�@<�@<��@<c�@<7�@<�@;�@;��@;�@:��@:1�@9�@9�^@9u�@9*0@8֡@8~(@8M@89X@8-�@7˒@7j�@7>�@6��@5��@5�t@5S&@4��@4tT@4!@3�r@3�}@3��@3��@3�@2��@2� @2B[@1�@1��@1��@1IR@0�D@0 �@/��@/�	@/dZ@/+@/S@.��@.�@.R�@.:*@.�@.u@-�@-f�@-�@,��@,�@,c�@,/�@,�@+�@+��@+��@+��@++@*^5@)��@)��@)��@)Vm@(�E@(�I@( �@'�0@'y�@&�@&��@&�@%�@%[W@%2a@$�@$��@$�@$>B@#�@#C�@"��@"�1@":*@!�o@!��@!?}@ ��@ �@ �@ ��@ h�@�m@K�@)_@Y@ں@��@s�@ff@#:@�^@�h@|@�@�E@�4@w�@�@��@��@F�@.I@@�]@�h@\�@�9@��@��@S&@A @5�@&�@��@��@`�@2�@7@�+@خ@��@{J@n/@J#@��@�@-@ϫ@�S@k�@/@;@~(@q@oi@`�@D�@��@�V@Z�@@ں@��@��@��@��@l�@�@��@zx@7L@�@��@_@PH@H@C-@>B@7�@1'@(�@7@@�@P�@1�@�@��@��@�\@{�@i�@Z�@Q@L0@8�@��@�=@��@�@�@V@�@��@j@N�@*�@	�@�w@~�@iD@X�@K�@,�@!-@�@
҉@
�'@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�3B�B�GB�vB��B��B�B�B��B�UB��B�	B~�B	�B	�B	�(B
/OB
WYB
�B
��B
�!B
�`B
��B
�B
�rB
�^B
��B
�B
�B
�EB
�vB
�B
�aB
�2B
��BUB�B
�BoB/OB)�B7BKBe�B~B��B�'B��B�jB�;B�nB�XB��B�^B��B��B�B�BkBB}BBSBEB�B�B��B� B�B��B�|B�IB��Bq�B`�BHfB9B
��B
�:B
��B
~�B
^�B
=�B
 vB
B	��B	ԯB	�uB	��B	v�B	9�B	�B�RB�
B��B��B�fB��B��B�$B�pB��B�(B��B��B�B��B��B	,B	~B	�B	$B	A�B	_B	o�B	}�B	�6B	��B	��B	�;B	�B	�PB
�B
WB
�B
%`B
%�B
 B
�B
�B
;B
�B
�B
)B	�fB	�B	�oB	�B	�B	��B	�vB	�8B	�fB	�	B
�B
 B
 �B
B
%B
�B
B
!�B
#�B
$�B
%FB
OB
�B
�B	�}B	��B	��B	��B	�^B	�	B	�B	�RB	�qB
AB
B
	�B
oB
MB
�B
:B
�B
.B
oB
"B
1B
 B	��B
_B
�B	�B	��B	��B	��B	�OB	�_B	��B	��B	��B	�B	�&B	��B	�hB	� B	ѝB	�oB	�oB	ѷB	��B	ӏB	�@B	�[B	��B	�B	�mB	�SB	�B	ּB	��B	�sB	�sB	�B	�SB	өB	ӏB	��B	��B	�oB	�[B	��B	�aB	��B	�=B	��B	ڠB	�	B	�#B	��B	�B	ևB	چB	�)B	�QB	�yB	֡B	��B	��B	�@B	�{B	ևB	�mB	�gB	�$B	��B	��B	�bB	�B	��B	�xB	ȚB	�KB	̳B	̳B	ȚB	��B	͟B	�B	��B	͹B	��B	�JB	��B	��B	�DB	�B	��B	��B	�]B	�qB	��B	��B	�B	�B	��B	�'B	�aB	ðB	��B	��B	�?B	ňB	��B	�B	�%B	�EB	�KB	ɆB	�lB	�7B	�RB	�B	�B	�=B	�lB	�	B	�xB	˒B	�JB	�JB	�B	ΥB	��B	ϫB	��B	ϫB	ӏB	��B	��B	�]B	�WB	��B	�~B	��B	�IB	��B	ۦB	��B	�kB	ؓB	�1B	�CB	�!B	�!B	�!B	��B	�B	ܬB	��B	��B	ڠB	��B	�B	�~B	�!B	�!B	�OB	�B	�5B	��B	��B	�4B	�B	�4B	�B	��B	�B	�B	��B	�B	��B	�FB	��B	�B	�LB	�B	�mB	�B	�B	�B	��B	�DB	�B	�B	�B	�B	��B	�B	�6B	�B	�)B	�]B	��B	��B	�B	�hB	�nB	�B	�hB	��B	��B	��B	��B	�B	��B	�B	�?B	��B	�TB	��B	�nB	�LB	�	B	��B	�XB	��B	��B	�B	�B	��B	��B	��B	�8B	�8B	��B	��B	�B	�B	��B	�B	��B	��B	��B	�B	��B	�B	�<B	�B	�"B	�qB	��B	�]B	�(B	�(B	�]B	�BB	��B	�}B
 �B
B
UB
AB
'B
uB
B
{B
�B
�B
�B
�B
mB
�B
�B
�B
YB
�B
zB
�B
�B
�B
B
�B
	B
	�B

	B

#B
^B
B
JB
B
B
jB
�B
�B
�B
B
VB
pB
�B
pB
�B
vB
\B
�B
bB
�B
bB
�B
B
�B
:B
�B
�B
�B
[B
[B
�B
B
FB
FB
�B
�B
�B
�B
B
2B
MB
�B
�B
B
�B

B
�B
?B
YB
�B
�B
+B
�B
�B
yB
�B
�B
�B
B
7B
�B
=B
WB
�B
qB
�B
�B
�B
~B
~B
~B
�B
B
�B
 'B
 BB
 'B
 'B
 BB
!-B
!�B
!�B
"hB
"�B
"�B
"�B
#�B
#�B
$&B
$ZB
$�B
%B
%,B
%FB
&2B
&2B
&LB
&fB
%�B
&LB
&LB
&LB
&fB
'B
'�B
(�B
(�B
)DB
)_B
)yB
)�B
)�B
*B
)�B
*eB
*�B
+QB
+QB
+kB
+kB
+�B
+�B
+�B
,B
,WB
,qB
,�B
,�B
,�B
-wB
./B
-�B
.�B
.�B
.�B
/ B
/B
/iB
/�B
/�B
0!B
0;B
0!B
0!B
0B
0�B
0�B
0�B
1B
1B
1AB
1�B
1�B
1�B
1�B
2-B
2�B
3B
3�B
3�B
3�B
49B
4nB
4TB
4nB
4nB
4�B
4�B
4�B
4�B
4�B
5tB
5ZB
5�B
5�B
6+B
6�B
6�B
7B
7�B
7�B
7�B
8RB
8lB
8�B
9>B
9$B
9�B
9�B
:DB
:�B
:�B
;JB
;�B
<B
<�B
="B
=<B
=�B
=�B
=�B
>wB
>�B
>�B
?cB
?�B
@OB
@�B
@�B
AB
A B
AoB
AUB
A�B
B'B
BAB
B[B
BuB
B�B
CB
C-B
CaB
C�B
DMB
DgB
D�B
ESB
E�B
E�B
E�B
F%B
FB
E�B
E�B
FYB
G_B
GEB
G_B
G�B
HB
G�B
H1B
HKB
H1B
HfB
H�B
H�B
H�B
IB
IlB
I7B
I7B
IRB
I�B
I�B
I�B
JrB
J=B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
K)B
K)B
KxB
KxB
K�B
K�B
L~B
L�B
LdB
LJB
LJB
LdB
L�B
L�B
MPB
M6B
M�B
M�B
M�B
NpB
OB
OBB
PB
PHB
PbB
PbB
P�B
P�B
PbB
P}B
P�B
QB
QNB
Q4B
Q4B
Q4B
QhB
Q�B
R:B
R�B
S&B
S[B
S�B
S�B
TB
TaB
TaB
TFB
T{B
T{B
UMB
U�B
U�B
U�B
U�B
U�B
VmB
V�B
V�B
V�B
V�B
V�B
V�B
W$B
W?B
WYB
WsB
WsB
WsB
W�B
XB
XEB
XyB
X�B
X�B
X�B
Y1B
YKB
Y�B
Y�B
Y�B
Y�B
ZB
ZQB
ZQB
[#B
[qB
[qB
\B
\]B
\�B
\�B
]B
]IB
]B
]B
]�B
^�B
^�B
^�B
_;B
_;B
_!B
_�B
`BB
`vB
`�B
aB
aHB
a�B
a�B
a�B
a�B
a�B
a�B
bB
b4B
b4B
b�B
cB
c:B
cTB
c�B
c�B
c�B
c�B
dB
dB
dB
d&B
d�B
d�B
d�B
d�B
d�B
ezB
e�B
e�B
e�B
e�B
fB
ffB
f�B
f�B
f�B
f�B
g8B
gmB
g�B
g�B
g�B
h�B
h�B
iB
i_B
i�B
i�B
j0B
jeB
j�B
j�B
j�B
j�B
kkB
k�B
lB
k�B
l=B
l�B
l�B
l�B
l�B
mCB
mwB
m�B
n�B
o5B
o�B
o�B
pUB
pUB
p�B
p�B
p�B
qB
qB
qAB
qAB
q�B
r-B
r-B
raB
r|B
r|B
r|B
r�B
s3B
sMB
s�B
s�B
s�B
s�B
tB
tB
tB
tB
t�B
t�B
u%B
utB
u�B
u�B
vB
v+B
v�B
v�B
v�B
v�B
v�B
wLB
wLB
w�B
w�B
xB
xB
xB
xB
xB
xB
x�B
x�B
x�B
x�B
x�B
y$B
yXB
yXB
yXB
yrB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
zxB
zxB
z�B
z�B
z�B
z�B
{B
{B
{0B
{0B
{B
{B
{B
{�B
{�B
|�B
|jB
|jB
|jB
|�B
}B
}B
}"B
}"B
}qB
}�B
}�B
}�B
}�B
~(B
~(B
~]B
~wB
~�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�3B�B�GB�vB��B��B�B�B��B�UB��B�	B~�B	�B	�B	�(B
/OB
WYB
�B
��B
�!B
�`B
��B
�B
�rB
�^B
��B
�B
�B
�EB
�vB
�B
�aB
�2B
��BUB�B
�BoB/OB)�B7BKBe�B~B��B�'B��B�jB�;B�nB�XB��B�^B��B��B�B�BkBB}BBSBEB�B�B��B� B�B��B�|B�IB��Bq�B`�BHfB9B
��B
�:B
��B
~�B
^�B
=�B
 vB
B	��B	ԯB	�uB	��B	v�B	9�B	�B�RB�
B��B��B�fB��B��B�$B�pB��B�(B��B��B�B��B��B	,B	~B	�B	$B	A�B	_B	o�B	}�B	�6B	��B	��B	�;B	�B	�PB
�B
WB
�B
%`B
%�B
 B
�B
�B
;B
�B
�B
)B	�fB	�B	�oB	�B	�B	��B	�vB	�8B	�fB	�	B
�B
 B
 �B
B
%B
�B
B
!�B
#�B
$�B
%FB
OB
�B
�B	�}B	��B	��B	��B	�^B	�	B	�B	�RB	�qB
AB
B
	�B
oB
MB
�B
:B
�B
.B
oB
"B
1B
 B	��B
_B
�B	�B	��B	��B	��B	�OB	�_B	��B	��B	��B	�B	�&B	��B	�hB	� B	ѝB	�oB	�oB	ѷB	��B	ӏB	�@B	�[B	��B	�B	�mB	�SB	�B	ּB	��B	�sB	�sB	�B	�SB	өB	ӏB	��B	��B	�oB	�[B	��B	�aB	��B	�=B	��B	ڠB	�	B	�#B	��B	�B	ևB	چB	�)B	�QB	�yB	֡B	��B	��B	�@B	�{B	ևB	�mB	�gB	�$B	��B	��B	�bB	�B	��B	�xB	ȚB	�KB	̳B	̳B	ȚB	��B	͟B	�B	��B	͹B	��B	�JB	��B	��B	�DB	�B	��B	��B	�]B	�qB	��B	��B	�B	�B	��B	�'B	�aB	ðB	��B	��B	�?B	ňB	��B	�B	�%B	�EB	�KB	ɆB	�lB	�7B	�RB	�B	�B	�=B	�lB	�	B	�xB	˒B	�JB	�JB	�B	ΥB	��B	ϫB	��B	ϫB	ӏB	��B	��B	�]B	�WB	��B	�~B	��B	�IB	��B	ۦB	��B	�kB	ؓB	�1B	�CB	�!B	�!B	�!B	��B	�B	ܬB	��B	��B	ڠB	��B	�B	�~B	�!B	�!B	�OB	�B	�5B	��B	��B	�4B	�B	�4B	�B	��B	�B	�B	��B	�B	��B	�FB	��B	�B	�LB	�B	�mB	�B	�B	�B	��B	�DB	�B	�B	�B	�B	��B	�B	�6B	�B	�)B	�]B	��B	��B	�B	�hB	�nB	�B	�hB	��B	��B	��B	��B	�B	��B	�B	�?B	��B	�TB	��B	�nB	�LB	�	B	��B	�XB	��B	��B	�B	�B	��B	��B	��B	�8B	�8B	��B	��B	�B	�B	��B	�B	��B	��B	��B	�B	��B	�B	�<B	�B	�"B	�qB	��B	�]B	�(B	�(B	�]B	�BB	��B	�}B
 �B
B
UB
AB
'B
uB
B
{B
�B
�B
�B
�B
mB
�B
�B
�B
YB
�B
zB
�B
�B
�B
B
�B
	B
	�B

	B

#B
^B
B
JB
B
B
jB
�B
�B
�B
B
VB
pB
�B
pB
�B
vB
\B
�B
bB
�B
bB
�B
B
�B
:B
�B
�B
�B
[B
[B
�B
B
FB
FB
�B
�B
�B
�B
B
2B
MB
�B
�B
B
�B

B
�B
?B
YB
�B
�B
+B
�B
�B
yB
�B
�B
�B
B
7B
�B
=B
WB
�B
qB
�B
�B
�B
~B
~B
~B
�B
B
�B
 'B
 BB
 'B
 'B
 BB
!-B
!�B
!�B
"hB
"�B
"�B
"�B
#�B
#�B
$&B
$ZB
$�B
%B
%,B
%FB
&2B
&2B
&LB
&fB
%�B
&LB
&LB
&LB
&fB
'B
'�B
(�B
(�B
)DB
)_B
)yB
)�B
)�B
*B
)�B
*eB
*�B
+QB
+QB
+kB
+kB
+�B
+�B
+�B
,B
,WB
,qB
,�B
,�B
,�B
-wB
./B
-�B
.�B
.�B
.�B
/ B
/B
/iB
/�B
/�B
0!B
0;B
0!B
0!B
0B
0�B
0�B
0�B
1B
1B
1AB
1�B
1�B
1�B
1�B
2-B
2�B
3B
3�B
3�B
3�B
49B
4nB
4TB
4nB
4nB
4�B
4�B
4�B
4�B
4�B
5tB
5ZB
5�B
5�B
6+B
6�B
6�B
7B
7�B
7�B
7�B
8RB
8lB
8�B
9>B
9$B
9�B
9�B
:DB
:�B
:�B
;JB
;�B
<B
<�B
="B
=<B
=�B
=�B
=�B
>wB
>�B
>�B
?cB
?�B
@OB
@�B
@�B
AB
A B
AoB
AUB
A�B
B'B
BAB
B[B
BuB
B�B
CB
C-B
CaB
C�B
DMB
DgB
D�B
ESB
E�B
E�B
E�B
F%B
FB
E�B
E�B
FYB
G_B
GEB
G_B
G�B
HB
G�B
H1B
HKB
H1B
HfB
H�B
H�B
H�B
IB
IlB
I7B
I7B
IRB
I�B
I�B
I�B
JrB
J=B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
K)B
K)B
KxB
KxB
K�B
K�B
L~B
L�B
LdB
LJB
LJB
LdB
L�B
L�B
MPB
M6B
M�B
M�B
M�B
NpB
OB
OBB
PB
PHB
PbB
PbB
P�B
P�B
PbB
P}B
P�B
QB
QNB
Q4B
Q4B
Q4B
QhB
Q�B
R:B
R�B
S&B
S[B
S�B
S�B
TB
TaB
TaB
TFB
T{B
T{B
UMB
U�B
U�B
U�B
U�B
U�B
VmB
V�B
V�B
V�B
V�B
V�B
V�B
W$B
W?B
WYB
WsB
WsB
WsB
W�B
XB
XEB
XyB
X�B
X�B
X�B
Y1B
YKB
Y�B
Y�B
Y�B
Y�B
ZB
ZQB
ZQB
[#B
[qB
[qB
\B
\]B
\�B
\�B
]B
]IB
]B
]B
]�B
^�B
^�B
^�B
_;B
_;B
_!B
_�B
`BB
`vB
`�B
aB
aHB
a�B
a�B
a�B
a�B
a�B
a�B
bB
b4B
b4B
b�B
cB
c:B
cTB
c�B
c�B
c�B
c�B
dB
dB
dB
d&B
d�B
d�B
d�B
d�B
d�B
ezB
e�B
e�B
e�B
e�B
fB
ffB
f�B
f�B
f�B
f�B
g8B
gmB
g�B
g�B
g�B
h�B
h�B
iB
i_B
i�B
i�B
j0B
jeB
j�B
j�B
j�B
j�B
kkB
k�B
lB
k�B
l=B
l�B
l�B
l�B
l�B
mCB
mwB
m�B
n�B
o5B
o�B
o�B
pUB
pUB
p�B
p�B
p�B
qB
qB
qAB
qAB
q�B
r-B
r-B
raB
r|B
r|B
r|B
r�B
s3B
sMB
s�B
s�B
s�B
s�B
tB
tB
tB
tB
t�B
t�B
u%B
utB
u�B
u�B
vB
v+B
v�B
v�B
v�B
v�B
v�B
wLB
wLB
w�B
w�B
xB
xB
xB
xB
xB
xB
x�B
x�B
x�B
x�B
x�B
y$B
yXB
yXB
yXB
yrB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
zxB
zxB
z�B
z�B
z�B
z�B
{B
{B
{0B
{0B
{B
{B
{B
{�B
{�B
|�B
|jB
|jB
|jB
|�B
}B
}B
}"B
}"B
}qB
}�B
}�B
}�B
}�B
~(B
~(B
~]B
~wB
~�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105003  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175700  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175700  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175700                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025708  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025708  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                